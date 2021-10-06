#lang racket

(require net/http-easy
         racket/async-channel
         (except-in srfi/1 delete)
         
         "irc.rkt")

(define pieces.names
  '((p . "black pawn")
    (n . "black knight")
    (b . "black bishop")
    (r . "black rook")
    (q . "black queen")
    (k . "black king")
    (P . "white pawn")
    (N . "white knight")
    (B . "white bishop")
    (R . "whtie rook")
    (Q . "white queen")
    (K . "white king")))

(define (swap a.b)
  (cons (cdr a.b) (car a.b)))

(define names.pieces
  (map swap pieces.names))

(define pieces
  (map car pieces.names))

(define (piece->name piece)
  (let ((name (assq piece pieces.names)))
    (and name (cdr name))))

(define participants
  (make-hash))

(define (available-pieces)
  (filter (lambda (p)
            (not (lookup-piece p)))
          pieces))

(define add-messages
  '(ok already-assigned piece-taken marbles-full))

(define (add-participant who piece)
  (cond ((member who (hash-values participants))
         'already-assigned)
        ((not piece) ;; was #f from randomly getting a piece
         'marbles-full)
        ((not (lookup-piece piece))
         (hash-set! participants piece who) 'ok)
        (else 'piece-taken)))

(define (participant-piece who)
  (let ((there (member who (hash-values participants))))
    (and there
         (list-ref (hash-keys participants)
                   (- (hash-count participants) (length there))))))

(define (lookup-piece piece)
  (hash-ref participants piece #f))

(define (remove-participant who)
  (let ((piece (participant-piece who)))
    (and piece
         (hash-remove! participants piece))))

(define (reset-marbles)
  (set! participants (make-hash)))

(define (random-piece)
  (let* ((ps (available-pieces))
         (n (length ps)))
    (and (> n 0)
         (list-ref ps (random n)))))

(define *oauth-token*
  (symbol->string
   (with-input-from-file "token_j.txt"
     read)))

(define *username*
  (symbol->string
   (with-input-from-file "user_j.txt"
     read)))

(define twitch-connection
  (make-parameter #f))

(define irl-semaphore
  (make-semaphore 1))

(define (boot)
  (define-values (c ready)
    (irc-connect "irc.chat.twitch.tv"
                 6697
                 *username*
                 *username*
                 *username*
                 #:ssl 'auto
                 #:password (string-append "oauth:" *oauth-token*)))
  (sync ready)
  (twitch-connection c)
  (irc-send-command c "CAP REQ" ":twitch.tv/commands")
  (irc-send-command c "CAP REQ" ":twitch.tv/tags")
  (irc-join-channel c (string-append "#" *username*))
  ;; (irc-join-channel C "#spennythompson")
  )

(define (is-moderator? message)
  (equal? "1" (cdr (assq 'mod (irc-message-tags message)))))

(define commands
  '("play"
    "leave"
    "who"
    "what"
    "pieces"
    "pieces-free"
    "reset"
    "commands"))

(define (arguments->piece args)
  (write args) (newline)
  (match args
    (`(,fen-char)
     (string->symbol fen-char)
     (write (string->symbol fen-char))
     (newline)
     (string->symbol fen-char))
    (`(,color ,english-name)
     (define name.piece
       (assoc (string-join (list color english-name))
              names.pieces))
     (and name.piece (cdr name.piece)))
    (_ #f)))

(define (response-message message)
  (match message
    ((irc-message tags pref "PRIVMSG" `(,where ,what) message-whole)
     (define who
       (cdr (assq 'display-name (irc-message-tags message))))
     (match (string-split what)
       ('("?play")
        (let* ((piece (random-piece))
               (result (add-participant who piece)))
          (match result
            ('ok
             (format "@~a you have the ~a"
                     who
                     (piece->name piece)))
            ('already-assigned
             (format "@~a you already have a piece"
                     who))
            ('marbles-full
             (format "@~a marbbies is full"
                     who)))))
       ('("?leave")
        (remove-participant who)
        (format "@~a left the game" who))
       ('("?what")
        (let ((piece (participant-piece who)))
          (if piece
              (format "@~a you have the ~a"
                      who
                      (and piece
                           (piece->name piece)))
              (format "@~a you are not in the current irl marbbies" who))))
       (`("?who" . ,args)
        (let* ((piece (arguments->piece args))
               (pig (lookup-piece piece)))
          (cond ((not (member piece pieces))
                 (format "@~a, ~a is not a piece. expecting one of: ~a"
                         who
                         piece
                         (string-join (map symbol->string pieces) ", ")))
                (else
                 (if pig
                     (format "@~a @~a has the ~a"
                             who
                             pig
                             (piece->name piece))
                     (format "@~a the ~a isn't taken"
                             who
                             (piece->name piece)))))))
       (`("?force" ,who)
        (let* ((piece (random-piece))
               (result (add-participant who (random-piece))))
          (match result
            ('marbles-full
             (format "@~a irl marbbies is full" who))
            (_ #f))))
       ('("?pieces")
        (format "@~a ~a"
                who
                (string-join (map piece->name (hash-keys participants)) ", ")))
       ('("?commands")
        (format "@~a the commands are: ~a"
                who
                (string-join commands ", ")))
       ('("?pieces-free")
        (format "@~a remaining pieces: ~a"
                who
                (string-join (map piece->name (available-pieces)) ", ")))
       ('("?reset")
        (cond ((is-moderator? message)
               ;; todo improve conditions
               (reset-marbles)
               (format "irl marbles has been reset by @~a" who))
              (else
               (format "@~a the command \"?reset\" is only available to moderators"
                       who))))
       (_ #f))) ;; unrecognized command/not applicable
    (_ #f))) ;; other kinds of messages

(define (respond-to-message message)
  (write message) (newline)
  (match message
    ((irc-message _ _ "PRIVMSG" `(,where ,what)  _)
     (define response
       (call-with-semaphore irl-semaphore
                            (lambda ()
                              (response-message message))))
     (when response
       (irc-send-message (twitch-connection) where response)))
    (_ (void))))

(define (gogo)
  (let loop ()
    (define message
      (async-channel-get (irc-connection-incoming (twitch-connection))))
    (thread
     (lambda ()
       (respond-to-message message)))
    (loop)))

(define (main)
  (boot)
  (gogo))
