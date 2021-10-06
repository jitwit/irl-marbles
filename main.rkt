#lang racket

(require net/http-easy
         racket/async-channel
         (except-in srfi/1 delete)
         
         "irc.rkt")

(define pieces->names
  '((p "black pawn")
    (n "black knight")
    (b "black bishop")
    (r "black rook")
    (q "black queen")
    (k "black king")
    (P "white pawn")
    (N "white knight")
    (B "white bishop")
    (R "whtie rook")
    (Q "white queen")
    (K "white king")))

(define pieces
  (map car pieces->names))

(define (piece->name piece)
  (let ((name (assq piece pieces->names)))
    (and name (cadr name))))

(define participants
  (make-hash))

(define (available-pieces)
  (filter (lambda (p)
            (not (hash-ref participants p #f)))
          pieces))

(define add-messages
  '(ok already-assigned piece-taken marbles-full))

(define (add-participant who piece)
  (cond ((member who (hash-values participants)) 'already-assigned)
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

(define C (void))

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
  (set! C c)
  (irc-send-command C "CAP REQ" ":twitch.tv/commands")
  (irc-send-command C "CAP REQ" ":twitch.tv/tags")
  (irc-join-channel C (string-append "#" *username*))
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

(define (respond-to-message message where what)
  (let ((who (cdr (assq 'display-name (irc-message-tags message)))))
    (and who
         (match (string-split what)
           ('("?play")
            ;; todo mutex or something 
            (let* ((piece (random-piece))
                   (result (add-participant who (random-piece))))
              (match result
                ('ok
                 (irc-send-message C
                                   where
                                   (format "@~a you have the ~a"
                                           who
                                           (piece->name piece))))
                ('already-assigned
                 (irc-send-message C
                                   where
                                   (format "@~a you already have a piece"
                                           who)))
                ('marbles-full
                 (irc-send-message C
                                   where
                                   (format "@~a marbbies is full"
                                           who)))))
            (write participants) (newline))
           ('("?leave")
            (remove-participant who)
            (irc-send-message C
                              where
                              (format "@~a left the game"
                                      who))
            (write participants) (newline))
           ('("?what")
            (let ((piece (participant-piece who)))
              (irc-send-message C
                                where
                                (if piece
                                    (format "@~a you have the ~a"
                                            who
                                            (and piece
                                                 (piece->name piece)))
                                    (format "@~a you are not in the irl marbbies"
                                            who)))
              (write participants)) (newline))
           (`("?who" ,piece)
            (let* ((piece (string->symbol piece))
                   (pig (lookup-piece piece)))
              (write (list pig piece)) (newline)
              (cond ((not (member piece pieces))
                     (irc-send-message C
                                       where
                                       (format "@~a, ~a is not a piece. expecting one of: ~a"
                                               who
                                               piece
                                               (string-join
                                                (map symbol->string pieces)
                                                ", "))))
                    (else
                     (irc-send-message C
                                       where
                                       (if pig
                                           (format "@~a, ~a has the ~a"
                                                   who
                                                   pig
                                                   (piece->name piece))
                                           (format "@~a, ~a isn't taken"
                                                   (piece->name piece)
                                                   who)))))
              (write participants)) (newline))
           (`("?force" ,who)
            (let* ((piece (random-piece))
                   (result (add-participant who (random-piece))))
              (match result
                ('marbles-full
                 (irc-send-message C
                                   where
                                   (format "@~a marbbies is full"
                                           who)))
                (_ (void)))))
           ('("?pieces")
            (irc-send-message C
                              where
                              (format "@~a ~a"
                                      who
                                      (string-join
                                       (map piece->name (hash-keys participants))
                                       ", "))))
           ('("?commands")
            (irc-send-message C
                              where
                              (format "@~a the commands are: ~a"
                                      who
                                      (string-join
                                       commands
                                       ", "))))
           ('("?pieces-free")
            (irc-send-message C
                              where
                              (format "@~a ~a"
                                      who
                                      (string-join
                                       (map piece->name (available-pieces))
                                       ", "))))
           ('("?reset")
            (when (is-moderator? message)
              ;; todo improve conditions
              (reset-marbles)
              (irc-send-message C
                                where
                                (format "irl marbles has been reset by @~a"
                                        who))))
           (_
            (write (list where what)) (newline)
            (write message) (newline))))))

(define (gogo)
  (let loop ()
    (define message (async-channel-get (irc-connection-incoming C)))
    (match message
      ((irc-message _ _ "PRIVMSG" `(,where ,what)  _)
       (thread
        (lambda ()
          (respond-to-message message where what))))
      (_
       (write message)
       (newline)))
    (loop)))

(define (main)
  (boot)
  (gogo))
