#lang racket

(require racket/async-channel
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
    (R . "white rook")
    (Q . "white queen")
    (K . "white king")
    (p2 . "black pawn2")
    (b2 . "black bishop2")
    (r2 . "black rook2")
    (q2 . "black queen2")
    (k2 . "black king2")
    (P2 . "white pawn2")
    (B2 . "white bishop2")
    (R2 . "white rook2")
    (Q2 . "white queen2")
    (K2 . "white king2")))

(define prioritized-pieces
  '(p n b r q k P N B R Q K))

(define (swap a.b)
  (cons (cdr a.b) (car a.b)))

(define (remove-at name)
  (if (and (non-empty-string? name) (eqv? #\@ (string-ref name 0)))
      (substring name 1)
      name))

(define names.pieces
  (map swap pieces.names))

(define pieces
  (map car pieces.names))

(define (piece->name piece)
  (let ((name (assq piece pieces.names)))
    (and name (cdr name))))

;; table mapping pieces to usernames, the core state.
(define participants
  (make-hash))

;; list of pieces that haven't been assigned
(define (available-pieces)
  (filter (compose not lookup-piece) pieces))

;; add an entry to the participants table unless there are conflicts
(define (add-participant who piece)
  (cond ((member who (hash-values participants))
         'already-assigned)
        ((not piece) ;; was #f from randomly getting a piece
         'marbles-full)
        ((not (lookup-piece piece))
         (hash-set! participants piece who) 'ok)
        (else 'piece-taken)))

;; find out what piece someone specific has
(define (participant-piece who)
  (let ((there (member who (hash-values participants))))
    (and there
         (list-ref (hash-keys participants)
                   (- (hash-count participants) (length there))))))

;; find out who has a given piece
(define (lookup-piece piece)
  (hash-ref participants piece #f))

;; remove assignment of given person
(define (remove-participant who)
  (let ((piece (participant-piece who)))
    (and piece
         (hash-remove! participants piece))))

(define (reset-marbles)
  (set! participants (make-hash)))

;; randomly return one of the available pieces or #f if none are left
(define (random-piece)
  (define available (available-pieces))
  (define first-set (lset-intersection eq? available prioritized-pieces))
  (let* ((ps (if (null? first-set) available first-set))
         (n (length ps)))
    (and (> n 0)
         (list-ref ps (random n)))))

;; configuration to log in to twitch
(define *oauth-token*
  (symbol->string
   (with-input-from-file "token.txt"
     read)))

(define *username*
  (symbol->string
   (with-input-from-file "user.txt"
     read)))

;; network connection to twitch irc network
(define twitch-connection
  (make-parameter #f))

;; semaphore to protect state among threads
(define irl-semaphore
  (make-semaphore 1))

(define (is-moderator? message)
  (equal? "1" (cdr (assq 'mod (irc-message-tags message)))))

(define (is-room-owner? message)
  (equal? (cdr (assq 'room-id (irc-message-tags message)))
          (cdr (assq 'user-id (irc-message-tags message)))))

;; allow destructive actions only by moderators or room owner (because
;; for whatever reason they don't get mod . "1" tag?)
(define (grant-permission? message)
  (or (is-moderator? message)
      (is-room-owner? message)))

;; list of commands
(define commands
  '("play"
    "leave"
    "kick"
    "who"
    "what"
    "pieces"
    "pieces-free"
    "lineup"
    "reset"
    "commands"))

;; take arguments to ?who command and figure out piece
(define (arguments->piece args)
  (match args
    (`(,fen-char)
     (string->symbol fen-char))
    (`(,color ,english-name)
     (define name.piece
       (assoc (string-join (list color english-name))
              names.pieces))
     (and name.piece (cdr name.piece)))
    (_ #f)))

;; handle commands and return response text
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
       (`("?kick" ,pisser)
        (let ((pisser (remove-at pisser)))
          (cond ((or (is-moderator? message)
                     (is-room-owner? message)
                     (equal? pisser who))
                 (remove-participant pisser)
                 (format "@~a left the game" pisser))
                (else
                 (format "@~a only moderators or ~a can kick @~a"
                         who pisser pisser)))))
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
               (result (add-participant who piece)))
          (match result
            ('marbles-full
             (format "@~a irl marbbies is full" who))
            (_ #f))))
       ('("?pieces")
        (format "@~a ~a"
                who
                (string-join (map piece->name (hash-keys participants)) ", ")))
       ('("?pieces-free")
        (format "@~a remaining pieces: ~a"
                who
                (string-join (map piece->name (available-pieces)) ", ")))
       ('("?lineup")
        (format "@~a the current lineup: ~a"
                who
                (string-join (map (lambda (p.w)
                                    (format "~a: ~a"
                                            (piece->name (car p.w))
                                            (cdr p.w)))
                                  (hash->list participants))
                             ", ")))
       ('("?reset")
        (cond ((or (is-moderator? message)
                   (is-room-owner? message))
               (reset-marbles)
               (format "irl marbles has been reset by @~a" who))
              (else
               (format "@~a the command \"?reset\" is only available to moderators"
                       who))))
       ('("?commands")
        (format "@~a the commands are: ~a"
                who
                (string-join commands ", ")))
       (_ #f))) ;; unrecognized command/not applicable
    (_ #f))) ;; other types of messages

;; respond to applicable messages
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

;; connect to twitch and grab connection in twitch-connection parameter
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
  (irc-join-channel c "#spennythompson"))

;; main loop
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
