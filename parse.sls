;; -*- mode: scheme; coding: utf-8 -*-
;; An IRC parser library useful for both IRC clients and servers.
;; Copyright © 2008, 2009, 2010, 2012, 2017 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

;; IRC protocol parsing.

(library (irc-protocol parse)
  (export irc-parse-condition?
          parse-message parse-message-bytevector
          extended-prefix? prefix-split prefix-nick
          parse-isupport isupport-defaults
          string-irc=? string-upcase-irc string-downcase-irc
          irc-match? parse-channel-mode)
  (import (rnrs)
          (only (srfi :1 lists) make-list drop-right append-map
                map-in-order)
          (only (srfi :13 strings) string-index string-map)
          (industria bytevectors)
          (industria text strings)
          (irc-protocol private))

  (define-condition-type &irc-parse &condition
    make-irc-parse-condition irc-parse-condition?)

  (define (parse-error who msg . irritants)
    (raise (condition
            (make-who-condition who)
            (make-message-condition msg)
            (make-irritants-condition irritants)
            (make-irc-parse-condition))))

  (define (ascii->string bv)
    (list->string (map (lambda (b) (integer->char (fxand #x7f b)))
                       (bytevector->u8-list bv))))

  (define (parse-cmd who cmd)
    (cond ((char-numeric? (string-ref cmd 0))
           (unless (and (= (string-length cmd) 3)
                        (for-all char-numeric? (string->list cmd)))
             (parse-error who "Malformed numerical command" cmd))
           (string->number cmd))
          (else
           (string->symbol cmd))))

  ;; (parse-message ":irc.example.net PONG irc.example.net :irc.example.net")
  ;; => "irc.example.net"
  ;; => PONG
  ;; => ("irc.example.net" "irc.example.net")

  ;; (parse-message ":user!ident@example.com PRIVMSG #test :this is a test")
  ;; => "user!ident@example.com"
  ;; => PRIVMSG
  ;; => ("#test" "this is a test")

  ;; (parse-message "PING irc.example.net" "irc.example.net")
  ;; => "irc.example.net"
  ;; => PING
  ;; => ("irc.example.net")

  ;; (parse-message "PING irc.example.net")
  ;; => #f
  ;; => PING
  ;; => ("irc.example.net")

  ;; The second return value is the command, and it is either a number
  ;; or a symbol.
  (define parse-message
    (case-lambda
      ((msg)
       (parse-message msg #f))
      ((msg remote-server)
       (define who 'parse-message)
       (let-values (((prefix start)
                     (if (string-index msg #\: 0 1)
                         (let ((idx (string-index msg #\space 1)))
                           (values (substring msg 1 idx)
                                   (+ idx 1)))
                         (values remote-server 0))))
         (define (skip-space msg i)
           (if (or (>= i (string-length msg))
                   (not (char=? #\space (string-ref msg i))))
               i
               (skip-space msg (+ i 1))))
         (define (return args)
           (let* ((args (reverse args))
                  (cmd (car args)))
             ;; Ignore this error, because some ircds sends more parameters in ISUPPORT.
             ;; (when (> (length (cdr args)) 15)
             ;;   (parse-error 'parse-message
             ;;                "Too many parameters"
             ;;                prefix cmd args))
             (values prefix (parse-cmd who cmd) (cdr args))))
         (let loop ((start start)
                    (tokens '()))
           (cond ((= start (string-length msg))
                  (return tokens))
                 ((char=? #\: (string-ref msg start))
                  ;; Everything behind : is the last parameter
                  (return
                   (cons (substring msg (+ start 1) (string-length msg))
                         tokens)))
                 ((string-index msg #\space start)
                  => (lambda (end)
                       (loop (skip-space msg end)
                             (cons (substring msg start end) tokens))))
                 (else
                  (return
                   (cons (substring msg start (string-length msg))
                         tokens)))))))))

  ;; The parse-message-bytevector function reads an IRC message from
  ;; the given bytevector and only within the given range. Use this
  ;; function if you don't want to transcode messages from latin-1 or
  ;; utf-8. The return values are the same as for parse-message, but
  ;; the parameters are returned as bytevectors.

  ;; This function is useful for a server that shouldn't transcode the
  ;; message parameter of PRIVMSGs etc. Different channels often use
  ;; different encodings.
  (define parse-message-bytevector
    (case-lambda
      ((msg)
       (parse-message-bytevector msg 0 (bytevector-length msg) #f))
      ((msg bvstart)
       (parse-message-bytevector msg bvstart (bytevector-length msg) #f))
      ((msg bvstart bvend)
       (parse-message-bytevector msg bvstart bvend #f))
      ((msg bvstart bvend remote-server)
       (define who 'parse-message-binary)
       (let-values (((prefix start)
                     (if (bytevector-u8-index msg (char->integer #\:)
                                              bvstart (min (+ bvstart 1) bvend))
                         (let ((idx (bytevector-u8-index msg (char->integer #\space)
                                                         (+ bvstart 1) bvend)))
                           (values (subbytevector msg (+ bvstart 1) idx)
                                   (+ idx 1)))
                         (values remote-server bvstart))))
         (define (skip-space msg i)
           (if (or (>= i bvend)
                   (not (= (char->integer #\space) (bytevector-u8-ref msg i))))
               i
               (skip-space msg (+ i 1))))
         (define (return args)
           (let* ((args (reverse args))
                  (cmd (ascii->string (car args))))
             ;; (when (> (length (cdr args)) 15)
             ;;   (parse-error 'parse-message-binary
             ;;                "Too many parameters"
             ;;                prefix cmd args))
             (values (if (bytevector? prefix) (ascii->string prefix) prefix)
                     (parse-cmd who cmd)
                     (cdr args))))
         (let loop ((start start)
                    (tokens '()))
           (cond ((= start bvend)
                  (return tokens))
                 ((= (char->integer #\:) (bytevector-u8-ref msg start))
                  ;; Everything behind : is the last parameter
                  (return
                   (cons (subbytevector msg (+ start 1) bvend)
                         tokens)))
                 ((bytevector-u8-index msg (char->integer #\space) start)
                  => (lambda (end)
                       (loop (skip-space msg end)
                             (cons (subbytevector msg start end) tokens))))
                 (else
                  (return
                   (cons (subbytevector msg start bvend)
                         tokens)))))))))

;;; Procedures for parsing prefixes and so on. These deal only with
;;; strings.

  (define (extended-prefix? p)
    (and (string? p)
         (string-index p #\!)
         (string-index p #\@)
         #t))

  ;; This function splits an extended prefix.

  ;; (prefix-split "nickname!username@hostname")
  ;; => "nickname"
  ;; => "username"
  ;; => "hostname"
  (define (prefix-split prefix)
    (let* ((ex (string-index prefix #\!))
           (at (string-index prefix #\@ ex)))
      (values (substring prefix 0 ex)
              (substring prefix (+ 1 ex) at)
              (substring prefix (+ 1 at) (string-length prefix)))))

  (define (prefix-nick prefix)
    (let-values (((nick user host) (prefix-split prefix)))
      nick))

  (define (char-ascii-upcase c)
    (let ((i (char->integer c)))
      (if (<= 97 i 122)
          (integer->char (+ (- i 97) 65))
          c)))

  (define (char-rfc1459-upcase c)
    ;; ISO646-SE2, Swedish ASCII, except ` and @ are left alone.
    (let ((i (char->integer c)))
      (if (<= 97 i 126)
          (integer->char (+ (- i 97) 65))
          c)))

  (define (char-strict-rfc1459-upcase c)
    (let ((i (char->integer c)))
      (if (<= 97 i 125)
          (integer->char (+ (- i 97) 65))
          c)))

  (define (string-upcase-irc str mapping)
    (case mapping
      ((rfc1459)
       (string-map char-rfc1459-upcase str))
      ((ascii)
       (string-map char-ascii-upcase str))
      ((strict-rfc1459)
       (string-map char-strict-rfc1459-upcase str))
      (else
       (string-upcase str))))

  (define (char-ascii-downcase c)
    (let ((i (char->integer c)))
      (if (<= 65 i 90)
          (integer->char (+ (- i 65) 97))
          c)))

  (define (char-rfc1459-downcase c)
    (let ((i (char->integer c)))
      (if (<= 65 i 94)
          (integer->char (+ (- i 65) 97))
          c)))

  (define (char-strict-rfc1459-downcase c)
    (let ((i (char->integer c)))
      (if (<= 65 i 93)
          (integer->char (+ (- i 65) 97))
          c)))

  (define (string-downcase-irc str mapping)
    (case mapping
      ((rfc1459)
       (string-map char-rfc1459-downcase str))
      ((ascii)
       (string-map char-ascii-downcase str))
      ((strict-rfc1459)
       (string-map char-strict-rfc1459-downcase str))
      (else
       (string-downcase str))))

  (define string-irc=?
    (case-lambda
      ((x y mapping)
       (string=? (string-downcase-irc x mapping)
                 (string-downcase-irc y mapping)))
      ((x y)
       (string-irc=? x y 'rfc1459))))

  ;; Wildcard matching as per section 2.5 of RFC2812. Suitable for
  ;; matching masks like foo*!*@example.com against prefixes. Uses
  ;; lots of evil pruning and backtracking that makes it good enough
  ;; for many real-world patterns and inputs.
  (define (irc-match? pattern input)
    (define (avancez pattern input plen ilen)
      (let lp ((p 0) (i 0) (p* #f) (i* #f))
        ;; (write (list (and (< p plen) (substring pattern p plen))
        ;;              (and (< i ilen) (substring input i ilen))
        ;;              (and p* (substring pattern p* plen))
        ;;              (and i* (substring input i* ilen))))
        ;; (newline)
        (let ((pc (and (not (= p plen)) (char-ascii-downcase (string-ref pattern p))))
              (ic (and (not (= i ilen)) (char-ascii-downcase (string-ref input i)))))
          (cond ((not ic)               ;no more input
                 (cond ((not pc) #t)    ;no more pattern
                       ((eqv? pc #\*)
                        (lp (+ p 1) i p* i*))
                       ((eqv? i* ilen) #f) ;can't backtrack
                       (i*                 ;backtrack
                        (lp p* i* p* (+ i* 1)))
                       (else #f)))    ;more pattern, but no more input
                ((and pc (or (char=? ic pc)
                             (char=? pc #\?)) ;wildone
                      (not (char=? #\* ic pc)))
                 (lp (+ p 1) (+ i 1) p* i*)) ;pattern and input matched
                ;; Pattern and input didn't match, or end of pattern
                ((eqv? pc #\*)         ;wildmany
                 (or (= (+ p 1) plen)  ;* at the end matches all input
                     (lp (+ p 1) i (+ p 1) i)))
                ((and (eqv? pc #\\)     ;escapes
                      (not (= (+ p 1) plen))
                      (char=? ic (char-ascii-downcase (string-ref pattern (+ p 1)))))
                 (lp (+ p 2) (+ i 1) p* i*))
                ((and pc (not (char=? pc #\\))
                      (not (string-index input pc i)))
                 #f)        ;prune (seems to work pretty well for IRC)
                ((eqv? p p*)            ;backtrack
                 (lp p* (+ i 1) p* i*))
                (p*                     ;backtrack
                 (lp (if (char=? ic (char-ascii-downcase (string-ref pattern p*)))
                         (+ p* 1)       ;prune
                         p*)
                     (+ i 1) p* i*))
                (else #f)))))         ;more input, but no more pattern
    (let ((plen (string-length pattern))
          (ilen (string-length input)))
      (if (and (not (zero? plen))
               (not (zero? ilen))
               (not (memv (string-ref pattern (- plen 1)) '(#\? #\*)))
               (not (char=? (char-ascii-downcase (string-ref pattern (- plen 1)))
                            (char-ascii-downcase (string-ref input (- ilen 1))))))
          #f                            ;prune
          (avancez pattern input plen ilen))))

  ;; http://www.irc.org/tech_docs/005.html

  ;; (parse-isupport '("CHANLIMIT=#&:100" "CHANNELLEN=50" "EXCEPTS=e" "INVEX=I"
  ;;                   "CHANMODES=eIb,k,l,imnpst" "KNOCK" "AWAYLEN=160" "ELIST=CMNTU"
  ;;                   "SAFELIST" "are supported by this server"))
  ;; =>
  ;; '((CHANLIMIT . ((#\# . 100) (#\& . 100))) (CHANNELLEN . 50)
  ;;   (EXCEPTS . #\e) (INVEX . #\I)
  ;;   (CHANMODES . ("eIb" "k" "l" "imnpst")) (KNOCK . #t)
  ;;   (AWAYLEN . 160) (ELIST . (#\C #\M #\N #\T #\U))
  ;;   (SAFELIST . #t))

  ;; (parse-isupport '("CALLERID" "CASEMAPPING=rfc1459" "DEAF=D" "KICKLEN=160" "MODES=4"
  ;;                   "NICKLEN=15" "PREFIX=(ohv)@%+" "STATUSMSG=@%+" "TOPICLEN=350"
  ;;                   "NETWORK=foo" "MAXLIST=beI:200" "MAXTARGETS=4" "CHANTYPES=#&"
  ;;                   "are supported by this server"))
  ;; =>
  ;; '((CALLERID . #t) (CASEMAPPING . rfc1459) (DEAF . #\D)
  ;;   (KICKLEN . 160) (MODES . 4) (NICKLEN . 15)
  ;;   (PREFIX . ((#\o . #\@) (#\h . #\%) (#\v . #\+)))
  ;;   (STATUSMSG . (#\@ #\% #\+)) (TOPICLEN . 350) (NETWORK . "foo")
  ;;   (MAXLIST . ((#\b . 200) (#\e . 200) (#\I . 200)))
  ;;   (MAXTARGETS . 4) (CHANTYPES . (#\# #\&)))
  (define (parse-isupport args)
    (define (mode-ref arg def)
      (if (null? (cdr arg)) def (string-ref (cadr arg) 0)))
    (define (parse pv)
      (let ((pv (string-split pv #\= 1)))
        (let ((parameter (string->symbol (car pv))))
          (cons parameter
                (case parameter
                  ((CASEMAPPING) (string->symbol (cadr pv)))
                  ((MODES
                    MAXCHANNELS NICKLEN MAXBANS TOPICLEN
                    KICKLEN CHANNELLEN SILENCE AWAYLEN
                    MAXTARGETS WATCH
                    MONITOR)            ;ratbox
                   (if (null? (cdr pv)) +inf.0 (string->number (cadr pv))))
                  ((INVEX) (mode-ref pv #\I))
                  ((EXCEPTS) (mode-ref pv #\e))
                  ((DEAF) (mode-ref pv #\D))
                  ((CALLERID) (mode-ref pv #\g))
                  ((CHANMODES)
                   (append-map (lambda (modes type)
                                 (map cons* (string->list modes)
                                      (make-list (string-length modes) type)))
                               (string-split (cadr pv) #\,)
                               '(address ;takes a nick or address
                                 always  ;always has a parameter
                                 only   ;only has a parameter when set
                                 never))) ;never has a parameter
                  ((CHANTYPES ELIST STATUSMSG) (string->list (cadr pv)))
                  ((CHANLIMIT MAXLIST)
                   (append-map (lambda (x)
                                 (let ((chars:number (string-split x #\: 1)))
                                   (map cons* (string->list (car chars:number))
                                        (make-list (string-length
                                                    (car chars:number))
                                                   (string->number
                                                    (cadr chars:number) 10)))))
                               (string-split (cadr pv) #\,)))
                  ((PREFIX)
                   (let ((m/s (string-split (cadr pv) #\) 1 1)))
                     (map cons* (string->list (car m/s))
                          (string->list (cadr m/s)))))
                  (else
                   ;; Don't rely on the format of these ones
                   (if (null? (cdr pv)) #t (cadr pv))))))))
    (map parse (drop-right args 1)))

  ;; http://www.irc.org/tech_docs/draft-brocklesby-irc-isupport-03.txt
  (define (isupport-defaults)
    (parse-isupport
     '("CASEMAPPING=rfc1459" "CHANNELLEN=200" "CHANTYPES=#&"
       "MODES=3" "NICKLEN=9" "PREFIX=(ov)@+"
       "CHANMODES=beIqd,k,lfJ,imnpst"   ;nicked from irssi
       "are the defaults")))

  ;; Parser for channel MODE commands. Please note that ircds normally
  ;; filter the changes so that multiple conflicting changes are
  ;; replaced with the first or the last one. Some ircds allow multiple
  ;; conflicting changes of the prefix/address types.
  (define (parse-channel-mode PREFIX CHANMODES modes)
    (define (type char)
      ;; address: list management (missing parameter is a query);
      ;; always: always has a parameter; only: only when being set;
      ;; never: never has a parameter.
      (cond ((eqv? char #\+) '+)
            ((eqv? char #\-) '-)
            ((assv char PREFIX) 'prefix)
            ((assv char CHANMODES) => cdr)
            (else 'never)))
    (define (get-param)
      (and (not (null? modes))
           (let ((param (car modes)))
             (set! modes (cdr modes))
             param)))
    (define modifier '+)
    (define (parse-char c)
      (case (type c)
        ((+) (set! modifier '+) #f)
        ((-) (set! modifier '-) #f)
        ((never)
         (list modifier c 'channel))
        ((only)
         (list modifier c (if (eqv? modifier '+) (get-param) #f)))
        ((always)
         (list modifier c (get-param)))
        ((address prefix)
         (cond ((get-param) => (lambda (p) (list modifier c p)))
               ((eq? (type c) 'prefix) #f)
               (else (list '? c 'channel))))
        (else #f)))
    (let lp ((ret '()))
      (cond ((get-param) =>
             (lambda (modes)
               (lp (append ret
                           (filter list?
                                   (map-in-order parse-char
                                                 (string->list modes)))))))
            (else ret)))))
