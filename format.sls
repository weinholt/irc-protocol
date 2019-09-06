;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2008-2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; IRC protocol message formatting.

(library (irc-protocol format)
  (export irc-format-condition?
          format-message-raw format-message-and-verify
          format-message-with-whitewash)
  (import (rnrs)
          (only (srfi :1 lists) make-list drop-right append-map
                map-in-order)
          (only (srfi :13 strings) string-index string-map)
          (industria bytevectors)
          (industria strings)
          (only (irc-protocol parse) irc-parse-condition? parse-message-bytevector)
          (irc-protocol private))

  (define-condition-type &irc-format &condition
    make-irc-format-condition irc-format-condition?)

  (define (format-error who msg . irritants)
    (raise (condition
            (make-who-condition who)
            (make-message-condition msg)
            (make-irritants-condition irritants)
            (make-irc-format-condition))))

  ;; Output a message to the given port. `prefix' specifies the prefix
  ;; part of the message, i.e. name of the server that originated the
  ;; message. IRC clients should use #f as prefix. The `cmd' is a
  ;; symbol or string representing an IRC command, but it can also be
  ;; an integer (should be between 000 and 999) that will be
  ;; zero-padded. Only servers send numerical commands. The rest of
  ;; the arguments are the parameters for the given command, which can
  ;; be either numbers, strings or bytevectors. Only the last of the
  ;; parameters can contain whitespace.

  ;; (call-with-values open-bytevector-output-port
  ;;   (lambda (port extract)
  ;;     (format-message-raw port (utf-8-codec)
  ;;                         "irc.example.net" 001 "luser"
  ;;                         "Welcome to the Example Internet Relay Chat Network luser")
  ;;     (bytevector->string (extract) (make-transcoder (utf-8-codec)))))

  ;; => ":irc.example.net 001 luser :Welcome to the Example Internet Relay Chat Network luser\r\n"

  ;; (call-with-values open-bytevector-output-port
  ;;   (lambda (port extract)
  ;;     (format-message-raw port (utf-8-codec)
  ;;                         #f 'PRIVMSG "#example"
  ;;                         "This is a message to a channel")
  ;;     (bytevector->string (extract) (make-transcoder (utf-8-codec)))))

  ;; => "PRIVMSG #example :This is a message to a channel\r\n"

  (define (format-message-raw port codec prefix cmd . parameters)
    (let ((t (make-transcoder codec)))
      (when prefix
        (put-u8 port (char->integer #\:))
        (put-bytevector port (parameter->bytevector prefix t))
        (put-u8 port (char->integer #\space)))
      (cond ((number? cmd)
             (when (< cmd 10)
               (put-u8 port (char->integer #\0)))
             (when (< cmd 100)
               (put-u8 port (char->integer #\0)))
             (put-bytevector port (string->bytevector
                                   (number->string cmd) t)))
            ((symbol? cmd)
             (put-bytevector port (string->bytevector
                                   (symbol->string cmd) t)))
            (else
             (put-bytevector port (string->bytevector cmd t))))
      (let lp ((p parameters))
        (cond ((null? p)
               (if #f #f))
              ((null? (cdr p))
               ;; Last parameter, if it contains a space character
               ;; then prefix the whole parameter with a colon.
               (put-u8 port (char->integer #\space))
               (let ((b (parameter->bytevector (car p) t)))
                 (when (or (bytevector-u8-index b (char->integer #\space))
                           (zero? (bytevector-length b)))
                   (put-u8 port (char->integer #\:)))
                 (put-bytevector port b)))
              (else
               (put-u8 port (char->integer #\space))
               (put-bytevector port (parameter->bytevector (car p) t))
               (lp (cdr p)))))
      (put-bytevector port '#vu8(13 10))))

  ;; The format-message-and-verify function outputs an IRC message to
  ;; the given port, but first it reads back the formatted message and
  ;; compares it with the input to make sure it is the same.

  ;; Example of something that works fine:

  ;; (call-with-values open-bytevector-output-port
  ;;   (lambda (port extract)
  ;;     (format-message-and-verify port (utf-8-codec)
  ;;                                "irc.example.net" 'NOTICE "ErrantUser"
  ;;                                "The server has taken a liking to you")
  ;;     (bytevector->string (extract) (make-transcoder (utf-8-codec)))))

  ;; => ":irc.example.net NOTICE ErrantUser :The server has taken a liking to you\r\n"

  ;; Here DrAbuse tried to make our bot respond with an embedded newline:

  ;; (call-with-values open-bytevector-output-port
  ;;   (lambda (port extract)
  ;;     (format-message-and-verify port (utf-8-codec)
  ;;                                #f 'NOTICE "#abusers"
  ;;                                "DrAbuse: your answer is: 123\r\nJOIN 0")
  ;;     (bytevector->string (extract) (make-transcoder (utf-8-codec)))))

  ;; The above raises an &irc-format exception, because of the
  ;; newline.

  (define (format-message-and-verify port codec prefix cmd . parameters)
    (let ((bv (call-with-bytevector-output-port
                (lambda (p)
                  (apply format-message-raw p codec prefix cmd parameters))))
          (t (make-transcoder codec (eol-style none))))
      (call-with-values
        (lambda ()
          (guard (con
                  ((irc-parse-condition? con)
                   (format-error 'format-message-and-verify
                                 (condition-message con)
                                 prefix cmd parameters)))
            (parse-message-bytevector bv 0 (min (bytevector-u8-index bv (char->integer #\return))
                                                (bytevector-u8-index bv (char->integer #\linefeed))))))
        (lambda (prefix* cmd* parameters*)
          (let ((parameters (map (lambda (x) (parameter->bytevector x t)) parameters)))
            (if (equal? (list prefix* cmd* parameters*)
                        (list prefix cmd parameters))
                (put-bytevector port bv)
                (format-error 'format-message-and-verify
                              "Malformed message"
                              'wanted prefix cmd parameters
                              'got prefix* cmd* parameters*)))))))

  ;; format-message-with-whitewash replaces carriage return and
  ;; newlines in the parameters. Hopefully you will make sure yourself
  ;; that the prefix, the command and all but the last parameter are
  ;; sane.

  ;; (utf8->string
  ;;  (call-with-bytevector-output-port
  ;;    (lambda (port)
  ;;      (format-message-with-whitewash port (utf-8-codec)
  ;;                                     #f 'NOTICE "#abusers"
  ;;                                     "DrAbuse: your answer is: 123\r\nJOIN 0"))))

  ;; => "NOTICE #abusers :DrAbuse: your answer is: 123  JOIN 0\r\n"
  (define (format-message-with-whitewash port codec prefix cmd . parameters)
    (define (wash bv)
      (call-with-bytevector-output-port
        (lambda (p)
          (do ((i 0 (fx+ i 1)))
              ((= i (bytevector-length bv)))
            (let ((b (bytevector-u8-ref bv i)))
              (put-u8 p (if (memv b '(#x0a #x0d #x00)) #x20 b)))))))
    (let ((t (make-transcoder codec (eol-style none))))
      (apply format-message-raw port codec prefix cmd
             (let lp ((p parameters)
                      (l '()))
               (cond ((null? p)
                      (reverse l))
                     ((null? (cdr p))
                      (lp (cdr p)
                          (cons (wash (parameter->bytevector (car p) t))
                                l)))
                     (else
                      (lp (cdr p) (cons (car p) l)))))))))
