#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2010-2019 Göran Weinholt <goran@weinholt.se>

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

(import
  (rnrs)
  (srfi :64 testing)
  (industria bytevectors)
  (irc-protocol parse)
  (irc-protocol format))

(define (fmt-whitewash prefix cmd . parameters)
  (utf8->string (apply fmt-whitewash/bv prefix cmd parameters)))

(define (fmt-whitewash/bv prefix cmd . parameters)
  (call-with-bytevector-output-port
    (lambda (p)
      (apply format-message-with-whitewash p (utf-8-codec)
             prefix cmd parameters))))

(define (parse str)
  (let-values ((x (parse-message (substring str 0 (- (string-length str) 2)))))
    x))

(define (parse/bv str)
  (let-values ((x (parse-message-bytevector
                   (subbytevector str 0 (- (bytevector-length str) 2)))))
    x))

(test-begin "irc")

;; See what happens when the last parameter is empty, and when there's
;; more than one space between parameters.
(test-equal "TOPIC #test :\r\n"
            (fmt-whitewash #f 'TOPIC "#test" ""))

(test-equal '(#f TOPIC ("#test" ""))
            (parse (fmt-whitewash #f 'TOPIC "#test" "")))

(test-equal '(#f TOPIC ("#test"))
            (parse "TOPIC #test \r\n"))

(test-equal '(#f TOPIC ("#test"))
            (parse "TOPIC    #test   \r\n"))

(test-equal '(#f TOPIC ("#test" ""))
            (parse "TOPIC    #test   :\r\n"))

(test-equal '(#f TOPIC ("#test" " "))
            (parse "TOPIC    #test   : \r\n"))

;; utf-8 equivalent
(test-equal '(#f TOPIC (#vu8(35 116 101 115 116)))
            (parse/bv (string->utf8 "TOPIC #test \r\n")))

(test-equal '(#f TOPIC (#vu8(35 116 101 115 116)))
            (parse/bv (string->utf8 "TOPIC  #test    \r\n")))

(test-equal '(#f TOPIC (#vu8(35 116 101 115 116) #vu8()))
            (parse/bv (string->utf8 "TOPIC  #test    :\r\n")))

(test-equal '(#f TOPIC (#vu8(35 116 101 115 116) #vu8(32)))
            (parse/bv (string->utf8 "TOPIC  #test    : \r\n")))


;; Examples..
(test-equal "NOTICE #abusers :DrAbuse: your answer is: 123  JOIN 0\r\n"
            (utf8->string
             (call-with-bytevector-output-port
               (lambda (port)
                 (format-message-with-whitewash port (utf-8-codec)
                                                #f 'NOTICE "#abusers"
                                                "DrAbuse: your answer is: 123\r\nJOIN 0")))))

(test-equal ":irc.example.net NOTICE ErrantUser :The server has taken a liking to you\r\n"
            (utf8->string
             (call-with-bytevector-output-port
               (lambda (port)
                 (format-message-and-verify port (utf-8-codec)
                                            "irc.example.net" 'NOTICE "ErrantUser"
                                            "The server has taken a liking to you")))))

(test-equal
 ":irc.example.net 001 luser :Welcome to the Example Internet Relay Chat Network luser\r\n"
 (utf8->string
  (call-with-bytevector-output-port
    (lambda (port)
      (format-message-raw port (utf-8-codec)
                          "irc.example.net" 001 "luser"
                          "Welcome to the Example Internet Relay Chat Network luser")))))

(test-equal
 "PRIVMSG #example :This is a message to a channel\r\n"
 (utf8->string
  (call-with-bytevector-output-port
    (lambda (port)
      (format-message-raw port (utf-8-codec)
                          #f 'PRIVMSG "#example"
                          "This is a message to a channel")))))

;;; Channel mode commands

(define (pchan modes)
  (parse-channel-mode (cdr (assq 'PREFIX (isupport-defaults)))
                      (cdr (assq 'CHANMODES (isupport-defaults)))
                      modes))

;; only
(test-equal '((+ #\l "50")) (pchan '("+l" "50")))
(test-equal '((- #\l #f)) (pchan '("-l")))

;; never
(test-equal '((+ #\m channel) (- #\m channel)) (pchan '("m-m")))
(test-equal '((- #\m channel) (+ #\m channel)) (pchan '("-m+m")))

;; always
(test-equal '((+ #\k "foo") (- #\k #f)) (pchan '("+k-k" "foo")))
(test-equal '((+ #\k "foo") (- #\k "foo")) (pchan '("+k-k" "foo" "foo")))

;; address
(test-equal '((+ #\e "*!*@*") (- #\e "*!*@*")) (pchan '("+e" "*!*@*" "-e" "*!*@*")))
(test-equal '((+ #\e "*!*@*") (? #\e channel)) (pchan '("+e" "*!*@*" "e")))

;; prefix
(test-equal '((+ #\o "Procrustes") (- #\o "Procrustes")) (pchan '("+o" "Procrustes" "-o" "Procrustes")))
(test-equal '((+ #\o "Procrustes")) (pchan '("o" "Procrustes")))
(test-equal '() (pchan '("o")))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
