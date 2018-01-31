#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2010-2018 Göran Weinholt <goran@weinholt.se>

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
  (srfi :78 lightweight-testing)
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

;; See what happens when the last parameter is empty, and when there's
;; more than one space between parameters.
(check (fmt-whitewash #f 'TOPIC "#test" "")
       =>
       "TOPIC #test :\r\n")

(check (parse (fmt-whitewash #f 'TOPIC "#test" ""))
       =>
       '(#f TOPIC ("#test" "")))

(check (parse "TOPIC #test \r\n")
       =>
       '(#f TOPIC ("#test")))

(check (parse "TOPIC    #test   \r\n")
       =>
       '(#f TOPIC ("#test")))

(check (parse "TOPIC    #test   :\r\n")
       =>
       '(#f TOPIC ("#test" "")))

(check (parse "TOPIC    #test   : \r\n")
       =>
       '(#f TOPIC ("#test" " ")))

;; utf-8 equivalent
(check (parse/bv (string->utf8 "TOPIC #test \r\n"))
       =>
       '(#f TOPIC (#vu8(35 116 101 115 116))))

(check (parse/bv (string->utf8 "TOPIC  #test    \r\n"))
       =>
       '(#f TOPIC (#vu8(35 116 101 115 116))))

(check (parse/bv (string->utf8 "TOPIC  #test    :\r\n"))
       =>
       '(#f TOPIC (#vu8(35 116 101 115 116) #vu8())))

(check (parse/bv (string->utf8 "TOPIC  #test    : \r\n"))
       =>
       '(#f TOPIC (#vu8(35 116 101 115 116) #vu8(32))))


;; Examples..
(check (utf8->string
        (call-with-bytevector-output-port
          (lambda (port)
            (format-message-with-whitewash port (utf-8-codec)
                                           #f 'NOTICE "#abusers"
                                           "DrAbuse: your answer is: 123\r\nJOIN 0"))))
       => "NOTICE #abusers :DrAbuse: your answer is: 123  JOIN 0\r\n")

(check (utf8->string
        (call-with-bytevector-output-port
          (lambda (port)
            (format-message-and-verify port (utf-8-codec)
                                       "irc.example.net" 'NOTICE "ErrantUser"
                                       "The server has taken a liking to you"))))
       => ":irc.example.net NOTICE ErrantUser :The server has taken a liking to you\r\n")

(check
 (utf8->string
  (call-with-bytevector-output-port
    (lambda (port)
      (format-message-raw port (utf-8-codec)
                          "irc.example.net" 001 "luser"
                          "Welcome to the Example Internet Relay Chat Network luser"))))
 => ":irc.example.net 001 luser :Welcome to the Example Internet Relay Chat Network luser\r\n")

(check
 (utf8->string
  (call-with-bytevector-output-port
    (lambda (port)
      (format-message-raw port (utf-8-codec)
                          #f 'PRIVMSG "#example"
                          "This is a message to a channel"))))
 => "PRIVMSG #example :This is a message to a channel\r\n")

;;; Channel mode commands

(define (pchan modes)
  (parse-channel-mode (cdr (assq 'PREFIX (isupport-defaults)))
                      (cdr (assq 'CHANMODES (isupport-defaults)))
                      modes))

;; only
(check (pchan '("+l" "50")) => '((+ #\l "50")))
(check (pchan '("-l")) => '((- #\l #f)))

;; never
(check (pchan '("m-m")) => '((+ #\m channel) (- #\m channel)))
(check (pchan '("-m+m")) => '((- #\m channel) (+ #\m channel)))

;; always
(check (pchan '("+k-k" "foo")) => '((+ #\k "foo") (- #\k #f)))
(check (pchan '("+k-k" "foo" "foo")) => '((+ #\k "foo") (- #\k "foo")))

;; address
(check (pchan '("+e" "*!*@*" "-e" "*!*@*")) => '((+ #\e "*!*@*") (- #\e "*!*@*")))
(check (pchan '("+e" "*!*@*" "e")) => '((+ #\e "*!*@*") (? #\e channel)))

;; prefix
(check (pchan '("+o" "Procrustes" "-o" "Procrustes")) => '((+ #\o "Procrustes") (- #\o "Procrustes")))
(check (pchan '("o" "Procrustes")) => '((+ #\o "Procrustes")))
(check (pchan '("o")) => '())



(check-report)
