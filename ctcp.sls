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

;; Client-To-Client Protocol

(library (irc-protocol ctcp)
  (export ctcp-message?)
  (import (rnrs)
          (only (srfi :1 lists) make-list drop-right append-map
                map-in-order)
          (only (srfi :13 strings) string-index string-map)
          (industria bytevectors)
          (industria text strings))

  ;; Nobody seems to support the full feature set of CTCP. A common
  ;; limitation is to only permit one CTCP per message.

  ;; http://www.irchelp.org/irchelp/rfc/ctcpspec.html

  (define (ctcp-message? msg)
    (and (>= (string-length msg) 2)
         (char=? #\x01 (string-ref msg 0))
         (char=? #\x01 (string-ref msg (- (string-length msg) 1)))))

  ;; TODO: CTCP parsing and formatting

  )
