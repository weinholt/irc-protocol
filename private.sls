;; -*- mode: scheme; coding: utf-8 -*-
;; An IRC parser library useful for both IRC clients and servers.
;; Copyright © 2008-2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Library internals.

(library (irc-protocol private)
  (export parameter->bytevector)
  (import (rnrs)
          (only (srfi :1 lists) make-list drop-right append-map
                map-in-order)
          (only (srfi :13 strings) string-index string-map)
          (industria bytevectors)
          (industria strings))

 (define (parameter->bytevector x transcoder)
    (cond ((bytevector? x)
           x)
          ((string? x)
           (string->bytevector x transcoder))
          ((number? x)
           (string->bytevector (number->string x) transcoder))
          ((symbol? x)
           (string->bytevector (symbol->string x) transcoder))
          (else
           (error 'parameter->bytevector
                  "Parameters can be bytevectors, strings, numbers or symbols."
                  x)))))
