;; -*- mode: scheme; coding: utf-8 -*-
;; An IRC parser library useful for both IRC clients and servers.
;; Copyright © 2008-2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Client-To-Client Protocol

(library (irc-protocol ctcp)
  (export ctcp-message?)
  (import (rnrs))

  ;; Nobody seems to support the full feature set of CTCP. A common
  ;; limitation is to only permit one CTCP per message.

  ;; http://www.irchelp.org/irchelp/rfc/ctcpspec.html

  (define (ctcp-message? msg)
    (and (>= (string-length msg) 2)
         (char=? #\x01 (string-ref msg 0))
         (char=? #\x01 (string-ref msg (- (string-length msg) 1)))))

  ;; TODO: CTCP parsing and formatting

  )
