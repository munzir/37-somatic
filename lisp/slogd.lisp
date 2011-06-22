;; Copyright 2011, Georgia Tech Research Corporation
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; * Neither the name of the copyright holder(s) nor the names of its
;;   contributors may be used to endorse or promote products derived
;;   from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package :sns)


(defun slogd-init ()
  (open-channels)
  (post-event :notice :proc-starting)
  (clix:openlog "slogd")
  )


(defun syslog-priority (x)
  (ecase x
    (:emerg :log-emerg)
    (:alert :log-alert)
    (:crit :log-crit)
    (:err :log-err)
    (:warning :log-warning)
    (:notice :log-notice)
    (:info :log-info)
    (:debug :log-debug)))

(defun slogd-run ()
  (post-event :notice :proc-running)
  (loop
     for event = (next-event)
     do
       (clix:syslog (syslog-priority (slot-value event 'somatic::priority))
                    "[~A].(~A)~@[ ~A~]"
                    (slot-value event 'somatic::ident)
                    (slot-value event 'somatic::code)
                    (slot-value event 'somatic::comment))
     until (eq :sys-halt (slot-value event 'somatic::code))))

(defun slogd-destroy ()
  (post-event :notice :proc-stopping)
  (clix:closelog)
  (post-event :notice :proc-halted)
  (close-channels))



(defun slogd-toplevel ()
  (let ((*ident* "slogd"))
    (slogd-init)
    (slogd-run)
    (slogd-destroy)
    (libc::exit 0)))
