;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2011, Georgia Tech Research Corporation
;;;; All rights reserved.
;;;;
;;;; Author(s): Neil T. Dantam <ntd@gatech.edu>
;;;; Georgia Tech Humanoid Robotics Lab
;;;; Under Direction of Prof. Mike Stilman
;;;;
;;;;
;;;; This file is provided under the following "BSD-style" License:
;;;;
;;;;
;;;;   Redistribution and use in source and binary forms, with or
;;;;   without modification, are permitted provided that the following
;;;;   conditions are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials provided
;;;;     with the distribution.
;;;;
;;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;;   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;;   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;;   AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;;   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;;;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;;   POSSIBILITY OF SUCH DAMAGE.

(in-package :sns)


(defun slogd-init ()
  (setq *pid* (libc:getpid))
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

(defun slog-syslog (event)
  (clix:syslog (syslog-priority (slot-value event 'somatic::priority))
               "[~A].(~A)~@[.(~A)~]~@[ ~A~]~@[ ~A~]"
               (slot-value event 'somatic::ident)
               (slot-value event 'somatic::code)
               (slot-value event 'somatic::type)
               (when (find (slot-value event 'somatic::code)
                           '(:proc-starting :proc-running
                             :proc-stopping :proc-halted))
                 (format nil "~A@~A"
                         (slot-value event 'somatic::pid)
                         (slot-value event 'somatic::host)))
               (slot-value event 'somatic::comment)))

(defun slogd-run ()
  (post-event :notice :proc-running)
  (loop
     for event = (next-event)
     do (slog-syslog event)
     until (eq :sys-halt (slot-value event 'somatic::code)))
  (slog-syslog (post-event :notice :proc-stopping)))

(defun slogd-destroy ()
  (slog-syslog (post-event :notice :proc-halted))
  (clix:closelog)
  (close-channels))



(defun slogd-toplevel ()
  (let ((*ident* "slogd"))
    (slogd-init)
    (slogd-run)
    (slogd-destroy)
    (sleep 2)
    (clix::exit 0)))
