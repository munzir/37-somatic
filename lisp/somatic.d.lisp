#!/usr/bin/sbcl --script
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

;; find quicklisp
(let ((ql0 (merge-pathnames "quicklisp/setup.lisp"
                            (user-homedir-pathname)))
      (ql1 (merge-pathnames ".quicklisp/setup.lisp"
                            (user-homedir-pathname))))
  (cond
    ((probe-file ql0)
     (load ql0))
    ((probe-file ql1)
     (load ql1))
    (t (format t "Can't find quicklisp.~%")
       (sb-ext:quit :unix-status 1))))

;; load package
(ql:quickload "somatic")

(defvar *event-frame-count* 512)
(defvar *event-frame-size* 4096)

;; load config
(when (probe-file (pathname "/etc/somatic/somatic-config.lisp"))
  (load "/etc/somatic/somatic-config.lisp"))

;; dispatch
(labels ((start ()
           (unless (probe-file (pathname "/dev/shm/achshm-event"))
             (ach:create-channel "event" *event-frame-count* *event-frame-size*)
             (let ((chan (ach:open-channel "event")))
               (ach:chmod chan #o666)
               (ach:close-channel chan)))
           (ensure-directories-exist (pathname "/var/run/somatic/slogd.out"))
           (clix:daemonize :out "/var/run/somatic/slogd.out"
                           :err "/var/run/somatic/slogd.err"
                           :pid "/var/run/somatic/slogd.pid")
           (sns::slogd-toplevel))
         (stop  ()
           (sns::open-channels)
           (sns::post-event :notice :sys-halt)
           (sns::close-channels)))
  ;; parse arg
  (let ((arg (cadr sb-ext:*posix-argv*)))
    (cond
      ((string= arg "start")  (start))
      ((string= arg "stop")  (stop))
      (t (format t "Usage: somatic {start | stop}")))))




;;; Local Variables: ***
;;; mode:lisp ***
;;; End: ***
