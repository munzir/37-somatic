;; Copyright 2010, Georgia Tech Research Corporation
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

(defvar *speech* nil)

(defun speech-open (&key host)
  (assert (null *speech*))
  (cond
    (host (setq *speech*
                (sb-ext:run-program "ssh" (list host "festival")
                                    :input :stream :search t :wait nil
                                    :output nil :error nil)))
    (t (setq *speech*
             (sb-ext:run-program "festival" nil :input :stream :search t :wait nil
                                 :output nil :error nil)))))

(defun speech-format (control-string &rest args)
  (apply #'format (sb-ext:process-input *speech*) control-string args)
  (finish-output (sb-ext:process-input *speech*)))

(defun say (text)
  (format t "~&! ~S !~%" text)
  (speech-format "(SayText \"~A\")~%" text)
  text)

(defun speech-close ()
  (speech-format "(exit)~%")
  (sb-ext:process-close *speech*)
  (setq *speech* nil))
