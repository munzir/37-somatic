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


(defvar *channel-event-read*)
(defvar *channel-event-write*)

(defvar *ident* "somatic")
(defvar *facility* 0)
(defvar *host* (machine-instance))
(defvar *pid* (libc:getpid))

(defun open-channels ()
  (setq *channel-event-read* (ach:open-channel "event"))
  (setq *channel-event-write* (ach:open-channel "event"))
  (ach:flush *channel-event-read*)
  )

(defun close-channels ()
  (ach:close-channel *channel-event-read*)
  (ach:close-channel *channel-event-write*))


(defun post-event (priority code &key
                   (ident *ident*)
                   (facility *facility*)
                   (pid *pid*)
                   (host *host*)
                   comment)
  (let ((event (make-instance 'somatic::event
                                     :priority priority
                                     :code code
                                     :ident ident
                                     :facility facility
                                     :pid pid
                                     :host host
                                     :comment comment)))
    (ach:put *channel-event-write*
             (pb:encode event))
    event))

(defun next-event ()
  (pb:unpack (ach:wait-next *channel-event-read*)
             (make-instance 'somatic::event)))


