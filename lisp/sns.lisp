;; Copyright 2009, Georgia Tech Research Corporation
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


(defpackage :sns
  (:use :cl :numeri))

(in-package :sns)

(defun matrix-msg-rows (matrix-msg)
  (declare (somatic:matrix matrix-msg))
  (if (slot-boundp matrix-msg 'somatic:rows)
      (slot-value matrix-msg 'somatic:rows)
      (let ((cols (slot-value matrix-msg 'somatic:cols))
            (length (length (slot-value matrix-msg 'somatic::data))))
        (assert cols)
        (assert (= 0 (mod length cols)))
        (/ length cols))))

(defun matrix-msg-cols (matrix-msg)
  (declare (somatic:matrix matrix-msg))
  (if (slot-boundp matrix-msg 'somatic:cols)
      (slot-value matrix-msg 'somatic:cols)
      (let ((rows (slot-value matrix-msg 'somatic:rows))
            (length (length (slot-value matrix-msg 'somatic::data))))
        (assert rows)
        (assert (= 0 (mod length rows)))
        (/ length rows))))

(defun ply-matrix-msg (matrix-msg)
  (declare (somatic:matrix matrix-msg))
  (let ((m (make-double-matrix (matrix-msg-rows matrix-msg)
                               (matrix-msg-cols matrix-msg))))
    ;;(vector-copy (slot-value matrix-msg 'somatic:data) m)
    (replace (numeri::vector-data m) (slot-value matrix-msg 'somatic:data))
    m))


(defun ply-point-cloud-msg-points (point-cloud-msg)
  (ply-matrix-msg (slot-value point-cloud-msg 'somatic:points)))
