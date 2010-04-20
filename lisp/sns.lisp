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

(defun sns-map (result-type function thing type)
  (ach:ach-map result-type
               (lambda (buffer)
                 (funcall function
                          (pb:unpack buffer (make-instance type))))
               thing))

;;; Message Translation Generic Functions ;;;
(defgeneric ply (msg))
(defgeneric opine (value))

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


;;; Basic Numeric Types ;;;

;;(defmethod opine ((v (simple-vector double-float (*))))
  ;;(let ((msg (make-instance 'somatic::vector)))
    ;;(setf (slot-value msg 'somatic::data) v)
    ;;msg))

(defmethod opine ((v double-matrix))
  (let ((msg (make-instance 'somatic::matrix)))
    (setf (slot-value msg 'somatic::data)
          (numeri::double-matrix-data v))
    (setf (slot-value msg 'somatic::rows )
          (matrix-rows v))
    (setf (slot-value msg 'somatic::cols )
          (matrix-cols v))
    msg))


(defun ply-matrix-msg (matrix-msg)
  (declare (somatic:matrix matrix-msg))
  (let ((m (make-double-matrix (matrix-msg-rows matrix-msg)
                               (matrix-msg-cols matrix-msg))))
    ;;(vector-copy (slot-value matrix-msg 'somatic:data) m)
    (replace (numeri::vector-data m) (slot-value matrix-msg 'somatic:data))
    m))



(defmethod ply ((msg somatic::vector))
  (let ((lisp-vec (slot-value msg 'somatic::data)))
    (make-array (length lisp-vec)
                :element-type 'double-float
                :initial-contents lisp-vec)))

(defmethod ply ((msg somatic::force-moment))
  (vector-seq-cat (list (ply (slot-value msg 'somatic::force))
                        (ply (slot-value msg 'somatic::moment)))))


(defun ply-point-cloud-msg-points (point-cloud-msg &key discard-zeros)
  (let ((matrix-msg  (slot-value point-cloud-msg 'somatic:points)))
    (assert (= 3 (matrix-msg-rows matrix-msg)))
    (apply #'vector
           (loop
              with data = (slot-value matrix-msg 'somatic:data)
              for i = 0 then (+ i 3)
              while (< i (length data))
              for j = (1+ i)
              for k = (1+ j)
              for x = (aref data i)
              for y = (aref data j)
              for z = (aref data k)
              when (or (null discard-zeros)
                       (not (and (zerop x) (zerop y) (zerop z))))
              collect (double-vector x y z)))))


(defgeneric opine-timespec (time))

(defmethod opine-timespec ((time somatic::timespec))
  time)

(defmethod opine-timespec ((time number))
  (let ((msg (make-instance 'somatic::timespec)))
    (multiple-value-bind (sec nsec) (truncate time)
      (setf (slot-value msg 'somatic::sec)
            sec)
      (setf (slot-value msg 'somatic::nsec)
            (truncate (* 1e9 nsec))))
    msg))

(defun timespec->seconds (time)
  (declare (type somatic::timespec time))
  (+ (slot-value time 'somatic::sec)
     (/ (slot-value time 'somatic::nsec)
        1000000000)))

(defun opine-vector (v)
  (make-instance 'somatic::vector
                 :data (make-array (length v)
                                   :element-type 'double-float
                                   :initial-contents v)))

(defun opine-transform (&key translation basis rotation)
  (let ((msg (make-instance 'somatic::transform)))
    (setf (slot-value msg 'somatic::translation)
          (opine-vector
           (cond
             (translation translation)
             (t (make-array 3 :element-type 'double-float
                            :initial-contents '(0d0 0d0 0d0))))))

    (setf (slot-value msg 'somatic::rotation)
          (opine-vector
           (cond
             (rotation rotation)
             (basis (numeri::quat<-rotmat basis))
             (t (make-array 4 :element-type 'double-float
                            :initial-contents '(0d0 0d0 0d0 1d0))))))
    msg))

(defun opine-setpoint( &key jointspace translation rotation time)
  (let ((msg (make-instance 'somatic::setpoint)))
    (when (or translation rotation)
      (setf (slot-value msg 'somatic::workspace)
            (opine-transform :translation translation
                             :rotation rotation)))
    (when jointspace
      (setf (slot-value msg 'somatic::jointspace)
            (opine-vector jointspace)))
    (when time
      (setf (slot-value msg 'somatic::time)
            (opine-timespec time)))
    msg))

(defun opine-label-vector (v &key label time)
  (let ((msg (make-instance 'somatic::label-vector
                            :x (opine-vector v))))
    (when label
      (setf (slot-value msg 'somatic::label)
            label))
    (when time
      (setf (slot-value msg 'somatic::time)
            (opine-timespec time)))
    msg))

(defmethod ply ((msg somatic::label-vector))
  (let ((lisp-vec (slot-value (slot-value msg 'somatic::x)
                              'somatic::data)))
    (values (slot-value msg 'somatic::label)
            (make-array (length lisp-vec)
                        :element-type 'double-float
                        :initial-contents lisp-vec))))

