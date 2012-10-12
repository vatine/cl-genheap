;;; Copyright 2011 Google Inc. All Rights Reserved.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package :genheap)

(defclass heap ()
  ((test :reader test :initarg :test)
   (key :reader key :initarg :key)
   (heap :reader heap :initarg :heap)
   ))

(defun make-heap (&key (initial-size 16) (test #'<) (key #'identity))
  (make-instance 'heap  :test test :key key
		 :heap (make-array initial-size :fill-pointer 0)))

(defun heap-parent (ix)
  (truncate (1- ix) 2))

(defun heap-children (ix)
  (let ((double (+ ix ix)))
    (list (+ double 1) (+ double 2))))

(defun heap-invariant-check (heap test key parent)
  (let ((children (heap-children parent))
	(len (length heap)))
    (labels ((ref (ix)
	       (funcall key (aref heap ix)))
	     (check (parent child)
	       (funcall test (ref parent) (ref child))))
      (loop for child in children
	   if (and (< child len)
		   (not (check parent child)))
	   collect (format nil "Broke for p=~d (~a) and c=~d (~a)~%"
			   parent (ref parent) child (ref child))))))
(defun heap-invariant-holds (heap-object)
  (let ((len (length (heap heap-object))))
    (let ((results (loop for ix from 0 below len
		      append (heap-invariant-check (heap heap-object)
						   (test heap-object)
						   (key heap-object)
						   ix))))
      (format t "~{~a~}~%" results))))

(defun heapify-step (heap ix test key)
  (heap-sift-down heap ix test key))

(defun heapify (sequence &key (test #'<) (key #'identity))
  (let* ((len (length sequence))
	 (heap-object (make-heap :test test :key key :initial-size len))
	 (ix 0))
    (flet ((populate (val)
	     (setf (aref (heap heap-object) ix) val)
	     (incf ix)))
      (map nil #'populate sequence)
      (setf (fill-pointer (heap heap-object)) len)
      (loop for ix from (heap-parent (1- len)) downto 0
	   do (heapify-step (heap heap-object) ix test key))
      heap-object)))


(defun heap-sift-up (heap pos test key)
  (cond ((zerop pos) heap) 
	(t (labels ((ref (ix)
		      (funcall key (aref heap ix))))
	     (let ((parent (heap-parent pos)))
	       (cond ((funcall test (ref pos) (ref parent))
		      (rotatef (aref heap pos) (aref heap parent))
		      (heap-sift-up heap parent test key))
		     (t heap)))))))

(defun heap-sift-down (heap pos test key)
  (let ((children (heap-children pos))
	(fill (fill-pointer heap))
	(val (funcall key (aref heap pos)))) 
    (labels ((ref (ix) (if (< ix fill) (funcall key (aref heap ix)) val)))
      (let* ((c1ix (car children))
	     (c2ix (cadr children))
	     (m1 (funcall test (ref c1ix) val))
	     (m2 (funcall test (ref c2ix) val)))	
	(cond ((= (apply #'min fill children) fill) heap)
	      ((and m1 m2) (cond ((funcall test
					   (ref c1ix)
					   (ref c2ix))
				  (rotatef (aref heap pos) (aref heap c1ix))
				  (heap-sift-down heap c1ix test key))
				 (t
				  (rotatef (aref heap pos) (aref heap c2ix))
				  (heap-sift-down heap c2ix test key))))
	      (m1 (rotatef (aref heap pos) (aref heap c1ix))
		  (heap-sift-down heap c1ix test key))
	      (m2 (rotatef (aref heap pos) (aref heap c2ix))
		  (heap-sift-down heap c2ix test key))
	      (t heap))))))


(defun heap-insert (heap val test key)
  (let ((len (car (array-dimensions heap)))
	(pos (fill-pointer heap)))
    (vector-push-extend val heap (+ 2 len))
    (heap-sift-up heap pos test key)))

(defun heap-remove (heap test key)
  (let ((val (aref heap 0))
	(tail-ix (1- (fill-pointer heap))))
    (setf (aref heap 0) (aref heap tail-ix))
    (decf (fill-pointer heap))
    (heap-sift-down heap 0 test key)
    val))

(defmethod insert (value (heap heap))
  (heap-insert (heap heap) value (test heap) (key heap))
  value)

(defmethod emptyp ((heap heap))
  (zerop (fill-pointer (heap heap))))

(defmethod remove ((heap heap))
  (unless (emptyp heap)
    (heap-remove (heap heap) (test heap) (key heap))))

(defmethod peek ((heap heap))
  (unless (emptyp heap)
    (aref (heap heap) 0)))

(defmethod heap-p (not-heap)
  nil)

(defmethod heap-p ((heap heap))
  t)

(defmacro declare-heap (&key (test 'ignore) (key 'ignore))
  (list test key)
  (values))
