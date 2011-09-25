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

(defclass class-heap ()
  ())

(defvar *class-heap-lookup* (make-hash-table :test #'equal))

(defgeneric class-heap-sift-up (heap store pos)
  )
(defgeneric class-heap-sift-down (heap store pos)
  )
(defgeneric class-heap-insert (heap val)
  )
(defgeneric class-heap-remove (heap)
  )
(defvar *heap-class-id* 0)

(defmethod class-heap-insert ((heap class-heap) val)
  (let ((store (heap heap)))
    (let ((len (car (array-dimensions store)))
	  (pos (fill-pointer store)))
      (vector-push-extend val store (+ len len))
      (class-heap-sift-up heap store pos))))

(defmethod class-heap-remove ((heap class-heap))
  (let ((store (heap heap)))
    (let ((val (aref store 0))
	  (tail-ix (1- (fill-pointer store))))
      (rotatef (aref store 0) (aref store tail-ix))
      (decf (fill-pointer store))
      (class-heap-sift-down heap store 0)
      val)))

(defmethod insert (value (heap class-heap))
  (class-heap-insert heap value))
(defmethod remove ((heap class-heap))
  (unless (emptyp heap)
    (class-heap-remove heap)))

(defun make-class-symbol ()
  (intern (format nil "HEAP-CLASS-~d" (incf *heap-class-id*)) (find-package :heaps-classes)))

(defmacro declare-heap-class (test &key (key #'identity))
  (let ((skip-key (or (eql key #'identity)
		      (eql key 'identity))))
    (let ((hkey (cons (eval test) (eval key)))
	  (ref (cond (skip-key `(ref (ix) (aref store ix)))
		     (t `(ref (ix) (funcall ,key (aref store ix))))))
	  (fill-ref (cond (skip-key `(ref (ix) (if (< ix fill)
						   (aref store ix)
						   val)))
			  (t `(ref (ix) (if (< ix fill)
					    (funcall ,key (aref store ix))
					    val)))))
	  (class-name (make-class-symbol)))
      `(cond ((gethash ',hkey *class-heap-lookup*))
	     (t (progn
		  (defclass ,class-name (class-heap) ())
		  (setf (gethash ',hkey *class-heap-lookup*) ',class-name)
		  (defmethod class-heap-sift-up ((heap ,class-name) store pos)
		    (cond ((zerop pos) heap)
			  (t (labels (,ref)
			       (let ((parent (heap-parent pos)))
				 (cond ((funcall ,test (ref pos) (ref parent))
					(rotatef (aref store pos) (aref store parent))
					(class-heap-sift-up heap store parent))
				       (t heap)))))))
		  (defmethod class-heap-sift-down ((heap ,class-name) store pos)
		    (labels (,ref)
		      (let ((children (heap-children pos))
			    (fill (fill-pointer store))
			    (val (ref pos)))
			(labels (,fill-ref)
			  (let* ((c1ix (car children))
				 (c2ix (cadr children))
				 (m1 (funcall ,test (ref c1ix) val))
				 (m2 (funcall ,test (ref c2ix) val)))
			    (cond ((= (apply #'min fill children) fill) heap)
				  ((and m1 m2)
				   (cond ((funcall ,test
						   (ref c1ix)
						   (ref c2ix))
					  (rotatef (aref store pos)
						   (aref store c1ix))
					  (class-heap-sift-down heap store c1ix))
					 (t (rotatef (aref store pos)
						     (aref store c2ix))
					    (class-heap-sift-down heap store c2ix))))
				  (m1 (rotatef (aref store pos)
					       (aref store c1ix))
				      (class-heap-sift-down heap store c1ix))
				  (m2 (rotatef (aref store pos)
					       (aref store c2ix))
				      (class-heap-sift-down heap store c2ix))
				  (t heap)))))))))))))
  
	   
(defun make-class-heap (test &key (initial-size 16) (key #'identity))
  (let ((hkey (cons test key)))
    (let ((class (gethash hkey *class-heap-lookup*)))
      (cond (class (make-instance class :heap (make-array initial-size :fill-pointer 0)))
	    (t (error "No heap class defined for test ~a, key ~a." test key))))))


(defun class-heapify (sequence test &key (key #'identity))
  (let ((hkey (cons test key))
	(len (length sequence)))
    (let ((class (gethash hkey *class-heap-lookup*)))
      (cond (class (let ((heap-object (make-class-heap test
						       :key key
						       :initial-size len)))
		     (let ((heap (heap heap-object))
			   (ix 0))
		       (setf (fill-pointer heap) (1- len))
		       (flet ((populate (val)
				(setf (aref heap ix) val)
				(incf ix)))
			 (map nil #'populate sequence)
			 (loop for ix from (heap-parent (1- len)) downto 0
			      do (class-heap-sift-down heap-object heap ix))))
		     heap-object)) 
	    (t (error "No heap class defined for test ~a, key ~a." test key))))))

(defmethod heap-p (not-heap)
  nil)

(defmethod heap-p ((heap heap-class))
  t)
