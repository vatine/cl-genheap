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

(in-package :genheap-test)

(defvar *number-list* (loop for n from 1 to 100 collect (random 1000)))
(defvar *sorted-number-list* nil)

(defun get-sorted-numbers ()
  (or *sorted-number-list*
      (setf *sorted-number-list*
	    (let ((copy (copy-list *number-list*)))
	      (sort copy #'<)))))
