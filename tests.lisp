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

(genheap:declare-heap :test #'<)
(genheap:declare-heap :test #'>)

(def-suite genheap-test-suite
    :description "Test suite for genheap public interface")
(in-suite genheap-test-suite)

(test package-exists
  (is (not (null (find-package :genheap)))))

(test heapify-test
  (is (equal (get-sorted-numbers)
	     (let ((heap (genheap:heapify *number-list* :test #'<)))
	       (loop until (genheap:emptyp heap)
		    collect (genheap:remove heap)))))
  (is (equal (reverse (get-sorted-numbers))
	     (let ((heap (genheap:heapify *number-list* :test #'>)))
	       (loop until (genheap:emptyp heap)
		    collect (genheap:remove heap))))))

(test insert-peek-test
  (is (= 3 (let ((heap (genheap:make-heap)))
	     (genheap:insert 7 heap)
	     (genheap:insert 4 heap)
	     (genheap:insert 3 heap)
	     (genheap:peek heap)))))

(test emptyp-test
  (is (genheap:emptyp (genheap:make-heap)))
  (is (let ((heap (genheap:make-heap)))
	(genheap:insert 3 heap)
	(genheap:remove heap)
	(genheap:emptyp heap))))
