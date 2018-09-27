
(defun run-tests-21 ()
  (let ((test-val '(a b c d))
	(expected '(A ALFA B C D)))
    (test-equal "Insert alfa at 2nd index" 
		(insert-at 'alfa test-val 2) expected)))
