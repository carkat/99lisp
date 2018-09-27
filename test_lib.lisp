(defun test-equal (name result expected)
  (format t "~a: ~a" name (if (equal result expected)
			        (format nil "PASS~%")
				(format nil "FAIL ~%~a not equal~%~a~%"
					result expected))))

(defun test-nequal (name result expected)
  (format t "~a: ~a" name (if (not (equal result expected))
			        (format nil "PASS~%")
				(format nil "FAIL ~%~a does equal~%~a~%"
					result expected))))

(defun run-tests (n)
  (if (= n 1)
      (run-tests-1)
      (run-tests-21)))
