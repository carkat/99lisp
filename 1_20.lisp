(defun last-box (list)
  "problem 01"
  (if (rest list)
      (last-box (cdr list))
      list))

(defun last-but-1 (list)
  "problem 02"
  (if (third list)
      (last-but-1 (cdr list))
      list))

(defun element-at (list nth)
  "problem 03"
  (labels ((el-loop (list nth index)
	     (cond ((and list (equal nth index))
		    (car list))
		   ((first list)
		    (el-loop (cdr list) nth (+ 1 index)))
		   (t '()))))
    (el-loop list nth 1)))

(defun len (list)
  "problem 04"
  (labels ((count-items (list index)
	     (if list
		 (count-items (cdr list) (+ 1 index))
		 index)))
    (count-items list 0)))
    
(defun reverse-list (list)
  "problem 05"
  (labels ((reverse-loop (list result)
	     (if list
		 (reverse-loop (cdr list) (cons (first list) result))
		 result)))
    (reverse-loop list '())))

(defun palindrome? (list)
  "problem 06"
  (equal (reverse-list list) list))

(defun flatten (list)
  "problem 07
  if there is another list in the appended list
  mapcar each item to a list so apply append is recursive
  "
  (let ((result (apply #'append list)))
    (if (remove-if-not #'listp result)
	(flatten
	 (mapcar (lambda (x)
		   (if (listp x)
		       x
		       (list x)))
	 result))
	result)))

(defun eliminate-seq-dups (list)
  "problem 08"
  (labels ((seq-dups-loop (list result)
	     (if list
		 (cond ((equal (first list) (first result))
			(seq-dups-loop (cdr list) result))
		       (t (seq-dups-loop (cdr list)
					 (cons (car list) result))))
		 (reverse result))))
    (seq-dups-loop list '())))

(defun subseq-dups (list)
  "problem 09"
  (labels ((seq-dups-loop (list result)
	     (if list
		 (cond ((equal (first list) (caar result))
			(seq-dups-loop (cdr list)
				       (cons (cons (first list)
						   (car result))
					     (cdr result))))
		       (t (seq-dups-loop (cdr list)
					 (cons (list (car list)) result))))
		 (reverse result))))
    (seq-dups-loop list '())))


(defun seq-dup-count (list)
  "problem 10"
  (mapcar
   (lambda (x) (list (len x) (car x)))
   (subseq-dups list)))
    
(defun modified-seq-dup-count (list)
  "problem 11"
  (mapcar
   (lambda (x) (if (equal 1 (car x)) (cadr x) x))
   (seq-dup-count list)))

(defun decode-seq-loop (n x result)
  "problem 12 helper"
  (if (> n 0) 
      (decode-seq-loop (- n 1) x (cons x result))
      result))

(defun decode-seq-dup-count (list)
  "problem 12"
  (apply #'append (mapcar (lambda (x)
	    (if (listp x)
		(decode-seq-loop (car x) (cadr x) '())
		(list x)))
   (modified-seq-dup-count list))))

(defun direct-subseq (list)
  "problem 13
   recreate the modified seq dup count without
   creating sublists first
  "
  (labels ((add-pair (current result)
	     (cons (list (len current) (car current)) result))

	   (continue-loop (list result current)
	     "cond 1: continue grouping if the next character is a match
              cond 2: a group exists, but the next character is not a match
              cond 3: the next in list has a sequential duplicates
              cond 4: the next in list has no sequential duplicates
             "
	     (cond ((and current (equal (car list) (car current)))
		    (direct-loop (cdr list) result (cons (car list) current)))

		   (current (direct-loop list (add-pair current result) '()))

		   ((equal (car list) (cadr list))
		    (direct-loop (cdr list) result (cons (car list) current)))

		   (t (direct-loop (cdr list) (cons (car list) result) '()))))

	   (direct-loop (list result current)
	     (if list
		 (continue-loop list result current)
		 (reverse result))))

    (direct-loop list '() '())))

(defun repeat (n val)
  (labels ((repeat-loop (n val result)
	     (if (> n 0)
		 (repeat-loop (- n 1) val (cons val result))
		 result)))
    (repeat-loop n val '())))

(defun duplicate (list)
  "problem 14"
  (labels ((duploop (list result)
	     (if list
		 (let ((char (car list)))
		   (duploop (cdr list) (cons char (cons char result))))
		 (reverse result))))
    (duploop list '())))

(defun replicate (list n)
  "problem 15"
  (labels ((reploop (list n result)
	     (if list
		 (reploop (cdr list) n
			  (append (repeat n (car list)) result))
		 (reverse result))))
    (reploop list n '())))

(defun drop-every (list nth)
  "problem 16"
  (labels ((drop-loop (list nth index result)
	     (cond ((and list (equal (mod index nth) 0))
		    (drop-loop (cdr list) nth (+ 1 index) result))
		   (list (drop-loop (cdr list) nth
				   (+ 1 index) (cons (car list) result)))
		   (t (reverse result)))))
    (drop-loop list nth 1 '())))
  

(defun split-n (list n)
  "problem 17"
  (labels ((sploop (list n result)
	     (if (> n 0)
		 (sploop (cdr list) (- n 1) (cons (car list) result))
		 (cons (reverse result) (list list)))))
    (sploop list n '())))

(defun slice (list min max)
  "problem 18"
  (let ((fix-min (- min 1)))
    (car (split-n
	  (cadr (split-n list fix-min))
	  (- max fix-min)))))

(defun rotate (list n)
  "problem 19"
  (labels ((get-split (list n)
	     (let ((result (split-n list n)))
	       (append (second result) (first result)))))
    (if (> n 0)
	(get-split list n)
	(reverse (get-split (reverse list) (* -1 n))))))

(defun remove-nth (list n)
  "problem 20"
  (labels ((remoop (list n index collect)
	     (if (equal n index)
		 (append (reverse collect) (cdr list))
		 (remoop (cdr list) n (+ 1 index)
			 (cons (car list) collect)))))
    (remoop list n 1 '())))



