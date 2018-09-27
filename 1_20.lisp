(defun last-box (lst)
  "problem 01"
  (if (second lst)
      (last-box (cdr lst))
      lst))

(defun last-but-1 (lst)
  "problem 02"
  (if (third lst)
      (last-but-1 (cdr lst))
      lst))

(defun element-at (lst nth)
  "problem 03"
  (labels ((el-loop (lst nth index)
	     (cond ((and lst (equal nth index))
		    (car lst))
		   ((first lst)
		    (el-loop (cdr lst) nth (+ 1 index)))
		   (t '()))))
    (el-loop lst nth 1)))

(defun len (lst)
  "problem 04"
  (labels ((count-items (lst index)
	     (if lst
		 (count-items (cdr lst) (+ 1 index))
		 index)))
    (count-items lst 0)))
    
(defun reverse-lst (lst)
  "problem 05"
  (labels ((reverse-loop (lst result)
	     (if lst
		 (reverse-loop (cdr lst) (cons (first lst) result))
		 result)))
    (reverse-loop lst '())))

(defun palindrome? (lst)
  "problem 06"
  (equal (reverse-lst lst) lst))

(defun flatten (lst)
  "problem 07
  if there is another list in the appended list
  mapcar each item to a list so apply append is recursive
  "
  (let ((result (apply #'append lst)))
    (if (remove-if-not #'listp result)
	(flatten
	 (mapcar (lambda (x)
		   (if (listp x)
		       x
		       (list x)))
	 result))
	result)))

(defun eliminate-seq-dups (lst)
  "problem 08"
  (labels ((seq-dups-loop (lst result)
	     (if lst
		 (cond ((equal (first lst) (first result))
			(seq-dups-loop (cdr lst) result))
		       (t (seq-dups-loop (cdr lst)
					 (cons (car lst) result))))
		 (reverse result))))
    (seq-dups-loop lst '())))

(defun subseq-dups (lst)
  "problem 09"
  (labels ((seq-dups-loop (lst result)
	     (if lst
		 (cond ((equal (first lst) (caar result))
			(seq-dups-loop (cdr lst)
				       (cons (cons (first lst)
						   (car result))
					     (cdr result))))
		       (t (seq-dups-loop (cdr lst)
					 (cons (list (car lst)) result))))
		 (reverse result))))
    (seq-dups-loop lst '())))


(defun seq-dup-count (lst)
  "problem 10"
  (mapcar
   (lambda (x) (list (len x) (car x)))
   (subseq-dups lst)))
    
(defun modified-seq-dup-count (lst)
  "problem 11"
  (mapcar
   (lambda (x) (if (equal 1 (car x)) (cadr x) x))
   (seq-dup-count lst)))

(defun decode-seq-loop (n x result)
  "problem 12 helper"
  (if (> n 0) 
      (decode-seq-loop (- n 1) x (cons x result))
      result))

(defun decode-seq-dup-count (lst)
  "problem 12"
  (apply #'append (mapcar (lambda (x)
	    (if (listp x)
		(decode-seq-loop (car x) (cadr x) '())
		(list x)))
   (modified-seq-dup-count lst))))

(defun direct-subseq (lst)
  "problem 13
   recreate the modified seq dup count without
   creating sublists first
  "
  (labels ((add-pair (current result)
	     (cons (list (len current) (car current)) result))

	   (continue-loop (lst result current)
	     "cond 1: continue grouping if the next character is a match
              cond 2: a group exists, but the next character is not a match
              cond 3: the next in list has a sequential duplicates
              cond 4: the next in list has no sequential duplicates
             "
	     (cond ((and current (equal (car lst) (car current)))
		    (direct-loop (cdr lst) result (cons (car lst) current)))

		   (current (direct-loop lst (add-pair current result) '()))

		   ((equal (car lst) (cadr lst))
		    (direct-loop (cdr lst) result (cons (car lst) current)))

		   (t (direct-loop (cdr lst) (cons (car lst) result) '()))))

	   (direct-loop (lst result current)
	     (if lst
		 (continue-loop lst result current)
		 (reverse result))))

    (direct-loop lst '() '())))

(defun repeat (n val)
  (labels ((repeat-loop (n val result)
	     (if (> n 0)
		 (repeat-loop (- n 1) val (cons val result))
		 result)))
    (repeat-loop n val '())))

(defun duplicate (lst)
  "problem 14"
  (labels ((duploop (lst result)
	     (if lst
		 (let ((char (car lst)))
		   (duploop (cdr lst) (cons char (cons char result))))
		 (reverse result))))
    (duploop lst '())))

(defun replicate (lst n)
  "problem 15"
  (labels ((reploop (lst n result)
	     (if lst
		 (reploop (cdr lst) n
			  (append (repeat n (car lst)) result))
		 (reverse result))))
    (reploop lst n '())))

(defun drop-every (lst nth)
  "problem 16"
  (labels ((drop-loop (lst nth index result)
	     (cond ((and lst (equal (mod index nth) 0))
		    (drop-loop (cdr lst) nth (+ 1 index) result))
		   (lst (drop-loop (cdr lst) nth
				   (+ 1 index) (cons (car lst) result)))
		   (t (reverse result)))))
    (drop-loop lst nth 1 '())))
  

(defun split-n (lst n)
  "problem 17"
  (labels ((sploop (lst n result)
	     (if (> n 0)
		 (sploop (cdr lst) (- n 1) (cons (car lst) result))
		 (cons (reverse result) (list lst)))))
    (sploop lst n '())))

(defun slice (lst min max)
  "problem 18"
  (let ((fix-min (- min 1)))
    (car (split-n
	  (cadr (split-n lst fix-min))
	  (- max fix-min)))))

(defun rotate (lst n)
  "problem 19"
  (labels ((get-split (lst n)
	     (let ((result (split-n lst n)))
	       (append (second result) (first result)))))
    (if (> n 0)
	(get-split lst n)
	(reverse (get-split (reverse lst) (* -1 n))))))

(defun remove-nth (lst n)
  "problem 20"
  (labels ((remoop (lst n index collect)
	     (if (equal n index)
		 (append (reverse collect) (cdr lst))
		 (remoop (cdr lst) n (+ 1 index)
			 (cons (car lst) collect)))))
    (remoop lst n 1 '())))



