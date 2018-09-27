(defun run-tests-1 ()
  (let ((test-val '(a b c d)))
    (test-equal "Last box is (D)" (last-box test-val) '(d))
    (test-equal "Last 2 box is (C D)" (last-but-1 test-val) '(c d))
    (test-equal "1st element is a" (element-at test-val 1) 'a)
    (test-nequal "1st element is not b" (element-at test-val 1) 'b)
    (test-equal "3rd element is c" (element-at test-val 3) 'c)
    (test-equal "4th element is d" (element-at test-val 4) 'd)
    (test-equal "Reverse list" (reverse-lst test-val) '(d c b a))
    (test-equal "Length equal" (len '()) 0)
    (test-equal "Length equal" (len test-val) 4))

  (let ((palindrome-str '(x a m a x)))
    (test-equal "Palindrome" (palindrome? palindrome-str) t)
    (test-equal "Palindrome" (palindrome? '(a)) t)
    (test-equal "Not Palindrome" (palindrome? '(a b c)) NIL))

  (let ((test-val1 '((a b c) (d e f) (g h i)))
	(test-val2 '((a (b c)) (d e f) (g h i)))
	(flat-val '(a b c d e f g h i)))
    (test-equal "List is flat" (flatten test-val1) flat-val)
    (test-equal "List is flat" (flatten test-val2) flat-val))

  (let ((test-val '(a a a b b c d d d d d e f f g)))
    (test-equal "No sequential duplicates"
		(eliminate-seq-dups test-val) '(a b c d e f g))
    (test-equal "Subsequence of sequential duplicates"
		(subseq-dups test-val)
		'((a a a) (b b) (c) (d d d d d) (e) (f f) (g)))

    (let ((seq-result (seq-dup-count test-val))
	  (modified-expected '((3 a) (2 b) c (5 d) e (2 f) g))
	  (seq-expected '((3 a) (2 b) (1 c) (5 d) (1 e) (2 f) (1 g))))
      (test-equal "First count is 3" (caar seq-result) 3)

      (test-equal "Modified subsequence dup count"
		  (modified-seq-dup-count test-val)
		  modified-expected)
      (test-equal "Decode subsequence dup count"
		  (decode-seq-dup-count test-val) test-val)
      (test-equal "Direct subsequence dup count"
		  (direct-subseq test-val) modified-expected)
      (test-equal "Subsequnce dup count"
		  (seq-dup-count test-val) seq-expected)))

  (let ((test-val '(a b c c d))
	(duplicate-expected '(a a b b c c c c d d)))
    (test-equal "Duplicate each item"
		(duplicate test-val) duplicate-expected))

  (let ((test-val '(a b c))
	(expected '(a a a b b b c c c)))
    (test-equal "Replicate each 3 times"
		(replicate test-val 3) expected))

  (let ((test-val '(1 2 3 4 5 6 7 8 9))
	(expected '(1 2 4 5 7 8)))
    (test-equal "Drop every third in list"
		(drop-every test-val 3) expected))

  (let ((test-val '(a b c d e f g h i k))
	(expected '((A B C) (D E F G H I K))))
    (test-equal "Split on third character"
		(split-n test-val 3) expected))

  (let ((test-val '(a b c d e f g h i k))
	(expected '(C D E F G)))
    (test-equal "Slice list to indices 3-7"
		(slice test-val 3 7) expected))

  (let ((test-val '(a b c d e f g h i j k))
	(expected1 '(c d e f g h i j k a b))
	(expected2 '(j k a b c d e f g h i)))
    (test-equal "Rotate 2 positions"
		(rotate test-val 2) expected1)
    (test-equal "Rotate -2 positions"
		(rotate test-val -2) expected2))

  (let ((test-val '(a b c d))
	(expected1 '(b c d))
	(expected2 '(a c d))
	(expected3 '(a b d)))
    (test-equal "Delete first"
		(remove-nth test-val 1) expected1)
    (test-equal "Delete second"
		(remove-nth test-val 2) expected2)
    (test-equal "Delete third"
		(remove-nth test-val 3) expected3)))
