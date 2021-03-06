(defun hex-print (data &optional (width 40))
  (do ((pos 0 (incf pos))
       (travel 0))
      ((>= pos (length data)))
    (format t "~2,'0X" (char-int (elt data pos)))
    (setf travel (+ 2 travel))
    (if (>= travel width)
      (progn
	(format t "~%")
	(setf travel 0))
      (progn
	(format t " ")
	(incf travel)))))

(defun new-hex-print (data &optional (width 40))
  (do ((step-size (round (/ width 3)))
       (step 0 (incf step))
       (start 0 (* step-size step)))
      ((> start (length data)))
    (let*
	((len (length data))
	 (end (min (+ start step-size) len))
	 (seq (subseq data start (min end (length data)))))
      (format t "~{~2,'0X~^ ~}~&" (map 'sequence #'char-int seq)))))

(defun tokenize-string (str)
  (let ((result (list))
	(whitespace (list #\Space #\Tab))
	(operators (list #\( #\) #\/ #\* #\+ #\- #\< #\> #\^ #\& #\| #\,)))
    (labels ((whitespacep (c) (find c whitespace))
	     (operatorp (c) (find c operators))
	     (commentp (c) (char= c #\;))
	     (char-class (c)
	       (cond
		 ((whitespacep c) #'whitespacep)
		 ((operatorp c) #'operatorp)
		 ((alphanumericp c) #'alphanumericp)
		 ((commentp c) #'commentp))))
      (do* ((cur-char-class #'whitespacep char-class-at-start)
	    (start (position-if-not cur-char-class str) end)
	    (char-class-at-start (char-class (elt str start)) (char-class (elt str start)))
	    (end (position-if-not char-class-at-start str :start start) (position-if-not char-class-at-start str :start start)))
	   ((null start))
	(if (not (eql #'whitespacep char-class-at-start))
	    (if (null end)
		(progn
		  (setf result (append result (list (subseq str start))))
		  (return result))
		(setf result (append result (list (subseq str start end))))))))))

