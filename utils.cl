(defun split-multi-delims (str &key (delims " ") keep-delims)
  (do ((result (make-array 1 :fill-pointer 0 :adjustable t))
       (pos 0 (1+ pos))
       (word-start 0))
      ((> pos (length str))
       result)
    (if (= pos (length str))
	(vector-push-extend (subseq str word-start pos) result)
	(let ((c (elt str pos)))
	  (when (or (position c delims) (position c keep-delims))
	    (unless (= word-start pos)
	      (vector-push-extend (subseq str word-start pos) result)
	      (when (position c keep-delims)
		(vector-push-extend (subseq str pos (1+ pos)) result)
		(setf word-start (1+ pos)))))))))

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

(defun recursive-word-split (str &key (delims " ") keep-delims)
  (print str)
  (let* ((all-delims (concatenate 'string delims keep-delims))
	 (next-non-delim-pos (next-non-delimiter-pos str all-delims))
	 (next-delim-pos (next-delimiter-pos str all-delims :start next-non-delim-pos)))
    (cond
      ((null next-delim-pos) (cons (subseq str next-non-delim-pos) nil)) ; Last word? End the list
      (t (cond                                                           ; Not the last word, but it might be a delimiter we want to keep
	   (keep-delims (cons
			 (subseq str next-non-delim-pos next-delim-pos)
			 (cons
			  (next-delimiter str keep-delims :start next-delim-pos)
			  (recursive-word-split (subseq str next-delim-pos) :delims delims :keep-delims keep-delims))))
	   (t (cons (subseq str next-non-delim-pos next-delim-pos)
		    (recursive-word-split
		     (subseq str next-delim-pos)
		     :delims delims
		     :keep-delims keep-delims))))))))

(defun x-recursive-word-split (str &key (delims " ") keep-delims)
  (print str)
  (let* ((all-delims (concatenate 'string delims keep-delims))
	 (next-non-delim-pos (next-non-delimiter-pos str all-delims))
	 (next-delim-pos (next-delimiter-pos str all-delims :start next-non-delim-pos)))
    (cond
      ((null next-delim-pos) (cons (subseq str next-non-delim-pos) nil))
      (t (let ((word (subseq str next-non-delim-pos next-delim-pos)))
	   (cond
	     ((and keep-delims (next-non-delimiter-pos str delims)) (cons word (cons (next-delimiter str keep-delims)
					   (x-recursive-word-split (subseq str next-delim-pos) :delims delims :keep-delims keep-delims))))
	     (t (cons word (x-recursive-word-split (subseq str next-delim-pos) :delims delims :keep-delims keep-delims)))))))))

(defun next-delimiter (str delims &key (start 0))
  (let ((ndp (next-delimiter-pos str delims :start start)))
    (cond
      ((null ndp) nil)
      (t (subseq str ndp (1+ ndp))))))

(defun next-non-delimiter-pos (str delims &key (start 0))
  (position-if-not (lambda (c) (position c delims)) str :start start))

(defun next-delimiter-pos (str delims &key (start 0))
  (position-if (lambda (c) (position c delims)) str :start start))
