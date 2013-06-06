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


    
(defun split-string (str &key (delims " "))
  (let* ((next-non-delim-pos (next-non-delimiter-pos str delims))
	 (next-delim-pos (next-delimiter-pos str delims :start next-non-delim-pos)))
    (cond
      ((null next-delim-pos) (cons (subseq str next-non-delim-pos) nil)) ; Last word? End the list
      (t (cons (subseq str next-non-delim-pos next-delim-pos)
		    (recursive-word-split
		     (subseq str next-delim-pos)
		     :delims delims))))))

(defun next-delimiter (str delims &key (start 0))
  (let ((ndp (next-delimiter-pos str delims :start start)))
    (cond
      ((null ndp) nil)
      (t (subseq str ndp (1+ ndp))))))

(defun next-non-delimiter-pos (str delims &key (start 0))
  (position-if-not (lambda (c) (position c delims)) str :start start))

(defun next-delimiter-pos (str delims &key (start 0))
  (position-if (lambda (c) (position c delims)) str :start start))


(defvar *whitespace* (list #\Space #\Tab))
(defvar *operators* (list #\( #\) #\/ #\* #\+ #\- #\< #\> #\^ #\& #\| #\,))
(defun whitespacep (c) (find c *whitespace*))
(defun operatorp (c) (find c *operators*))
(defun except-char-class-p (pred) (remove pred (list #'whitespacep #'operatorp #'alphanumericp)))
  
(defun char-class (c)
  (cond
    ((whitespacep c) #'whitespacep)
    ((operatorp c) #'operatorp)
    ((alphanumericp c) #'alphanumericp)))

(defun position-if-any-except (pred str &key (start 0))
  (min (position-if (first (remove pred (list #'whitespacep #'operatorp #'alphanumericp))) str :start start)
       (position-if (second (remove pred (list #'whitespacep #'operatorp #'alphanumericp))) str :start start)))

(defun tokenize-string (str)
  (do* ((char-class-p #'whitespacep (char-class (elt str end)))
	(len (length str))
	(start (position-if-not char-class-p str) (position-if-not char-class-p str :start end))
	(end (position-if-not #'(lambda (c) (not (funcall char-class-p c))) str :start start) (position-if-not #'(lambda (c) (not (funcall char-class-p c))) str :start start)))
       ((or (null start) (null end) (> start len) (> end len)))
    (format t "~&start=~d end=~d subseq=~a" start end (subseq str start end))))
