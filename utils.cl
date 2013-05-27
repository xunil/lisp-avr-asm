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

;;;; Recursive word split solution
;;;; defun recursive-word-split ...
;;;; if (string doesn't contain any delimiters)
;;;;    return (list string)
;;;; else
;;;;    word <- subseq ...
;;;;    return (list word (recursive-word-split ...parameters...))
(defun recursive-word-split (str &key (delims " ") keep-delims)
  (let* ((all-delims (concatenate 'string delims keep-delims))
	 (next-delim (next-delimiter str all-delims))
	 (next-delim-pos (position next-delim str)))
    (cond
      ((null next-delim) str)
      (t (list (subseq str 0 next-delim-pos)
	       (recursive-word-split
		(subseq str (1+ next-delim-pos))
		:delims delims
		:keep-delims keep-delims))))))

(defun next-delimiter (str delims)
  (do ((i 0 (1+ i))
       (found nil))
      ((or (>= i (length delims)) found)
       (return (if found (char delims (1- i)) nil)))
    (let ((c (char delims i)))
      (setf found (not (null (position c str)))))))