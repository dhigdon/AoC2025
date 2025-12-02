;;; Advent of Code 2025 - Day 2 - Gift Shop

(defun split (s n)
  "Split 's' into pieces of 'n' length"
  (loop for i from 0 by n below (length s)
        collect (subseq s i (min (+ i n) (length s)))))

(defun all-string= (l)
  "Are all elements of 'l' string= to each other?"
  (or (null l)
      (let ((f (first l)))
        (every (lambda (e) (string= e f)) (rest l)))))

(defun repeated (s)
  "Does 's' contain sequences of repeated characters?"
  (loop for i from 1 to (floor (length s) 2)
		  when (all-string= (split s i))
		  return (values (parse-integer s) i)))

(defun invalid-value (n)
  "Part1 - is 'n' invalid? If so, return it"
  (let ((s (format nil "~A" n)))
	 (if (evenp (length s))
		(let* ((split (/ (length s) 2))
				 (left (subseq s 0 split))
				 (right (subseq s split)))
		  (if (string= left right) n 0))
		0)))

(defun invalid-value2 (n)
  "Part2 - is 'n' invalid? If so, return it"
  (if (repeated (format nil "~A" n)) n 0))


(defun decode (s cvt)
  "Decode xxx-yyy string and sum the 'cvt' of each digit between xxx and yyy"
  (let* ((split (position #\- s))
			(start (parse-integer s :start 0 :end split))
			(end   (parse-integer s :start (1+ split))))
	 (loop for i from start upto end
			 summing (funcall cvt i))))

(defun readdata (fname cvt)
  (with-open-file (f (pathname fname))
	 (loop for line = (read-line f nil)
			 while line
			 collecting (decode line cvt))))

(defun part1 () (apply #'+ (readdata "day2.txt" #'invalid-value)))

(defun part2 () (apply #'+ (readdata "day2.txt" #'invalid-value2)))

