;;; Advent of Code 2025, Day 3 - Lobby

(defun trim-crlf (str)
  (when str (string-trim '(#\Return #\Linefeed) str)))

(defun to-digit (c)
  (- (char-code c) (char-code #\0)))

(defun digits-to-num (&rest digits)
  (reduce #'(lambda (x y) (+ (* x 10) y)) digits))

(defun max-digit (s)
  (loop for i across s maximize (to-digit i)))

(defun max-pair (s)
  (loop for i from 0 below (1- (length s))
        maximize (digits-to-num
                   (to-digit (char s i))
                   (max-digit (subseq s (1+ i))))))

(defun part1 (fn)
  (with-open-file (f (pathname fn))
    (loop for line = (trim-crlf (read-line f nil))
          while line
          summing (max-pair line))))

;----------------------------------------------------------------------

(defun find-max (s start &optional (end (length s)))
  (when (< start end)
    (let (idx val)
      (dotimes (i (- end start) (values (to-digit val) (+ start idx)))
        (when (or (null idx)
                  (char> (char s (+ start i)) val))
          (setq idx i
                val (char s (+ start i))))))))

(defun max-n (s start end &optional result)
  (if (> end (length s))
    (nreverse result)
    (multiple-value-bind (val idx) (find-max s start end)
      (max-n s (1+ idx) (1+ end) (cons val result)))))

(defun part2-cvt (line)
  (apply #'digits-to-num (max-n line 0 (- (length line) 11))))

(defun part2 (fn)
  (with-open-file (f (pathname fn))
    (loop for line = (trim-crlf (read-line f nil))
          while line
          summing (part2-cvt line))))

