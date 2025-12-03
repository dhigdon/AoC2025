;;; Advent of Code 2025, Day 3 - Lobby

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
    (loop for line = (read-line f nil)
          while line
          summing (max-pair line))))

(defun remove-min (s)
  "remove first smallest digit"
  (loop for d from (char-code #\0) upto (char-code #\9)
        when (find (code-char d) s)
        return (remove (code-char d) s :count 1)))

(defun shrink (s l)
  (if (<= (length s) l)
    s
    (shrink (remove-min s) l)))

(defun part2 (fn)
  (with-open-file (f (pathname fn))
    (loop for line = (read-line f nil)
          while line
          summing (parse-integer (shrink line 12)))))

