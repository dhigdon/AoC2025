;;; Advent of Code 2025, Day 4 - Printing Department

(defun trim-crlf (str)
  (when str (string-trim '(#\Return #\Linefeed) str)))

(defun read-data (fn)
  (let (result)
    (with-open-file (f (pathname fn))
      (loop for line = (trim-crlf (read-line f nil))
            for row from 0
            while line
            do (let ((size (length line)))
                 (unless result
                   (setq result
                         ; NOTE - data is always square
                         (make-array (list size size)
                                     :element-type 'character)))
                 (dotimes (i size)
                   (setf (aref result row i) (char line i))))))
    result))

(defun get-field (field x y)
  "Return a list of adjacent coodinate pairs containing X"
  (destructuring-bind (rows cols) (array-dimensions field)
    (cond ((< x 0) #\.)
          ((< y 0) #\.)
          ((>= x cols) #\.)
          ((>= y rows) #\.)
          (t (aref field y x)))))

(defun clear-field (field x y)
  (setf (aref field y x) #\.))

(defun get-adjacents (field x y)
  (let (result)
    (if (char= (get-field field x y) #\@)
      (flet ((check (x y)
                    (when (char= (get-field field x y) #\@)
                      (push (cons x y) result))))
        (check (1- x) (1- y))
        (check (1- x)     y )
        (check (1- x) (1+ y))
        (check     x  (1- y))
        (check     x  (1+ y))
        (check (1+ x) (1- y))
        (check (1+ x)     y )
        (check (1+ x) (1+ y)))
      (setf result '(0 0 0 0 0 0 0 0)))
    result))

(defun part1 (field)
  (let ((count 0)
        result
        (rows (first (array-dimensions field)))
        (cols (second (array-dimensions field))))
    (dotimes (y rows)
      (dotimes (x cols)
        (when (< (length (get-adjacents field x y)) 4)
          (push (cons x y) result)
          (incf count))))
    (values count result)))

;;; Part 2

(defun run-phase (field)
  (multiple-value-bind (count rolls) (part1 field)
    (unless (zerop count)
      (mapc (lambda (c)
              (clear-field field (car c) (cdr c)))
            rolls)
      (length rolls))))

(defun part2 (field)
  (loop for x = (run-phase field)
        until (null x)
        summing x))

