;;; Advent of Code 2025, Day 6 - Trash Compactor

(defun trim-crlf (str)
  (when str (string-trim '(#\Return #\Linefeed) str)))

(defun trim-space (str)
  (when str (string-trim '(#\Space) str)))

(defun parse-intlist (s)
  (labels ((next (n result)
                 (multiple-value-bind (v p)
                   (parse-integer s :start n :junk-allowed t)
                   (if v
                     (next p (cons v result))
                     (nreverse result)))))
    (next 0 nil)))

(defun parse-operator (s &key (start 0))
  (if (<= (length s) start)
    (values nil start)
    (let ((c (char s start)))
      (cond ((char= c #\Space)      (parse-operator s :start (1+ start)))
            ((char= c #\Tab)        (parse-operator s :start (1+ start)))
            ((char= c #\Linefeed)   (values nil start))
            ((char= c #\Return)     (values nil start))
            ((char= c #\*)          (values #'* (1+ start)))
            ((char= c #\+)          (values #'+ (1+ start)))
            (t                      (values nil start))))))

(defun parse-ops (s)
  (labels ((next (n result)
                 (multiple-value-bind (v p)
                   (parse-operator s :start n)
                 (if v
                   (next p (cons v result))
                   (nreverse result)))))
    (next 0 nil)))

(defun part1 (fn)
  (apply #'+
  (with-open-file (f (pathname fn))
    (let* ((as (parse-intlist (read-line f)))
           (bs (parse-intlist (read-line f)))
           (cs (parse-intlist (read-line f)))
           (ds (parse-intlist (read-line f)))
           (os (parse-ops     (read-line f))))
      (mapcar #'funcall os as bs cs ds)))))

;;; Part 2 requires a very different interpretation of the data

(defun part2 (fn)
  (with-open-file (f (pathname fn))
    (let ((as (trim-crlf (read-line f)))
          (bs (trim-crlf (read-line f)))
          (cs (trim-crlf (read-line f)))
          (ds (trim-crlf (read-line f)))
          (os (trim-crlf (read-line f))))
      (loop with vs = nil and sum = 0
            for i downfrom (1- (length as)) to 0
            do (let ((a (char as i))
                     (b (char bs i))
                     (c (char cs i))
                     (d (char ds i)))
                 (let* ((s (coerce (list a b c d) 'string))
                        (v (parse-integer s :junk-allowed t)))
                   (when v (push v vs)))
                 (case (char os i)
                   (#\+
                    (incf sum (apply #'+ vs))
                    (setf vs nil))
                   (#\*
                    (incf sum (apply #'* vs))
                    (setf vs nil))))
            finally (return sum)))))

