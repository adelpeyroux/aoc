(defun emptyp (str)
  (= 0 (length str)))

(defun split (str delim)
  (labels ((split-acc (str delim lst)
             (let ((endpos (search delim str)))
               (if endpos
                   (let ((input (subseq str 0 endpos))
                         (rest-input (subseq str (1+ endpos))))
                     (split-acc rest-input delim (cons input lst)))
                   (cons str lst)))))
    (reverse (split-acc str delim '()))))

(defun read-lines (lines)
  (let ((input (read-line *standard-input* nil)))
    (cond (input (read-lines (cons input lines)))
          (t (reverse lines)))))

(defun hash (current next-char)
  (mod (* 17 (+ current (char-code next-char))) 256))

(defun process-entry (input)
  (let ((chars (coerce input 'list)))
    (reduce #'hash chars :initial-value 0)))

(defun read-numbers (input)
  (mapcar #'parse-integer (split input " ")))

(defun solve-01 (content)
  (let ((entries (split (car content) ",")))
    (apply #'+ (mapcar #'process-entry entries))))

(defun solve-02 (content)
  content)

(defun process (file solver)
  (let ((content (with-open-file (*standard-input* file)
                   (read-lines '()))))
    (funcall solver content)))
