
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

(defun read-numbers (input)
  (mapcar #'parse-integer (split input " ")))

(defun differentiate (nums)
  (labels ((dif-tail (nums result)
             (if (<= 2 (length nums))
                 (dif-tail (cdr nums) (cons (- (cadr nums)
                                               (car nums))
                                            result))
                 (reverse result))))
    (dif-tail nums '())))

(defun differentiate-until (nums pred)
  (labels ((du-tail (nums result)
             (if (all nums pred)
                 (reverse (cons nums result))
                 (du-tail (differentiate nums) (cons nums result)))))
    (du-tail nums '())))

(defun extrapolate (nums)
  (let ((diffs (differentiate-until nums #'zerop)))
    (apply #'+ (mapcar (lambda (xs) (car (last xs))) diffs))))

(defun interpolate (nums)
  (let ((diffs (differentiate-until nums #'zerop)))
    (reduce (lambda (x y) (- y x)) (reverse (mapcar #'first diffs)))))

(defun all (xs pred)
  (reduce (lambda (lhs rhs)
            (and lhs rhs))
          (mapcar pred xs)))

(defun solve-01 (content)
  (let ((numbers-list (mapcar #'read-numbers content)))
    (apply #'+ (mapcar #'extrapolate numbers-list))))

(defun solve-02 (content)
  (let ((numbers-list (mapcar #'read-numbers content)))
    (apply #'+ (mapcar #'interpolate numbers-list))))

(defun process (file solver)
  (let ((content (with-open-file (*standard-input* file)
                   (read-lines '()))))
    (funcall solver content)))
