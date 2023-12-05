;thank you fellow stranger
; from: Donnie Cameron
; url: https://stackoverflow.com/questions/2680864/how-to-remove-nested-parentheses-in-lisp
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun emptyp (str)
  (= 0 (length str)))

(defun range (start end)
  (labels ((recrange (current lst)
             (if (>= current start)
                 (recrange (1- current) (cons current lst))
                 lst)))
    (recrange (1- end) '())))

(defun nexts (start nb)
  (range (1+ start) (+ start nb 1)))

(defun split (str delim)
  (labels ((split-acc (str delim lst)
             (let ((endpos (search delim str)))
               (if endpos
                   (let ((input (subseq str 0 endpos))
                         (rest-input (subseq str (1+ endpos))))
                     (split-acc rest-input delim (cons input lst)))
                   (cons str lst)))))
    (reverse (split-acc str delim '()))))

(defun split-list-if (lst pred)
  (labels ((tailrecc (lst localacc globalacc)
             (if lst
                 (let ((head (car lst))
                       (tail (cdr lst)))
                   (if (funcall pred head)
                       (tailrecc tail '() (cons (reverse localacc) globalacc))
                       (tailrecc tail (cons head localacc) globalacc)))
                 (reverse (cons localacc globalacc)))))
    (tailrecc lst '() '())))

(defun seeds (seed-input)
  (mapcar #'parse-integer (split (string-trim " " (cadr (split seed-input ":"))) " ")))

(defun seeds-02 (seed-input)
  (let ((vals (mapcar #'parse-integer (split (string-trim " " (cadr (split seed-input ":"))) " "))))
    (labels ((tailer (values result)
               (if values
                   (tailer (cddr values) (cons (cons (car values) (cadr values)) result))
                   result)))
      (tailer vals '()))))

(defun container-name (inputs)
  (car (split (car inputs) " ")))

(defun read-range (input)
  (mapcar #'parse-integer (split input " ")))

(defun range-descriptions (inputs)
  (mapcar #'read-range (cdr inputs)))

(defun process-container-inputs (inputs)
  (let ((range-descs (range-descriptions inputs)))
    range-descs))

(defun read-containers (all)
  (let ((containers (cdr all)))
    (mapcar #'process-container-inputs containers)))

(defun get-in (value cont)
  (let* ((x (car cont))
         (y (cadr cont))
         (z (caddr cont))
         (d (- value y)))
    (if (and (<= 0 d) (< d z))
        (+ d x)
        nil)))

(defun propagate (value cont-values)
  (if cont-values
      (if (get-in value (car cont-values))
          (get-in value (car cont-values))
          (propagate value (cdr cont-values)))
      value))

(defun read-lines (lines)
  (let ((input (read-line *standard-input* nil)))
    (cond (input (read-lines (cons input lines)))
          (t (reverse (cons input lines))))))

(defun solve-01 (content)
  (let ((s (seeds (caar content)))
        (conts (read-containers content)))
    (mapcar (lambda (seed)
                           (reduce #'propagate conts :initial-value seed))
                         s)))

(defun solve-02 (content)
  (let ((sc (seeds-02 (caar content)))
        (conts (read-containers content))
        (result 999999999999))
    (dolist (c sc)
      (dotimes (count (cdr c))
        (setq result (min result (reduce #'propagate conts :initial-value (+ count (car c)))))))
    result))

(defun process (file solver)
  (let ((content (with-open-file (*standard-input* file) (split-list-if (read-lines '()) #'emptyp))))
    (funcall solver content)))
