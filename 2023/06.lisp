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

(defun read-lines (lines)
  (let ((input (read-line *standard-input* nil)))
    (cond (input (read-lines (cons input lines)))
          (t (reverse (cons input lines))))))

(defun numbers (entry)
  (when entry
    (mapcar #'parse-integer
            (remove-if #'emptyp
                       (split (cadr (split entry ":")) " ")))))

(defun numbers-02 (entry)
  (when entry
    (parse-integer (apply #'concatenate 'string
                          (remove-if #'emptyp
                                     (split (cadr (split entry ":")) " "))))))

(defun possibilities (data)
  (labels ((tailrecc (test-time time dist winners)
             (if (> test-time time)
                 winners
                 (let* ((a test-time)
                        (dt (- time test-time))
                        (d (* a dt)))
                   (if (> d dist)
                       (tailrecc (1+ test-time) time dist (cons test-time winners))
                       (tailrecc (1+ test-time) time dist winners))))))
    (tailrecc 0 (car data) (cdr data) '())))

(defun solve-01 (content)
  (let* ((nums (mapcar #'numbers content))
         (data (pairlis (car nums) (cadr nums))))
    (apply #'* (mapcar #'length (mapcar #'possibilities data)))))

(defun solve-02 (content)
  (let* ((nums (mapcar #'numbers-02 content))
         (data (cons (car nums) (cadr nums))))
    (length (possibilities data))))

(defun process (file solver)
  (let ((content (with-open-file (*standard-input* file) (read-lines '()))))
    (funcall solver content)))
