(defun split (str delim)
  (labels ((split-acc (str delim lst)
             (let ((endpos (search delim str)))
               (if endpos
                   (let ((input (subseq str 0 endpos))
                         (rest-input (subseq str (1+ endpos))))
                     (split-acc rest-input delim (cons input lst)))
                   (cons str lst)))))
    (reverse (split-acc str delim '()))))

(defun emptyp (str)
  (= 0 (length str)))

(defun id (input)
  (parse-integer (car (last (split (car (split input ":")) " ")))))

(defun winning (input)
  (mapcar #'parse-integer
          (remove-if #'emptyp
                     (split (string-trim
                             " "
                             (car (split (cadr (split input
                                                      ":"))
                                         "|")))
                            " "))))

(defun yours (input)
  (mapcar #'parse-integer
          (remove-if #'emptyp
                     (split (string-trim
                             " "
                             (cadr (split (cadr (split input
                                                       ":"))
                                          "|")))
                            " "))))

(defun intersect (l1 l2)
  (labels ((intersect-acc (l1 l2 acc)
             (if l1
                 (if (position (car l1) l2)
                     (intersect-acc (cdr l1) l2 (cons (car l1) acc))
                     (intersect-acc (cdr l1) l2 acc))
                 acc)))
    (intersect-acc l1 l2 '())))

(defun nb-found (input)
  (let* ((y (yours input))
         (w (winning input))
         (commons (intersect y w))
         (nb (length commons)))
    nb))

(defun compute-result (input)
  (let ((nb (nb-found input)))
    (if (= 0 nb)
        0
        (ash 1 (1- nb)))))

(defun range (start end)
  (labels ((recrange (current lst)
             (if (>= current start)
                 (recrange (1- current) (cons current lst))
                 lst)))
    (recrange (1- end) '())))

(defun nexts (start nb)
  (range (1+ start) (+ start nb 1)))

(defun read-lines (lines)
  (let ((input (read-line *standard-input* nil)))
    (if input
        (read-lines (cons input lines))
        (reverse lines))))

(defun solve-01 (lines)
  (reduce #'+ (mapcar #'compute-result lines)))

(defun solve-02 (lines)
  (let ((ids (mapcar #'id lines))
        (founds (mapcar #'nb-found lines)))
    (labels ((f (i lst)
               (let ((r (nexts i (nth (1- i) founds))))
                 (mapcar #'f r (make-list (length r)
                                          :initial-element (append r lst)))))
             (g (i)
               (cons i (f i '()))))
      (mapcar #'g ids))))

(defun process (file solver)
  (with-open-file (*standard-input* file)
    (let ((lines (read-lines '())))
      (funcall solver lines))))
