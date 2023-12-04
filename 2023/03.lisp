(defun build-empty-map (dimensions)
  (make-array dimensions :initial-element #\.))

(defun fill-line (array line content)
  (labels ((filler (cursor content)
             (if (and (>= cursor 0) (< cursor (array-dimension array 1)))
                 (progn
                  (setf (aref array line cursor) (car content))
                  (filler (1+ cursor) (cdr content)))
                 array)))
    (filler 0 content)))

(defun range (start end)
  (labels ((recrange (current lst)
             (if (>= current start)
                 (recrange (1- current) (cons current lst))
                 lst)))
    (recrange (1- end) '())))

(defun read-lines (lines)
  (let ((input (read-line *standard-input* nil)))
    (if input
        (read-lines (cons input lines))
        (reverse lines))))

(defun read-array-into (array)
  (labels ((add-line (line input)
             (fill-line array line (coerce input 'list))))
    (progn
      (mapc #'add-line (range 0 (array-dimension array 0)) (read-lines '()))
      array)))

(defun okishp (char)
  (and
   (char/= #\. char)
   (not (digit-char-p char))))

(defun get-window (array i j)
  (let ((rows (range (1- i) (+ i 2)))
        (cols (range (1- j) (+ j 2)))
        (win (build-empty-map '(3 3))))
    (progn
      (labels ((are-valid (r c arr)
                 (and
                  (<= 0 r) (< r (array-dimension arr 0))
                  (<= 0 c) (< c (array-dimension arr 1))))
               (adjust (value corrector) (+ (- value corrector) 1))
               (read-write (rrow rcol from into)
                 (let ((wrow (adjust rrow i))
                       (wcol (adjust rcol j)))
                   (when (and (are-valid rrow rcol from)
                              (are-valid wrow wcol into))
                     (setf (aref into wrow wcol)
                           (aref from rrow rcol))))))
        (dolist (row rows)
          (dolist (col cols)
            (read-write row col array win))))
      win)))

(defun window-is-ok (window pred)
  (let ((result nil))
    (progn
      (dotimes (r 3)
        (dotimes (c 3)
          (setq result (or result (funcall pred (aref window r c))))))
      (and result (alphanumericp (aref window 1 1))))))

(defun left (array i j)
  (if (and (<= 0 i) (< i (array-dimension array 0))
           (<= 1 j) (< j (array-dimension array 1)))
      (aref array i (1- j))
      nil))

(defun right (array i j)
  (if (and (<= 0 i) (< i (array-dimension array 0))
           (<= 0 j) (< j (1- (array-dimension array 1))))
      (aref array i (1+ j))
      nil))

(defun compute-mask (array pred)
  (let* ((rows (array-dimension array 0))
         (cols (array-dimension array 1))
         (mask (make-array (list rows cols) :initial-element nil)))
    (progn
      (dotimes (r rows)
        (dotimes (c cols)
          (let ((win (get-window array r c)))
              (setf (aref mask r c) (window-is-ok win pred)))))
      mask)))

(defun dilate-mask (mask array)
  (let ((modified nil))
    (progn
      (dotimes (r (array-dimension mask 0))
        (dotimes (c (array-dimension mask 1))
          (let* ((rv (right mask r c))
                 (lv (left mask r c))
                 (must-erode (and
                              (alphanumericp (aref array r c))
                              (or lv rv)
                              (not (aref mask r c)))))
              (when must-erode
                (setf (aref mask r c) t)
                (setq modified t)))))
      modified)))

(defun apply-mask (mask array)
  (let ((filtered (build-empty-map (array-dimensions array))))
    (progn
      (dotimes (r (array-dimension mask 0))
        (dotimes (c (array-dimension mask 1))
          (when (aref mask r c)
            (setf (aref filtered r c) (aref array r c)))))
      filtered)))

(defun to-str (array)
  (let ((result (make-array (list (apply #'* (array-dimensions array))) :initial-element #\Space)))
    (progn
      (dotimes (r (array-dimension array 0))
        (dotimes (c (array-dimension array 1))
          (let ((index (+ (* r (array-dimension array 1)) c)))
            (setf (aref result index) (aref array r c)))))
      (coerce result 'string))))

(defun split (str delim)
  (labels ((split-acc (str delim lst)
             (let ((endpos (search delim str)))
               (if endpos
                   (let ((input (subseq str 0 endpos))
                         (rest-input (subseq str (1+ endpos))))
                     (split-acc rest-input delim (cons input lst)))
                   (cons str lst)))))
  (reverse (split-acc str delim '()))))

(defun safe-parse-integer (str)
  (if (numberp str)
      str
      (if (parse-integer str :junk-allowed t)
          (parse-integer str :junk-allowed t)
          0)))

(defun magic (lst)
  (reduce (lambda (lhs rhs)
            (+ (safe-parse-integer lhs) (safe-parse-integer rhs)))
          lst))

(defun refine-mask (mask array)
  (if (dilate-mask mask array)
      (refine-mask mask array)
      mask))

(defun process (dimensions file)
  (let* ((content (build-empty-map dimensions))
         (new-content (with-open-file (*standard-input* file) (read-array-into content)))
         (mask (compute-mask new-content #'okishp))
         (refined-mask (refine-mask mask content))
         (filtered (apply-mask refined-mask content))
         (numbers (split (to-str filtered) ".")))
    (magic numbers)))
