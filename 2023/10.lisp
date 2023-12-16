(defun build-empty-map (dimensions init)
  (make-array dimensions :initial-element init))

(defun make-index (i j)
  (cons i j))

(defun i (index)
  (car index))

(defun j (index)
  (cdr index))

(defun east (index)
  (make-index (i index) (1+ (j index))))

(defun west (index)
  (make-index (i index) (1- (j index))))

(defun north (index)
  (make-index (1- (i index)) (j index)))

(defun south (index)
  (make-index (1+ (i index)) (j index)))

(defun index-is-valid (index array)
  (and
   (< -1 (i index) (array-dimension array 0))
   (< -1 (j index) (array-dimension array 1))))

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

(defun to-array (lines)
  (let ((array (build-empty-map (list (length lines) (length (car lines))) nil)))
    (labels ((add-line (line input)
               (fill-line array line (coerce input 'list))))
      (progn
        (mapc #'add-line (range 0 (array-dimension array 0)) lines)
        array))))

(defun is-char? (array index char)
  (char-equal char (aref array (i index) (j index))))

(defun find-start-index (array)
  (let ((rows (array-dimension array 0))
         (cols (array-dimension array 1)))
      (dotimes (r rows)
        (dotimes (c cols)
          (when (char-equal #\S (aref array r c))
            (return-from find-start-index (cons r c)))))))

(defun array-max (array)
  (let ((rows (array-dimension array 0))
        (cols (array-dimension array 1))
        (max-value -1))
    (dotimes (r rows)
      (dotimes (c cols)
        (when (> (aref array r c) max-value)
          (setf max-value (aref array r c)))))
    max-value))

(defun propagate (array start-index)
  (let ((mask (build-empty-map (array-dimensions array) -1)))
    (labels ((propagate-tail (index value)
               (when (and (index-is-valid index array) (= -1 (aref mask (i index) (j index))))
                 (setf (aref mask (i index) (j index)) value)
                 (cond
                   ((is-char? array index #\-)
                    (progn
                      (propagate-tail (west index) (1+ value))
                      (propagate-tail (east index) (1+ value))))
                   ((is-char? array index #\|)
                    (progn
                      (propagate-tail (north index) (1+ value))
                      (propagate-tail (south index) (1+ value))))
                   ((is-char? array index #\J)
                    (progn
                      (propagate-tail (north index) (1+ value))
                      (propagate-tail (west index) (1+ value))))
                   ((is-char? array index #\7)
                    (progn
                      (propagate-tail (west index) (1+ value))
                      (propagate-tail (south index) (1+ value))))
                   ((is-char? array index #\F)
                    (progn
                      (propagate-tail (south index) (1+ value))
                      (propagate-tail (east index) (1+ value))))
                   ((is-char? array index #\L)
                    (progn
                      (propagate-tail (north index) (1+ value))
                      (propagate-tail (east index) (1+ value))))))))
      (when (char-equal #\S (aref array (i start-index) (j start-index)))
        (setf (aref mask (i start-index) (j start-index)) 0)
        (when (and (index-is-valid (north start-index) array)
                   (or
                    (is-char? array (north start-index) #\7)
                    (is-char? array (north start-index) #\|)
                    (is-char? array (north start-index) #\F)))
          (propagate-tail (north start-index) 1))
        (when (and (index-is-valid (west start-index) array)
                   (or
                    (is-char? array (west start-index) #\F)
                    (is-char? array (west start-index) #\-)
                    (is-char? array (west start-index) #\L)))
          (propagate-tail (west start-index) 1))
        (when (and (index-is-valid (south start-index) array)
                   (or
                    (is-char? array (south start-index) #\J)
                    (is-char? array (south start-index) #\|)
                    (is-char? array (south start-index) #\L)))
          (propagate-tail (south start-index) 1))
        (when (and (index-is-valid (east start-index) array)
                   (or
                    (is-char? array (east start-index) #\7)
                    (is-char? array (east start-index) #\-)
                    (is-char? array (east start-index) #\J)))
          (propagate-tail (north start-index) 1)))
      mask)))

(defun solve-01 (array)
  (/ (+ 1
        (array-max (propagate array
                              (find-start-index array))))
     2))

(defun solve-02 (array)
  array)

(defun process (file solver)
  (let* ((content (with-open-file (*standard-input* file)
                    (read-lines '())))
         (array (to-array content)))
    (funcall solver array)))
