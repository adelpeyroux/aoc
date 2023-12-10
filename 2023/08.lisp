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

(defun read-move-instruction (input)
  (coerce input 'list))

(defun clean-direction-input (input)
  (coerce (remove #\(
                  (remove #\)
                          (remove #\,
                                  (coerce input 'list))))
          'string))


(defun read-node (input)
  (let* ((from (string-trim " " (car (split input "="))))
         (to-input (clean-direction-input (string-trim " " (cadr (split input "=")))))
         (to (split to-input " ")))
    (cons from (cons (car to) (cadr to)))))

(defun read-nodes (inputs)
  (mapcar #'read-node inputs))

(defun apply-move (from mv-instr nodes)
  (if mv-instr
      (cond
        ((char-equal #\R (car mv-instr)) (apply-move (cdr (cdr (assoc from
                                                                      nodes
                                                                      :test #'string-equal)))
                                                     (cdr mv-instr)
                                                     nodes))
        ((char-equal #\L (car mv-instr)) (apply-move (car (cdr (assoc from
                                                                      nodes
                                                                      :test #'string-equal)))
                                                     (cdr mv-instr)
                                                     nodes)))
      from))

(defun endsAp (str)
  (char-equal #\A (car (last (coerce str 'list)))))

(defun endsZp (str)
  (char-equal #\Z (car (last (coerce str 'list)))))

(defun how-many-move-until (from pred mv-instr nodes)
  (labels ((hmmu-tail (from count)
             (if (funcall pred from)
                 count
                 (hmmu-tail (apply-move from mv-instr nodes)
                            (1+ count)))))
    (hmmu-tail from 0)))


(defun how-many-move-to (from to mv-instr nodes)
  (labels ((hmmt-tail (from to count)
             (if (string-equal from to)
                 count
                 (hmmt-tail (apply-move from mv-instr nodes)
                            to
                            (1+ count)))))
    (hmmt-tail from to 0)))

(defun solve-01 (content)
  (let ((mv-instr (read-move-instruction (caar content)))
        (nodes (read-nodes (cadr content))))
    (* (length mv-instr)
       (how-many-move-to "AAA" "ZZZ" mv-instr nodes))))

(defun solve-02 (content)
  (let* ((mv-instr (read-move-instruction (caar content)))
         (nodes (read-nodes (cadr content)))
         (tos (mapcar #'car nodes)))
    (apply #'lcm
           (mapcar
            (lambda (x)
              (* (length mv-instr) x))
            (mapcar (lambda (node)
                     (how-many-move-until node #'endszp mv-instr nodes))
                   (remove-if-not #'endsAp tos))))))

(defun process (file solver)
  (let ((content (with-open-file (*standard-input* file)
                   (split-list-if (read-lines '()) #'emptyp))))
    (funcall solver content)))
