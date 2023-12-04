(defparameter *possibles* '(("1" . 1) ("one"   . 1)
                            ("2" . 2) ("two"   . 2)
                            ("3" . 3) ("three" . 3)
                            ("4" . 4) ("four"  . 4)
                            ("5" . 5) ("five"  . 5)
                            ("6" . 6) ("six"   . 6)
                            ("7" . 7) ("seven" . 7)
                            ("8" . 8) ("eight" . 8)
                            ("9" . 9) ("nine"  . 9)))

(defun get-indices-of (possibles str &key (from-end nil))
  (mapcar (lambda (seq1 seq2) (search seq1 seq2 :from-end from-end))
          (mapcar #'car possibles)
          (make-list (length *possibles*) :initial-element str)))

(defun find-first (pred lst)
  (if lst
      (if (funcall pred (car lst))
          (car lst)
          (find-first pred (cdr lst)))))

(defun ord-index (lst ord)
  (labels ((ord-index-tailrec (lst n actual index)
           (if lst
               (if (and (car lst) (or (funcall ord (car lst) actual)
                                      (= (car lst) actual)))
                   (ord-index-tailrec (cdr lst) (+ 1 n) (car lst) n)
                   (ord-index-tailrec (cdr lst) (+ 1 n) actual index))
               index)))
    (ord-index-tailrec lst 0 (find-first #'numberp lst) nil)))

(defun first-digit (str)
  (let ((indices (get-indices-of *possibles* str)))
    (cdr (nth (ord-index indices #'<) *possibles*))))

(defun last-digit (str)
  (let ((indices (get-indices-of *possibles* str :from-end t)))
    (cdr (nth (ord-index indices #'>) *possibles*))))

(defun extract-coordinates (str)
  (let ((first-num (first-digit str))
        (second-num (last-digit str)))
    (+ (* 10 first-num) second-num)))

(defun process (entries)
  (let ((entry (read-line *standard-input* nil)))
    (if (and entry (> (length entry) 0))
        (process (cons (extract-coordinates entry) entries))
        (apply #'+ entries))))

;;(load "01.lisp")
;;(with-open-file (*standard-input* "01.txt") (process '()))
