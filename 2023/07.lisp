(defparameter *order* (reverse '(#\A #\K #\Q #\J #\T
                                 #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2)))

(defparameter *order-02* (reverse '(#\A #\K #\Q #\T
                                 #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2
                                 #\J)))

(defun split (str delim)
  (labels ((split-acc (str delim lst)
             (let ((endpos (search delim str)))
               (if endpos
                   (let ((input (subseq str 0 endpos))
                         (rest-input (subseq str (1+ endpos))))
                     (split-acc rest-input delim (cons input lst)))
                   (cons str lst)))))
    (reverse (split-acc str delim '()))))

(defun counts (hand)
  (labels ((char-counts (curr-char curr-count chars result)
             (if chars
                 (if (char-equal curr-char (car chars))
                     (char-counts curr-char (1+ curr-count) (cdr chars) result)
                     (char-counts (car chars) 1 (cdr chars) (cons curr-count result)))
                 (sort (cons curr-count result) #'>))))
    (let ((decomposed (sort (coerce hand 'list) #'char>)))
      (char-counts (car decomposed) 0 decomposed '()))))

(defun counts-02 (hand)
  (let* ((wo-jokers (remove #\J hand))
         (nb-jokers (- (length hand) (length wo-jokers)))
         (counts-wo-jokers (counts wo-jokers)))
    (cons (+ (car counts-wo-jokers) nb-jokers) (cdr counts-wo-jokers))))

(defun emptyp (str)
  (= 0 (length str)))

(defun list> (lhs rhs)
  (if (or (not lhs) (not rhs))
      nil
      (if (= (car lhs) (car rhs))
          (list> (cdr lhs) (cdr rhs))
          (> (car lhs) (car rhs)))))

(defun hand> (lhs rhs)
  (list> (counts (car lhs)) (counts (car rhs))))

(defun hand= (lhs rhs)
  (and (not (hand> lhs rhs)) (not (hand> rhs lhs))))

(defun hand>-02 (lhs rhs)
  (list> (counts-02 (car lhs)) (counts-02 (car rhs))))

(defun hand=-02 (lhs rhs)
  (and (not (hand>-02 lhs rhs)) (not (hand>-02 rhs lhs))))

(defun hand-values (hand order)
  (mapcar (lambda (x) (position x order)) (coerce hand 'list)))

(defun order> (lhs rhs)
  (list> (hand-values lhs *order*) (hand-values rhs *order*)))

(defun order>-02 (lhs rhs)
  (list> (hand-values lhs *order-02*) (hand-values rhs *order-02*)))

(defun play> (lhs rhs)
  (if (hand= lhs rhs)
      (order> (car lhs) (car rhs))
      (hand> lhs rhs)))

(defun play>-02 (lhs rhs)
  (if (hand=-02 lhs rhs)
      (order>-02 (car lhs) (car rhs))
      (hand>-02 lhs rhs)))

(defun play (input)
  (let ((splitted (split input " ")))
    (cons (car splitted) (parse-integer (cadr splitted)))))

(defun read-lines (lines)
  (let ((input (read-line *standard-input* nil)))
    (cond (input (read-lines (cons input lines)))
          (t (reverse (cons input lines))))))

(defun magic (plays)
  (labels ((power (mult plays result)
             (if plays
                 (power (1- mult)
                        (cdr plays)
                        (+ result (* mult (cdr (car plays)))))
                 result)))
    (power (length plays) plays 0)))

(defun solve-01 (content)
  (let ((plays (mapcar #'play (remove-if #'emptyp content))))
    (magic (sort plays #'play>))))

(defun solve-02 (content)
  (let ((plays (mapcar #'play (remove-if #'emptyp content))))
    (magic (sort plays #'play>-02))))

(defun process (file solver)
  (let ((content (with-open-file (*standard-input* file) (read-lines '()))))
    (funcall solver content)))
