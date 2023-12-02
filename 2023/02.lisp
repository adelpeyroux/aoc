(defun cube-draw (rv gv bv)
  (pairlis '(red green blue) (list rv gv bv)))

(defparameter *limit* (cube-draw 12 13 14))

(defun color (draw color)
  (let ((fcons (assoc color draw)))
    (if fcons
        (cdr fcons)
        0)))

(defun red (draw)
  (color draw 'red))

(defun green (draw)
  (color draw 'green))

(defun blue (draw)
  (color draw 'blue))

(defun under-limit (cubes limit)
  (let ((cr (red cubes))
        (cg (green cubes))
        (cb (blue cubes))
        (lr (red limit))
        (lg (green limit))
        (lb (blue limit)))
    (and (<= cr lr)
         (<= cg lg)
         (<= cb lb))))

(defun check-game (draws limit)
  (every #'identity (mapcar (lambda (draw) (under-limit draw limit)) draws)))

(defun extract-game-input (input)
  (let ((endpos (search ":" input)))
    (subseq input 0 (1+ endpos))))

(defun extract-draws-input (input)
  (let ((startpos (search ":" input)))
    (subseq input (1+ startpos))))

(defun process-color (input)
  (let* ((clear-input (string-trim " " input))
         (pos (search " " clear-input))
         (value (parse-integer (subseq clear-input 0 pos)))
         (key (read-from-string (subseq clear-input pos))))
    (cons key value)))

(defun process-draw (input)
  (let ((endpos (search "," input)))
    (if endpos
        (let ((color-input (subseq input 0 endpos))
              (rest-input  (subseq input (1+ endpos))))
          (cons (process-color color-input) (process-draw rest-input)))
        (cons (process-color input) nil))))

(defun process-draws (input)
  (let ((endpos (search ";" input)))
    (if endpos
        (let ((draw-input (subseq input 0 endpos))
              (rest-input (subseq input (1+ endpos))))
          (cons (process-draw draw-input) (process-draws rest-input)))
        (cons (process-draw input) nil))))

(defun read-game-id (input)
  (let ((endpos (search ":" input))
        (startpos (search " " input)))
    (parse-integer (subseq input startpos endpos))))

(defun process-entry-one (input)
  (let* ((game-input  (extract-game-input input))
         (game-id     (read-game-id game-input))
         (draws-input (extract-draws-input input))
         (draws       (process-draws draws-input)))
    (if (check-game draws *limit*)
        game-id
        0)))

(defun draw-max (lhs rhs)
  (cube-draw (max (red lhs) (red rhs))
             (max (green lhs) (green rhs))
             (max (blue lhs) (blue rhs))))

(defun compute-max-draw (draws)
  (reduce #'draw-max draws))

(defun draw-power (draw)
  (* (red draw) (green draw) (blue draw)))

(defun process-entry-two (input)
  (let* ((draws-input (extract-draws-input input))
         (draws       (process-draws draws-input))
         (max-draw    (compute-max-draw draws)))
    (draw-power max-draw)))

(defun process-one (acc)
  (let ((input (read-line *standard-input* nil)))
    (if input
        (process-one (cons (process-entry-one input) acc))
        (apply #'+ acc))))

(defun process-two (acc)
  (let ((input (read-line *standard-input* nil)))
    (if input
        (process-two (cons (process-entry-two input) acc))
        (apply #'+ acc))))
