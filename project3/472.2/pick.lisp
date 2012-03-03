#|
for both proj2 and proj3 you'll have to select "things" weighted by some
bias on how much we like "things" (e.g. stinky roads we do *not* like)
it uses a set of useful techniques that i'll walk us through on thursday

the following code is my generic "pick with bias code".  here's the output
of the !pick1 and !pick2 demo.  each demo is 10000 picks, biased by
the weights within th e bias

CL-USER> (!pick1)
"
 d = #S(BIAS
        :ALL ((128 4) (64 3) (32 5) (16 6) (8 2) (4 7) (2 1) (1 0))
        :N 7
        :SORTED T
        :SUM 255)
picks = ((4 50186) (3 25034) (5 12507) (6 6377) (2 3136) (7 1591) (1 793)
         (0 376))
"
CL-USER> (!pick2)
"
 d = #S(BIAS
        :ALL ((128 BLUE) (64 GREEN) (32 TEAL) (16 NAVY) (8 RED) (4 GRAY)
              (2 WHITE) (1 BLACK))
        :N 7
        :SORTED T
        :SUM 255)
picks = ((BLUE 50178) (GREEN 25144) (TEAL 12519) (NAVY 6283) (RED 3137)
         (GRAY 1585) (WHITE 772) (BLACK 382))
"

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; test suite
(defun !pick1 ()
  "returns some index to a list, weighted by the
   bias at each index"
  (labels ((inits (d)
             (dolist (one '(1 2  8 64 128 32 16 4))
               (add d one))))
    (pick-prim #'inits)))

(defun !pick2 ()
  "returns some form, weighted by some weight
   to each form"
  (labels ((inits (d)
             (dolist (one '((1 black) (2 white)  (8 red) (64 green)
                            (128 blue) (32 teal) (16 navy) (4 gray)))
               (add d (first one) (second one)))))

    (pick-prim #'inits)
    ))

(defun pick-prim (inits)
  (let ((d (make-bias)) out (counts (make-hash-table)))
    (funcall inits d)
    (dotimes (i 100000)
      (incf (gethash  (any d) counts 0)))
    (maphash (lambda (k v) (push `(,k ,v) out)) counts)
    (format nil " ~& d = ~a~&picks = ~a~%" d (sort out '> :key 'cadr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; code
(defstruct bias all (n -1) sorted (sum 0))

(defmethod add ((d bias) one &optional what)
  (with-slots (all n sum sorted) d
    (incf n)
    (unless what
      (setf what n))
    (setf all (append all  `((,one ,what)))
          sorted nil)
    (incf sum one)))

(defmethod ready ((d bias))
  (with-slots (all sorted) d
    (unless sorted
      (setf all (sort all #'> :key #'car)
            sorted t))))

(defmethod any ((d bias))
  (ready d)
  (with-slots (all n sum) d
    (let ((x (random sum)))
      (labels
          ((worker (l x)
             (let* ((head   (car l))
                    (weight  (first head))
                    (value   (second head))
                    (xnu (- x weight)))
               (if (< xnu 0)
                   value
                   (worker (cdr l) xnu)))))
        (worker all x)))))
