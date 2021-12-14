(setq filename (car command-line-args-left))

(defun read-file (filename)
  "Returns the input file as 2-dim array"
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (setq data nil)
    (while (not (eobp))
      (setq data (cons (vconcat (mapcar (lambda (x) (- x 48))
					(buffer-substring (line-beginning-position)
							  (line-end-position))))
		       data))
      (forward-line 1))
    (vconcat (reverse data))))

(defun get-value (data pos)
  "Get a single value on the matrix"
  (elt (elt data (cdr pos)) (car pos)))

(defun set-value (data pos value)
  "Set a single value on the matrix"
  (aset (elt data (cdr pos)) (car pos) value))

(defun increase-all (data)
  "Increase all values of the matrix"
  (cl-map 'vector (lambda (x) (cl-map 'vector #'1+ x)) data))
  
(defun increase-some (data some)
  "Increase only the values in the matrix listed in some"
  (let (pos value)
    (dolist (pos some)
      (setq value (get-value data pos)) 
      (if (> value 0)
	  (set-value data pos (1+ value))))
    data))

(defun explosions (data)
  "Check which fields exploded in the 2-dim array"
  (let (res)
    (dotimes (i 10)
      (dotimes (j 10)
	(let (v)
	  (setq v (aref (aref data j) i))
	  (if (> v 9) (setq res (cons (cons i j) res))))))
    res))

(defun expand-pos (pos)
  "Compute all neighbouring positions of a single position"
  (let ((x (car pos)) (y (cdr pos)))
    (append (if (> x 0) (list (cons (1- x) y)) nil)
	    (if (< x 9) (list (cons (1+ x) y)) nil)
	    (if (> y 0) (list (cons x (1- y))) nil)
	    (if (< y 9) (list (cons x (1+ y))) nil)
	    (if (and (> x 0) (> y 0)) (list (cons (1- x) (1- y))) nil)
	    (if (and (< x 9) (> y 0)) (list (cons (1+ x) (1- y))) nil)
	    (if (and (> x 0) (< y 9)) (list (cons (1- x) (1+ y))) nil)
	    (if (and (< x 9) (< y 9)) (list (cons (1+ x) (1+ y))) nil))))

(defun increment-and-explode (data)
  "Compute one iteration of incrementing all elements and exploding"
  (let (incdata exploding flashes)
    (setq incdata (increase-all data))
    (setq exploding (explosions incdata))
    (setq flashes 0)
    (while exploding
      (let (pos)
	(setq pos (car exploding))
	(if (> (get-value incdata pos) 0)
	    (setq flashes (1+ flashes)))
	(set-value incdata pos 0)
	(increase-some incdata (expand-pos pos))
	(setq exploding (explosions incdata))))
    (cons flashes incdata)))

(defun all-flash (data)
  "Are all elements flashing?"
  (= (apply #'+ (cl-map 'list (lambda (x) (seq-reduce #'+ x 0)) data)) 0))

(defun count-flashes (data iterations)
  "Count the number of flashes for a given number of iterations"
  (let (result flashes)
    (setq flashes 0)
    (dotimes (c iterations)
      (setq result (increment-and-explode data))
      (setq data (cdr result))
      (setq flashes (+ flashes (car result))))
    flashes))

(defun count-until-sync (data)
  "Count the number of iterations needed until everything flashes"
  (let (result counter)
    (setq counter 0)
    (while (not (all-flash data))
      (setq data (cdr (increment-and-explode data)))
      (setq counter (1+ counter)))
    counter))

(setq data (read-file filename))
(print (count-flashes data 100))
(print (count-until-sync data))
