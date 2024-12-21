(defun imperative (lst)
  (let ((L 0)
        (R (1- (list-length lst)))
        (k 0)
        (copy (copy-list lst)))
    (loop
      (when (>= L R) (return copy))
      
      (do ((i L (1+ i)))
          ((= i R) (setq R k))
        (when (> (nth i copy) (nth (1+ i) copy))
          (rotatef (nth i copy) (nth (1+ i) copy))
          (setq k i)))

      (do ((i (1- R) (1- i)))
          ((< i L) (setq L (1+ k)))
        (when (> (nth i copy) (nth (1+ i) copy))
          (rotatef (nth i copy) (nth (1+ i) copy))
          (setq k i))))))

(defun check-imperative (name lst expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (imperative lst) expected)
          name))

(defun test-imperative ()
  (check-imperative "test1" '(0.3 5 0 -1 -3 -3 5.1) '(-3 -3 -1 0 0.3 5 5.1))
  (check-imperative "test2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-imperative "test3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-imperative "test4" '(2 2 2 2 0) '(0 2 2 2 2)))
