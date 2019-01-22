(defun char-counts (box-id)
  (loop for char across box-id
        with counts = (make-hash-table :test 'eq)
        do (incf (gethash char counts 0))
        finally (return counts)))

(defun twos-and-threes-p (char-counts)
  (loop for v being the hash-values in char-counts
        with twos = 0
        and threes = 0
        do (case v
             (2 (incf twos))
             (3 (incf threes)))
        finally (return (values (> twos 0) (> threes 0)))))

(defun process-input-file (file)
  (with-open-file (stream file)
    (loop for box-id = (read-line stream nil)
          with twos = 0
          and threes = 0
          while box-id
          do (multiple-value-bind (has-twos has-threes)
                 (twos-and-threes-p (char-counts box-id))
               (when has-twos (incf twos))
               (when has-threes (incf threes)))
          finally (return (* twos threes)))))

(defun main ()
  (format t "Result: ~a~%" (time (process-input-file "input.txt"))))

(main)
