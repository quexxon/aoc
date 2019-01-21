(in-package :part-two)

(defun find-repeated-sum (ints)
  (let ((prev-sums (make-hash-table :test 'eq))
        (prev-sum 0))
    (loop named outer while t do
      (loop for int in ints
            for sum = (+ int prev-sum)
            do (cond ((gethash sum prev-sums)
                      (return-from outer sum))
                     (t (setq prev-sum sum)
                        (setf (gethash sum prev-sums) t)))))))

(defun process-input-file (file)
  (with-open-file (stream file)
    (loop for int = (read stream nil)
          while int collect int into ints
          finally (return (find-repeated-sum ints)))))

(defun main ()
  (print-result (time (process-input-file "input.txt"))))
