(in-package :part-one)

(defun process-input-file (file)
  (when (probe-file file)
    (collect-sum (scan-file file))))

(defun main ()
  (print-result (time (process-input-file "input.txt"))))
