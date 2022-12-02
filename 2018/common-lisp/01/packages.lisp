(defpackage :utils
  (:use :cl)
  (:export :print-result))

(defpackage :part-one
  (:use :cl :series :utils)
  (:export :main))

(defpackage :part-two
  (:use :cl :utils)
  (:export :main))
