(defn- find-repeated-sum [numbers]
  (def previous-sums @{})
  (var sum 0)
  (var found-repeat false)
  (while (not found-repeat)
    (loop [n :in numbers :until found-repeat]
      (if (in previous-sums (+= sum n))
        (set found-repeat true)
        (set (previous-sums sum) true))))
  sum)

(defn main [& args]
  (var f nil)
  (try
    (do
      (set f (file/open "./input.txt" :r))
      (def numbers
        (->> (file/read f :all)
             (string/trim)
             (string/split "\n")
             (map scan-number)))
      (print "Part 1: " (+ ;numbers))
      (print "Part 2: " (find-repeated-sum numbers)))
    ([err]
     (print "Error: " err)))
  (if (not (nil? f))
    (file/close f)))
