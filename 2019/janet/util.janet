(defn read-input [input]
  (var f nil)
  (var lines [])
  (try
    (do (set f (file/open input :r))
        (set lines (->> (file/read f :all)
                        (string/trim)
                        (string/split "\n")
                        (map string/trim))))
    ([err] (print "Error: " err)))
  (when (not (nil? f))
    (file/close f))
  lines)
