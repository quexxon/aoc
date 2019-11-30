(defn read-input [path]
  (var f nil)
  (var input nil)
  (try
    (do
      (set f (file/open path :r))
      (set input
           (->> (file/read f :all)
                (string/trim)
                (string/split "\n"))))
    ([err] (print "Error: " err)))
  (when (not (nil? f))
    (file/close f))
  input)

(defn count-letters [str]
  (reduce (fn [counts letter]
            (set (counts letter)
                 (+ 1 (or (in counts letter) 0)))
            counts)
          @{}
          (string/bytes str)))

(defn box-value [box]
  (def value @{2 false 3 false})
  (var early-exit false)
  (let [counts (count-letters box)]
    (loop [count :in (values counts) :until early-exit]
      (cond
        (== count 2) (set (value 2) true)
        (== count 3) (set (value 3) true))
      (when (and (in value 2) (in value 3))
        (set early-exit true)))
    value))

(defn calculate-checksum [boxes]
  (var twos 0)
  (var threes 0)
  (loop [box :in boxes]
    (let [value (box-value box)]
      (when (in value 2)
        (++ twos))
      (when (in value 3)
        (++ threes))))
  (* twos threes))

(defn diff-chars [s1 s2]
  (var count 0)
  (loop [i :range [0 (- (length s1) 1)]]
    (when (not (== (in s1 i) (in s2 i)))
      (++ count)))
  count)

(defn find-off-by-one [input]
  (var offset 1)
  (var match nil)
  (loop [box1 :in input :after (++ offset) :until match]
    (loop [box2 :in (slice input offset) :until match]
      (when (one? (diff-chars box1 box2))
        (set match [box1 box2]))))
  match)

(defn common-bytes [s1 s2]
  (var bytes @[])
  (var i 0)
  (loop [byte :in s1 :after (++ i)]
    (when (== byte (in s2 i))
      (array/push bytes byte)))
  (string/from-bytes ;bytes))

(defn main [& args]
  (let [input (read-input "./input.txt")]
    (when (nil? input)
      (error "Failed to read input"))
    (print "Part 1: " (calculate-checksum input))
    (let [[s1 s2] (find-off-by-one input)]
      (print "Part 2: " (common-bytes s1 s2)))))
