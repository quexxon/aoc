(import "../util" :as util)

(def Computer
  @{:pc 0
    :flags @{:halt false}
    :memory @[]
    :instructions {1 {:method :add :arity 3}
                   2 {:method :multiply :arity 3}
                   99 {:method :halt :arity 0}}
    :run (fn [self &opt pc]
           (let [pc (or pc 0)]
             (set (self :pc) pc)
             (set ((self :flags) :halt) false)
             (while (not (get-in self [:flags :halt]))
               (let [opcode (:read self (self :pc))
                     instruction ((self :instructions) opcode)]
                 (if (nil? instruction)
                   (error (string "Invalid opcode: " opcode))
                   (:apply-instruction self instruction))))))
    :apply-instruction (fn [self {:method method :arity arity}]
                         (var args @[])
                         (loop [offset :range [1 (+ arity 1)]]
                           (let [address (+ (self :pc) offset)
                                 arg (:read self address)]
                             (array/push args arg)))
                         (method self ;args)
                         (unless (= method :halt)
                           (:increment-pc self (+ arity 1))))
    :read (fn [self address] (in (self :memory) address))
    :write (fn [self address data] (set ((self :memory) address) data))
    :increment-pc (fn [self &opt inc]
                    (let [inc (or inc 1)]
                      (+= (self :pc) inc)))
    :apply-operator (fn [self x-addr y-addr dest operator]
                      (let [x (:read self x-addr)
                            y (:read self y-addr)]
                        (:write self dest (operator x y))))
    :add (fn [self x-addr y-addr dest]
           (:apply-operator self x-addr y-addr dest +))
    :multiply (fn [self x-addr y-addr dest]
                (:apply-operator self x-addr y-addr dest *))
    :halt (fn [self] (set ((self :flags) :halt) true))})

(defn make-computer [memory]
  (let [state @{:pc 0
                :flags @{:halt false}
                :memory memory}]
    (table/setproto state Computer)))

(defn part-one [memory]
  (let [computer (make-computer (array/slice memory))]
    (set ((computer :memory) 1) 12)
    (set ((computer :memory) 2) 2)
    (:run computer)
    (printf "Part 1: %d\n" (get (computer :memory) 0))))

(defn part-two [memory]
  (let [target-output 19690720]
    (var break false)
    (loop [noun :range [0 100] :until break]
      (loop [verb :range [0 100] :until break]
        (let [computer (make-computer (array/slice memory))]
          (set ((computer :memory) 1) noun)
          (set ((computer :memory) 2) verb)
          (:run computer)
          (when (== (:read computer 0) target-output)
            (printf "Part 2: %d\n" (+ (* 100 noun) verb))
            (set break true)))))))

(defn main [& args]
  (let [memory (as-> (util/read-input "../../input/02.txt") _
                     (in _ 0)
                     (string/split "," _)
                     (map scan-number _))]
    (part-one memory)
    (part-two memory)))
