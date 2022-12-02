(import "../../util" :as util)

(defn- mass->fuel [mass]
  (- (math/floor (/ mass 3)) 2))

(defn- fuel-requirement [module]
  ((fn recur [sum mass]
     (let [fuel (mass->fuel mass)]
       (cond
         (or (zero? fuel) (neg? fuel)) sum
         :else (recur (+ sum fuel) fuel))))
   0 module))

(defn main [& args]
  (let [modules (->> (util/read-input "./input.txt")
                     (map scan-number))]
    (printf "Part 1: %d\n" (+ ;(map mass->fuel modules)))
    (printf "Part 2: %d\n" (+ ;(map fuel-requirement modules)))))
