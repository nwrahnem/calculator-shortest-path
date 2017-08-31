(ns calculator-the-game.core
  (:use [clojure.string :only [blank? ends-with? join]])
  (:use [clojure.test :only [function?]])
  (:gen-class))

(load "operations")

(defmethod print-method clojure.lang.PersistentQueue [q, w] ; Overload the printer for queues so they look like fish
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(defn arity [v]
  (->> v meta :arglists (map count) first))

(defn apply-operations
  [operations value reversed?]
  (loop [ops operations result '()]
    (if (nil? (peek ops))
      result
      (let [op (peek ops)
            value (apply (ns-resolve 'calculator-the-game.core (first op))
                         (->> (second op) (cons reversed?) (cons value)))
            result (if (seq? value)
                     (loop [result result value value]
                       (if (nil? (peek value))
                         result
                         (recur (conj result (peek value))
                                (pop value))))
                     (conj result value))]
        (recur (pop ops) result)))))

(defn recreate-path
  [int-path operations]
  (loop [full-path [(str "start: " (first int-path))] int-path (vec (rseq int-path))]
    (if (nil? (peek (pop int-path)))
      full-path
      (recur
        (conj full-path
              (loop [ops operations]
                (let [op (peek ops)]
                  (if (= (peek (pop int-path))
                         (apply (ns-resolve 'calculator-the-game.core (first op))
                                (->> (second op) (cons false) (cons (peek int-path)))))

                    (str (first op)
                         (loop [op-params (second op) result ""]
                           (if (nil? (first op-params))
                             result
                             (recur (rest op-params) (str result " " (first op-params))))))
                    (recur (pop ops)))))
              (peek (pop int-path)))
        (pop int-path)))))

(defn extract-path
  [match fromStart fromEnd reversed operations]
  (let [hash-nav (fn [start-point hash-map]
                   (loop [path [start-point]]
                     (let [next (get hash-map (first path))]
                       (if (nil? next)
                         path
                         (recur (into [next] path))))))
        start-map (if reversed fromEnd fromStart)
        end-map (if reversed fromStart fromEnd)]
    (recreate-path
      (into
        (hash-nav match start-map)
        (rest (reverse (hash-nav match end-map))))
      operations)))

(defn compute-path
  [start end operations]
  (loop [startQueue (conj clojure.lang.PersistentQueue/EMPTY start)
         endQueue (conj clojure.lang.PersistentQueue/EMPTY end)
         startVisited {start nil}
         endVisited {end nil}
         reversed? false]
    (do (println (str "Nodes visited: " (+ (count startVisited) (count endVisited))))
        (let [matches (filter (partial contains? startVisited) endQueue)]
          (if (not (empty? matches))
            (extract-path (first matches) startVisited endVisited reversed? operations)

            (let [nextValues
                  (loop [queue startQueue children clojure.lang.PersistentQueue/EMPTY visitedMapping []]
                    (if (nil? (peek queue))
                      {:children children :visitedMapping visitedMapping}

                      (let [values (filter (fn [x] (not (contains? startVisited x)))
                                           (apply-operations operations (peek queue) reversed?))]
                        (recur
                          (pop queue)
                          (apply conj children values)
                          (apply conj visitedMapping
                                 (reduce (fn [val coll] (conj val coll (peek queue)))
                                         [], values))))))]
              (recur
                endQueue
                (:children nextValues)
                endVisited
                (apply assoc startVisited (:visitedMapping nextValues))
                (not reversed?))))))))

(defn -main
  ""
  [& args]
  (loop []
    (let [start (str-to-int (do (println "Enter a start.") (read-line)))
          end (str-to-int (do (println "\nEnter a target.") (read-line)))
          operations
          (do (println "\nEnter a list of operations. Enter nothing to stop.")
              (loop [operations []]
                (let [input (do (println "\nEnter a function.") (read-line))]
                  (if (blank? input)
                    operations
                    (let [function (ns-resolve 'calculator-the-game.core (symbol input))]
                      (if (var? function)
                        (let [args (doall (for [x (range (- (arity function) 2))]
                                            (do
                                              (println (join (list "Enter " (+ x 1) " argument(s).")))
                                              (read-line))))]
                          (recur (conj operations (list (symbol input) args))))
                        (do (println "Not a function.")
                            (recur operations))))))))]
      (do (println "Calculating...")
          (doall (for [step (compute-path start end operations)]
                   (println (pr-str step))))
          (recur))))
  )

