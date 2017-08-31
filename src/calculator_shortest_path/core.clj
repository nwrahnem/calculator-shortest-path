(ns calculator-shortest-path.core
  (:require [clojure.string :refer [blank? ends-with? join]])
  (:require [clojure.test :refer [function?]])
  (:gen-class))

(load "operations")

(defmethod print-method clojure.lang.PersistentQueue [q, w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(defn arity
  "Returns the arity of the function v."
  [v]
  (->> v meta :arglists (map count) first))

(defn apply-operations
  "Returns a list of values that are the result of applying all the given operations to value."
  [operations value reversed?]
  (loop [ops operations result '()]
    (if-not (seq ops)
      result
      (let [op (peek ops)
            value (apply (ns-resolve 'calculator-shortest-path.core (first op))
                         (->> (second op) (cons reversed?) (cons value)))
            result (if (seq? value)
                     (into result value)
                     (conj result value))]
        (recur (pop ops) result)))))

(defn recreate-path
  "Appends the list of traversed operations to the result."
  [int-path operations]
  (list int-path
        (loop [full-path [] int-path (vec (rseq int-path))]
          (if-not (seq (pop int-path))
            full-path
            (recur
              (conj full-path
                    (loop [ops operations]
                      (let [op (peek ops)]
                        (if (= (peek (pop int-path))
                               (apply (ns-resolve 'calculator-shortest-path.core (first op))
                                      (->> (second op) (cons false) (cons (peek int-path)))))

                          op
                          (recur (pop ops))))))
              (pop int-path))))))

(defn extract-path
  "Extracts the path from the visited lists."
  [match startVisited endVisited reversed operations]
  (let [hash-nav (fn [start-point hash-map]
                   (loop [path [start-point] node start-point]
                     (if-let [next (get hash-map node)]
                       (recur (conj path next) next)
                       path)))
        start-map (if reversed endVisited startVisited)
        end-map (if reversed startVisited endVisited)]
    (recreate-path
      (into
        (vec (rseq (hash-nav match start-map)))
        (rest (hash-nav match end-map)))
      operations)))

(defn compute-path
  "Computes the path from start to end using the given set of operations, where operations is a collection of the form
   (('operation1 (arg1 arg2 ..)) ('operation2 (arg1 arg2 ..)) ..) with each operation being a symbol that refers to
   a function defined in operations.clj or elsewhere within the namespace."
  [start end operations]
  (loop [startQueue (conj clojure.lang.PersistentQueue/EMPTY start)
         endQueue (conj clojure.lang.PersistentQueue/EMPTY end)
         startVisited {start nil}
         endVisited {end nil}
         reversed? false]
    (do (println (str "Nodes visited: " (+ (count startVisited) (count endVisited))))
        (let [matches (filter (partial contains? startVisited) endQueue)]
          (if (seq matches)
            (extract-path (first matches) startVisited endVisited reversed? operations)
            (let [nextValues
                  (loop [queue startQueue children clojure.lang.PersistentQueue/EMPTY visitedMapping []]
                    (if-not (seq queue)
                      {:children children :visitedMapping visitedMapping}

                      (let [values (filter (complement (partial contains? startVisited))
                                           (apply-operations operations (peek queue) reversed?))]
                        (recur
                          (pop queue)
                          (apply conj children values)
                          (apply conj visitedMapping (reduce #(conj %1 %2 (peek queue)) [], values))))))]
              (recur
                endQueue
                (:children nextValues)
                endVisited
                (apply assoc startVisited (:visitedMapping nextValues))
                (not reversed?))))))))

(defn -main
  "Example command line client for entering integer input."
  [& args]
  (loop []
    (let [start (parse-int (do (println "\nEnter a start.") (read-line)))
          end (parse-int (do (println "\nEnter a target.") (read-line)))
          operations
          (do (println "\nEnter a list of operations. Enter nothing to stop.")
              (loop [operations []]
                (let [input (do (println "\nEnter a function.") (read-line))]
                  (if (blank? input)
                    operations
                    (let [function (ns-resolve 'calculator-shortest-path.core (symbol input))]
                      (if (var? function)
                        (let [args (doall (for [x (range (- (arity function) 2))]
                                            (do
                                              (println (join (list "Enter " (inc x) " argument(s).")))
                                              (read-line))))]
                          (recur (conj operations (list (symbol input) args))))
                        (do (println "Not a function.")
                            (recur operations))))))))]
      (do (println "Calculating...")
          (let [result (compute-path start end operations)
                int-path (first result)
                op-path (second result)]
            (do (println (str "\nstart: " (first int-path)))
                (doall (map (fn [a b]
                              (println (str (first a)
                                            (loop [op-params (second a) result ""]
                                              (if (nil? (first op-params))
                                                result
                                                (recur (rest op-params) (str result " " (first op-params)))))
                                            ": " b)))
                            op-path, (rest int-path)))))
          (recur)))))

