(in-ns 'calculator-the-game.core)

(declare add sub div mult rev app)

(defn str-to-int [string]
  (try (Long/valueOf string)
       (catch NumberFormatException e (new BigInteger string))))

(defn add [a reversed? op-value]
  (let [b (str-to-int op-value)]
    (if (false? reversed?)
      (+' a b)
      (sub a false op-value))))

(defn sub [a reversed? op-value]
  (let [b (str-to-int op-value)]
    (if (false? reversed?)
      (-' a b)
      (add a false op-value))))

(defn div [a reversed? op-value]
  (let [b (str-to-int op-value)]
    (if (false? reversed?)
      (let [c (/ a b)]
        (if (ratio? c) '() c))
      (mult a false op-value))))

(defn mult [a reversed? op-value]
  (let [b (str-to-int op-value)]
    (if (false? reversed?)
      (*' a b)
      (div a false op-value))))

(defn rev [a reversed?]
  (if (and reversed? (= (take-last 1 (str a)) (seq "0")))
    '()
    (let [number-reversed
          (loop [number a number-reversed 0]
            (if (= number 0)
              number-reversed
              (recur
                (quot number 10)
                (+' (rem number 10) (*' number-reversed 10)))))]
      (if (not reversed?)
        number-reversed
        (loop [numbers (list number-reversed)]
          (if (>= (count (str (first numbers))) 20)
            numbers
            (recur (conj numbers (*' (first numbers) 10)))))))))

(defn app [a reversed? op-value]
  (if (not reversed?)
    (str-to-int (str (str a) op-value))
    (let [a (str a)
          end (str op-value)]
      (if (and (ends-with? a end) (not (= a end)) (not (= a end)))
        (str-to-int (subs a 0 (- (count a) (count end))))
        '()))))
