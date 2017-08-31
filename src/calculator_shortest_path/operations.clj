(in-ns 'calculator-shortest-path.core)

;;;; Define all operations in this file. Each function must define its operation applied normally and in reverse.
;;;;
;;;; Operation functions have arguments [operand reversed? arg1 arg2 ... ] and may return a value or a list of values
;;;; that are the possible results of applying the operation to the operand. An empty list may be returned to indicate
;;;; no result.
;;;;
;;;; Returned values must be of the same type as operand.

;;; // Sample Operation Set //

(declare add sub div mult app)

(defn parse-int [value]
  (try (Long/valueOf value)
       (catch NumberFormatException e (new BigInteger value))))

(defn add
  "Adds arg1 to operand.
  Reversed: Subtracts arg1 from operand"
  [operand reversed? arg1]
  (let [b (parse-int arg1)]
    (if (false? reversed?)
      (+' operand b)
      (sub operand false arg1))))

(defn sub
  "Subtracts arg1 from operand.
  Reversed: Adds arg1 to operand"
  [operand reversed? arg1]
  (let [b (parse-int arg1)]
    (if (false? reversed?)
      (-' operand b)
      (add operand false arg1))))

(defn div
  "Divides operand by arg1.
  Reversed: Multiplies operand by arg1"
  [operand reversed? arg1]
  (let [b (parse-int arg1)]
    (if (false? reversed?)
      (let [c (/ operand b)]
        (if (ratio? c) '() c))
      (mult operand false arg1))))

(defn mult
  "Multiplies operand by arg1.
  Reversed: Divides operand by arg1"
  [operand reversed? arg1]
  (let [b (parse-int arg1)]
    (if (false? reversed?)
      (*' operand b)
      (div operand false arg1))))

(defn app
  "Appends arg1 to operand.
  Reversed: Removes arg1 from the end of operand if possible"
  [operand reversed? arg1]
  (if (not reversed?)
    (parse-int (str (str operand) arg1))
    (let [operand (str operand)
          arg1 (str arg1)]
      (if (and (ends-with? operand arg1) (not= operand arg1) (not= operand (str "-" arg1)))
        (parse-int (subs operand 0 (- (count operand) (count arg1))))
        '()))))
