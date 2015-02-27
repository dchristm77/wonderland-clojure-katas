(ns alphabet-cipher.coder
  (:use [clojure.string :only (join lower-case upper-case)]))

;; I'm too lazy to type out all those letters; besides this was also a good exercise
;; for me.
(defn create-matrix [s]
  (loop [len (.length s) acc [(vec s)]]
    (if (> len 1)
      (recur (dec len)
             (conj acc
                   (let [last-set (last acc)]
                     (conj (vec (rest last-set)) (first last-set)))))
      acc)))

(def alpha-matrix (create-matrix "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn calc-row-col-val
  "converts a character into a int value representing a row or column in the matrix (0 - 25)"
  [charactor]
  {:pre [(char? charactor) (>= (int (Character/toUpperCase charactor)) (int \A))
         (<= (int (Character/toUpperCase charactor)) (int \Z))]}
  (- (int (Character/toUpperCase charactor)) 65))

(defn repeat-keyword
  "If the message is longer than the keyword then the keyword is concatenated
   to itself until it is equal or greater than the length of the message"
  [keyword message]
  (if (> (.length message) (.length keyword))
    (loop [msg-length (.length message)
           newkeyword keyword]
      (if (> msg-length (.length newkeyword))
        (recur msg-length (.concat newkeyword keyword))
        newkeyword))
    keyword))

(defn gather-coord
  "Creates a vector of coordinate vectors for each letter in the message where
  the coordinates are in [row col] format"
  [keyword message]
  (loop [message message
         secretword (repeat-keyword keyword message)
         count (.length message)
         acc []]
    (if (< 0 count)
      (recur (rest message) (rest secretword) (dec count)
             (conj acc [(calc-row-col-val (first message)) (calc-row-col-val (first secretword))]))
      acc)))

(defn find-row-for-col-value
  "Find the row value (uppercase alpha) for a given value (alpha) in a given column (alpha)"
  [value column]
  (let [nvalue (calc-row-col-val value)
        ncolumn (calc-row-col-val column)]
    (char (+ 65
             (if (>= nvalue ncolumn)
               (- nvalue ncolumn)
               (- (+ 26 nvalue) ncolumn))))))

(defn find-value-for-row-column
  "Find the value in the matrix that corresponds with the given row and column"
  [row col]
  (let [nrow (calc-row-col-val row)
        ncol (calc-row-col-val col)]
    (char (+ 65
             (let [sum (+ nrow ncol)]
               (if (>= sum 26)
                 (- sum 26)
                 sum))))))

;; As it turns out creating the matrix and looking up values was unnecessary.
;; The correct value can be determined by adding the column/row values (A-Z -> 0-25)
;; And if the sum is greater than 26 (size of matrix) subtract 26
(defn old-old-encode [keyword message]
  (join
    (map (comp lower-case (partial get-in alpha-matrix))
         (gather-coord keyword message))))

(defn old-encode [keyword message]
  ((comp lower-case join)
    (loop [message message
           secretword (repeat-keyword keyword message)
           count (.length message)
           acc []]
      (if (< 0 count)
        (recur (rest message) (rest secretword) (dec count)
               (conj acc (find-value-for-row-column (first message) (first secretword))))
        acc))))

(defn old-decode [keyword message]
  ((comp lower-case join)
    (loop [message message
           secretword (repeat-keyword keyword message)
           count (.length message)
           acc []]
      (if (< 0 count)
        (recur (rest message) (rest secretword) (dec count)
               (conj acc (find-row-for-col-value (first message) (first secretword))))
        acc))))

;; So much duplication above. refactor out the common code into another
;; function that takes a function to apply (the only difference)
(defn apply-encode-decode [keyword message matrix-function]
  ((comp lower-case join)
    (loop [message message
           secretword (repeat-keyword keyword message)
           count (.length message)
           acc []]
      (if (< 0 count)
        (recur (rest message) (rest secretword) (dec count)
               (conj acc (matrix-function (first message) (first secretword))))
        acc))))

;; final versions!!
(defn encode [keyword message]
  (apply-encode-decode keyword message find-value-for-row-column))

(defn decode [keyword message]
  (apply-encode-decode keyword message find-row-for-col-value))
