(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(defn set->predicate [a-set]
  (fn [x] (or (and (nil? x)
                   (contains? a-set nil))
              (boolean (a-set x)))))

(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x))))

(defn pred-or [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (or (= "" string)
      (nil? string)
      (every? whitespace? string)))

(defn has-award? [book award]
  (contains? (:awards book) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (let [pred (fn [award] (has-award? book award))]
    (= (sort awards) (sort (filter pred awards)))))

(defn truthy? [x]
  (and true (boolean x)))

(defn my-some [pred a-seq]
  (first (filter truthy? (map pred a-seq))))

(defn my-every? [pred a-seq]
  (empty? (filter (complement truthy?) (map pred a-seq))))

(defn prime? [n]
  (let [pred (fn [x] (== 0 (mod n x)))]
    (not (some pred (range 2 n)))))
;^^
