(ns helm-lambda-context-cider)

(def special-arg-identity "%")
(def special-arg-nth #"%[0-9]")

(defn wrap-string [s1 s2]
  "Add s1 to the beginning and end of s2"
  (str s1 s2 s1))

(defn substitute-special-arg [sym single-or-coll]
  "Substitutes the special args defined by the special-arg-* vars with
  their proper values. For example, % refers to single-or-coll and %0
  refers to the 0th element in single-or-coll"
  (let [sym-str (str sym)]
    (cond (= sym (symbol special-arg-identity))
          single-or-coll
          
          (not (nil? (re-matches special-arg-nth sym-str)))
          (single-or-coll (Character/getNumericValue (last sym-str)))
          
          :else sym)))

(defmulti make-candidates
  "Converts data so it can be used as a set of candidates in helm. In
  other words, a list of the stringed representation of data."
  (fn [data]
    (cond
      (map? data) :default
      (coll? data) :coll
      (or (char? data) (string? data)) :string
      :else :default)))

(defmethod make-candidates :coll
  [coll]
  (map #(if (string? %) (wrap-string "\"" %) (str %)) coll))

(defmethod make-candidates :string
  [s]
  (list (wrap-string "\"" s)))

(defmethod make-candidates :default
  [data]
  (list data))

(defn apply-expression
  "Given an expression string, expr-str, and a candidate string,
  c-str, this function will first read each, substitute any special
  arguments in the expression, and then evaluate the expression."
  [expr c]
  (let [form (map #(substitute-special-arg % c) expr)]
    (eval form)))

(defn transform-candidates [c-all c-marked expr-string]
  (let [c-allv (vec c-all)
        c-markedv (vec c-marked)
        expr (read-string expr-string)]
    (if (nil? c-marked)
      (make-candidates (mapv #(apply-expression expr (read-string %)) c-allv))
      (make-candidates (vector (apply-expression expr (mapv read-string c-markedv)))))))













