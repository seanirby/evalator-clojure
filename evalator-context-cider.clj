(ns evalator-context-cider)

(defn special-arg-to-index [special-arg-str]
  (Integer. (clojure.string/join "" (rest special-arg-str))))

(defn substitute-numbered-special-args [expr-str c special-arg-str]
  (let [pattern (re-pattern (format "%s[0-9]+" special-arg-str))]
    (clojure.string/replace expr-str pattern #(pr-str (nth c (special-arg-to-index %))))))

(defn substitute-identity-special-args [expr-str c special-arg-str]
  (clojure.string/replace expr-str special-arg-str (pr-str c)))

(defn substitute-special-args [expr-str c special-arg-str]
  (-> expr-str
    (substitute-numbered-special-args c special-arg-str)
    (substitute-identity-special-args c special-arg-str)))

(defn eval-expression [expr-str c special-arg-str]
  (let [form-str (substitute-special-args expr-str c special-arg-str)]
    (eval (read-string form-str))))

(defn make-equiv-expr [exprs special-arg-str]
  (let [sub #(clojure.string/replace %2 special-arg-str %1)]
    (reduce sub exprs)))

(defn make-candidates [input mode initial?]
  "Possibly read and evaluate the arg INPUT before calling
  `make-candidates'"
  (let [data (if initial? (eval (read-string input)) input)]
    (cond (= :explicit mode)
          (if initial?
            (list (pr-str data))
            (list (pr-str (first data))))

          (and (coll? data) (not (map? data)))
          (map pr-str data)

          :else
          (list (pr-str data)))))

(defn transform-candidates [c-all c-marked expr-str mode special-arg-str]
  (let [c-allv (vec c-all)
        c-markedv (vec c-marked)]
    (if (nil? c-marked)
      (make-candidates
        (mapv #(eval-expression expr-str (read-string %) special-arg-str) c-allv)
        mode
        false)
      
      (make-candidates
        (vector (eval-expression expr-str (mapv read-string c-markedv) special-arg-str))
        mode
        false))))










