(ns evalator-context-cider)

;; value is swapped when evalator session starts
(def special-arg-str (atom nil))

(defn swap-special-arg-str [str]
  "Swaps special-arg-str atom with STR"
  (swap! special-arg-str #(identity %2) str))

(defn special-arg-to-index []
  (Integer. (clojure.string/join "" (rest @special-arg-str))))

(defn substitute-numbered-special-args [expr-str c]
  (let [pattern (re-pattern (format "%s[0-9]+" @special-arg-str))]
    (clojure.string/replace expr-str pattern #(pr-str (nth c (special-arg-to-index))))))

(defn substitute-identity-special-args [expr-str c]
  (clojure.string/replace expr-str @special-arg-str (pr-str c)))

(defn substitute-special-args [expr-str c]
  (-> expr-str
    (substitute-numbered-special-args c)
    (substitute-identity-special-args c)))

(defn eval-expression [expr-str]
  "Evaluate the expression string, EXPR-STR."
  (eval (read-string expr-str)))

(defn make-equiv-expr [exprs]
  (let [sub #(clojure.string/replace %2 @special-arg-str %1)]
    (reduce sub exprs)))

(defn make-candidates [input mode initial?]
  "See slot documentation in evalator-context.el in evalator package."
  (let [data (if initial? (eval (read-string input)) input)]
    (cond (= :explicit mode)
          (if initial?
            (list (pr-str data))
            (list (pr-str (first data))))

          (and (coll? data) (not (map? data)))
          (map pr-str data)

          :else
          (list (pr-str data)))))

(defn transform-candidates [cands expr-str mode collect?]
  "See slot documentation in evalator-context.el in evalator package."
  (let [cands-v (vec cands)]
    (if collect?
      (make-candidates
        (vector (eval-expression (subst-special-args expr-str (mapv read-string cands-v))))
        mode
        false)
      (make-candidates
        (mapv #(eval-expression (subst-special-args expr-str (read-string %))) cands-v)
        mode
        false))))
