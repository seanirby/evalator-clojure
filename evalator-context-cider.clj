(ns evalator-context-cider)

;; value is swapped when evanaltor session starts
(def special-arg-str (atom nil))

(defn swap-special-arg-str [str]
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

(defn eval-expression [expr-str c]
  (let [form-str (substitute-special-args expr-str c)]
    (eval (read-string form-str))))

(defn make-equiv-expr [exprs]
  (let [sub #(clojure.string/replace %2 @special-arg-str %1)]
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

(defn transform-candidates [c-all c-marked expr-str mode]
  (let [c-allv (vec c-all)
        c-markedv (vec c-marked)]
    (if (nil? c-marked)
      (make-candidates
        (mapv #(eval-expression expr-str (read-string %)) c-allv)
        mode
        false)
      
      (make-candidates
        (vector (eval-expression expr-str (mapv read-string)))
        mode
        false))))
