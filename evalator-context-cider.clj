(ns evalator-context-cider
  (:require [clojure.string :as s :refer [replace join]]))

;; value is swapped when evalator session starts
(def special-arg-str (atom nil))

(defn swap-special-arg-str [str]
  "Swaps special-arg-str atom with STR"
  (swap! special-arg-str #(identity %2) str))

(defn numbered-arg-pattern
  "Return the regex pattern used to match a numbered special arg like
   \" ⒺN \".  If QUOTE? is true then a pattern is returned that can also
   match a quoted numbered special arg like \"'ⒺN\"."
  ([]
   (numbered-arg-pattern false))
  ([quote?]
   (let [frmt (if quote? "'?%s([0-9]+)" "%s([0-9]+)")]
     (re-pattern (format frmt @special-arg-str)))))

(defn identity-arg-pattern
  "Return the regex pattern used to match identity special args.  If
   QUOTE? is true then a pattern is returned that can also match a
   quoted identity special arg like \"'Ⓔ\"."
  ([]
   (identity-arg-pattern false))
  ([quote?]
   (let [frmt (if quote? "'?%s" "%s")]
     (re-pattern (format frmt @special-arg-str)))))

(defn subst-numbered-special-args [expr-str c]
  "Substitute any special args of the form \"ⒺN\" in EXPR-STR with the
  Nth element in C."
  (s/replace expr-str (numbered-arg-pattern) #(pr-str (nth c (Integer. (last %))))))

(defn subst-identity-special-args [expr-str c]
  "Substitute any special args of the form \"Ⓔ\" in EXPR-STR with C."
  (s/replace expr-str (identity-arg-pattern) (pr-str c)))

(defn subst-special-args [expr-str c]
  "Substitute any special args in EXPR-STR.  Identity special args
   like \"Ⓔ\" are substituted with the value of C.  Numbered special
   args like \"ⒺN\" are substituted with the Nth element in C."
  (-> expr-str
    (subst-numbered-special-args c)
    (subst-identity-special-args c)))

(defn eval-expression [expr-str]
  "Evaluate the expression string, EXPR-STR."
  (eval (read-string expr-str)))

(defn make-equiv-expr [exprs]
  "See slot documentation in evalator-context.el in evalator package."
  (let [sub-numbered-args #(s/replace %2 (numbered-arg-pattern true) (format "(nth %s $1)" %1))
        sub-identity-args #(s/replace %2 (identity-arg-pattern true) %1)
        sub (fn [e1 e2] (sub-identity-args e1 (sub-numbered-args e1 e2)))]
    (reduce sub exprs)))

(defn make-candidates [input mode]
  "See slot documentation in evalator-context.el in evalator package."
  (let [data (eval (read-string input))]
    (cond (= :explicit mode)
          (list (pr-str data))

          (sequential? data)
          (map pr-str data)

          :else
          (list (pr-str data)))))

(defn transform-candidates [cands expr-str collect?]
  "See slot documentation in evalator-context.el in evalator package."
  (let [cands-v (vec cands)]
    (map pr-str
      (if collect?
        (list (eval-expression (subst-special-args expr-str (mapv read-string cands-v))))
        (map #(eval-expression (subst-special-args expr-str (read-string %))) cands-v)))))

