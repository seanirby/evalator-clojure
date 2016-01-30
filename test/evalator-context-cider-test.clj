(ns evalator-context-cider-test.core)
(refer 'clojure.test)
(load-file "../evalator-context-cider.clj")
(refer 'evalator-context-cider)

(defn special-arg-fixture [f]
  ;; @ is easier to type
  (swap! special-arg-str #(identity %2) "@")
  (f)
  ;; In case the test modifies this
  (swap! special-arg-str #(identity %2) "@"))

(defrecord TestRec [foo bar baz])

(use-fixtures :each special-arg-fixture)

(deftest swap-special-arg-str-test
  (is (= "!"
        (swap-special-arg-str "!"))))

(deftest numbered-arg-pattern-test
  (is (= (print-str #"@([0-9]+)")
        (print-str (numbered-arg-pattern))))
  (is (= (print-str #"'?@([0-9]+)")
        (print-str (numbered-arg-pattern true)))))

(deftest identity-arg-pattern-test
  (is (= (print-str #"@")
        (print-str (identity-arg-pattern))))
  (is (= (print-str #"'?@")
        (print-str (identity-arg-pattern true)))))

(deftest subst-numbered-special-args-test
  (is (= "foo bar baz"
        (subst-numbered-special-args "foo @0 baz" ['bar]))))

(deftest subst-identity-special-args-test
  (is (= "foo bar baz"
        (subst-identity-special-args "foo @ baz" 'bar))))

(deftest subst-special-args-test
  (is (= "0 [0 1 2]"
        (subst-special-args "@0 @" [0 1 2]))))

(deftest eval-expression-test
  (is (= 4
        (eval-expression "(+ 2 2)"))))

(deftest make-equiv-expr-test
  (is (= "(conj (nth (vector 0 1 2) 2) (vector 0 1 2))"
        (make-equiv-expr '("(vector 0 1 2)" "(conj @2 @)")))))

(deftest make-candidates-test
  ;; Lists
  (is (= '("0" "1" "2")
        (make-candidates "'(0 1 2)" :normal)))
  (is (= '("(0 1 2)")
        (make-candidates "'(0 1 2)" :explicit)))
  
  ;; Vectors
  (is (= '("0" "1" "2")
        (make-candidates "[0 1 2]" :normal)))
  (is (= '("[0 1 2]")
        (make-candidates "[0 1 2]" :explicit)))

  ;; Lazy seqs
  (is (= '("0" "1" "2")
        (make-candidates "(take 3 (range))" :normal)))
  (is (= '("(0 1 2)")
        (make-candidates "(take 3 (range))" :explicit)))
  
  ;; Hashmaps
  (is (= '("{:foo :bar}")
        (make-candidates "{:foo :bar}" :normal)))
  (is (= '("{:foo :bar}")
        (make-candidates "{:foo :bar}" :explicit)))

  ;; Sets
  (is (= '("#{:foo}")
        (make-candidates "#{:foo}" :normal)))
  (is (= '("#{:foo}")
        (make-candidates "#{:foo}" :explicit)))

  ;; Strings
  (is (= '("\"foo bar baz\"")
        (make-candidates "\"foo bar baz\"" :normal)))
  (is (= '("\"foo bar baz\"")
        (make-candidates "\"foo bar baz\"" :explicit)))

  ;; General Objects
  (is (= '("#evalator_context_cider_test.core.TestRec{:foo 1, :bar 2, :baz 3}")
        (make-candidates "(TestRec. 1 2 3)" :normal)))
  (is (= '("#evalator_context_cider_test.core.TestRec{:foo 1, :bar 2, :baz 3}")
        (make-candidates "(TestRec. 1 2 3)" :explicit))))

(deftest transform-candidates-test
  (is (= '("1" "2" "3")
        (transform-candidates '("0" "1" "2") "(inc @)" false)))
  (is (= '("3")
        (transform-candidates '("0" "1" "2") "(reduce + '@)" true))))

(run-tests)










