(ns helm-lambda-context-cider)

(defn wrap-string [s1 s2]
  (str s1 s2 s1))

(defmulti make-candidates
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

(defn transform-candidates [c-all c-marked expr]
  (if (nil? c-marked)
    (map #(apply-expression expr %) c-all)
    (list (apply-expression expr c-marked))))

(defn apply-expression [expr c]
  (identity expr)
  (identity c))

;; (lambda (expression-str x)
;;          (let ((expression (read expression-str)))
;;            (if (and (sequencep x) (not (stringp x)))
;;                (let* ((xs (mapcar 'read x))
;;                       (ns (number-sequence 0 (1- (length xs))))
;;                       (arg-names (mapcar (lambda (n) (intern (concat "%" (int-to-string n)))) ns))
;;                       (% xs)
;;                       (f `(lambda ,arg-names ,expression)))
;;                  (apply (eval f) (append xs nil)))
;;              (let* ((% (read x)))
;;                (eval expression)))))













