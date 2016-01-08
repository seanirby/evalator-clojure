(require 'cider)
(require 'helm-lambda-utils)
(require 'helm-lambda-context)
(require 'eieio)

(defvar helm-lambda-context-cider-ns "helm-lambda-context-cider")
(defvar helm-lambda-context-cider-file-name load-file-name)

(defun current-ns ()
  (nrepl-dict-get (cider-nrepl-sync-request:eval "(ns-name *ns*)") "value"))
(defun helm-lambda-context-cider-inject ()
  "Makes helm-lambda-context-cider namespace available."
  ;; Remember current ns so it can be switched back to after injection
  (let* ((ns-curr (cider-current-ns)))
    (my-sp (concat "cider-namespace is: " (cider-current-ns)))
    (my-sp (concat "current namespace is: " ns-curr))
    (cider-nrepl-sync-request:eval
     (helm-lambda-utils-get-file-string
      (expand-file-name "helm-lambda-context-cider.clj"
                        (file-name-directory
                         (or helm-lambda-context-cider-file-name buffer-file-name)))))
    (my-sp (concat "current-namespace is: " (current-ns)))
    (cider-nrepl-sync-request:eval (concat "(in-ns '" ns-curr ")"))
    (my-sp (concat "current-namespace is: " (current-ns)))))

(defun helm-lambda-context-cider-require ()
  "Requires helm-lambda-context-cider-ns in current ns fully-qualified."
  (cider-nrepl-sync-request:eval
   (concat "(require '(" helm-lambda-context-cider-ns "))")))

(defun helm-lambda-context-cider-to-arg-string (arg)
  "Converts arg to its stringed representation so it can be evaluated
by nrepl.  arg should only be a list, string, or nil."
  (cond ((consp arg) (concat "'" (prin1-to-string arg)))
        ((equal nil arg) "nil")
        (t (concat "\"" arg "\""))))

(defun helm-lambda-context-cider-make-expression-string (fname args stringifyp)
  "Given a clojure function name, fname, and its args, this function
will create a valid expression string so that it can be passed to
nrepl for evaluation.  If the args originated from an elispp "
  (let ((expression-list `(
                           "("
                           ,helm-lambda-context-cider-ns "/"
                           ,fname
                           ,@(mapcar (lambda (s) (concat " " (if stringifyp (helm-lambda-context-cider-to-arg-string s) s))) args)
                           ")"
                           )))
    (my-sp (mapconcat 'identity expression-list ""))
    (mapconcat 'identity expression-list "")))

(defun helm-lambda-context-cider-eval (fname args stringifyp)
  (let ((expression (helm-lambda-context-cider-make-expression-string fname args stringifyp)))
    (my-sp expression)
    (cider-nrepl-sync-request:eval expression)))

(setq helm-lambda-context-cider
      (make-instance
       'helm-lambda-context

       :name
       "cider"

       :init
       (lambda ()
         (helm-lambda-context-cider-inject)
         (helm-lambda-context-cider-require))

       :make-candidates
       (lambda (input)
         (let ((result (helm-lambda-context-cider-eval "make-candidates" `(,input) nil)))
           (read (nrepl-dict-get result "value"))))

       :transform-candidates-try
       (lambda (context candidates-all candidates-marked expression)
         (setq cider-show-error-buffer t)
         (let ((result (helm-lambda-context-cider-eval "transform-candidates"
                                                       `(,candidates-all
                                                         ,candidates-marked
                                                         ,expression)
                                                       t)))
           (setq cider-show-error-buffer t)
           (my-sp result)
           (if (equal nil (nrepl-dict-get result "err"))
               (read (nrepl-dict-get result "value"))
             candidates-all
             (my-sp result))))

       :transform-candidates
       (lambda (context candidates-all candidates-marked expression)
         (let ((result (helm-lambda-context-cider-eval "transform-candidates"
                                                       `(,candidates-all
                                                         ,candidates-marked
                                                         ,expression)
                                                       t)))
           (read (nrepl-dict-get result "value"))))

       :apply-expression
       (lambda () nil)))


(provide 'helm-lambda-context-cider)
;; Return helm-lambda should eva         (helm-lambda-cider-inject)
;; ;; Evaluates input and returns message id.
;; ;; Callback is run multiple times for some reason
;; (cider-nrepl-request:eval "(+ 1 1)" (lambda (a) (my-sp "running callback") (my-sp a)))
;; ;;--> delegates to nrepl-request-:eval
;; ;;  --> delegates to nrepl-send-request

;; (cider-nrepl-sync-request:eval "testyvar" "test.core")
;; ;;--> delegates to nrepl-sync-request:eval
;; ;;  --> delegates to nrepl-send-sync-request
