(require 'cider)
(require 'helm-lambda-utils)
(require 'helm-lambda-context)
(require 'eieio)

(defvar helm-lambda-context-cider-ns "helm-lambda-context-cider")
(defvar helm-lambda-context-cider-file-name load-file-name)

(defun helm-lambda-context-cider-inject ()
  "Makes helm-lambda-context-cider namespace available."
  ;; Remember current ns so it can be switched back to after injection
  (let* ((ns-curr (cider-current-ns)))
    (cider-nrepl-sync-request:eval
     (helm-lambda-utils-get-file-string
      (expand-file-name "helm-lambda-context-cider.clj"
                        (file-name-directory
                         (or helm-lambda-context-cider-file-name buffer-file-name)))))
    (cider-nrepl-sync-request:eval (concat "(in-ns '" ns-curr ")"))))

(defun helm-lambda-context-cider-require ()
  "Requires helm-lambda-context-cider-ns in current ns fully-qualified."
  (cider-nrepl-sync-request:eval
   (concat "(require '(" helm-lambda-context-cider-ns "))")))

(defun helm-lambda-context-cider-to-arg-string (arg)
  "Converts arg to its stringed representation so it can be evaluated
by nrepl.  arg should only be a list, string, or nil."
  (cond ((consp arg) (concat "'" (prin1-to-string arg)))
        ((equal nil arg) "nil")
        (t (prin1-to-string arg))))

(defun helm-lambda-context-cider-make-expression-string (fname args stringifyp)
  "Given a clojure function name, fname, and its args, this function
will create a valid expression string so that it can be passed to
nrepl for evaluation.  If the args originated from an elispp "
  (let ((expression-list `(
                           "("
                           ,helm-lambda-context-cider-ns "/" ,fname
                           ,@(mapcar (lambda (s) (concat " " (if stringifyp (helm-lambda-context-cider-to-arg-string s) s))) args)
                           ")"
                           )))
    (mapconcat 'identity expression-list "")))

(defun helm-lambda-context-cider-eval (fname args stringifyp)
  "Used to evaluate one of the functions defined in 'helm-lambda-context-cider.clj'"
  (let ((expression (helm-lambda-context-cider-make-expression-string fname args stringifyp)))
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
         ;; TODO need to find a way to ignore errors during this time and turn them back on without a buffer popping up
         (setq cider-show-error-buffer nil)
         (let ((result (helm-lambda-context-cider-eval "transform-candidates"
                                                       `(,candidates-all
                                                         ,candidates-marked
                                                         ,expression)
                                                       t)))
         (setq cider-show-error-buffer nil)
           (if (equal nil (nrepl-dict-get result "err"))
               (read (nrepl-dict-get result "value"))
             candidates-all)))

       :transform-candidates
       (lambda (context candidates-all candidates-marked expression)
         (let ((result (helm-lambda-context-cider-eval "transform-candidates"
                                                       `(,candidates-all
                                                         ,candidates-marked
                                                         ,expression)
                                                       t)))
           (read (nrepl-dict-get result "value"))))

       ;; Not needed
       :apply-expression
       (lambda () nil)))

(provide 'helm-lambda-context-cider)
