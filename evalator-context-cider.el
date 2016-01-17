(require 'cider)
(require 'evalator-utils)
(require 'evalator-context)
(require 'eieio)

(defvar evalator-context-cider-special-arg nil)
(defvar evalator-context-cider-ns "evalator-context-cider")
(defvar evalator-context-cider-file-name load-file-name)

(defun evalator-context-cider-inject ()
  "Makes evalator-context-cider namespace available."
  ;; Remember current ns so it can be switched back to after injection
  (let* ((ns-curr (cider-current-ns)))
    (cider-nrepl-sync-request:eval
     (evalator-utils-get-file-string
      (expand-file-name "evalator-context-cider.clj"
                        (file-name-directory
                         (or evalator-context-cider-file-name buffer-file-name)))))
    (cider-nrepl-sync-request:eval (concat "(in-ns '" ns-curr ")"))))

(defun evalator-context-cider-require ()
  "Requires evalator-context-cider-ns in current ns fully-qualified."
  (cider-nrepl-sync-request:eval
   (concat "(require '(" evalator-context-cider-ns "))")))

(defun evalator-context-cider-swap-special-arg ()
  (let ((sa (evalator-context-get-special-arg evalator-context-cider)))
    (evalator-context-cider-eval "swap-special-arg-str" `(,sa))))

(defun evalator-context-cider-to-arg-string (arg)
  "Converts arg to its stringed representation so it can be evaluated
by nrepl.  arg should only be a list, string, or nil."
  (cond ((consp arg) (concat "'" (prin1-to-string arg)))
        ((equal nil arg) "nil")
        (t (prin1-to-string arg))))

(defun evalator-context-cider-make-expression-string (fname args)
  "Given a clojure function name, fname, and its args, this function
will create a valid expression string so that it can be passed to
nrepl for evaluation."
  (let ((expression-list `(
                           "("
                           ,evalator-context-cider-ns "/" ,fname
                           ,@(mapcar (lambda (s)
                                       (concat " " (evalator-context-cider-to-arg-string s))) args)
                           ")"
                           )))
    (mapconcat 'identity expression-list "")))

(defun evalator-context-cider-eval (fname args)
  "Used to evaluate one of the functions defined in 'evalator-context-cider.clj'"
  (let ((expression (evalator-context-cider-make-expression-string fname args)))
    (cider-nrepl-sync-request:eval expression)))

(defun evalator-context-cider-result-or-error (result)
  (let ((val (nrepl-dict-get result "value"))
        (ex  (nrepl-dict-get result "ex"))
        (err (nrepl-dict-get result "err"))
        (out (nrepl-dict-get result "out")))
    (cond (val (read val))
          (ex  (signal 'evalator-error (list ex out)))
          (err (signal 'evalator-error (list err out)))
          (t   (signal 'evalator-error (list out))))))

(defun evalator-context-cider-init ()
  (evalator-context-cider-inject)
  (evalator-context-cider-require)
  (evalator-context-cider-swap-special-arg))

(defun evalator-context-cider-make-equiv-expr (exprs)
  (let ((result (evalator-context-cider-eval "make-equiv-expr" `(,exprs))))
    (read (nrepl-dict-get result "value"))))

(defun evalator-context-cider-make-candidates (input mode initial-p)
  ;; TODO shouldn't do this
  (setq cider-show-error-buffer nil)
  (let* ((initial-p-sym (if initial-p 'true 'false))
         (result (evalator-context-cider-eval "make-candidates" `(,input ,mode ,initial-p-sym))))
    (evalator-context-cider-result-or-error result)))

(defun evalator-context-cider-transform-candidates (candidates-all candidates-marked expression mode)
  (let ((result (evalator-context-cider-eval "transform-candidates"
                                             `(,candidates-all
                                               ,candidates-marked
                                               ,expression
                                               ,mode))))
    (evalator-context-cider-result-or-error result)))

(defvar evalator-context-cider
  (make-instance
   'evalator-context

   :name
   "cider"

   :special-arg
   'evalator-context-cider-special-arg

   :init
   'evalator-context-cider-init

   :make-candidates
   'evalator-context-cider-make-candidates

   :transform-candidates
   'evalator-context-cider-transform-candidates

   :make-equiv-expr
   'evalator-context-cider-make-equiv-expr))

(provide 'evalator-context-cider)
