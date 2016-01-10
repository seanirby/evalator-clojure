(require 'cider)
(require 'evalator-utils)
(require 'evalator-context)
(require 'eieio)

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

(defun evalator-context-cider-to-arg-string (arg)
  "Converts arg to its stringed representation so it can be evaluated
by nrepl.  arg should only be a list, string, or nil."
  (cond ((consp arg) (concat "'" (prin1-to-string arg)))
        ((equal nil arg) "nil")
        (t (prin1-to-string arg))))

(defun evalator-context-cider-make-expression-string (fname args stringifyp)
  "Given a clojure function name, fname, and its args, this function
will create a valid expression string so that it can be passed to
nrepl for evaluation.  If the args originated from an elispp "
  (let ((expression-list `(
                           "("
                           ,evalator-context-cider-ns "/" ,fname
                           ,@(mapcar (lambda (s) (concat " " (if stringifyp (evalator-context-cider-to-arg-string s) s))) args)
                           ")"
                           )))
    (mapconcat 'identity expression-list "")))

(defun evalator-context-cider-eval (fname args stringifyp)
  "Used to evaluate one of the functions defined in 'evalator-context-cider.clj'"
  (let ((expression (evalator-context-cider-make-expression-string fname args stringifyp)))
    (cider-nrepl-sync-request:eval expression)))

(setq evalator-context-cider
      (make-instance
       'evalator-context

       :name
       "cider"

       :init
       (lambda ()
         (evalator-context-cider-inject)
         (evalator-context-cider-require))

       :make-candidates
       (lambda (input)
         (let ((result (evalator-context-cider-eval "make-candidates" `(,input) nil)))
           (read (nrepl-dict-get result "value"))))

       :transform-candidates-try
       (lambda (candidates-all candidates-marked expression)
         ;; TODO need to find a way to ignore errors during this time and turn them back on without a buffer popping up
         (setq cider-show-error-buffer nil)
         (let ((result (evalator-context-cider-eval "transform-candidates"
                                                       `(,candidates-all
                                                         ,candidates-marked
                                                         ,expression)
                                                       t)))
         (setq cider-show-error-buffer nil)
           (if (equal nil (nrepl-dict-get result "err"))
               (read (nrepl-dict-get result "value"))
             candidates-all)))

       :transform-candidates
       (lambda (candidates-all candidates-marked expression)
         (let ((result (evalator-context-cider-eval "transform-candidates"
                                                       `(,candidates-all
                                                         ,candidates-marked
                                                         ,expression)
                                                       t)))
           (read (nrepl-dict-get result "value"))))))

(provide 'evalator-context-cider)
