;;; evalator-context-cider.el --- Clojure evaluation context for evalator via CIDER.
;; 
;; Copyright © , Sean Irby
;; Author: Sean Irby
;; Maintainer: Sean Irby <sean.t.irby@gmail.com>
;; URL: http://www.github.com/seanirby/evalator-context-cider
;; Version: 1.0.0
;; Keywords: languages, clojure, cider, helm
;; Package-Requires: ((cider "0.10.0") (evalator "1.0.0"))
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; This file is not a part of GNU Emacs
;; 
;;; Commentary:
;; 
;;; Code:


(require 'cider)
(require 'evalator-utils)
(require 'evalator-context)
(require 'eieio)

(defvar evalator-context-cider-special-arg nil)
(defvar evalator-context-cider-ns "evalator-context-cider")
(defvar evalator-context-cider-file-name load-file-name)

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

(defun evalator-context-cider-inject ()
  "Make evalator-context-cider namespace available."
  ;; Remember current ns so it can be switched back to after injection
  (let* ((ns-curr (cider-current-ns)))
    (cider-nrepl-sync-request:eval
     (evalator-utils-get-file-string
      (expand-file-name "evalator-context-cider.clj"
                        (file-name-directory
                         (or evalator-context-cider-file-name buffer-file-name)))))
    (cider-nrepl-sync-request:eval (concat "(ns " ns-curr ")"))))

(defun evalator-context-cider-require ()
  "Requires evalator-context-cider-ns in current ns fully-qualified."
  (cider-nrepl-sync-request:eval
   (concat "(require '(" evalator-context-cider-ns "))")))

(defun evalator-context-cider-swap-special-arg (special-arg-str)
  "Swaps the special arg defined in evalator-context-cider.clj.
Value is swapped with SPECIAL-ARG-STR."
  (evalator-context-cider-eval "swap-special-arg-str" `(,special-arg-str)))

(defun evalator-context-cider-to-arg-string (arg)
  "Convert arg to its string representation.
ARG should only be a list, string, t, or nil."
  (cond ((consp arg) (concat "'" (prin1-to-string arg)))
        ((equal t arg) "true")
        ((equal nil arg) "nil")
        (t (prin1-to-string arg))))

(defun evalator-context-cider-make-expression-string (fname args)
  "Create an expression string to pass to nrepl.
Accepts a clojure function name, FNAME, and its arguments, ARGS."
  (let ((expression-list `(
                           "("
                           ,evalator-context-cider-ns "/" ,fname
                           ,@(mapcar (lambda (s)
                                       (concat " " (evalator-context-cider-to-arg-string s))) args)
                           ")"
                           )))
    (mapconcat 'identity expression-list "")))

(defun evalator-context-cider-eval (fname args)
  "Evaluate a clojure function.
Accepts a clojure function name, FNAME, and its arguments, ARGS"
  (let ((expression (evalator-context-cider-make-expression-string fname args)))
    (cider-nrepl-sync-request:eval expression)))

(defun evalator-context-cider-result-or-error (result)
  "Check the result of an nrepl evaluation.
Accepts an nrepl evaluation result, RESULT.  If the result was
successful read its value and return it.  If an error occured, signal
an error with the error string contained within RESULT."
  (let ((val        (nrepl-dict-get result "value"))
        (error-str  (ansi-color-apply (or (nrepl-dict-get result "ex")
                                          (nrepl-dict-get result "err")
                                          "")))
        (out-str    (ansi-color-apply (or (nrepl-dict-get result "out") ""))))
    (if val
        (read val)
      (signal 'evalator-error (list error-str out-str)))))

(defun evalator-context-cider-init ()
  "See slot documentation in evalator-context.el in evalator package."
  (if cider-mode
      (progn
        (evalator-context-cider-inject)
        (evalator-context-cider-require)
        (evalator-context-cider-swap-special-arg (evalator-context-get-special-arg evalator-context-cider)))
    (progn
      (message (concat "CIDER must be running before evalator can be started.\n"
                       "Run 'M-x cider-jack-in' to start a CIDER server for this project and try again."))
      nil)))

(defun evalator-context-cider-make-equiv-expr (exprs)
  "See slot documentation in evalator-context.el in evalator package."
  (let ((result (evalator-context-cider-eval "make-equiv-expr" `(,exprs))))
    (read (nrepl-dict-get result "value"))))

(defun evalator-context-cider-make-candidates (input mode)
  "See slot documentation in evalator-context.el in evalator package."
  (let* ((result (evalator-context-cider-eval "make-candidates" `(,input ,mode))))
    (evalator-context-cider-result-or-error result)))

(defun evalator-context-cider-transform-candidates (cands expr-str collect-p)
  "See slot documentation in evalator-context.el in evalator package."
  (let ((result (evalator-context-cider-eval "transform-candidates"
                                             `(,cands ,expr-str ,collect-p))))
    (evalator-context-cider-result-or-error result)))

(provide 'evalator-context-cider)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-context-cider.el ends here
