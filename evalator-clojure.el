;;; evalator-clojure.el --- Clojure evaluation context for evalator via CIDER.
;;
;; Copyright © , Sean Irby
;; Author: Sean Irby
;; Maintainer: Sean Irby <sean.t.irby@gmail.com>
;; URL: http://www.github.com/seanirby/evalator-clojure
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
;;  This package extends the evalator package by adding support for
;;  the Clojure language.
;;
;;  The commands `evalator-clojure' and `evalator-clojure-explicit'
;;  can each be used to start an evalator session that uses Clojure as
;;  the evaluation language.
;;
;;  `evalator-clojure' will start evalator in normal mode.
;;
;;  `evalator-clojure-explicit' will start evalator in explicit mode.
;;
;;   NOTE:
;;   This package uses CIDER to execute Clojure in EMACS so a CIDER
;;   session must be running and connected to for the functions
;;   mentioned above to work.
;;
;;; Code:


(require 'cider)
(require 'evalator)
(require 'eieio)

(defvar evalator-clojure-special-arg nil)
(defvar evalator-clojure-context nil)
(defvar evalator-clojure-ns "evalator-clojure")
(defvar evalator-clojure-file-name load-file-name)

;;;###autoload
(defun evalator-clojure-context ()
  (if evalator-clojure-context
      evalator-clojure-context
    (progn
      (setq evalator-clojure-context
            (make-instance
             'evalator-context

             :name
             "clojure"

             :special-arg
             'evalator-clojure-special-arg

             :init
             'evalator-clojure-init

             :make-candidates
             'evalator-clojure-make-candidates

             :transform-candidates
             'evalator-clojure-transform-candidates

             :make-equiv-expr
             'evalator-clojure-make-equiv-expr)))))

(defun evalator-clojure-inject ()
  "Make evalator-clojure namespace available if not already."
  (let* (;; Store current namespace so it can be switched back to after injection
         (ns-curr (cider-current-ns))
         ;;Check if the evalator-clojure namespace needs to be loaded
         (result  (cider-nrepl-sync-request:eval (format "(find-ns '%s)" evalator-clojure-ns)))
         (load-namespace-p (equal "nil" (nrepl-dict-get result "value"))))
    (when load-namespace-p
      (cider-nrepl-sync-request:eval
       (evalator-utils-get-file-string
        (expand-file-name "evalator-clojure.clj"
                          (file-name-directory
                           (or evalator-clojure-file-name buffer-file-name)))))
      (cider-nrepl-sync-request:eval (concat "(ns " ns-curr ")")))))

(defun evalator-clojure-require ()
  "Requires evalator-clojure-ns in current ns fully-qualified."
  (cider-nrepl-sync-request:eval
   (concat "(require '(" evalator-clojure-ns "))")))

(defun evalator-clojure-swap-special-arg (special-arg-str)
  "Swaps the special arg defined in evalator-clojure.clj.

Value is swapped with SPECIAL-ARG-STR."
  (evalator-clojure-eval "swap-special-arg-str" `(,special-arg-str)))

(defun evalator-clojure-to-arg-string (arg)
  "Convert arg to its string representation.

ARG should only be a list, string, t, or nil."
  (cond ((consp arg) (concat "'" (prin1-to-string arg)))
        ((equal t arg) "true")
        ((equal nil arg) "nil")
        (t (prin1-to-string arg))))

(defun evalator-clojure-make-expression-string (fname args)
  "Create an expression string to pass to nrepl.

Accepts a clojure function name, FNAME, and its arguments, ARGS."
  (let ((expression-list `(
                           "("
                           ,evalator-clojure-ns "/" ,fname
                           ,@(mapcar (lambda (s)
                                       (concat " " (evalator-clojure-to-arg-string s))) args)
                           ")"
                           )))
    (mapconcat 'identity expression-list "")))

(defun evalator-clojure-eval (fname args)
  "Evaluate a clojure function.

Accepts a clojure function name, FNAME, and its arguments, ARGS"
  (let ((expression (evalator-clojure-make-expression-string fname args)))
    (cider-nrepl-sync-request:eval expression)))

(defun evalator-clojure-result-or-error (result)
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

(defun evalator-clojure-init ()
  "See slot documentation in evalator-context.el in evalator package."
  (if (cider-connections)
      (progn
        (evalator-clojure-inject)
        (evalator-clojure-require)
        (evalator-clojure-swap-special-arg (evalator-context-get-special-arg evalator-clojure-context)))
    (progn
      (message (concat "CIDER must be running before evalator can be started.\n"
                       "Run 'M-x cider-jack-in' to start a CIDER server and try again."))
      nil)))

(defun evalator-clojure-make-equiv-expr (exprs)
  "See slot documentation in evalator-context.el in evalator package."
  (let ((result (evalator-clojure-eval "make-equiv-expr" `(,exprs))))
    (read (nrepl-dict-get result "value"))))

(defun evalator-clojure-make-candidates (input mode)
  "See slot documentation in evalator-context.el in evalator package."
  (let* ((result (evalator-clojure-eval "make-candidates" `(,input ,mode))))
    (evalator-clojure-result-or-error result)))

(defun evalator-clojure-transform-candidates (cands expr-str collect-p)
  "See slot documentation in evalator-context.el in evalator package."
  (let ((result (evalator-clojure-eval "transform-candidates"
                                       `(,cands ,expr-str ,collect-p))))
    (evalator-clojure-result-or-error result)))

;;;###autoload
(defun evalator-clojure (&optional mode)
  (interactive)
  (evalator mode 'evalator-clojure-context))

;;;###autoload
(defun evalator-clojure-explicit ()
  (interactive)
  (evalator-clojure :explicit))

(provide 'evalator-clojure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-clojure.el ends here
