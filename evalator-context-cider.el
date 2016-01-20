;;; evalator-context-cider.el --- CIDER evaluation context for evalator
;; 
;; Copyright © , Sean Irby
;; Author: Sean Irby
;; Maintainer: Sean Irby <sean.t.irby@gmail.com>
;; URL: http://www.github.com/seanirby/evalator
;; Version: 0.0.1
;; Keywords: languages, clojure, cider, helm
;; Package-Requires: ((evalator "0.0.1"))
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
  ""
  (let* ((initial-p-sym (if initial-p 'true 'false))
         (result (evalator-context-cider-eval "make-candidates" `(,input ,mode ,initial-p-sym))))
    (evalator-context-cider-result-or-error result)))

(defun evalator-context-cider-transform-candidates (cands expr-str mode &optional collect-p)
  ""
  (let ((result (evalator-context-cider-eval "transform-candidates"
                                             `(,cands
                                               ,expr-str
                                               ,mode
                                               ,collect-p))))
    (evalator-context-cider-result-or-error result)))

(provide 'evalator-context-cider)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-context-cider.el ends here
