;;; evalator-context-cider-test.el --- Tests for evalator-context-cider-test.el
;; 
;; Author: Sean Irby
;; Copyright © , Sean Irby
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


(require 'ert)
(require 'el-mock)
(eval-when-compile
  (require 'cl))
(require 'evalator-context-cider)
(require 'noflet)

(setq evalator-context-cider-special-arg "Ⓔ")

;; Used noflet because I couldn't find a way to mock 'cider-nrepl-sync-request:eval' twice
(ert-deftest evalator-context-cider-inject ()
  (noflet ((cider-nrepl-sync-request:eval (expr) expr))
          (with-mock
           (stub cider-current-ns => "foo.core")
           (stub evalator-utils-get-file-string => "evalator-context-cider.clj contents")
           (should (equal "(in-ns 'foo.core)"
                          (evalator-context-cider-inject))))))

(ert-deftest evalator-context-cider-require-test ()
  (with-mock
   (stub evalator-context-cider-ns => "evalator-context-cider")
   (mock (cider-nrepl-sync-request:eval "(require '(evalator-context-cider))") => t)
   (evalator-context-cider-require)))


(ert-deftest evalator-context-cider-swap-special-arg-test ()
  (with-mock
   (mock (evalator-context-cider-eval "swap-special-arg-str" '("Ⓔ")) => t)
   (evalator-context-cider-swap-special-arg)))

(ert-deftest evalator-context-cider-to-arg-string-test ()
  (should (equal "'(\"cand-1\" \"cand-2\" \"cand-3\")"
                 (evalator-context-cider-to-arg-string '("cand-1" "cand-2" "cand-3"))))
  (should (equal "true"
                 (evalator-context-cider-to-arg-string t)))
  (should (equal "nil"
                 (evalator-context-cider-to-arg-string nil)))
  (should (equal ":explicit"
                 (evalator-context-cider-to-arg-string :explicit))))

(ert-deftest evalator-context-cider-make-expression-string-test ()
  (should
   (equal "(evalator-context-cider/my-func '(1 2) \"(+ 1 1)\" nil)"
          (evalator-context-cider-make-expression-string "my-func"
                                                         '((1 2) "(+ 1 1)" nil)))))

(ert-deftest evalator-context-cider-eval-test ()
  (with-mock
   (mock (evalator-context-cider-make-expression-string "my-func" *) => "(my-func)")
   (mock (cider-nrepl-sync-request:eval "(my-func)") => t)
   (evalator-context-cider-eval "my-func" nil)))

;; TODO make this more robust once I've finalized error signals
(ert-deftest evalator-context-cider-result-or-error ()
  (noflet ((nrepl-dict-get (plst prop) (plist-get plst prop)))
          (with-mock
           (stub signal => t)
           (should (equal :foo
                          (evalator-context-cider-result-or-error '("value" ":foo"))))
           (should (evalator-context-cider-result-or-error '("ex" "exception text")))
           (should (evalator-context-cider-result-or-error '("err" "error text")))
           (should (evalator-context-cider-result-or-error '())))))

(ert-deftest evalator-context-cider-init ()
  (with-mock
   (mock (evalator-context-cider-inject) :times 1)
   (mock (evalator-context-cider-require) :times 1)
   (mock (evalator-context-cider-swap-special-arg) :times 1)
   (evalator-context-cider-init)))

(ert-deftest evalator-context-cider-make-equiv-expr-test ()
  (with-mock
   (stub evalator-context-cider-eval)
   (stub nrepl-dict-get => "\"(conj (vector 1 2 3) 4)\"")
   (should (equal "(conj (vector 1 2 3) 4)"
                  (evalator-context-cider-make-equiv-expr '("(vector 1 2 3)" "(conj Ⓔ 4)"))))))

(ert-deftest evalator-context-cider-make-candidates-test ()
  (with-mock
   (mock (evalator-context-cider-eval "make-candidates" '("(vector 1 2 3)"
                                                          nil
                                                          true)))
   (mock (evalator-context-cider-result-or-error *) => t)
   (evalator-context-cider-make-candidates "(vector 1 2 3)" nil t)))

(ert-deftest evalator-context-cider-transform-candidates-test ()
  (with-mock
   (mock (evalator-context-cider-eval "transform-candidates" '(("foo")
                                                               "(concat Ⓔ \"bar\")"
                                                               :explicit
                                                               nil)))
   (mock (evalator-context-cider-result-or-error *) => t)
   (evalator-context-cider-transform-candidates '("foo") "(concat Ⓔ \"bar\")" :explicit nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-context-cider-test.el ends here
