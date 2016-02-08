;;; evalator-clojure-test.el --- Tests for evalator-clojure-test.el
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
(require 'evalator-clojure)
(require 'noflet)

(setq evalator-clojure-special-arg "Ⓔ")

;; Used noflet because I couldn't find a way to mock 'cider-nrepl-sync-request:eval' twice
;; TODO add test for when namespace doesn't need to be loaded
(ert-deftest evalator-clojure-inject-test ()
  (noflet ((cider-nrepl-sync-request:eval (expr) expr))
          (with-mock
           (stub cider-current-ns => "foo.core")
           (stub evalator-utils-get-file-string => "evalator-clojure.clj contents")
           (noflet ((nrepl-dict-get (_r _s) "nil"))
                   (should (equal "(ns foo.core)"
                                  (evalator-clojure-inject))))
           )))

(ert-deftest evalator-clojure-require-test ()
  (with-mock
   (stub evalator-clojure-ns => "evalator-clojure")
   (mock (cider-nrepl-sync-request:eval "(require '(evalator-clojure))") => t)
   (evalator-clojure-require)))


(ert-deftest evalator-clojure-swap-special-arg-test ()
  (with-mock
   (mock (evalator-clojure-eval "swap-special-arg-str" '("Ⓔ")) => t)
   (evalator-clojure-swap-special-arg "Ⓔ")))

(ert-deftest evalator-clojure-to-arg-string-test ()
  (should (equal "'(\"cand-1\" \"cand-2\" \"cand-3\")"
                 (evalator-clojure-to-arg-string '("cand-1" "cand-2" "cand-3"))))
  (should (equal "true"
                 (evalator-clojure-to-arg-string t)))
  (should (equal "nil"
                 (evalator-clojure-to-arg-string nil)))
  (should (equal ":explicit"
                 (evalator-clojure-to-arg-string :explicit))))

(ert-deftest evalator-clojure-make-expression-string-test ()
  (should
   (equal "(evalator-clojure/my-func '(1 2) \"(+ 1 1)\" nil)"
          (evalator-clojure-make-expression-string "my-func"
                                                   '((1 2) "(+ 1 1)" nil)))))

(ert-deftest evalator-clojure-eval-test ()
  (with-mock
   (mock (evalator-clojure-make-expression-string "my-func" *) => "(my-func)")
   (mock (cider-nrepl-sync-request:eval "(my-func)") => t)
   (evalator-clojure-eval "my-func" nil)))

;; TODO make this more robust once I've finalized error signals
(ert-deftest evalator-clojure-result-or-error ()
  (noflet ((nrepl-dict-get (plst prop) (plist-get plst prop)))
          (with-mock
           (stub signal => t)
           (should (equal :foo
                          (evalator-clojure-result-or-error '("value" ":foo"))))
           (should (evalator-clojure-result-or-error '("ex" "exception text")))
           (should (evalator-clojure-result-or-error '("err" "error text")))
           (should (evalator-clojure-result-or-error '())))))

(ert-deftest evalator-clojure-init-test ()
  (with-mock
   (mock (evalator-context-get-special-arg *) :times 1)
   (mock (evalator-clojure-inject) :times 1)
   (mock (evalator-clojure-require) :times 1)
   (mock (evalator-clojure-swap-special-arg *) :times 1)
   (noflet ((cider-connections () t))
           (evalator-clojure-init))
   (noflet ((cider-connections () nil))
           (mock (message *))
           (equal nil
                  (evalator-clojure-init)))))

(ert-deftest evalator-clojure-make-equiv-expr-test ()
  (with-mock
   (stub evalator-clojure-eval)
   (stub nrepl-dict-get => "\"(conj (vector 1 2 3) 4)\"")
   (should (equal "(conj (vector 1 2 3) 4)"
                  (evalator-clojure-make-equiv-expr '("(vector 1 2 3)" "(conj Ⓔ 4)"))))))

(ert-deftest evalator-clojure-make-candidates-test ()
  (with-mock
   (mock (evalator-clojure-eval "make-candidates" '("(vector 1 2 3)" nil)))
   (mock (evalator-clojure-result-or-error *) => t)
   (evalator-clojure-make-candidates "(vector 1 2 3)" nil)))

(ert-deftest evalator-clojure-transform-candidates-test ()
  (with-mock
   (mock (evalator-clojure-eval "transform-candidates" '(("foo")
                                                         "(concat Ⓔ \"bar\")"
                                                         nil)))
   (mock (evalator-clojure-result-or-error *) => t)
   (evalator-clojure-transform-candidates '("foo") "(concat Ⓔ \"bar\")" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-clojure-test.el ends here
