;;; Rapid Scheme --- An expander for R7RS programs

;; Copyright (C) 2016 Marc Nieper-Wißkirchen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-library (rapid expansion primitive-operators)
  (export
   operator-eq?
   operator-boolean?
   operator-string->number
   operator-number->string
   operator-fixnum?
   operator-fxnegative?
   operator-fx<
   operator-fx=
   operator-fx+
   operator-fx-
   operator-flonum?
   operator-nan?
   operator-exact?
   operator-cons
   operator-pair?
   operator-car
   operator-cdr
   operator-set-car!
   operator-set-cdr!
   operator-null?
   operator-symbol?
   operator-symbol->string
   operator-char?
   operator-string?
   operator-string->list
   operator-list->string
   operator-make-vector
   operator-vector-ref
   operator-vector-set!
   operator-vector?
   operator-vector-length
   operator-procedure?
   operator-apply
   operator-call-with-current-continuation
   operator-error
   operator-set-exception-handler!
   operator-current-output-port
   operator-write-char
   operator-exit
   operator-make-rtd
   operator-rtd-constructor
   operator-rtd-predicate
   operator-rtd-accessor
   operator-rtd-mutator
   operator-ccm
   operator-wcm
   
   ;; XXX
   operator+
   operator-display operator-newline operator-string-append) ;; TODO
  (import (scheme base)
	  (rapid expressions))
  (include "primitive-operators.scm"))
