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

(define-library (rapid expressions)
  (export expression? expression-syntax                   
	  make-reference reference? reference-location
	  make-primitive-reference primitive-reference? primitive-reference-symbol
	  make-literal literal? literal-value
	  make-undefined undefined?
	  make-procedure-call procedure-call?
	  procedure-call-operator procedure-call-operands
	  make-primitive-operation primitive-operation?
	  primitive-operation-operator primitive-operation-operands
	  make-procedure expression-procedure? procedure-clauses
	  make-assignment assignment? assignment-location assignment-expression
	  make-multiple-assignment multiple-assignment?
	  multiple-assignment-formals multiple-assignment-expression
	  make-letrec*-expression letrec*-expression?
	  letrec*-expression-bindings letrec*-expression-body
	  make-letrec-expression letrec-expression?
	  letrec-expression-bindings letrec-expression-body
	  make-let-values-expression let-values-expression?
	  let-values-expression-binding let-values-expression-body
	  make-sequence sequence? sequence-expressions
	  make-conditional conditional?
	  conditional-test conditional-consequent conditional-alternate
	  make-operator operator? operator-identifier
	  make-location location? location-syntax
	  make-binding binding?
	  binding-formals binding-expression binding-syntax
	  make-formals formals?
	  formals-fixed-arguments formals-rest-argument formals-syntax
	  formals-locations formals-location
	  make-clause clause? clause-formals clause-body clause-syntax
	  expression-map expression-for-each
	  expression->datum
	  bindings)
  (import (scheme base)                    (scheme write) ;;;XXX
	  (scheme case-lambda)
	  (rapid lists)
	  (rapid comparators)
	  (rapid expansion syntactic-closures)
	  (rapid syntax)
	  (rapid read)
	  (rapid tables))
  (include "expressions.scm"))
