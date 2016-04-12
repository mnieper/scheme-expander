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

(define-library (rapid expansion syntactic-environments)
  (export make-syntactic-environment
	  syntactic-environment?
	  with-syntactic-environment
	  with-scope
	  with-isolated-references

	  ;; Denotations
	  make-denotation
	  denotation?
	  denotation-syntax
	  make-primitive
	  primitive?
	  primitive-symbol
	  make-special-form
	  special-form?
	  special-form-expander
	  
	  lookup-binding!
	  lookup-denotation!
	  lookup-syntax!
	  syntactic-binding?
	  syntactic-binding-syntax
	  binding-denotation
	  insert-binding!
	  insert-binding-from!
	  insert-bindings-from!
	  delete-binding!
	  derive-syntactic-environment
	  get-syntactic-environment
	  syntactic-environment)
  (import (scheme base)
	  (scheme case-lambda)
	  (rapid format)
	  (rapid comparators)
	  (rapid boxes)
	  (rapid sets)
	  (rapid maps)
	  (rapid syntax)
	  (rapid read)
	  (rapid error))
  (include "syntactic-environments.scm"))
