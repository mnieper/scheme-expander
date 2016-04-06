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

(define-library (rapid expansion syntactic-closures)
  (export make-syntactic-closure syntactic-closure?
	  capture-syntactic-environment
	  close-syntax
	  unclose-form
	  identifier? make-synthetic-identifier identifier=?
	  call-in-syntactic-closure
	  sc-lookup-binding! sc-lookup-denotation! sc-lookup-syntax!)
  (import (scheme base)                   (scheme write)
	  (scheme case-lambda)
	  (rapid boxes)
	  (rapid comparators)
	  (rapid maps)
	  (rapid syntax)
	  (rapid expansion syntactic-environments))
  (include "syntactic-closures.scm"))
