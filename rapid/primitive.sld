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

(define-library (rapid primitive)
  (export letrec*-values
	  letrec
	  let-values
	  quote
	  case-lambda
	  if
	  set!
	  set-values!
	  begin

	  eq?

	  fixnum?
	  flonum?
	  exact?
	  nan?

	  fx+
	  fx-
	  fx<
	  fx=
	  fxnegative?
	  number->string
	  string->number
	  
	  boolean?

	  symbol?
	  symbol->string
	  
	  char?
	  
	  string?
	  string->list
	  list->string
	  
	  make-vector
	  vector-ref
	  vector-set!
	  vector?
	  vector-length

	  cons
	  pair?
	  car
	  cdr
	  set-car!
	  set-cdr!
	  null?
	  
	  error
	  set-exception-handler!

	  procedure?
	  call-with-current-continuation
	  apply

	  current-output-port
	  write-char
	  
	  exit

	  make-rtd
	  rtd-constructor
	  rtd-predicate
	  rtd-accessor
	  rtd-mutator
	  
	  ;; XXX
	  display newline
	  +
	  string-append)
  (import (rename (scheme base) (error scheme-error))
	  (scheme inexact)
	  (scheme process-context)
	  (scheme case-lambda)
	  (scheme write) ;; XXX
	  )
  (include "primitive.scm"))
