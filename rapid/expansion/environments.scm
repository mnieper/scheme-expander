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

(define-record-type <environment>
  (make-environment bindings syntactic-environment)
  environment?
  (bindings environment-bindings)
  (syntactic-environment environment-syntactic-environment))

(define-syntax environment
  (syntax-rules ()
    ((environment ((formals init) ...) . syntactic-bindings)
     (environment-aux (formals ...) () ((formals init) ...) syntactic-bindings))))

(define-syntax environment-aux
  (syntax-rules ()
    ((environment-aux () location-bindings environment-bindings syntactic-bindings)
     (let location-bindings
       (make-environment
	 (bindings . environment-bindings)
	 (syntactic-environment . syntactic-bindings))))
    ((environment-aux ((x ...) formals ...)
		      (location-binding ...)
		      environment-bindings
		      syntactic-bindings)
     (environment-aux (formals ...)
		      (location-binding ... (x (make-location #f)) ...)
		      environment-bindings
		      syntactic-bindings))
    ((environment-aux ((x ... . y) formals ...)
		      (location-binding ...)
		      environment-bindings
		      syntactic-bindings)
     (environment-aux (formals ...)
		      (location-binding ... (x (make-location #f)) ... (y (make-location #f)))
		      environment-bindings
		      syntactic-bindings))))
