;;; Symbols used in the parser.

(defpackage slr-symbols
  (:use :common-lisp)
  (:export *for-debug*
           *empty-symbol*
           *success-symbol*
           *beginning-symbol*
           *ending-symbol*
           *reducing-symbol*))
(in-package :slr-symbols)

(defparameter *for-debug* nil)
(defparameter *empty-symbol* '|ε|)
(defparameter *success-symbol* 'OK)
(defparameter *beginning-symbol* 'BEGIN)
(defparameter *ending-symbol* '$)
(defparameter *reducing-symbol* 'REDUCE)
