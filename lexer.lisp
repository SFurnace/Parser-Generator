;;; Simple lexer.
;;;
;;; Author: Dr.CZ
;;; Date: 2017-5-18

(load "./regex.lisp")
(load "./special-symbols.lisp")

(defpackage slr-lexer
  (:use :common-lisp :alexandria
        :slr-regex :slr-symbols)
  (:export ~>
           deflexer
           do-lex
           token
           token-p
           token-type
           token-val
           token-range-start
           token-range-end
           *error-token-name*))

(in-package :slr-lexer)

;;; ----------------------------------------------------------
;;; structure
;;; ----------------------------------------------------------

(defstruct lexer
  (items nil :type list :read-only t))

(defstruct lex-item
  (name nil :type symbol :read-only t)
  (regex nil :type list :read-only t)
  (discard nil :type boolean :read-only t))

(defmethod print-object ((l lex-item) s)
  (format s "~a : ~s " (lex-item-name l) (lex-item-regex l)))

(defstruct token
  (type nil :type symbol)
  (val "" :type string)
  (range-start nil :type fixnum :read-only t)
  (range-end nil :type fixnum :read-only t))

(defmethod print-object ((tok token) s)
  (format s "[~a: ~s (~a,~a)]" (token-type tok) (token-val tok) (token-range-start tok) (token-range-end tok)))

(defun ~> (name regen-str &key discard)
  (make-lex-item :name name :regex (regex regen-str) :discard discard))

(defun deflexer (lst)
  (make-lexer :items lst))

;;; ----------------------------------------------------------
;;; interface
;;; ----------------------------------------------------------

(defparameter *error-token-name* 'error-tok)

(defgeneric do-lex (lexer in) (:documentation "user given lexer on input source"))

(defmethod do-lex ((lexer lexer) (str string) &aux (str-len (length str)))
  (loop with pos = 0 and all-result = '() and error = nil and error-buffer = '()
     do (let ((m nil))
          (loop for item in (lexer-items lexer) do
               (multiple-value-bind (result range-start range-end)
                   (match (lex-item-regex item) str pos)
                 (when result
                   (setf m t pos range-end)
                   (when (not (emptyp error-buffer))
                     (push (make-token :type *error-token-name*
                                       :val (format nil "~{~a~}" (reverse error-buffer))
                                       :range-start (- range-start (length error-buffer))
                                       :range-end range-start)
                           all-result)

                     (setf error-buffer '()))
                   (unless (lex-item-discard item)
                     (push (make-token :type (lex-item-name item)
                                       :val result
                                       :range-start range-start
                                       :range-end range-end)
                           all-result))
                   (return))))
          (unless m (setf error t)))
     if error do (progn
                   (push (char str pos) error-buffer)
                   (incf pos)
                   (setf error nil))
     until (>= pos str-len)
     finally (progn
               (when (not (emptyp error-buffer))
                 (push (make-token :type *error-token-name*
                                   :val (format nil "~{~a~}" (reverse error-buffer))
                                   :range-start 0
                                   :range-end str-len)
                       all-result))
               (push (make-token :type *ending-symbol*
                                 :range-start str-len
                                 :range-end str-len)
                     all-result)
               (return (reverse all-result)))))

(defmethod do-lex ((lexer lexer) (in stream))
  (do-lex lexer (alexandria:read-stream-content-into-string in)))

(defmethod do-lex ((lexer lexer) (p pathname))
  (with-open-file (s p)
    (do-lex lexer s)))
