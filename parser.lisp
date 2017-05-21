;;; Simple Parser
;;;
;;; Author: Dr.CZ
;;; Date: 2017-5-18

(load "./lexer.lisp")
(load "./director.lisp")

(defpackage slr-parser
  (:use :common-lisp :alexandria
        :slr-director :slr-lexer :slr-symbols)
  (:export ->
           parse
           treenode
           treenode-p
           treenode-type
           treenode-children
           treenode-properties
           elt-tree
           print-treenode
           set-property
           get-property
           rem-property
           tok->treenode))
(in-package :slr-parser)

;;; ----------------------------------------------------------
;;; structure
;;; ----------------------------------------------------------

(defstruct treenode
  (type nil :type symbol)
  (children '() :type list)
  (properties (make-hash-table) :type hash-table :read-only t))

(defmethod set-property ((node treenode) (p-name symbol) p-value)
  (setf (gethash p-name (treenode-properties node)) p-value))

(defmethod get-property ((node treenode) (p-name symbol) &optional default)
  (or (gethash p-name (treenode-properties node))
      default))

(defmethod rem-property ((node treenode) (p-name symbol))
  (remhash p-name (treenode-properties node)))

(defmethod print-treenode ((node treenode) &key (prefix "") (is-tail t) (print-properties t) (stream *standard-output*))
  (let* ((type (treenode-type node))
         (children (treenode-children node))
         (properties (treenode-properties node))
         (node-properties
          (format nil "[ ~{~a~^ ~} ]"
                  (loop for k being the hash-key in properties
                     using (hash-value v)
                     collect (format nil "~a: ~a " k v)))))
    (format stream "~a~a~a  ~a~%"
            prefix
            (if is-tail "└── " "├── ")
            type
            (if print-properties node-properties ""))
    (loop with mark = (1- (length children))
       for i from 0 below (length children)
       for c in children
       do (print-treenode c
                          :prefix (format nil "~a~a" prefix (if is-tail "    " "│   "))
                          :is-tail (if (= i mark) t nil)
                          :print-properties print-properties))))

(defmethod print-object ((node treenode) s)
  (print-treenode node :print-properties nil :stream s))

(defun elt-tree (node &rest lst)
  (declare (type treenode node))
  (if (not lst)
      node
      (apply #'elt-tree
             (elt (treenode-children node) (car lst))
             (cdr lst))))

(defun tok->treenode (tok)
  (declare (type token tok))
  (let ((node (make-treenode :type (token-type tok))))
    (set-property node :str-val (token-val tok))
    (set-property node :range-start (token-range-start tok))
    (set-property node :range-end (token-range-end tok))
    (return-from tok->treenode node)))

;;; ----------------------------------------------------------
;;; parser
;;; ----------------------------------------------------------

(defparameter *abandon-table* (make-hash-table :test #'equalp))

(defun -> (name form &key abandon)
  "build a generation with `name` and `form`"
  (let ((g (make-generation :name name :form form)))
    (setf (gethash g *abandon-table*) abandon)
    g))


(defun parse0 (tokens slr1 reduce-table state-stack input-stack)
  (loop
     (let* ((current-state (first state-stack))
            (tok (first tokens)))
       (typecase tok
         ((or token treenode)
          (let* ((type (if (token-p tok) (token-type tok) (treenode-type tok)))
                 (actions (get-2d-hash slr1 current-state type)))
            (cond
              ((member *success-symbol* actions)
               (return (first input-stack)))

              ((and (assoc :S actions) (assoc :R actions))
               (handler-case
                   (progn
                     (parse0 (cdr tokens) slr1 reduce-table
                             (cons (cdr (assoc :S actions)) state-stack)
                             (cons (car tokens) input-stack)))
                 (simple-error (c)
                   (declare (ignore c))
                   (return
                     (let* ((g (gethash (cdr (assoc :R actions)) reduce-table))
                            (g-name (generation-name g))
                            (g-form (generation-form g))
                            (g-abandon (gethash g *abandon-table*))
                            (node (make-treenode :type g-name)))
                       (loop for i from (1- (length g-form)) downto 0
                          do (progn
                               (pop state-stack)
                               (let ((tok0 (pop input-stack)))
                                 (cond
                                   ((and (token-p tok0)
                                         (not (member i g-abandon)))
                                    (push (tok->treenode tok0) (treenode-children node)))
                                   ((treenode-p tok0)
                                    (push tok0 (treenode-children node)))))))
                       (parse0 (cons node tokens) slr1 reduce-table state-stack input-stack))))
                 (:no-error (tree) (return tree))))

              ((assoc :S actions)
               (push (cdr (assoc :S actions)) state-stack)
               (push (pop tokens) input-stack))

              ((assoc :R actions)
               (let* ((g (gethash (cdr (assoc :R actions)) reduce-table))
                      (g-name (generation-name g))
                      (g-form (generation-form g))
                      (g-abandon (gethash g *abandon-table*))
                      (node (make-treenode :type g-name)))
                 (loop for i from (1- (length g-form)) downto 0
                    do (progn
                         (pop state-stack)
                         (let ((tok0 (pop input-stack)))
                           (cond
                             ((and (token-p tok0)
                                   (not (member i g-abandon)))
                              (push (tok->treenode tok0) (treenode-children node)))
                             ((treenode-p tok0)
                              (push tok0 (treenode-children node)))))))
                 (push node tokens)))

              (t (error (concatenate 'string
                                     "Can't parse it: no corresponding instruction in the direction table.~%"
                                     "Current token: ~a~%"
                                     "The Consumed tokens: ~{~a~^ ~}~%"
                                     "The following tokens: ~{~a~^ ~}~%")
                        tok
                        (reverse (mapcar #'(lambda (tok0) (if (token-p tok0) (token-val tok0) (treenode-type tok0))) input-stack))
                        (mapcar #'(lambda (tok0) (token-val tok0)) tokens))))))

         (t (error "Known token. type: ~s" tok))))))

(defun parse (tokens d-table)
  (let ((slr1 (slr1-table-table d-table))
        (reduce-table (slr1-table-indexed-generations d-table))
        (state-stack '(0))
        (input-stack '()))
    (parse0 tokens slr1 reduce-table state-stack input-stack)))
