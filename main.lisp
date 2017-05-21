;;; SLR(1) Parser Generator
;;;
;;; Author: Dr.CZ
;;; Date: 2017-5-19

(load "./parser.lisp")

(defpackage slr
  (:use :common-lisp :alexandria
        :slr-lexer :slr-parser
        :slr-regex :slr-director :slr-symbols)
  (:import-from :sb-ext *posix-argv*))
(in-package :slr)

(defparameter *lexer*
  (deflexer
      (list
       (~> 'empty "[\\t\\b\\r\\n ]+" :discard t)
       (~> 'grammar-mark "(grammar|GRAMMAR)(?=[ \\t\\b\\n\\r{])")
       (~> 'grammar-splitter "->")
       (~> 'lex-mark "(lex|LEX)(?=[ \\t\\b\\n\\r{])")
       (~> 'lex-splitter ":")
       (~> 'line-splitter ";")
       (~> 'start "\\{")
       (~> 'end "}")
       (~> 'choice-mark "\\|")
       (~> 'ignore-mark "\\*")
       (~> 'identifier "[a-zA-Z_][a-zA-Z0-9_]*")
       (~> 'str "\"((\\\\\")|[^\"])*\""))))

(defparameter *parser*
  (defslr1 'G
      (list
       (-> 'G            '(lex-mark start lexes end grammar-mark identifier start grammars end) :abandon '(0 1 3 4 6 8))
       (-> 'ids          '(identifier ids))
       (-> 'ids          '(ignore-id ids))
       (-> 'ids          '(identifier))
       (-> 'ids          '(ignore-id))
       (-> 'ignore-id    '(ignore-mark identifier) :abandon '(0))
       (-> 'lexes        '(identifier lex-splitter str line-splitter lexes) :abandon '(1 3))
       (-> 'lexes        '(identifier lex-splitter str line-splitter) :abandon '(1 3))
       (-> 'lexes        '(ignore-id lex-splitter str line-splitter lexes) :abandon '(1 3))
       (-> 'lexes        '(ignore-id lex-splitter str line-splitter) :abandon '(1 3))
       (-> 'grammars     '(identifier grammar-splitter branches line-splitter grammars) :abandon '(1 3))
       (-> 'grammars     '(identifier grammar-splitter branches line-splitter) :abandon '(1 3))
       (-> 'branches     '(ids choice-mark branches) :abandon '(1))
       (-> 'branches     '(ids)))))


(defun generate-lexer (node)
  (declare (type treenode node))
  (cond
    ((eql 'lexes (treenode-type node))
     (let* ((children (treenode-children node))
            (id (first children))
            (str (second children)))
       (if (= 2 (length children))
           (if (eql 'ignore-id (treenode-type id))
               (list (~> (intern (get-property (elt-tree id 0) :str-val))
                         (string-trim "\"" (get-property str :str-val))
                         :discard t))
               (list (~> (intern (get-property id :str-val)) (string-trim "\"" (get-property str :str-val)))))
           (if (eql 'ignore-id (treenode-type id))
               (cons (~> (intern (get-property (elt-tree id 0) :str-val))
                         (string-trim "\"" (get-property str :str-val))
                         :discard t)
                     (generate-lexer (third children)))
               (cons (~> (intern (get-property id :str-val))
                         (string-trim "\"" (get-property str :str-val)))
                     (generate-lexer (third children)))))))
    (t (error "~s is not a lexes node" node))))

(defun collect-ids (node counter)
  (declare (type treenode node) (type fixnum counter))
  (cond
    ((eql 'ids (treenode-type node))
     (let* ((children (treenode-children node))
            (id (first children)))
       (if (= 1 (length children))
           (if (eql 'ignore-id (treenode-type id))
               (cons (list (intern (get-property (elt-tree id 0) :str-val))) (list counter))
               (cons (list (intern (get-property id :str-val))) '()))
           (let ((result (collect-ids (second children) (1+ counter))))
             (if (eql 'ignore-id (treenode-type id))
                 (cons (cons (intern (get-property (elt-tree id 0) :str-val)) (car result))
                       (cons counter (cdr result)))
                 (cons (cons (intern (get-property id :str-val)) (car result))
                       (cdr result)))))))
    (t (error "~s is not a ids node." node))))

(defun collect-branches (node id)
  (declare (type treenode node))
  (cond
    ((eql 'branches (treenode-type node))
     (let* ((children (treenode-children node))
            (ids-info (collect-ids (first children) 0)))
       (if (= 1 (length children))
           (list (-> id (car ids-info) :abandon (cdr ids-info)))
           (cons (-> id (car ids-info) :abandon (cdr ids-info))
                 (collect-branches (second children) id)))))
    (t (error "~s is not a branches node." node))))

(defun generate-grammar (node)
  (declare (type treenode node))
  (cond
    ((eql 'grammars (treenode-type node))
     (let* ((children (treenode-children node))
            (id (first children))
            (branches (second children)))
       (if (= 2 (length children))
           (collect-branches branches (intern (get-property id :str-val)))
           (append (collect-branches branches (intern (get-property id :str-val)))
                   (generate-grammar (third children))))))
    (t (error "~s is not a grammars node." node))))


(defun do-parser (grammar-file files &key (print-properties t) (stop-lex nil))
  (with-open-file (s grammar-file)
    (let* ((tree (parse (do-lex *lexer* s) *parser*)))
      (let ((*lexer* (deflexer (generate-lexer (elt-tree tree 0))))
            (*parser* (defslr1 (intern (get-property (elt-tree tree 1) :str-val))
                          (generate-grammar (elt-tree tree 2))))
	    (*for-debug* t))
        (dolist (file files)
          (with-open-file (s file)
            (let ((l (do-lex *lexer* s)))
              (if stop-lex
                  (format t "~a~%" l)
                  (print-treenode (parse l *parser*)
                                  :print-properties print-properties)))))))))

(defun print-grammar (grammar-file)
  (with-open-file (s grammar-file)
    (let* ((tree (parse (do-lex *lexer* s) *parser*))
           (*parser* (defslr1 (intern (get-property (elt-tree tree 1) :str-val))
                         (generate-grammar (elt-tree tree 2)))))
      (print-slr1-table *parser*))))

(defun main (&optional (args *posix-argv*))
  (handler-case
      (cond
        ((or (= 1 (length args))
             (member "-h" args :test #'string-equal)
             (member "--help" args :test #'string-equal))
         (format t (string-trim '(#\Space #\Tab #\Newline) "
This is a SLR(1) parser generator.
Usage:
    slr1 grammar-file files ...

Optional:
    -h                        show this help info.
    --lex                     stop after lex step.
    --print-properties        print properties of AST nodes.
")))

        ((= (length args) 2)
         (print-grammar (second args)))

        ((> (length args) 2)
         (let ((sl (member "--lex" args :test #'string-equal))
               (pp (member "--print-properties" args :test #'string-equal))
               (files (remove-if #'(lambda (s)
                                     (member s '("--print-properties"
                                                 "-h" "--help" "--lex")
                                             :test #'string-equal))
                                 (cddr args))))
           (do-parser (second args) files :print-properties pp :stop-lex sl))))

    (simple-error (c) (format t "~a~%" c))))

;; (main (list " " "./test/theads.txt" "./test/t1.txt"))
(sb-ext:save-lisp-and-die "slr1" :toplevel #'main :executable t :compression t)
