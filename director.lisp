;;;; Make LR(1) Direct Table.
;;;;
;;;; Author: CZ
;;;; Date: 2017-04-27
;;;; Version: 1.0

(unless (find-package :alexandria)
  (ql:quickload :alexandria))

(load "./special-symbols.lisp")

(defpackage slr-director
  (:use :common-lisp :alexandria :slr-symbols)
  (:export make-generation
           generation
           generation-p
           generation-name
           generation-form
           defslr1
           slr1-table-p
           slr1-table-head
           slr1-table-table
           slr1-table-first-set
           slr1-table-follow-set
           slr1-table-indexed-generations
           slr1-table-non-terminal
           print-slr1-table
           get-2d-hash
           find-first-set
           find-all-first-set
           find-follow-set
           find-all-follow-set
           *table-printing-block-len*))
(in-package :slr-director)

;;; ----------------------------------------------------------
;;; Configure
;;; ----------------------------------------------------------

(defparameter *table-printing-block-len* 15)

;;; ----------------------------------------------------------
;;; Utils
;;; ----------------------------------------------------------

;; Two-dimension hashtable

(defun get-2d-hash (table key1 key2 &optional default)
  "regard a hashtable as two-dimension hashtable, get value with key1 and key2"
  (let ((table2 (gethash key1 table)))
    (if table2 (gethash key2 table2) default)))

(defun put-2d-hash (table key1 key2 value)
  "regard a hashtable as two-dimension hashtable, put the value to place in [key1, key2]"
  (let ((table2 (gethash key1 table)))
    (if table2
        (setf (gethash key2 table2) value)
        (progn (setf table2 (make-hash-table)
                     (gethash key2 table2) value
                     (gethash key1 table) table2)))))

(defsetf get-2d-hash put-2d-hash "bind setf-macro for 2d-hashtable operators.")

;;; ----------------------------------------------------------
;;; Generation
;;; ----------------------------------------------------------

(defstruct (generation
             (:constructor
              make-generation (&key
                               name form
                               &aux
                               (form (remove *empty-symbol* form)))))
  "use this structure to represent generation equations"
  (name nil :type symbol :read-only t)
  (form nil :type list :read-only t))

(defmethod print-object ((g generation) stream)
  (format stream "[ ~a -> ~:[ε~;~{~a~^ ~}~] ]"
          (generation-name g) (generation-form g) (generation-form g)))

(defun make-indexed-generations (generation-lst)
  (let ((table (make-hash-table :test #'equalp)))
    (mapcar #'(lambda (i n)
                (setf (gethash i table) n)
                (setf (gethash n table) i))
            (alexandria:iota (length generation-lst))
            generation-lst)
    table))

(defun find-non-terminal (generation-lst)
  "find all non-terminal tokens in a generation-lst"
  (let ((result '()))
    (dolist (g generation-lst)
      (push (generation-name g) result))
    (remove-duplicates result)))

(defun find-terminal (generation-lst &optional non-terminal)
  "find all terminal tokens in a generation-lst"
  (let ((non-terminal (or non-terminal (find-non-terminal generation-lst)))
        (result '()))
    (dolist (g generation-lst)
      (dolist (i (generation-form g))
        (push i result)))
    (intersection (remove-duplicates result) non-terminal)))

(defun find-first-set (name generation-lst
                       &key
                         container non-terminal token-stack
                       &aux
                         (container (or container (make-hash-table)))
                         (non-terminal (or non-terminal
                                           (find-non-terminal generation-lst))))
  "find first set of the given generation-list"
  (cond
    ;; use container to avoid some duplicate computing
    ((gethash name container) (gethash name container))
    ;; the first-set of a terminal-token only contains itself
    ((not (member name non-terminal)) (list name))
    ;; use a token-stack to avoid recursive
    ((member name token-stack) '())
    (t (let ((found-all t)    ; whether push result to container
             (result '()))
         (dolist (g (remove-if-not #'(lambda (g)
                                       (eql (generation-name g) name))
                                   generation-lst))
           (cond
             ((eql (generation-form g) nil) ; ε
              (push *empty-symbol* result))

             ((not (member (first (generation-form g)) non-terminal)) ; terminal
              (push (first (generation-form g)) result))

             (t (let ((can-be-empty t)) ; whether push ε to result
                  (dolist (tok (generation-form g))
                    (cond
                      ((not (member tok non-terminal))
                       (setf can-be-empty nil)
                       (push result tok)
                       (return))      ; end with a terminal

                      ((member tok token-stack)
                       (setf can-be-empty nil found-all nil)
                       (return))

                      (t (let ((set (find-first-set
                                     tok generation-lst
                                     :container container
                                     :non-terminal non-terminal
                                     :token-stack (cons name token-stack))))
                           (cond
                             ((member *empty-symbol* set)
                              (setf result
                                    (append result (remove *empty-symbol* set))))

                             (t (setf result (append result set) can-be-empty nil)
                                (return)))))))
                  (if can-be-empty (push *empty-symbol* result))))))
         (setf result (remove-duplicates result))
         (if found-all (setf (gethash name container) result))
         result))))

(defun find-follow-set (name generation-lst
                        &key
                          container first-set-container non-terminal token-stack
                        &aux
                          (container (or container (make-hash-table)))
                          (first-set-container (or first-set-container
                                                   (make-hash-table)))
                          (non-terminal (or non-terminal
                                            (find-non-terminal generation-lst))))
  "find follow set of the given generation-list"
  (cond
    ((gethash name container) (gethash name container))
    ((not (member name non-terminal)) '())
    ((member name token-stack) '())
    (t (let ((found-all t)
             (result '()))
         (dolist (g (remove-if-not #'(lambda (g)
                                       (member name (generation-form g)))
                                   generation-lst))
           (let ((rest (cdr (member name (generation-form g))))
                 (can-be-superset t))
             (dolist (r rest)
               (cond
                 ((not (member r non-terminal))
                  (setf can-be-superset nil)
                  (push r result)
                  (return))

                 ((member r token-stack) ; use a token-stack to avoid recursive
                  (setf can-be-superset nil found-all nil)
                  (return))

                 (t (let ((rest-first-set
                           (find-first-set r generation-lst
                                           :container first-set-container
                                           :non-terminal non-terminal)))
                      (setf result
                            (append result (remove *empty-symbol* rest-first-set)))
                      (unless (member *empty-symbol* rest-first-set)
                        (setf can-be-superset nil)
                        (return))))))
             (when can-be-superset
               (if (not (member (generation-name g) token-stack))
                   (setf result
                         (append result
                                 (find-follow-set
                                  (generation-name g) generation-lst
                                  :container container
                                  :non-terminal non-terminal
                                  :token-stack (cons name token-stack))))
                   (setf found-all nil)))))
         (setf result (remove-duplicates result))
         (if found-all (setf (gethash name container) result))
         result))))

(defun find-all-first-set (generation-lst)
  "make a hashtable to store first-sets of all non-terminal tokens"
  (let ((non-terminal (find-non-terminal generation-lst))
        (result (make-hash-table)))
    (dolist (tok non-terminal)
      (find-first-set tok generation-lst
                      :container result
                      :non-terminal non-terminal))
    (remhash *beginning-symbol* result)
    result))

(defun find-all-follow-set (generation-lst)
  "make a hashtable to store first-sets of all non-terminal tokens"
  (let ((non-terminal (find-non-terminal generation-lst))
        (first-set-container (make-hash-table))
        (result (make-hash-table)))
    (dolist (tok non-terminal (remhash *beginning-symbol* result))
      (find-follow-set tok generation-lst
                       :container result
                       :first-set-container first-set-container
                       :non-terminal non-terminal))
    (remhash *beginning-symbol* result)
    result))

;;; ----------------------------------------------------------
;;; LR Items
;;; ----------------------------------------------------------

(defstruct (lritem
             (:constructor
              make-lritem (&key
                           name dot-left dot-right
                           &aux
                           (dot-left (remove *empty-symbol* dot-left))
                           (dot-right (remove *empty-symbol* dot-right)))))
  "represent LR(0) items"
  (name nil :type symbol :read-only t)
  (dot-left '() :type list :read-only t)
  (dot-right '() :type list :read-only t))

(defmethod print-object ((l lritem) stream)
  (format stream "[ ~a -> ~{~a ~}. ~{~a~^ ~} ]"
          (lritem-name l)
          (lritem-dot-left l)
          (lritem-dot-right l)))

(defmacro => (name dot-left dot-right)
  "make a lritem"
  `(make-lritem :name ,name
                :dot-left ,dot-left
                :dot-right ,dot-right))

(defun lritem->generation (item)
  "make a generation from a lriterm"
  (make-generation :name (lritem-name item)
                   :form (append (lritem-dot-left item)
                                 (lritem-dot-right item))))

(defun generations->first-lritems (generation-lst)
  (let ((result '()))
    (dolist (g generation-lst)
      (let ((name (generation-name g))
            (form (generation-form g)))
        (push (=> name '() form) result)))
    result))

(defun find-begin-lritem (lritem-lst)
  (find-if #'(lambda (i)
               (and (eql (lritem-name i) *beginning-symbol*)
                    (eql (lritem-dot-left i) nil)))
           lritem-lst))

(defun lritem-consume-one (item)
  "let the giving lritem consumes one token and return the result"
  (if (null (lritem-dot-right item))
      item
      (=> (lritem-name item)
          (append (lritem-dot-left item)
                  (list (first (lritem-dot-right item))))
          (rest (lritem-dot-right item)))))

(defun expand-lritem (item lritem-lst non-terminal &optional expanded)
  "expand a lritem"
  (cond
    ((or (null (lritem-dot-right item))
         (not (member (car (lritem-dot-right item)) non-terminal)))
     (list item))

    (t (let* ((result `(,item))
              (tokens-need-expand (car (lritem-dot-right item)))
              (new-items (remove-if-not
                          #'(lambda (item)
                              (and (eql (lritem-name item) tokens-need-expand)
                                   (eql (lritem-dot-left item) nil)))
                          lritem-lst))
              (items-need-expand (remove-if-not
                                  #'(lambda (item)
                                      (not (member (car (lritem-dot-right item)) expanded)))
                                  new-items)))
         (setf result (append result new-items))
         (push (car (lritem-dot-right item)) expanded)
         (dolist (n items-need-expand (remove-duplicates result :test #'equalp))
           (setf result
                 (append result
                         (expand-lritem n lritem-lst non-terminal expanded))))))))

;;; ----------------------------------------------------------
;;; DFA
;;; ----------------------------------------------------------

(defstruct slr1-table
  "represent a SLR(1) table"
  (table nil :read-only t :type hash-table)
  (indexed-generations nil :read-only t :type hash-table)
  (non-terminal nil :read-only t :type list)
  (first-set nil :read-only t :type hash-table)
  (follow-set nil :read-only t :type hash-table))

(defmethod slr1-table-head ((d-table slr1-table))
  (let* ((non-terminal (slr1-table-non-terminal d-table))
         (table (slr1-table-table d-table))
         (lines (alexandria:hash-table-values table))
         (symbols (remove *reducing-symbol*
                          (remove-duplicates
                           (mapcan #'(lambda (l)
                                       (alexandria:hash-table-keys l))
                                   lines)))))
    (mapcar #'(lambda (n s) (cons s n))
            (alexandria:iota (length symbols))
            (sort symbols
                  #'(lambda (s1 s2)
                      (cond
                        ((and (member s1 non-terminal) (not (member s2 non-terminal)))
                         nil)
                        ((and (member s2 non-terminal) (not (member s1 non-terminal)))
                         t)
                        (t (string< (symbol-name s1) (symbol-name s2)))))))))

(defun print-indexed-generations (indexed-generations stream)
  (dolist (i (sort
              (remove-if-not #'numberp
                             (alexandria:hash-table-keys indexed-generations))
              #'<))
    (format stream "R~a: ~a~%" i (gethash i indexed-generations)))
  (format stream "~%"))

(defun print-table-head (heads stream)
  (format stream "~va" *table-printing-block-len*  " ")
  (dolist (h heads)
    (format stream "~v@:<~a~>" *table-printing-block-len* (car h))))

(defun print-table-line (l heads stream)
  (format stream "~%")
  (let ((n (car l))
        (table2 (cdr l)))
    (format stream "~v@:<~a~>" *table-printing-block-len* n)

    (dolist (h heads)
      (cond
        ((gethash (car h) table2)
         (let* ((str "")
                (actions (gethash (car h) table2)))
           (dolist (a actions)
             (cond
               ((symbolp a)
                (setf str (concatenate 'string str (format nil "~a|" a))))
               ((eql (car a) :S)
                (setf str (concatenate 'string str (format nil "S~a|" (cdr a)))))
               ((eql (car a) :R)
                (setf str (concatenate 'string str (format nil "R~a|" (cdr a)))))
               (t (setf str (concatenate 'string str (format nil "ERR|"))))))
           (format stream "~v@:<~a~>" *table-printing-block-len* (string-right-trim "|" str))))
        (t (format stream "~va" *table-printing-block-len* " "))))))

(defun print-first-set (first-set stream)
  (format stream "~%~%First Set:~%")
  (dolist (k (alexandria:hash-table-keys first-set))
    (format stream "~a: ~{~a ~}~%" k (gethash k first-set))))

(defun print-follow-set (follow-set stream)
  (format stream "~%Follow Set:~%")
  (dolist (k (alexandria:hash-table-keys follow-set))
    (format stream "~a: ~{~a ~}~%" k (gethash k follow-set))))

(defun print-slr1-table (d-table &optional (stream *standard-output*))
  (declare (type slr1-table d-table) (stream stream))
  (let* ((heads (slr1-table-head d-table))
         (lines (sort (alexandria:hash-table-alist (slr1-table-table d-table))
                      #'< :key #'car)))
    (print-indexed-generations (slr1-table-indexed-generations d-table) stream)
    (print-table-head heads stream)
    (dolist (l lines)
      (print-table-line l heads stream))
    (print-first-set (slr1-table-first-set d-table) stream)
    (print-follow-set (slr1-table-follow-set d-table) stream)))


(defstruct state
  "represent the states in slr1-table"
  (num 0 :read-only t :type fixnum)
  (lritems '() :type list))

(defparameter *state-counter* -1 "use to mark the states")

(defun next-state-number ()
  "get next state-number"
  (incf *state-counter*))

(defun state-consumable-tokens (state)
  "return all tokens which the given state can accept"
  (or (remove-duplicates
       (mapcar #'(lambda (item)
                   (if (null (lritem-dot-right item))
                       *reducing-symbol*
                       (first (lritem-dot-right item))))
               (state-lritems state)))
      (list *reducing-symbol*)))

(defun make-init-state-stack (lritem-lst non-terminal)
  (list (make-state
         :num (next-state-number)
         :lritems (expand-lritem (find-begin-lritem lritem-lst)
                                 lritem-lst
                                 non-terminal))))

(defun make-slr1 (generation-lst)
  "use the given generation-list to make a slr1-table"
  (let* ((*state-counter* -1)
         (non-terminal (find-non-terminal generation-lst))
         (first-set (find-all-first-set generation-lst))
         (follow-set (find-all-follow-set generation-lst))
         (lritem-lst (generations->first-lritems generation-lst))
         (indexed-generations (make-indexed-generations generation-lst))
         (state-stack (make-init-state-stack lritem-lst non-terminal))
         (seen-states (copy-list state-stack))
         (DFA-table (make-hash-table)))
    (loop
       (cond
         ;; check stop condition
         ((null state-stack)
          (return (make-slr1-table :table DFA-table
                                   :indexed-generations indexed-generations
                                   :non-terminal non-terminal
                                   :first-set first-set
                                   :follow-set follow-set)))

         (t
          (let* ((current-state (pop state-stack))
                 (consumable-token (state-consumable-tokens current-state)))
            ;; handle every possible tokens
            (dolist (token consumable-token)
              (cond
                ((eql token *ending-symbol*)
                 (push *success-symbol*
                       (get-2d-hash DFA-table
                                    (state-num current-state)
                                    *ending-symbol*)))

                ((eql token *reducing-symbol*)
                 (let* ((item (find-if #'(lambda (item)
                                           (eql (lritem-dot-right item) nil))
                                       (state-lritems current-state)))
                        (index (gethash (lritem->generation item)
                                        indexed-generations)))
                   (if (not index) (break))
                   (dolist (follow-tok (gethash (lritem-name item) follow-set))
                     (push (cons :R index)
                           (get-2d-hash DFA-table
                                        (state-num current-state)
                                        follow-tok)))))

                (t (let* ((token-lritems (remove-if-not
                                          #'(lambda (item)
                                              (let ((item-r (lritem-dot-right item)))
                                                (and (not (null item-r))
                                                     (eql token (first item-r)))))
                                          (state-lritems current-state)))
                          (next-state-lriterms (remove-duplicates
                                                (mapcan #'(lambda (item)
                                                            (expand-lritem (lritem-consume-one item)
                                                                           lritem-lst
                                                                           non-terminal))
                                                        token-lritems)
                                                :test #'equalp))
                          (seen (find-if #'(lambda (item)
                                             (equalp next-state-lriterms
                                                     (state-lritems item)))
                                         seen-states)))
                     (if seen
                         (push (cons :S (state-num seen))
                               (get-2d-hash DFA-table
                                            (state-num current-state)
                                            token))
                         (let ((new-state (make-state :num (next-state-number)
                                                      :lritems next-state-lriterms)))
                           (push new-state seen-states)
                           (push new-state state-stack)
                           (push (cons :S (state-num new-state))
                                 (get-2d-hash DFA-table
                                              (state-num current-state)
                                              token))))))))))))))

(defun defslr1 (start-item lst)
  (push (make-generation :name *beginning-symbol*
                         :form (list start-item *ending-symbol*))
        lst)
  (make-slr1 lst))
