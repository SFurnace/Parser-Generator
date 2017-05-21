;;; A simple implementation of Regex, just for fun.
;;;
;;; Author: Dr.CZ
;;; Date:   2017-5-18
;;;
;;; supported: . ? * + {n,m} [^a-z] (regex)
;;;            (?=regex) (?!regex) (?<=regex) (?<! regex)
;;;            some basic escape char like \n \t \r \b

(unless (find-package :alexandria)
  (ql:quickload :alexandria))

(defpackage slr-regex
  (:use :common-lisp :alexandria)
  (:export regex match))
(in-package :slr-regex)

;;; ----------------------------------------------------------
;;; utils
;;; ----------------------------------------------------------

(defun escape-helper (c)
  "help to escape given char"
  (declare (type character c))
  (case c
    (#\t #\tab)
    (#\n #\newline)
    (#\r #\return)
    (#\b #\backspace)
    (t c)))

(defun next-int (str &optional (start 0))
  "get next integer or nil"
  (let ((p start))
    (loop for i from start below (length str)
       do (let ((c (char str i)))
            (if (digit-char-p c)
                (setf p (1+ i))
                (return))))
    (if (> p start)
        (return-from next-int
          (parse-integer (subseq str start p)))
        (values nil 0))))

;;; ----------------------------------------------------------
;;; regex
;;; ----------------------------------------------------------

(defun find-match-\) (str &optional (start 0)
                      &aux (str-len (length str)))
  (loop with mark = 0 and i = (1+ start)
     while (< i str-len)
     do (let ((c (char str i)))
          (cond
            ((eql c #\\)
             (cond
               ((>= (1+ i) str-len)
                (error "Bad ends of a escape char."))
               (t (incf i))))
            ((eql c #\))
             (if (= mark 0)
                 (return-from find-match-\) i)
                 (decf mark)))
            ((eql c #\()
             (incf mark)))
          (incf i)))
  (error "Can't find #\): ~s" (subseq str start)))

(defun {-helper (str pos)
  "get int1 int2 and the length of the consumed string"
  (let (correct r1 r2 r3)
    (multiple-value-bind (i1 l1)
        (next-int str pos)
      (when (and i1
                 (< (+ pos l1) (length str))
                 (eql (char str (+ pos l1)) #\,))
        (multiple-value-bind (i2 l2)
            (next-int str (+ pos l1 1))
          (when (and i2
                     (< (+ pos l1 1 l2) (length str))
                     (eql (char str (+ pos l1 1 l2)) #\}))
            (setf correct t r1 i1 r2 i2 r3 (+ pos l1 1 l2 1))))))
    (if correct
        (values r1 r2 r3)
        (error "Bad {} form: ~s" (subseq str pos)))))

(defun find-match-\] (str &optional (start 0)
                      &aux (str-len (length str)))
  (loop with i = start
     do (let ((c (char str i)))
          (cond
            ((eql c #\\) (cond
                           ((>= (1+ i) str-len)
                            (error "Bad ends of a escape char."))
                           (t (incf i 2))))
            ((eql c #\]) (return-from find-match-\] i))
            (t (incf i))))
     until (>= i str-len))
  (error "Can't find ]: ~s" (subseq str start)))

(defun [-helper (str)
  "generate the correct regex form of corresponding string in []"
  (let ((pos 0)
        (reversed nil)
        (result '()))
    (when (eql (char str 0) #\^)
      (setf reversed t str (subseq str 1)))
    (loop
       (when (>= pos (length str))
         (return-from [-helper `(:char ,reversed ,@(remove-duplicates (reverse result)))))
       (let ((c (char str pos)))
         (cond
           ((and (eql c #\-)
                 (< 0 pos (1- (length str)))
                 (not (listp (first result))))
            (push `(:between ,(pop result) ,(char str (1+ pos))) result)
            (incf pos 2))
           ((and (eql c #\\)
                 (< pos (1- (length str))))
            (push (escape-helper (char str (1+ pos))) result)
            (incf pos 2))
           (t (push c result) (incf pos 1)))))))

(defun consume-char (str &optional (pos 0) &aux (str-len (length str)))
  "generate one step's form in a regex"
  (declare (type string str) (type fixnum pos))
  (cond
    ((>= pos str-len)
     (values nil pos))
    (t (let ((result '(:seq)))
         (loop
            (if (>= pos str-len)
                (return-from consume-char (values (reverse result) pos))
                (let ((c (char str pos)))
                  (case c
                    (#\\ (if (< (1+ pos) str-len)
                             (progn
                               (push (escape-helper (char str (1+ pos))) result)
                               (incf pos 2))
                             (error "Bad ends of a escape char.")))

                    (#\. (push :any result) (incf pos))

                    ((#\? #\* #\+ #\{)
                     (if (> (length result) 1)
                         (let ((last-one (pop result)))
                           (cond
                             ((and (listp last-one) (eql (first last-one) :repeat))
                              (error "Multiple repeat form"))
                             ((eql c #\{)
                              (multiple-value-bind (i1 i2 len)
                                  ({-helper str (incf pos))
                                (push (list :repeat i1 i2 last-one) result)
                                (incf pos len)))
                             ((eql c #\?) (push (list :repeat 0 1 last-one) result) (incf pos))
                             ((eql c #\*) (push (list :repeat 0 :inf last-one) result) (incf pos))
                             ((eql c #\+) (push (list :repeat 1 :inf last-one) result) (incf pos))))
                         (error "Nothing before a ~a" c)))

                    (#\[ (let ((end-pos (find-match-\] str pos)))
                           (when (= end-pos (1+ pos))
                             (error "Nothing in the []: ~a" (subseq str pos)))
                           (push ([-helper (subseq str (1+ pos) end-pos)) result)
                           (setf pos (1+ end-pos))))

                    (#\| (return-from consume-char (values (reverse result) (1+ pos))))

                    (#\( (cond
                           ((and (< (+ 2 pos) str-len)
                                 (member (subseq str (1+ pos) (+ 3 pos)) '("?=" "?!") :test #'string-equal))
                            (let ((negative (if (string-equal (subseq str (1+ pos) (+ 3 pos)) "?=") nil t))
                                  (end-pos (find-match-\) str pos)))
                              (push `(:lookahead ,negative ,(regex (subseq str (+ 3 pos) end-pos))) result)
                              (setf pos (1+ end-pos))))

                           ((and (< (+ 3 pos) str-len)
                                 (member (subseq str (1+ pos) (+ 4 pos)) '("?<=" "?<!") :test #'string-equal))
                            (let ((negative (if (string-equal (subseq str (1+ pos) (+ 4 pos)) "?<=") nil t))
                                  (end-pos (find-match-\) str pos)))
                              (push `(:lookbehind ,negative ,(regex (subseq str (+ 4 pos) end-pos))) result)
                              (setf pos (1+ end-pos))))

                           (t (let ((end-pos (find-match-\) str pos)))
                                (push (regex (subseq str (1+ pos) end-pos)) result)
                                (setf pos (1+ end-pos))))))

                    (t (push c result) (incf pos))))))))))

(defun regex (str)
  "generate a regex from the given  string"
  (declare (type string str))
  (let ((str-len (length str))
        (pos 0)
        (result '(:or)))
    (loop
       (multiple-value-bind (one next-pos)
           (consume-char str pos)
         (when one (push one result))
         (if (>= next-pos str-len)
             (return)
             (setf pos next-pos))))
    (when (or (= str-len 0)
              (and (> str-len 0)
                   (char= #\| (char str 0)))
              (and (> str-len 1)
                   (char= #\| (char str (1- str-len)))
                   (not (char= #\\ (char str (- str-len 2))))))
      (push :empty result))
    (reverse result)))

(defun match (regex str &optional (start 0) &aux (str-len (length str)))
  "try to match the regex and the string from the start"
  (declare (type (or list character keyword) regex) (type string str))
  (case (if (or (characterp regex) (keywordp regex)) regex (first regex))
    (:or            (dolist (s (cdr regex))
                      (multiple-value-bind (result range-start range-end)
                          (match s str start)
                        (when result
                          (return-from match (values result range-start range-end)))))
                    (values nil start start))

    (:repeat        (loop with counter = 0 and all-result = "" and all-end = start do
                         (multiple-value-bind (result range-start range-end)
                             (match (fourth regex) str all-end)
                           (declare (ignore range-start))
                           (cond
                             ((and (stringp result)
                                   (> (length result) 0))
                              (incf counter)
                              (setf all-result (concatenate 'string all-result result)
                                    all-end range-end))

                             (t (if (and (>= counter (second regex))
                                         (or (eql :inf (third regex))
                                             (<= counter (third regex))))
                                    (return (values all-result start all-end))
                                    (return (values nil start start))))))))

    (:seq           (loop with all-result = "" and all-end = start
                       for s in (cdr regex)
                       do (multiple-value-bind (result range-start range-end)
                              (match s str all-end)
                            (declare (ignore range-start))
                            (cond
                              (result (setf all-result (concatenate 'string all-result result)
                                            all-end range-end))
                              (t (return (values nil start start)))))
                       finally (return (values all-result start all-end))))

    (:char          (dolist (c (cddr regex))
                      (multiple-value-bind (result range-start range-end)
                          (match c str start)
                        (declare (ignore range-start))
                        (when (and result (not (second regex)))
                          (return-from match (values (subseq str start (1+ start)) start range-end)))
                        (when (and (not result) (second regex))
                          (return-from match (values (subseq str start (1+ start)) start (1+ range-end))))))
                    (values nil start start))

    (:between       (if (and (< start str-len)
                             (char<= (second regex) (char str start) (third regex)))
                        (values (subseq str start (1+ start)) start (1+ start))
                        (values nil start start)))

    (:lookahead     (let ((negative (second regex)))
                      (multiple-value-bind (result range-start range-end)
                          (match (third regex) str start)
                        (declare (ignore range-start range-end))
                        (cond
                          ((and result (not negative)) (values "" start start))
                          ((and (not result) negative) (values "" start start))
                          (t (values nil start start))))))

    (:lookbehind    (let ((negative (second regex)))
                      (loop for i from start downto 0
                         do (let ((s (subseq str i start)))
                              (cond
                                ((and (full-match (third regex) s) (not negative))
                                 (return-from match (values "" start start)))
                                ((and (not (full-match (third regex) s)) negative)
                                 (return-from match (values "" start start)))))
                         finally (values nil start start))))

    (:any           (if (< start str-len)
                        (values (subseq str start (1+ start)) start (1+ start))
                        (values nil start start)))

    (:empty         (values "" start start))
    (t              (cond
                      ((>= start (length str))
                       (values nil start start))
                      ((eql (char str start) regex)
                       (values (make-string 1 :initial-element regex) start (1+ start)))
                      (t (values nil start start))))))

(defun full-match (regex str)
  "match the regex and string, assume the result should be equal to the str itself."
  (string-equal str (match regex str)))
