;; Test cases for project-1.lisp file

(defun testfun (a)
  (+ 1 a)
  )


;; Scratch code for testing dfa-simulate

;; Make an DFA
(defparameter *dfa-0*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 0 q0))
           'q0
           '(q1)))

(finite-automaton-states *dfa-0*)
(finite-automaton-edges *dfa-0*)
(finite-automaton-delta *dfa-0*)
(finite-automaton-start *dfa-0*)
(finite-automaton-accept *dfa-0*)

(dfa-simulate1 *dfa-0* '(1))


(defun edelta (state list dfa)
  (if
   (null list) ; base case at empty string, list is the empty list
   state
   (edelta (first (funcall (finite-automaton-delta dfa) state (car list))) ; recursive case
	   (cdr list) dfa
	   ))
  )

(edelta (finite-automaton-start *dfa-0*) '(0 1) *dfa-0*)

(funcall (finite-automaton-delta *dfa-0*) 'q1 '0)

;;(fa-pdf *dfa-0* "./dfa-0.pdf")


(not (finite-automaton-accept *dfa-0*))
(not '())

;; Tests for dfa-simulate
(dfa-simulate *dfa-0* '(0 1))
;; => t
(dfa-simulate *dfa-0* '(0 0 0 0))
;; => nil
(dfa-simulate *dfa-0* '(0 1 0))
;; => nil
(dfa-simulate *dfa-0* '(1 1 1 1 0))
;; => nil
(dfa-simulate *dfa-0* '(0 0 0 0 0 0 1 1 1 1))
;; => t



;; Tests for e-closure

;; first test that the set-member function works as expected


(defun set-member (set item)
  (cond
    ((null set) nil)
    ((eq item (car set)) t)
    (t (set-member (cdr set) item))))


(set-member '(0 1 2 3) '4)


(defun set-member (set item)
  (cond
    ((eq item (car set)) t)
    ((not (eq nil (cdr set))) (set-member (cdr set) item))
    (t nil)))

(labels (
	 (testfunc (x) (1+ x))
	 (anotherfunc (y) (1+ y)))
  (testfunc (anotherfunc 55)))




(funcall (finite-automaton-delta *nfa-0*) 'q0 :epsilon))

(funcall (finite-automaton-delta *dfa-0*) 'q0 '0)

(fa-pdf *nfa-0* "outfile.pdf")

(fa-dot *nfa-0* "testout")

(coerce '(1 9) 'list)


(nfa-simulate *nfa-0* '(0 0 0 1))

(nfa-simulate *nfa-0* '(0 0 0 1))
(nfa-simulate *nfa-0* '())
(nfa-simulate *nfa-0* '(0 0 0 1))




;; Make an NFA
(defparameter *nfa-1*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q1
           '(q2)))

;; install the 'graphviz' package on Ubuntu to use the fa-pdf function
(fa-pdf *nfa-1* "nfa1.pdf")


(nfa-simulate *nfa-1* '(1))
;; => t
(nfa-simulate *nfa-1* '(0))
;; => nil
(nfa-simulate *nfa-1* '(0 0 0 1))
;; => t


(fa-pdf (nfa->dfa *nfa-1*) "nfa_to_dfa.pdf")

(setf test-hash (make-symbol-hash-table))

(hash-table-keys test-hash)

(hash-table-keys visited-hash)

(setf (gethash 'q3 visited-hash) t)

(setf (gethash 'q0 test-hash) t)
(setf (gethash 'q10 test-hash) t)

(sort (hash-table-keys test-hash) #'equal)

(gethash 'q110 test-hash)
(gethash 'q0 test-hash)

(test-hash)


(reduce (lambda (r x) (cons x r)) '(1 2 3) :initial-value '())


(defparameter *hash-table1* (make-hash-table))

(setf (gethash 'key1 *hash-table1*) t)

(gethash 'key1 *hash-table1*)
;; returns t, t

;(gethash 'key2 *hash-table1*)
;; returns nil, nil


;(nfa->dfa *nfa-1*)

					;(defparameter visited-hash (make-hash-table))

(defun testfun (a)
(+ 1 a)
)

(defun visit-subset (edges subset)
(if t
    t
    nil
    )
)

(visit-subset '(q0) '(q2))

;(if (gethash subset visited-hash) ;; returns may return t,t or nil,nil (if not present)
 ;   (list edges) ;; return Q'
  ; ;; update the visited-hash with the new subset
   ; (let
;	((setf (gethash subset visited-hash)))
 ;     (fold-left (lambda (edges input-symbol) (visit-symbol edges subset input-symbol)) edges alpnhabet))
   ; )
					;)

(defparameter alphabet '(0 1))


(defparameter nfa *nfa-1*)

;;;;;;
;;;;;;
(defun visit-symbol (edges subset-0 input-symbol)
(let*
    ((u-prime (move-e-closure nfa subset-0 input-symbol)))
  (if u-prime
      (visit-subset edges subset-0)
      (list 'q-prime `e-prime)
      )
  )
)


(defun visit-subset (edges subset)
	      ;; use the hashtable to track the set of visited subsets
	      ;; if a subset is visited, add it as a key and set the value to 't'
	      (if (gethash subset visited-hash) ;; returns may return t,t or nil,nil (if not present)
		  (list edges) ;; return Q'
		  ;; update the visited-hash with the new subset
		  
		  (let
		      ((visited-hash (hash-table-keys visited-hash)))
		    (fold-left (lambda (edges input-symbol) (visit-symbol edges subset input-symbol)) edges alphabet))
		  )
	      )

(defparameter visited-hash (make-hash-table))

(setf (gethash 'q0 visited-hash) t)

(visit-subset '(q1 0 q2) 'q0)

