(defparameter *nfa-1*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q1
           '(q2)))

(fa-pdf *nfa-1* "nfa1.pdf"
	)


(defparameter alphabet '(0 1))
n
(defparameter nfa *nfa-1*)



(defparameter visited-hash (make-hash-table))

(hash-table-keys visited-hash)

(visit-symbol '(q0 1 q1) '(q0) '1)

(move-e-closure *nfa-1* '(q1) '1)

;start state
;transitional
					;end state

(defparameter list1 '(q0))

(print 'list1)

(finite-automaton-edges nfa)

(move-e-closure nfa '(q0) '1)


(defparameter input-symbol 1)
(union edges `(,subset-0 ,input-symbol `,(move-e-closure nfa '(q1) input-symbol)))
`,(finite-automaton-accept nfa)

(defparameter a 2)

(visit-symbol () '(q0) '1)

(finite-automaton-start nfa)

(visit-subset () (list (finite-automaton-start nfa)))
(defparameter visited-hash (make-hash-table))

(defun visit-symbol (edges subset-0 input-symbol)

  (let
      ((u-prime (move-e-closure nfa subset-0 input-symbol)))
    (if u-prime
	(visit-subset (union
		       edges
		       (list `(,subset-0 ,input-symbol ,u-prime)))
		      u-prime)
	edges
	)
    )
  )

(member '(q2) '(q2 q1) :test #'state-predicate)


(setq state '(q2 q1))
(setq accept '(q2))

(intersection state accept)

(finite-automaton-accept nfa)

(defun combine-accept-state (accum state)
  (if (intersection state (finite-automaton-accept nfa))
      (cons state accum)
      (list accum)
      ))
(fold-left #'combine-accept-state nil (hash-table-keys visited-hash))


(fold-left #'combine-accept-state nil '(1 2 3 4)))

(defun combine-accept-state (accum state)
(cons state accum)
)

(fold-left #'combine-accept-state nil check-states)



(finite-automaton-accept nfa)

(check-states)

(hash-table-keys visited-hash)

(e-closure nfa (list (finite-automaton-start nfa)) '())


(nfa->dfa *nfa-1*)

(finite-automaton-accept *nfa-1*)


(defparameter check-states '((Q2) (Q2 Q1)))

(fa-pdf (nfa->dfa *nfa-1*) "dfa1.pdf")

(untrace visit-symbol)
(untrace visit-subset)

(fold-left
 (lambda (check-state old-accept)
   (if (intersection old-accept check-state :test #'predicate-state)
       (cons check-state ))))






(intersection '(Q1) '((Q0 Q1) (Q1)) :test #'state-predicate)

(hash-table-keys visited-hash)

(defun visit-subset (edges subset)
  ;; use the hashtable to track the set of visited subsets
  ;; if a subset is visited, add it as a key and set the value to 't'
  (if (intersection subset (hash-table-keys visited-hash) :test #'state-predicate) ;; returns may return t,t or nil,nil (if not present)
      edges ;; return E'
      ;; update the visited-hash with the new subset
      (progn
	(setf (gethash subset visited-hash) t)
	(fold-left
	 (lambda (edges input-symbol) (visit-symbol edges subset input-symbol))
	 edges alphabet))
      )
  )

(set-difference (finite-automaton-alphabet nfa) '(:epsilon))

(defparameter alphabet (remove :epsilon (finite-automaton-alphabet nfa)))

(defparameter edges (visit-subset nil '(q0)))

(visit-subset () '(q0))

 

(visit-subset '(q1 0 q2) '(q0))

(visit-subset nil (list (finite-automaton-start *nfa-1*)))


(finite-automaton-states *nfa-1*)
(finite-automaton-edges *nfa-1*)
(finite-automaton-delta *nfa-1*)
(finite-automaton-start *nfa-1*)
(finite-automaton-accept *nfa-1*)
