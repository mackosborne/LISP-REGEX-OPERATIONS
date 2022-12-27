;; Make an DFA
(defparameter *dfa-0*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 0 q0)
	     )
           'q0
           '(q1)))


(defparameter *dfa-1*
  (make-fa '((q0 0 q1)
             (q0 1 q1)
             (q1 0 q1)
             (q1 1 q0)
	     )
           'q0
           '(q0)))


(fa-pdf *dfa-0* "dfa-0.pdf")
(fa-pdf *dfa-1* "dfa-1.pdf")


(assert (eq t (dfa-equivalent *dfa-0* *dfa-0*)))

(assert (eq nil (dfa-equivalent *dfa-0* *dfa-1*)))

(fa-pdf (regex->nfa '(:union 0 1)) "union-0-1.pdf")

(fa-pdf (regex->nfa '(:union 1 0)) "union-1-0.pdf")

(fa-pdf (dfa-minimize (nfa->dfa (regex->nfa '(:union 0 1)))) "min-union-0-1.pdf")

;; should be true, because they are equivalent
(dfa-equivalent
 (dfa-minimize (nfa->dfa (regex->nfa '(:union 0 1))))
 (dfa-minimize (nfa->dfa (regex->nfa '(:union 1 0))))
 )



(fa-pdf (regex->nfa '(:union 1 0)) "union-0-1.pdf")
(fa-pdf (dfa-minimize (nfa->dfa (regex->nfa '(:concatenation 1 0)))) "union-0-1.pdf")


(defparameter nfa1 (regex->nfa '(:union 1 0)))

(defparameter nfa2 (regex->nfa '(:concatenation 1 0)))

(fa-pdf nfa1 "nfa1.pdf")

(fa-pdf (dfa-minimize nfa1) "nfa1-min.pdf")

(fa-empty nfa1)

(defparameter min-nfa1 (dfa-minimize nfa1))
(defparameter min-nfa2 (dfa-minimize nfa2))

(fa-pdf min-nfa1 "min-nfa1.pdf")
(fa-pdf min-nfa2 "min-nfa2.pdf")

(dfa-equivalent min-nfa1 min-nfa2)






(append '(1 2 3) '(4 5 6))

(dfa-equivalent
 (dfa-minimize (nfa->dfa (regex->nfa '(:concatenation 0 1))))
 (dfa-minimize (nfa->dfa (regex->nfa '(:concatenation 1 0))))
 )

(defparameter nfa2 (dfa-minimize (nfa->dfa (regex->nfa '(:concatenation 0 1)))))

(defparameter nfa3 (dfa-minimize (nfa->dfa (regex->nfa '(:concatenation 1 0)))))

(dfa-equivalent nfa2 nfa3)


(fa-pdf (dfa-minimize (nfa->dfa (regex->nfa '(:concatenation 1 0)))) "concat-1-0.pdf")


(fa-pdf (dfa-minimize (nfa->dfa (regex->nfa '(:union 0 1)))) "min-union-0-1.pdf")

(nfa->dfa (fa-reverse (nfa->dfa (regex->nfa '(:union 0 1)))))


(intersection '(1) '(0 1))

(intersection '((1)) '(0 (1) 1))


;; generate the accept states where dfa-1 accepts, but dfa2 doesn't
(defun generate-accept-states (dfa-0 dfa-1)
  (labels (
	   (xor-accept-states (state-0 state-1)
	     (if
	      (or 
	       (and
		(intersection `(,state-0) (finite-automaton-accept dfa-0))
		(not (intersection `(,state-1) (finite-automaton-accept dfa-1))))
	       (and
		(not (intersection `(,state-0) (finite-automaton-accept dfa-0)))
		(intersection `(,state-1) (finite-automaton-accept dfa-1)))
	       )
	      t
	      nil
	      )))
    (fold-left
     (lambda (final-accept-states accept-state)
       (if (null accept-state)
	   final-accept-states
	   (cons accept-state final-accept-states))
       )
     nil
     (fold-fa-states
      (lambda (accept-states state-0)
	(append
	 (map-fa-states
	  (lambda (state-1)
	    (if (xor-accept-states state-0 state-1)
		`(,state-0 ,state-1)
		))
	  dfa-1)
	 accept-states
	 )
	)
      nil
      dfa-0)
     )
    ))

(fa-pdf *dfa-0* "dfa-0.pdf")
(fa-pdf *dfa-1* "dfa-1.pdf")
	       

(generate-accept-states *dfa-0* *dfa-1*)
;; make a function that returns the set difference of two sets


(dfa-equivalent *dfa-0* *dfa-1*)

(dfa-equivalent *dfa-0* *dfa-0*)

