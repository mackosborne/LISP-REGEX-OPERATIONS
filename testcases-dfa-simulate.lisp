;; Make an DFA
(defparameter *dfa-0*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q2)
             (q1 0 q0)
	     (q2 0 q1)
	     )
           'q0
           '(q1)))

(finite-automaton-states *dfa-0*)
(finite-automaton-edges *dfa-0*)
(finite-automaton-delta *dfa-0*)
(finite-automaton-start *dfa-0*)
(finite-automaton-accept *dfa-0*)
(finite-automaton-alphabet *dfa-0*)

(fa-pdf *dfa-0* "dfa-0.pdf")

;; Test cases for simulation of dfa-0

(assert (eq t (dfa-simulate *dfa-0* '(1))))
(assert (eq t (dfa-simulate *dfa-0* '(1 0 1))))
(assert (eq t (dfa-simulate *dfa-0* '(1 1 0))))

(assert (eq nil (dfa-simulate *dfa-0* '())))
(assert (eq nil (dfa-simulate *dfa-0* '(0))))
(assert (eq nil (dfa-simulate *dfa-0* '(0 0))))
(assert (eq nil (dfa-simulate *dfa-0* '(1 1))))
(assert (eq nil (dfa-simulate *dfa-0* '(1 0))))
(assert (eq nil (dfa-simulate *dfa-0* '(1 1 1))))

