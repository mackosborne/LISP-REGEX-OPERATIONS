;; Make an NFA

(defparameter *nfa-0*
  (make-fa '((q0 :epsilon q1)
	     (q0 :epsilon q2)
	     (q1 0 q4)
	     (q1 0 q2)
	     (q2 1 q1)
	     (q2 :epsilon q3)
	     (q3 1 q4)
	     (q4 :epsilon q3))
	   'q0
           '(q4)))


(defparameter *nfa-1*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q0
           '(q2)))

(fa-pdf *nfa-0* "nfa-0.pdf")

(fa-pdf *nfa-1* "nfa-1.pdf")

(move-e-closure *nfa-0* '(q0) 0)
(move-e-closure *nfa-0* '(q0) 1)

(move-e-closure *nfa-1* '(q0) 0)
(move-e-closure *nfa-1* '(q0) 1)
(move-e-closure *nfa-1* '(q1) 1)
(move-e-closure *nfa-1* '(q2) 2)
(move-e-closure *nfa-1* '(q1) :epsilon)

;; test if the move-e-closure of q0 1 is (q2 q1) 
(assert (equal '(q0) (move-e-closure *nfa-1* '(q0) 0)))
(assert (equal '(q2 q1) (move-e-closure *nfa-1* '(q0) 1)))
(assert (equal '(q2 q1) (move-e-closure *nfa-1* '(q1) 1)))
(assert (equal '(q2) (move-e-closure *nfa-1* '(q1) :epsilon)))
(assert (equal '(q2) (move-e-closure *nfa-1* '(q2) 2)))
