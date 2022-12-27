;; Make an NFA

(defparameter *nfa-0*
  (make-fa '((q0 0 q0)
             (q0 :epsilon q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q0
           '(q2)))

(fa-pdf *nfa-0* "nfa-0.pdf")

(defparameter *nfa-1*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q0
           '(q2)))


(e-closure *nfa-0* '(q0) nil)


(fa-pdf *nfa-1* "nfa-1.pdf")

(e-closure *nfa-1* '(q0) nil)
(e-closure *nfa-1* '(q1) nil)
(e-closure *nfa-1* '(q2) nil)

;; test if the e-closure of q1 is (q2 q1) 
(assert (equal '(q0) (e-closure *nfa-1* '(q0) nil)))
(assert (equal '(q2 q1) (e-closure *nfa-1* '(q1) nil)))
(assert (equal '(q2) (e-closure *nfa-1* '(q2) nil)))
