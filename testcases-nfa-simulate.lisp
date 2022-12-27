;; Make an NFA
(defparameter *nfa-1*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q0
           '(q2)))

(fa-pdf *nfa-1* "nfa-1.pdf")

(nfa-simulate *nfa-1* '(0 1 2 1))

(assert (eq t (nfa-simulate *nfa-1* '(1))))
(assert (eq t (nfa-simulate *nfa-1* '(1 1))))
(assert (eq t (nfa-simulate *nfa-1* '(1 2))))

(assert (eq nil (nfa-simulate *nfa-1* '())))
(assert (eq nil (nfa-simulate *nfa-1* '(0))))
(assert (eq nil (nfa-simulate *nfa-1* '(0 0))))
(assert (eq nil (nfa-simulate *nfa-1* '(1 0))))






