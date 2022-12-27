;; Make an DFA
(defparameter *dfa-0*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q2)
             (q1 0 q0)
	     (q2 0 q1)
	     )
           'q0
           '(q1 q2)))



(fa-pdf (reverse-fa *dfa-0*) "dfa-0-rev.pdf")
(fa-pdf *dfa-0* "dfa-0.pdf")
