(newstate)

;; newstate is a function that constructs a unique state for a FA

;; Make an NFA
(defparameter *nfa-0*
  (make-fa '((q0 0 q0)
             (q0 :epsilon q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q0
           '(q2 q1)))

;; Make an NFA
(defparameter *nfa-1*
  (make-fa '((q3 0 q3)
             (q3 :epsilon q4)
             (q4 1 q4)
             (q4 :epsilon q5)
             (q5 2 q5))
           'q3
           '(q5 q4)))

;; Make an NFA
(defparameter *nfa-2*
  (make-fa '((q6 0 q7))
           'q6
           '(q7)))


(fa-pdf (fa-concatenate *nfa-0* *nfa-1*) "concat_nfa.pdf")

(fa-pdf *nfa-0* "nfa0.pdf")

(fa-pdf *nfa-1* "nfa1.pdf")

(fa-pdf (fa-union *nfa-0* *nfa-1*) "union_nfa.pdf")

(fa-pdf (fa-repeat *nfa-2*) "kleene_nfa.pdf")

(fa-pdf *nfa-2* "nfa2.pdf")
