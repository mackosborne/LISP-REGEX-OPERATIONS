;; optimize for debuggin
(declaim (optimize (debug 3)))


(defparameter *nfa-0*
  (make-fa '((q0 0 q0)
             (q0 :epsilon q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q0
           '(q2)))


;; this matches the example on L05 slide 24
(fa-pdf *nfa-0* "nfa-0.pdf")

(fa-pdf (nfa->dfa *nfa-0*) "dfa-0.pdf") 


;; example 1 on L05 slide 25
(defparameter *nfa-1*
  (make-fa '((q0 b q1)
             (q0 :epsilon q2)
             (q1 a q1)
             (q1 a q2)
	     (q1 b q2)
             (q2 a q0))
           'q0
           '(q0)))

(fa-pdf *nfa-1* "nfa-1.pdf")

(fa-pdf (nfa->dfa *nfa-1*) "dfa-1.pdf") 

;; exercise 3 on L05 slide 28
(defparameter *nfa-3*
  (make-fa '((q0 :epsilon q1)
             (q0 :epsilon q2)
             (q1 0 q2)
	     (q1 0 q4)
             (q2 1 q1)
	     (q2 :epsilon q3)
             (q3 1 q4)
	     (q4 :epsilon q3))
           'q0
           '(q4)))

(fa-pdf *nfa-3* "nfa-3.pdf")

(fa-pdf (nfa->dfa *nfa-3*) "dfa-3.pdf") 



(defparameter *nfa-2*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 :epsilon q2)               
             (q2 2 q2))
           'q0
           '(q2)))


;; Convert the nfa to a dfa
(nfa->dfa *nfa-2*)

;; Generate a PDF of the nfa
(fa-pdf *nfa-2* "nfa-2.pdf")

;; show the converted nfa
(fa-pdf (nfa->dfa *nfa-2*) "converted-nfa-2.pdf")

