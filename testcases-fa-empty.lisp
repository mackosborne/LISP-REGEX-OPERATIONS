;; Make an NFA
(defparameter *nfa-0*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
	     (q1 2 q2))
           'q0
           '(q1)))

(defparameter *nfa-1*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q0
           '(q2)))

(defparameter *nfa-empty*
  (make-fa '((q0 0 q0)
             (q1 1 q1)
	     )
           'q0
           '(q1)))

(defparameter *dfa-0*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q2)
             (q1 0 q0)
	     (q2 0 q1)
	     )
           'q0
           '(q1)))


(fa-pdf *nfa-0* "nfa-0.pdf")
(fa-pdf *nfa-1* "nfa-1.pdf")
(fa-pdf *nfa-empty* "nfa-empty.pdf")

(assert (eq nil (fa-empty *nfa-0*)))

(assert (eq nil (fa-empty *nfa-1*)))

(assert (eq t (fa-empty *nfa-empty*)))

(assert (eq nil (fa-empty *dfa-0*)))

(fa-empty *nfa-0*)


(defun fa-empty (fa)
  (let ((visited-hash (make-symbol-hash-table))
	(alphabet (finite-automaton-alphabet fa))
	(accept-states (finite-automaton-accept fa))
	)
    (labels (
	     (find-accept-state (state)
	       (break)
	       (if state
		   (if (intersection state accept-states)
		       t
		       (if (not (gethash state visited-hash))
			   (progn
			     (break)
			     (setf (gethash state visited-hash) t)
			     
			     (fold-left (lambda (seen-accept input-symbol) ;; fold over alphabet
					  (or seen-accept (find-accept-state (fa-transition fa (first state) input-symbol))))
					nil
					alphabet)))))))
    (not (find-accept-state (list (finite-automaton-start fa)))))))




				
			      
