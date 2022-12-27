;;;;;;;;;;;;;;;;;;;
;;;; Utilities ;;;;
;;;;;;;;;;;;;;;;;;;

;; optimize for debugging
;;(declaim (optimize (debug 3)))

(defun hash-table-keys (hash-table)
  "Return the hash table's keys as a list."
  (let ((result nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key result))
             hash-table)
    result))

(defun make-symbol-hash-table ()
  "Convenience function to make a hash table for FA symbols."
  (make-hash-table :test #'equal))

(defun fold-left (function initial-value list)
  "Convenience function for fold-left with an initial value."
  (reduce function list :initial-value initial-value))

(defun TODO (thing)
  "Placeholder for code to implement."
  (error "Unimplemented: ~A" thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STARTER DEFINITIONS FOR FINITE AUTOMATA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A structure type for finite automata
(defstruct finite-automaton
  "A Finite Automaton."
  states    ; state set as a list
  alphabet  ; input alphabet as a list
  edges     ; list of edges: (state-0 input-symbol state-1)
  delta     ; transition function : state-0 * input-symbol -> state-1
  start     ; start state
  accept)   ; accept set as a list

(defun make-fa (edges start accept)
  "Convenience constructor for finite automata"
  (flet ((add-state (hash state)
           (setf (gethash state hash) t)
           hash)
         (add-edge-states (hash edge)
           (destructuring-bind (state-0 input-symbol state-1) edge
             (declare (ignore input-symbol))
             (setf (gethash state-0 hash) t
                   (gethash state-1 hash) t))
           hash)
         (add-edge-input-symbol (hash edge)
           (destructuring-bind (state-0 input-symbol state-1) edge
             (declare (ignore state-0 state-1))
             (setf (gethash input-symbol hash) t))
           hash)
         (add-edge-transition (hash edge)
           (destructuring-bind (state-0 input-symbol state-1) edge
             (push state-1 (gethash (cons state-0 input-symbol) hash)))
           hash))
    (let ((state-hash (fold-left #'add-edge-states
                                 (fold-left #'add-state
                                            (add-state (make-symbol-hash-table)
                                                       start)
                                            accept)
                                 edges))
          (alphabet-hash (fold-left #'add-edge-input-symbol
                                    (make-symbol-hash-table)
                                    edges))
          (edge-hash (fold-left #'add-edge-transition
                                (make-symbol-hash-table)
                                edges)))
      (make-finite-automaton
       :states (hash-table-keys state-hash)
       :alphabet (hash-table-keys alphabet-hash)
       :edges edges
       :delta (lambda (state-0 input-symbol)
                (gethash (cons state-0 input-symbol) edge-hash))
       :start start
       :accept accept))))


;;; Higher-order conveience functions for Finite Automata ;;;

(defun map-fa-states (function fa)
  "Map FUNCTION over the FA's states."
  (map 'list function (finite-automaton-states fa)))

(defun map-fa-accept (function fa)
  "Map FUNCTION over the FA's accept states."
  (map 'list function (finite-automaton-accept fa)))

(defun fold-fa-states (function initial-value fa)
  "Fold FUNCTION over the FA's states."
  (fold-left function initial-value (finite-automaton-states fa)))

(defun fold-fa-alphabet (function initial-value fa)
  "Fold FUNCTION over the FA's alphabet."
  (fold-left function initial-value (finite-automaton-alphabet fa)))

(defun map-fa-edges (function fa)
  "Map FUNCTION over the FA's edges."
  (map 'list
       (lambda (edge)
         (destructuring-bind (state-0 input-symbol state-1) edge
           (funcall function state-0 input-symbol state-1)))
       (finite-automaton-edges fa)))




;;; Graphviz Output ;;;

(defun fa-dot (fa place)
  "Output a Graphviz dot file of FA."
  (let ((hash (make-symbol-hash-table)))
    ;; number the states
    (fold-fa-states (lambda (i state)
                      (setf (gethash state hash) i)
                      (1+ i))
                    0 fa)
    ;; Output the Graphviz dot file
    (labels ((state-number (state)
               (gethash state hash))
             (dot-symbol (thing) ; Pretty-print Greek letters
               (case thing
                 (:alpha "&alpha;")
                 (:beta "&beta;")
                 (:gamma "&gamma;")
                 (:delta "&delta;")
                 (:epsilon "&epsilon;")
                 (:zeta "&zeta;")
                 (:eta "&eta;")
                 (:theta "&theta;")
                 (:iota "&iota;")
                 (:kappa "&kappa;")
                 (:lambda "&lambda;")
                 (:mu "&mu;")
                 (:nu "&nu;")
                 (:xi "&xi;")
                 (:omicron "&omicron;")
                 (:pi "&pi;")
                 (:rho "&rho;")
                 (:sigma "&sigma;")
                 (:tau "&tau;")
                 (:upsilon "&upsilon;")
                 (:phi "&phi;")
                 (:chi "&chi;")
                 (:omega "&omega;")
                 (t thing)))
             (helper (stream)
               ;; output
               (format stream "~&digraph { ~%")
               ;; state labels
               (format stream "~:{~&  ~A[label=\"~A\"];~}"
                       (map-fa-states (lambda (state)
                                        (list (state-number state)
                                              state))
                                      fa))
               ;; start state
               (format stream "~&  start[shape=none];")
               (format stream "~&  start -> ~A;"
                       (state-number (finite-automaton-start fa)))
               ;; accept state
               (format stream "~:{~&  ~A [ shape=~A ];~}"
                       (map-fa-accept (lambda (q)
                                        (list (state-number q) "doublecircle"))
                                      fa))
               ;; edges
               (format stream "~:{~&  ~A -> ~A [fontsize=~D,label=\"~A\"];~%~}"
                       (map-fa-edges (lambda (state-0 input-symbol state-1)
                                       (list (state-number state-0)
                                             (state-number state-1)
                                             12 (dot-symbol input-symbol)))
                                     fa))
               ;; end
               (format stream "~&}~%")))
      (cond
        ((streamp place)
         (helper place))
        ((eq place t)
         (helper *standard-output*))
        ((null place)
         (with-output-to-string (s)
           (helper s)))
        ((or (stringp place)
             (pathnamep place))
         (with-open-file (stream place
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
         (helper stream)))
        (t (error "Unrecognized output type: ~A" place))))))



#+sbcl
(defun fa-pdf (fa filename)
  "Create a PDF of FA."
  (with-input-from-string (text (fa-dot fa nil))
    (sb-ext:run-program "dot" (list "-Tpdf")
                        :search t
                        :input text
                        :output filename
                        :if-output-exists :supersede)))


;;; Finite Automata Helper Functions ;;;

(defun fa-transition (fa state-0 input-symbol)
  "Return the list of successors of STATE-0 on INPUT-SYMBOL."
  (funcall (finite-automaton-delta fa)
           state-0 input-symbol))

(defun dfa-transition (fa state-0 input-symbol)
  "Return the successor of STATE-0 on INPUT-SYMBOL."
  (assert (not (eq input-symbol :epsilon)))
  (let ((successors (fa-transition fa state-0 input-symbol)))
    ;; DFA cannot have multiple successors, or it would be
    ;; nondeterministic
    (assert (or (null successors)
                (null (cdr successors))))
    (car successors)))

(defun newstate (&optional (arg "Q-"))
  "Construct a unique state for a finite automaton."
  (gensym arg))

(defun dfa-p (fa)
  "Is FA is deterministic?"
  (labels ((rec (hash edges)
             ;; Recurse over the edges and check for nondeterministic
             ;; transitions.
             ;; hash : (CONS state-0  input-symbol) -> (or T NIL)
             (if edges
                 (destructuring-bind ((state-0 input-symbol state-1) &rest edges) edges
                   (declare (ignore state-1))
                   (let ((key (list state-0 input-symbol)))
                     (unless (or (eq input-symbol :epsilon)
                                 (gethash key hash))
                       (setf (gethash key hash) t)
                       (rec hash edges))))
                 t)))
    (and (finite-automaton-p fa)
         (rec (make-symbol-hash-table)
              (finite-automaton-edges fa)))))

(defun dfa-add-reject (dfa &optional (alphabet (finite-automaton-alphabet dfa)))
  "Add a reject state to DFA."
  (assert (dfa-p dfa))
  ;; Index non-dead-state edges
  ;; hash : (CONS state-0 input-symbol) -> (OR T NIL)
  (let ((dead-state (newstate "dead")))
    ;; Create edges to a dead-state
    (let ((edges (fold-left (lambda (edges input-symbol) ; fold over alphabet
                              (fold-left (lambda (edges state) ; fold over states
                                           (if (fa-transition dfa state input-symbol)
                                               edges
                                               (cons (list state input-symbol dead-state)
                                                     edges)))
                                         ;; dead-state self transition
                                         (cons (list dead-state input-symbol dead-state)
                                               edges)
                                         (finite-automaton-states dfa)))
                            (finite-automaton-edges dfa)
                            alphabet)))
      ;; Result
      (make-fa  edges
                (finite-automaton-start dfa)
                (finite-automaton-accept dfa)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMPLETE THE FUNCTIONS BELOW ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 0: DFA Simulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Finite Automata Lecture: Algorithm 1
(defun dfa-simulate (dfa sequence)
  "True if DFA accepts SEQUENCE."
  (assert (dfa-p dfa))
  (labels ((move-state (state list)
	     (if
	      (null list) 
	      state ; base case at empty string, list is the empty list, return the current state
	      (move-state ; recursive case
	       (dfa-transition dfa state (first list))
	       (rest list) 
	       )
	      )
	     ))
    (let ((final-state (move-state (finite-automaton-start dfa)
			       (coerce sequence 'list)))) ; Coerce to list for simplicity
      (if (find final-state (finite-automaton-accept dfa) :test #'equal)
	  t
	  nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1: NFA Subset Construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Subset Construction Lecture: Algorithm 1
(defun e-closure (nfa unvisited-states initial-closure)
  (labels (
	   (set-member (set item)
	     (cond
	       ;; changed to 'equal' to check equality for an item in a set
	       ((equal item (car set)) t)
	       ((not (eq nil (cdr set))) (set-member (cdr set) item))
	       (t nil)
	       )
	     )
	   (visit (list-of-visited-states current-state)
	     (if (set-member list-of-visited-states current-state)
		 list-of-visited-states ;; if the current-state is in the list-of-visited-states return the list-of-visited-states
		 (e-closure
		  nfa
		  (fa-transition nfa current-state :epsilon)
		  (cons current-state list-of-visited-states)) 
		 )
	     )
	   )
    (fold-left
     #'visit
     initial-closure
     unvisited-states)
    )
  )

;; Subset Construction Lecture: Algorithm 2
;; implemented from algorithm on slides, L05 slide 17 (postlecture slides)
(defun move-e-closure (nfa initial-states token)
  (labels (
	   (visit (closure-post-move state-pre-move)
	     (e-closure
	      nfa
	      (fa-transition nfa state-pre-move token)
	      closure-post-move)
	     )
	   )
    (fold-left
     #'visit
     nil
     (e-closure nfa initial-states nil)
     ))
  )


;; Subset Construction Lecture: Algorithm 4
;; input is an NFA and a sequence of the form '(0 1)
;; output is either accept (t) or reject (nil)
(defun nfa-simulate (nfa sequence)
  "True if NFA accepts SEQUENCE."
  (labels ((move-state (subset list)
	     (if
	      (null list) 
	      subset ;; base case at empty string, empty list
	      (let ((u-prime (move-e-closure nfa subset (first list))))
		(move-state u-prime (rest list)))
	      )
	     )
	   )
    (let* ((q0 (finite-automaton-start nfa))
           (accept-states (finite-automaton-accept nfa))
           (u (e-closure nfa (list q0) nil))
           (list (coerce sequence 'list))
           (final-subset (move-state u list)))
      ;; check if the intersection of the final subset and f is not null
      (if (intersection final-subset accept-states
                        :test #'equal)
          t
          nil))
    )
  )


(defun state-predicate-atom (a b)
  "Predicate function to compare atomic states."
  (etypecase a
    ((or symbol string)
     (etypecase b
       ((or symbol string)
        (string-lessp a b))
       (number nil)))
    (number
     (etypecase b
       ((or symbol string)
        t)
       (number (<= a b))))))

(defun state-predicate (a b)
  "Predicate function to compare FA states."
  (etypecase a
    (atom (etypecase b
            (atom (state-predicate-atom a b))
            (list t)))
    (cons (etypecase b
            (atom nil)
            (cons (if (equal (car a) (car b))
                      (state-predicate (cdr a)
                                       (cdr b))
                      (state-predicate (car a)
                                       (car b))))))))

;; Subset Construction Lecture: Algorithm 5
(defun nfa->dfa (nfa)
  "Convert a nondeterministic finite automaton to a deterministic finite automaton."
  (let (
	(visited-hash (make-symbol-hash-table))
        (alphabet (remove :epsilon (finite-automaton-alphabet nfa))
		  )
	)
    (labels (
	     (sort-subset (u)
               ;; sort subsets so we can quickly test for previously
               ;; visited subsets in the hash table
               (sort u #'state-predicate))
	     (visit-symbol (edges subset-0 input-symbol)
	       (let
		   ((successor-subset (move-e-closure nfa subset-0 input-symbol)))
		 
		 (if successor-subset
		     (visit-subset
		      (union
		       edges
		       (list (list subset-0 input-symbol successor-subset))
		       )
		      successor-subset)
		     edges
		     )
		 )
	       )	       
	     (visit-subset (edges subset)
	       ;; track the set of visited states with the hashtable
	       ;; check if the subset is in the visited-hash table
	       (if
		(gethash (sort-subset subset) visited-hash)
		;; returns may return t,t or nil,nil (if not present)
		edges ;; return E'
		;; update the visited-hash with the new subset
		(progn
		  
  		  (setf (gethash (sort-subset subset) visited-hash) t)
		  (fold-left
		   (lambda (edges input-symbol)
		     (visit-symbol edges (sort-subset subset) input-symbol))
		   edges
		   alphabet))
		)
	       )
	     (combine-accept-state (accept-states state)
	       (if (intersection state (finite-automaton-accept nfa) :test #'equal)
		   (cons state accept-states)
		   accept-states
		   )
	       )
	     )
      (let* (
	     (new-start-state (e-closure nfa (list (finite-automaton-start nfa)) nil))
	     (edges (visit-subset nil new-start-state))
	     (accept-state (fold-left #'combine-accept-state nil (hash-table-keys visited-hash)))
	     )
	;; create a new fa using the make-fa function
	(make-fa
	 edges
	 new-start-state
	 accept-state)
	))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2: Regular Expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We represent regular expressions as S-expressions, using the
;; following operators:
;;
;; - :union
;; - :concatenation
;; - :kleene-closure
;;
;; The union and concatenation operators are n-ary.  Kleene-closure
;; takes a single argument.
;;
;; We can represent any regular language with a regular expression
;; using only the operators :union, :concatenation, :kleene-closure.
;; However, it is often convenient (and common practice) to define
;; additional operators. The SIMPLIFY-REGEX function will take as
;; input a regular expression with such additional operators and
;; return a simplified expression that contains only the operators
;; :union, :concatenation, ang :kleene-closure.  Specifically, it will
;; simplify the following operators:
;;
;; - :.     -> (:union alphabet...)
;; - (:? X) -> (:union X :epsilon)
;; - (:+ X) -> (:concatenation X (:kleene-closure X))
(defun simplify-regex (regex &optional alphabet)
  "Convert :., :?, :+ to only :union, :concatenation, :kleene-closure"
  (labels ((h (regex)
             (cond
	       ((eq regex :.)
		(assert alphabet)
		`(:union ,@alphabet))
	       ((atom regex)
                regex)
               (t
		(destructuring-bind (operator &rest args) regex
		  (cond
		    ((eq operator :?)
		     (if (atom (first args))
			 `(:union :epsilon ,(h (first args)) ,@(h (rest args)))
		    	 `(:union :epsilon (,@(h args)))))
		    ((eq operator :+)
		     `(:concatenation (,@(h args)) (:kleene-closure (,@(h args)))))
		    ((eq operator :.)
		     (assert alphabet)
		     (if (not args)
			 `(:union ,@alphabet)
			 `(:union ,@alphabet (,@(h args)))))
		    ((listp operator)
		     `(,@(h operator) ,@(h args)))
		    ((atom operator)
		     (if (not args)
			 `(,operator)
			 `(,operator ,@(h args))))
		    (t (format nil "Unsupported operator ~A" operator))
		    ))
		))))
    (h regex)))

;;; The functions FA-CONCATENATE, FA-UNION, and FA-REPEAT apply the
;;; corresponding regular operation (union, concatenation, and
;;; kleene-closure, respectively) to finite automata.  We will next
;;; use these functions as subroutines to convert a regular expression
;;; to an NFA.

;; Regular Expression Lecture: Concatenation.
;; Provided in complete form as an example
(defun fa-concatenate (nfa-1 nfa-2)
  "Find the concatenation of NFA-1 and NFA-2."
  (assert (not (intersection (finite-automaton-states nfa-1)
                             (finite-automaton-states nfa-2))))
  (let ((start (newstate))
        (accept (newstate)))
    (make-fa (append (list (list start :epsilon (finite-automaton-start nfa-1)))
                     (map 'list (lambda (x)
                                  (list x :epsilon (finite-automaton-start nfa-2)))
                          (finite-automaton-accept nfa-1))
                     (map 'list (lambda (x)
                                  (list x :epsilon accept))
                          (finite-automaton-accept nfa-2))
                     (finite-automaton-edges nfa-1)
                     (finite-automaton-edges nfa-2))
             start
             (list accept))))


;; Regular Expression Lecture: Union
(defun fa-union (nfa-1 nfa-2)
  "Find the union of NFA-1 and NFA-2."
  (assert (not (intersection (finite-automaton-states nfa-1)
                             (finite-automaton-states nfa-2))))
  (let ((start (newstate))
	(accept (newstate)))
    ;; add transitions to both start states of the input nfas from the new start state
    (make-fa (append (list (list start :epsilon (finite-automaton-start nfa-1)))
		     (list (list start :epsilon (finite-automaton-start nfa-2)))
		     (map 'list (lambda (x)
                                  (list x :epsilon accept))
                          (finite-automaton-accept nfa-1))
                     (map 'list (lambda (x)
                                  (list x :epsilon accept))
                          (finite-automaton-accept nfa-2))
                     (finite-automaton-edges nfa-1)
                     (finite-automaton-edges nfa-2))
	     start
	     (list accept))))

;; Regular Expression Lecture: Kleene-Closure
(defun fa-repeat (nfa)
  "Find the repetition / Kleene-closure of NFA."
  (let ((start (newstate))
	(accept (newstate)))
    ;; add an epsilon transition from the initial accept state to the start state
    (make-fa (append (list (list start :epsilon accept))
		     (list (list start :epsilon (finite-automaton-start nfa)))
		     ;; add edges from the accept states of the nfa to the start state
		     (map 'list (lambda (x)
                                  (list x :epsilon (finite-automaton-start nfa)))
                          (finite-automaton-accept nfa))
		     (map 'list (lambda (x)
                                  (list x :epsilon accept))
                          (finite-automaton-accept nfa))
		     (finite-automaton-edges nfa))
	     start
	     (list accept))))
  

;; McNaughton-Yamada-Thompson Algorithm Lecture: Algorithm 1
;;
;; Convert a regular expression to a nondeterministic finite
;; automaton.
;;
;; The following are examples of possible regular expressions.
;;
;; - (:concatenation a b c)
;; - (:union a b c :epsilon)
;; - (:union)
;; - (:kleene-closure a)
;; - (:concatenation (:union a b) (:kleene-closure c))
;; use the fa-concatenate, fa-union and fa-repeat functions
(defun regex->nfa (regex)
  "Convert a regular expression to an NFA."
  (cond
    ((null regex) ; Base case for empty set
     (make-fa nil (newstate) (list (newstate))))
    ((atom regex)
     ;; regex is an atom (0, 1, a, b,...), create a new one with just one transition
     (let
	 ((start-state (newstate))
	  (accept-state (newstate)))
       (make-fa
	(list start-state regex accept-state)
	start-state
	(list accept-state)))
     )
    ;; check if it is a union, concat, or kleene-closure
    ((eq (first regex) :union)
     ;; create new fa's for the regexes and then union, concat, or repeat them
     (reduce #'fa-union (map 'list #'regex->nfa (rest regex)) :initial-value (regex->nfa nil)))
    ((eq (first regex) :concatenation)
     (reduce #'fa-concatenate (map 'list #'regex->nfa (rest regex)) :initial-value (regex->nfa :epsilon)))
    ((eq (first regex) :kleene-closure)
     (fa-repeat (regex->nfa (first (rest regex)))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 3: Regular Decision and Closure Properties ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lecture: Decision Properties of Regular Languages, Emptiness
;; make an empty list our visited list - check if the state is in that list, if yes return
;; if no, add it to the visited states list
;; start at start state
;; check if this state accepts, if yes return NIL (not empty fa)
;; for all symbols in the alphabet (MAP FUNCTION)
;; if there is a state returned by the transition function, CALL VISIT <- RECURSIVE CASE
;; if we don't find an accept state when starting at the start state, the fa is empty
;; check all paths to see if there are any paths from the start state to an accept state
(defun fa-empty (fa)
  "Does FA represent the empty set?"
  ;; minimize the dfa, for this implementation, we just search from the start state
  ;; don't need to minimize the dfa
  ;; track the set of visited states with the hashtable
  (let ((visited-hash (make-symbol-hash-table))
	(accept-states (make-symbol-hash-table))
	)
    (labels (
	     (sort-subset (u)
               ;; sort subsets so we can quickly test for previously
               ;; visited subsets in the hash table
               (sort u #'state-predicate))
	     (find-accept-state (state)
	       ;(break)
	       ;; state should be a single state, but in a list
	       (if (first state)
		   (if (gethash (first state) accept-states)
		       t ;; we are at a state in the accepted states list, return true
		       ;; check if the subset is in the visited-hash table
		       (if
			(not (gethash (first state) visited-hash))
			(progn
			  (setf (gethash (first state) visited-hash) t)
			  ;; try to visit other states, use the fa's alphabet
			  ;; for every state to transition to, visit it   
			  (fold-fa-alphabet
			   (lambda (seen-accept input-symbol) ;; fold over alphabet
			     (or seen-accept (find-accept-state (fa-transition fa (first state) input-symbol))))
			   nil
			   fa)
			  )
			nil
			)
		       )
		   nil
		   )
	       )
	     )
      ;; add the accept states to the accept-states hash table
      (map 'list (lambda (accept-state) (setf (gethash accept-state accept-states) t)) (finite-automaton-accept fa))
      ;; if we don't find an accept state when starting at the start state, the fa is empty
      (not (find-accept-state (list (finite-automaton-start fa)))))))


(defun fa-reverse (fa)
  ;; make a new start state that maps to the old accept states
  ;; flip all of the edges
  ;; old start state becomes new accept state
  ;; include a check to see if there is more than 1 accept state
  "Generate the reverse of an FA."
  ;; create the new start state
  (let* (
	 (start 
	   (if
	    (not (cdr (finite-automaton-accept fa)))
	    ;; if there is only one accept state
	    (car (finite-automaton-accept fa))
	    (newstate))
	   )
	 ;; make a new fa that is the reverse fa, append all of the new edges together
	 
	 (new-fa
	   (make-fa
	    (append 
	     ;; new start state with :epsilon transitions to the old accept states
	     (map 'list (lambda (x)
			  (list start :epsilon x))
		  (finite-automaton-accept fa))
	     ;; flip all of the edges
	     (map-fa-edges
	      (lambda (state-0 input-symbol state-1)
		(list state-1 input-symbol state-0))
	      fa
	      )
	     )
	    ;; set the new start state as the start state
	    start
	    ;; old start state becomes new accept state
	    (list (finite-automaton-start fa))))
	 )
    new-fa
    ))

;; Lecture: Closure Properties of Regular Languages, State Minimization
(defun dfa-minimize (dfa)
  "Return an equivalent DFA with minimum state."
  (nfa->dfa
   (fa-reverse
    (nfa->dfa
     (fa-reverse dfa)
     )
    )
   )
  )

;; Lecture: Closure Properties of Regular Languages, Intersection
;; Intersection and difference use the product-dfa so we need that
;; generate the edges for the product dfa
(defun generate-product-edges (dfa-0 dfa-1)
  ;; generate the transitions
  ;; check if there is a transition for both
  (fold-left
   (lambda (non-null-edges edge)
     (if (not edge)
	 non-null-edges
	 (cons edge non-null-edges)
	 ))
   nil
   (fold-left
    (lambda (edges dfa0-edge)
      (append 
       (map
	'list
	(lambda (dfa1-edge)
	  (if
	   (eq
	    (second dfa0-edge)
	    (second dfa1-edge))
	   (list
	    (list (first dfa0-edge) (first dfa1-edge))
	    (second dfa0-edge)
	    (list (third dfa0-edge) (third dfa1-edge)))
	   )
	  )
	(finite-automaton-edges dfa-1))
       edges)
      )
    nil
    (finite-automaton-edges dfa-0)
    )
   )
  )

;; NOTE - this function doesn't set an accept state for the product dfa
;; use the (make-fa) function with (finite-automaton-start (dfa-product dfa-0 dfa-1)) and (finite-automaton-edges (dfa-product dfa-0 dfa-1) to get the start and edges
(defun dfa-product (dfa-0 dfa-1)
  ;; create an updated dfa that contains a cross product of the input dfas
  (let ((start-state (list (finite-automaton-start dfa-0) (finite-automaton-start dfa-1))))
    (make-fa 
     (generate-product-edges dfa-0 dfa-1)
     ;; set the bound start state as the start state
     start-state
     ;; set the accept state to nil
     nil
     )
    ))

  

;; Lecture: Closure Properties of Regular Languages, Intersection
;; We should return a new DFA that is the intersection of the two input DFAs
(defun dfa-intersection (dfa-0 dfa-1)
  (labels (
	   (and-accept-states (state-0 state-1)
	     (and
	      ;; check if the dfa-0 state is an accept state
	      (intersection (list state-0) (finite-automaton-accept dfa-0))
	      (intersection (list state-1) (finite-automaton-accept dfa-1)))
	     )
	   (generate-accept-states (dfa-0 dfa-1)
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
		     (if (and-accept-states state-0 state-1)
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
    (let*
	(
	 ;; add the reject states to the dfa's prior to creating the product dfa
	 (dfa-0-with-reject (dfa-add-reject dfa-0))
	 (dfa-1-with-reject (dfa-add-reject dfa-1))
	 (product-dfa (dfa-product dfa-0-with-reject dfa-1-with-reject))
	 ;; accept states are where both dfas accept
	 (accept-states (generate-accept-states dfa-0-with-reject dfa-1-with-reject))
	 )
      (make-fa
	(finite-automaton-edges product-dfa)
	(finite-automaton-start product-dfa)
	;; set the accept state to when one accepts and the other doesn't
	accept-states)
       )))


;; Lecture: Decision Properties of Regular Languages, Equivalence
(defun dfa-equivalent (dfa-0 dfa-1)
  "Do DFA-1 and DFA-2 recognize the same language?"
  (labels ((generate-accept-states (dfa-0 dfa-1)
	     (labels (
		      (xor-accept-states (state-0 state-1)
			(if
			 (or 
			  (and
			   ;; check if the dfa-0 state is an accept state
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
	       )))
    ;; create a product dfa, add explicit reject states
    ;; set the accept states to when one accepts and the other rejects
    ;; check if that dfa is empty
    (let*
        (
	 ;; add the reject states to the dfa's prior to creating the product dfa
	 (dfa-0-with-reject (dfa-add-reject dfa-0))
	 (dfa-1-with-reject (dfa-add-reject dfa-1))
	 (product-dfa (dfa-product dfa-0-with-reject dfa-1-with-reject))
	 ;; accept states are where one dfa accepts and the other doesn't
	 (accept-states (generate-accept-states dfa-0-with-reject dfa-1-with-reject))
	 (test-equivalent-dfa
	   (make-fa
	    (finite-automaton-edges product-dfa)
	    (finite-automaton-start product-dfa)
	    ;; set the accept state to when one accepts and the other doesn't
	    accept-states))
	 )
      (fa-empty test-equivalent-dfa)
      )))


(defun rename-fa-states (fa)
  "Rename FA state names to new states"
  ;; rename the states to a single name
  ;; keep the start, edges, and accept states the same - return an equivalent FA
  ;; use a hash table with the states as the key and the new state name,
  ;; generate the new states using (newstate) as a value
  (let ((state-names-hash (make-symbol-hash-table)))
    (progn
      (map-fa-states
       (lambda (old-state-name)
	 (if (not (gethash old-state-name state-names-hash))
	     ;; create new state names for the old state names
	     (setf (gethash old-state-name state-names-hash) (newstate))
	     ))
       fa)
      (make-fa
       ;; map over the edges
       (map-fa-edges
	(lambda (state-0 input-symbol state-1)
	  ;; make a new edge that has the modified state names
	  (list
	   (gethash state-0 state-names-hash)
	   input-symbol
	   (gethash state-1 state-names-hash)
	   )
	  )
	fa)
       ;; start state
       (gethash (finite-automaton-start fa) state-names-hash)
       ;; accept state list
       (map
	'list
	(lambda (old-accept-state)
	  (gethash old-accept-state state-names-hash)
	  )
	(finite-automaton-accept fa)
	)
       )
      )))
