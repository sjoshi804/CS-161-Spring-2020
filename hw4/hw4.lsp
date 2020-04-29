;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (backtrack-search delta NIL (list-of-n n))
)

(defun list-of-n (n)
  (cond 
    ((= 0 n) NIL)
    (t (cons n (list-of-n (- n 1))))
  )
)

(defun backtrack-search (delta assignment unassigned)
  (cond 
    ((check-assign delta assignment) ;check if current assignment is already invalid
      (or
        ;base case, if no more vars to be assigned, return assignment list
        (and (not unassigned) assignment) 
        ;assign next var value 0 and recurse
        (backtrack-search delta (cons (negate (first unassigned)) assignment) (rest unassigned))
        ;assign next var value 1 and recurse
        (backtrack-search delta (cons (first unassigned) assignment) (rest unassigned))
      )
    )
    (t NIL) ;if current assignment doesn't work, declare failure on branch and backtrack
  )
)

;check if assignment doesn't lead to contradiction for delta
(defun check-assign (delta assignment)
  (cond 
    ((not delta) t) ;if cnf empty, then any assignment works
    (t ;else check if assignment works for first clause and for rest
      (and 
        (check-assign-clause (first delta) assignment) 
        (check-assign (rest delta) assignment))
    )
  )
)

;check if assignment doesn't lead to contradiction for clause
(defun check-assign-clause (clause assignment)
  (let* 
    (
      (elem (first clause))
      (negated? (< elem 0))
      (elem-true? (and (member (abs elem) assignment) t))
      (elem-false? (and (member (negate (abs elem)) assignment) t))
    )
    (cond
      ;no value assigned to element, then unassigned var exists in clause, can be SAT
      ((and (not elem-true?) (not elem-false?)) t)
      ;if negated, assignment = 0 or if clause not empty recurse
      (negated? (or elem-false? (and (rest clause) (check-assign-clause (rest clause) assignment))))
      ;else, assignment = 1 or recurse if clause not empty
      (t (or elem-true? (and (rest clause) (check-assign-clause (rest clause) assignment))))
    )
  )
)

(defun negate (num)
  (* -1 num)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;            Testing Helper Functions                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Check solution
(defun check-solution (filename assignment)
  (let ((cnf (parse-cnf filename))) 
  (and     
    (= (length assignment) (first cnf))
    (satisfying-assignment (second cnf) assignment)
  ))
)

;Solve and test
(defun solve-and-check (filename)
  (or 
    (check-solution filename (solve-cnf filename))
    (not (solve-cnf filename))
  )
)

(defun satisfying-assignment (delta assignment)
  (cond 
    ((not delta) t) ;if empty then return true
    ;first clause is sat and rest is sat
    (t (and (satisfies-clause (first delta) assignment) (satisfying-assignment (rest delta) assignment)))
  )
)

(defun satisfies-clause (clause assignment)
  (cond 
    ;if clause empty then return true
    ((not clause) t)
    ;else either this term is true or rest has a term that is true
    (t (or (member (first clause) assignment) (satisfies-clause (rest clause) assignment)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat-f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))
