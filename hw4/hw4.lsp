;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (backtrack-search delta NIL (list-1-to-n n))
)

(defun list-1-to-n (n)
  (cond 
    ((= n 0) NIL)
    (t (append (list-1-to-n (- n 1)) (list n)))
  ) 
)

(defun backtrack-search (delta assignments unassigned)
  (cond 
    ((check-assign delta assignments) ;check if current assignment is already invalid
      (or
        ;base case, if no more vars to be assigned, return assignment list
        (and (not unassigned) assignments) 
        ;assign next var value 0 and recurse
        (backtrack-search delta (cons (negate (first unassigned)) assignments) (rest unassigned))
        ;assign next var value 1 and recurse
        (backtrack-search delta (cons (first unassigned) assignments) (rest unassigned))
      )
    )
    (t NIL) ;if current assignment doesn't work, declare failure on branch and backtrack
  )
)

;check if assignment doesn't lead to contradiction for delta
(defun check-assign (delta assignments)
  (cond 
    ((not delta) t) ;if cnf empty, then any assignment works
    (t ;else check if assignment works for first clause and for rest
      (and 
        (check-assign-clause (first delta) assignments) 
        (check-assign (rest delta) assignments))
    )
  )
)

;check if assignment doesn't lead to contradiction for clause
(defun check-assign-clause (clause assignments)
  (let* 
    (
      (elem (first clause))
      (negated? (< elem 0))
      (elem_true? (and (member (abs elem) assignments) t))
      (elem_false? (and (member (negate (abs elem)) assignments) t))
    )
    (cond
      ;no value assigned to element, then unassigned var exists in clause, can be SAT
      ((and (not elem_true?) (not elem_false?)) t)
      ;if negated, assignment = 0 or if clause not empty recurse
      (negated? (or elem_false? (and (rest clause) (check-assign-clause (rest clause) assignments))))
      ;else, assignment = 1 or recurse 
      (t (or elem_true? (and (rest clause) (check-assign-clause (rest clause) assignments))))
    )
  )
)

(defun negate (num)
  (* -1 num)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
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

(defun check-solution (filename assignment)
  (let ((cnf (parse-cnf filename))) 
  (and     (= (length assignment) (first cnf))
    (check-assign (second cnf) assignment)
  ))
)