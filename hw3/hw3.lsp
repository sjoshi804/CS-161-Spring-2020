;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond
	((not s) t) ;if checked all rows, return true
	((goal-row-test (first s)) (goal-test (rest s))) ;check first row, if okay check rest of rows
  )
)

(defun goal-row-test (row) ;ensure that no row has a box or keeper not in goal state
	(cond
		((not row) t) ;check if row completed
		((and (not (isBox (first row))) (not (isKeeper (first row)))) (goal-row-test (rest row)))
	)
)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (col (car pos))
	 (row (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result 
	 	(list 
		 (try-move s row col (- row 1) col (- row 2) col) ;consider move into square up  
		 (try-move s row col row (+ col 1) row (+ col 2)) ;consdier move into square right
		 (try-move s row col (+ row 1) col (+ row 2) col) ;consider move into square down
		 (try-move s row col row (- col 1) row (- col 2)) ;consider move into square left
	 ))
	)
    (cleanUpList result);end
   );end let
);

;Helper function to get nth element of a list
(defun get-nth-el (the_list index)
	(cond 
		((>= index 0) (first (nthcdr index the_list)))
	)
)

;Helper function to set nth element of a list to val
(defun set-nth-el (the_list index val)
	(cond
		((= index 0) (cons val (rest the_list)))
		(t (cons (first the_list) (set-nth-el (rest the_list) (- index 1) val)))
	)
)

;Gets value at row, col else returns wall
(defun get-square (s row col)
	(or (get-nth-el (get-nth-el s row) col) wall) ;if get-nth-el returns nil then out of bounds so return wall
)

(defun set-square (s row col val)
	(set-nth-el s row (set-nth-el (get-nth-el s row) col val)) 
	;sets the col_th element of the row of s to val and 
	;then sets that row in s to the new row 
)

;try move - tries to move keeper from current pos to square 1 - returns new state if square 1 is a valid empty square or 
;square 1 is a box and square 2 is empty valid else retuns nil
;_r -> means row number, _c -> means col number
(defun try-move (s keeper_r keeper_c square_1_r square_1_c square_2_r square_2_c)
	(let* ((square1 (get-square s square_1_r square_1_c)) (square2 (get-square s square_2_r square_2_c))
		(keeperSquare (get-square s keeper_r keeper_c)) ;gets whats on keeper square
		(blankOrStar (cond ((isKeeperStar keeperSquare) star) ((isKeeper keeperSquare) blank)))) ;checks if keeper currently on blank or goal to accordingly replace after move
	(cond
		;if square 1 is a regular blank
		((isBlank square1) (set-square (set-square s square_1_r square_1_c keeper) keeper_r keeper_c blankOrStar)) 
		;if square 1 is a blank goal state
		((isStar square1) (set-square (set-square s square_1_r square_1_c keeperstar) keeper_r keeper_c blankOrStar)) 
		;if square 1 is regular box and square 2 is regular blank
		((and (isBox square1) (isBlank square2)) (set-square (set-square (set-square s square_2_r square_2_c box) square_1_r square_1_c keeper) keeper_r keeper_c blankOrStar))
		;if square 1 is box on goal and square 2 is regular blank
		((and (isBoxStar square1) (isBlank square2)) (set-square (set-square (set-square s square_2_r square_2_c box) square_1_r square_1_c keeperstar) keeper_r keeper_c blankOrStar))
		;if square 1 is a regular box and square 2 is blank goal state
		((and (isBox square1) (isStar square2)) (set-square (set-square (set-square s square_2_r square_2_c boxstar) square_1_r square_1_c keeper) keeper_r keeper_c blankOrStar))
		;if square 1 is a box on goal and square 2 is a blank goal state
		((and (isBoxStar square1) (isStar square2)) (set-square (set-square (set-square s square_2_r square_2_c boxstar) square_1_r square_1_c keeperstar) keeper_r keeper_c blankOrStar))
	))
)

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
(defun h1 (s)
	(cond 
		((not s) 0)
		(t (+ (count box (first s)) (h1 (rest s))))
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; Idea is to keep heuristic fast, spend time only using heuristic to detect dead games
; Rather than focusing on using the heuristic to order the games better
; If heuristic time is not a concern, t 0 -> can be replaced with t some other heuristic like manhattan dist
(defun h105032378 (s)
	(cond 
		((deadlock s) 4000)
		(t (min-dist s))
	)
)

;Idea in deadlock detection is to detect boxes that are not in goal state
;and cannot be moved anymore 
;These cases are unsolvable and hence assigned heuristic score of 4000 
;which is like an INT_MAX here
(defun deadlock (s)
	(or (not (boxesCanReachGoal (first s) 0 0)) (check-for-deadlock s 1 1)
	(firstLastColumnCheck s))
	;check if first row has boxes that can reach goals if so 
	;check for deadlock
	;check for deadlock due to box trapped in left most or right most column
)

(defun firstLastColumnCheck (s) 
	(let* 	
	(
		(first_two_col (get_first_two_col s))
		(first_col (first first_two_col))
		(second_col (second first_two_col))
		(last_two_col (get_first_two_col (reverse s)))
		(last_col (first last_two_col))
		(second_last_col (second last_two_col))
	)
	(or 
		(and 
			(isWallRow first_col)
			(not (boxesCanReachGoal second_col 0 0))
		)
		(not (boxesCanReachGoal first_col 0 0))
		(and 
			(isWallRow last_col)
			(not (boxesCanReachGoal second_last_col 0 0))
		)		
		(not (boxesCanReachGoal last_col 0 0))
	))
)

(defun isWallRow (row)
	(cond
		((not row) T)
		((not (isWall (first row))) NIL)
		(t (isWallRow (rest row)))
	)
)

(defun get_first_two_col (s)
	(cond
		((not s) (list NIL NIL))
		(t 

		(let 
			((recursive_case (get_first_two_col (rest s))))
		
			(list 
				(cons (first (first s)) (first recursive_case))
				(cons (second (first s)) (second recursive_case))
			)
		)
		
		)
	)
)


(defun boxesCanReachGoal (row num_box num_goals)
	(cond
		((not row) (<= num_box num_goals))
		((isWall (first row)) (and (<= num_box num_goals) (boxesCanReachGoal (rest row) 0 0)))
		((isBox (first row)) (boxesCanReachGoal (rest row) (+ num_box 1) num_goals))
		((or (isStar (first row)) (isKeeperStar (first row))) 
			(boxesCanReachGoal (rest row) num_box (+ num_goals 1)))
		(t (boxesCanReachGoal (rest row) num_box num_goals))
	)
)

;Checks if box is wall or NIL
(defun isObstacle (square)
	(or (not square) (isWall square))
)

(defun isUnmoveable (square)
	(or (isObstacle square) (isBoxStar square) (isBox square))
)


(defun check-for-deadlock (s row_num col_num)
	(let*
		((prev_row (car s)) ;get first row - set to previous row
		(curr_row (cadr s)) ;second row of state is current row
		(next_row (caddr s)) ;thirs row of state is next row
		 ;get list starting at relevant squares of prev_row
		(prev_squares (nthcdr (- col_num 1) prev_row))
		(up_left (car prev_squares)) ;first element of above list
		(up (cadr prev_squares)) ;second element of squares
		(up_right (caddr prev_squares)) ;third element of squares
		;get list starting at relevant squares of current_row
		(curr_squares (nthcdr (- col_num 1) curr_row)) 
		(left (car curr_squares)) ;first element of above list
		(current_square (cadr curr_squares)) ;second element of squares
		(right (caddr curr_squares)) 
		;get list starting at relevant squares of next_row
		(next_squares (nthcdr (- col_num 1) next_row)) 
		(down_left (car next_squares))
		(down (cadr next_squares)) 
		(down_right (caddr next_squares))) 
	(cond
		((not next_row) NIL) ;
		((and (isBox current_square)
			(or 
				;if row below is wall, check if boxes can reach goals
			       	(and (not (cdddr s)) (isWallRow next_row) (not (boxesCanReachGoal curr_row 0 0)))
				;check if box is in a corner
				(and (isObstacle up) (isObstacle right)) 
				(and (isObstacle up) (isObstacle left))
				(and (isObstacle down) (isObstacle right)) 
				(and (isObstacle up) (isObstacle left))
				;check if 2 or 4 box deadock
 				(and (isUnmoveable up) (isUnmoveable up_right) (isUnmoveable right))
				(and (isUnmoveable up) (isUnmoveable up_left) (isUnmoveable left))
				(and (isUnmoveable down) (isUnmoveable down_right) (isUnmoveable right))
				(and (isUnmoveable down) (isUnmoveable down_left) (isUnmoveable left))
				;wide special deadlock positions
				(and (isUnmoveable up) (isUnmoveable up_left) (isObstacle down_left)
				(not (elt curr_row (- col_num 2))) (isObstacle (elt curr_row (- col_num 2))))
			)
		)
			t
		)
		((and (isBlank current_square)
			(or 
				;blank centric deadlock 1
				(and up (isBox up) up_right (isBox up_right) right (isBox right) (isObstacle left) (isObstacle down))
			)
		)
			t
		)
		;check if last row has any boxes that can't reach goals
		((and (not (cdddr s)) (not (boxesCanReachGoal next_row 0 0))) t)
		
		(t ;not a deadlock position detectable by logic above
		(cond 
			((not right) (check-for-deadlock (rest s) (+ row_num 1) 1)) ;if row over, go to next row
			(t (check-for-deadlock s row_num (+ col_num 1))) ;else check next element in row
		))
	))
)

(defun min-dist (s)
	(let 
		((boxes (get-pos-of-x (list box -1 -1) s 0 0))
		 (stars (get-pos-of-x (list boxstar star keeperstar) s 0 0)))
	
	(+ (sokoban_to_closest s boxes) (boxes-to-closest boxes stars))
	)
)

(defun sokoban_to_closest (s y)
	(let*
	(
		(list_pos (getKeeperPosition s 0))
		(row_num (second list_pos))
		(col_num (first list_pos))
		(keeper-pos (cons row_num col_num))
	)
	(cond 
		((not y) 0)
		(t (from-x-to-closest-y keeper-pos y))
	)
	
	)
)

(defun boxes-to-closest (boxes y)
	(cond 
		((not boxes) 0)
		(t 
		(+ (from-x-to-closest-y (first boxes) y) (boxes-to-closest (rest boxes) y)))
	)
)

(defun get-pos-of-x (vals s row_num col_num)
	(cond 
		((not s) NIL)
		((not (caar s)) 
			(get-pos-of-x vals (rest s) (+ row_num 1) 0))
		((or (= (caar s) (first vals)) (= (caar s) (second vals)) (= (caar s) (third vals)))
			(cons 
				(cons row_num col_num) 
				(get-pos-of-x 
					vals 
					(cons (cdar s) (rest s))
					row_num
					(+ col_num 1)
				) 
			)
		)
		(t 
			(get-pos-of-x 
				vals 
				(cons (cdar s) (rest s))
				row_num
				(+ col_num 1)
			) 			
		)	
	) 
)

(defun manhattan-dist (x y)
	(+ (abs (- (first x) (first y))) (abs (- (cdr x) (cdr y))))
)

(defun from-x-to-closest-y (x list_of_y)
	(cond
		((not list_of_y) 5000)
		(t (min  (manhattan-dist x (first list_of_y)) (from-x-to-closest-y x (rest list_of_y))))
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 
	  '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 
	  '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
