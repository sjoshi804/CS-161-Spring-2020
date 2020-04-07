;check if key is in an ordered tree (L m R)
(defun TREE-CONTAINS (key tree) 
    (cond 
        ((not(listp tree)) (= key tree)) ;checks if tree is not a list, if so returns key==tree
        ((= (second tree) key) t) ;else tree is list and checks if m to key
        (t (or (TREE-CONTAINS key (first tree)) (TREE-CONTAINS key (third tree))))
        ;else recurses down L and R
    )
)

;Gets minimum element of an ordered tree (L m R)
(defun TREE-MIN (tree) 
    (cond 
        ((not(listp tree)) tree) ;; if tree is just 1 element then return that
        (t ;else first gets min of L and R respectively, min of m is m as it is just an element
            (let ((first_min (TREE-MIN (first tree))) (second_min (second tree)) (third_min (TREE-MIN (third tree))))
                (cond
                    ((and (< first_min second_min) (< first_min third_min)) first_min)
                    ((and (or (< second_min first_min) (= second_min first_min)) (< second_min third_min)) second_min)
                    (t third_min)
                )
            )
        )        
    )
)

;Pre-order traversal of tree
(defun TREE-ORDER  (tree) 
    (cond 
        ((not(listp tree)) (cons tree NIL)) ;checks if tree is not a list, if so returns entire tree as a list
        (t (append (TREE-ORDER (second tree)) (TREE-ORDER (first tree)) (TREE-ORDER (third tree))))
    )
)

;Extract sublist list
(defun SUB-LIST  (the_list start_pos len) 
    (cond 
        ((= len 0) NIL) ;if len = 0 return NIL
        ((NOT (= start_pos 0)) (SUB-LIST (rest the_list) (- start_pos 1) len)) ;if not starting at pos 0 then recurse until it does by taking rest
        (t (cons (first the_list) (SUB-LIST (rest the_list) start_pos (- len 1))))
    )
)

;Split list in middle (if not possible make first split larger than second)
(defun SPLIT-LIST (the_list)
    (let* ((len (length the_list)) (half_len (/ len 2)) (round_up_half_len (/ (+ len 1) 2)) (round_down_half_len (/ (- len 1) 2)))
        (cond
            ((evenp len) (list (SUB-LIST the_list 0 half_len) (SUB-LIST the_list half_len half_len)))
            (t (list (SUB-LIST the_list 0 round_up_half_len) (SUB-LIST the_list round_up_half_len round_down_half_len)))
        )
    )
)

;height of a binary tree
(defun BTREE-HEIGHT (tree) 
    (cond 
        ((not(listp tree)) 0) ;checks if tree is not a list, if so is a leaf so height = 0
        (t (let ((left_height (BTREE-HEIGHT (first tree)))  (right_height (BTREE-HEIGHT (second tree)))) 
            (cond ;if left tree taller than height is 1 + left_height else vice-a-versa
                ((> left_height right_height) (+ left_height 1))
                (t (+ right_height 1))                
            )
        ))
    )
)

;Take a list and make it a binary tree
(defun LIST2BTREE (the_list) 
    (cond 
        ((not(rest the_list)) (first the_list)) ;checks if list has only 1 element, if so returns just a leaf
        ((= (length the_list) 2) the_list)
        (t (list (LIST2BTREE (first (SPLIT-LIST the_list))) (LIST2BTREE (second (SPLIT-LIST the_list)))))
    )
)