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
        
    )
)