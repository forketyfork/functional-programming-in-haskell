data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node val left right) = val + treeSum left + treeSum right

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x left right) minVal maxVal =
    let leftSorted = isSortedTree left minVal x
        rightSorted = isSortedTree right x maxVal
        in x >= minVal && x < maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x + 1) Leaf Leaf)
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2)

treeToList :: Tree -> [Int]
treeToList Leaf = []
treeToList (Node x left right) = treeToList left ++ [x] ++ treeToList right

addElement :: Tree -> Int -> Tree
addElement Leaf x = Node x Leaf Leaf
addElement (Node y left right) x = 
    if x <= y 
        then Node x left (addElement right y)
        else Node x (addElement left y) right



expr_parser :: Parser Expr
expr_parser = buildExpressionParser optable term <?> "expression"

optable =
  let
    op name =
      Infix ( reservedOp name >> return (\x y ->(Op (MkOpExpr name x y))) )
    prefix name =
      Prefix ( reservedOp name >> return (\x -> (Pref (MkPrefixOpExpr name x))) )
  in
    [ 
        [ 
            op "*"  AssocLeft, 
            op "/"  AssocLeft, 
            op "%" AssocLeft 
        ], 
        [ 
            op "+"  AssocLeft, 
            op "-"  AssocLeft 
        ], 
        [ 
            prefix "-" 
        ] 
    ]
