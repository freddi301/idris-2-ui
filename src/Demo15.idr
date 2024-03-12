module Demo15

data TypeList : Type where
  TypeNil : TypeList
  TypeCons : Type -> TypeList -> TypeList

data ValueList : TypeList -> Type where
  ValueNil : ValueList TypeNil
  ValueCons : head -> ValueList tail -> ValueList (TypeCons head tail)

e1 = ValueCons 4 $ ValueCons "" $ ValueCons False $ ValueNil

data TypeTree : Type where
  TypeLeaf : Type -> TypeTree
  TypeBranch : TypeTree -> TypeTree -> TypeTree

data ValueTree : TypeTree -> Type where
  ValueLeaf : value -> ValueTree (TypeLeaf value)
  ValueBranch : ValueTree left -> ValueTree right -> ValueTree (TypeBranch left right)

e2 = ValueBranch (ValueLeaf True) (ValueBranch (ValueLeaf "") (ValueLeaf 42))

data TreeKey : TypeTree -> Type where
  GetBranchLeft : TreeKey left -> TreeKey (TypeBranch left right)
  GetBranchRight : TreeKey right -> TreeKey (TypeBranch left right)
  GetLeaf : TreeKey (TypeLeaf type)

getTypeTree : {tree : TypeTree} -> ValueTree tree -> TypeTree
getTypeTree _ = tree

e3 : TreeKey (TypeLeaf Bool)
e3 = GetLeaf

e4 : (TreeKey (TypeBranch (TypeLeaf Bool) (TypeLeaf String)))
e4 = GetBranchLeft GetLeaf

ensureKey : ValueTree tree -> TreeKey tree -> ()
ensureKey _ _ = ()

e5 = (ensureKey e2 (GetBranchLeft GetLeaf))

getType : (tree : TypeTree) -> TreeKey tree -> Type
getType (TypeLeaf type) (GetLeaf) = type
getType (TypeBranch left right) (GetBranchLeft next) = getType left next
getType (TypeBranch left right) (GetBranchRight next) = getType right next

getValue : {tree : TypeTree} -> ValueTree tree -> (key : TreeKey tree) -> getType tree key
getValue (ValueLeaf value) (GetLeaf) = value
getValue (ValueBranch left right) (GetBranchLeft next) = getValue left next
getValue (ValueBranch left right) (GetBranchRight next) = getValue right next

e6 = getValue e2 (GetBranchLeft GetLeaf)

e7 = getValue e2 (GetBranchRight $ GetBranchLeft $ GetLeaf)

setValue : {tree : TypeTree} -> ValueTree tree -> (key : TreeKey tree) -> getType tree key -> ValueTree tree
setValue (ValueLeaf value) (GetLeaf) newValue = ValueLeaf newValue
setValue (ValueBranch left right) (GetBranchLeft next) newValue = ValueBranch (setValue left next newValue) right
setValue (ValueBranch left right) (GetBranchRight next) newValue = ValueBranch left (setValue right next newValue)

e8 = setValue e2 (GetBranchRight $ GetBranchLeft $ GetLeaf) "Hello"
