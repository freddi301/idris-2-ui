module Demo32

import Data.Fin
import Data.List
import Data.Vect

import public Language.Reflection
import Language.Reflection.TT
%language ElabReflection

%default total

DataTypeDefinition : Nat -> Type
DataTypeDefinition n = List (List (Fin ))

DataTypeUnviverse : Nat -> Type
DataTypeUnviverse n = Vect n (DataTypeDefinition n)


data DataTypeInstance : DataTypeDefinition -> Type where
  MakeDataTypeInstance : (d : DataTypeDefinition) -> (c : Fin (length d)) -> Vect (index' d c) (DataTypeInstance d) -> DataTypeInstance d

MyNat : DataTypeDefinition
MyNat = [0, 1]

u1 : DataTypeInstance MyNat
u1 = MakeDataTypeInstance MyNat 0 []

u2 : DataTypeInstance MyNat
u2 = MakeDataTypeInstance MyNat 1 [MakeDataTypeInstance MyNat 0 []]

MyBool : DataTypeDefinition
MyBool = [0, 0]

v1 : DataTypeInstance MyBool
v1 = MakeDataTypeInstance MyBool 0 []

v2 : DataTypeInstance MyBool
v2 = MakeDataTypeInstance MyBool 1 []

getElimType : DataTypeDefinition -> Type