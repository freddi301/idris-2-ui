module Demo31

import public Language.Reflection
import Language.Reflection.TT
%language ElabReflection

%default total

namespace Context

  export myStateNat : Nat

  export myStateBool : Bool

myComponent = (myStateNat, myStateBool)

%macro
getTTImp : v -> Elab TTImp
getTTImp v = quote v

hoistVars : List String -> TTImp -> Elab (List (Name, TTImp), TTImp)
hoistVars ns v@(IVar fc n@(NS (MkNS ns') (UN (Basic name)))) =
  if ns' == ns
    then do
      ((_, vt) :: _) <- getType n
        | _ => fail "Could not get type"
      newName <- genSym name
      pure ([(newName, vt)], IVar fc newName)
    else pure ([], v)
hoistVars ns (IApp fc left right) = do
  (lns, left) <- hoistVars ns left
  (rns, right) <- hoistVars ns right
  pure (lns ++ rns, IApp fc left right)
hoistVars ns (INamedApp fc left nm right) = do
  (lns, left) <- hoistVars ns left
  (rns, right) <- hoistVars ns right
  pure (lns ++ rns, INamedApp fc left nm right)
hoistVars ns term = pure ([], term)

makeLambdas : (List (Name, TTImp), TTImp) -> TTImp
makeLambdas ([], term) = term
makeLambdas (((n, nt) :: ns), term) = assert_total $ ILam EmptyFC MW ImplicitArg (Just n) nt (makeLambdas (ns, term))

%macro
hoist : v -> Elab v
hoist v = do
  tt <- quote v
  (ns, tt') <- hoistVars ["Context", "Demo31"] tt
  check $ makeLambdas (ns, tt')

u1 = getTTImp myStateNat

u2 = getTTImp myComponent

u4 = getTTImp (\x:Nat=>x)

u5 = %runElab (hoistVars ["Context", "Demo31"] (getTTImp myComponent))

u6 = %runElab (pure $ makeLambdas u5)

u7 = hoist myComponent
