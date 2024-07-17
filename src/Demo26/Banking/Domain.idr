module Demo26.Banking.Domain

import Data.So

%default total

-- BEGIN library

mutual

  public export
  data History : (event : Type) -> Type where
    Nil : History event
    (::) :
      (present : event) -> (past : History event) ->
      Verified event => {auto 0 r : requirements present past} ->
      History event

  public export
  interface Verified (event : Type) where
    requirements : event -> History event -> Type

--- END library

public export
AccountId : Type
AccountId = Nat

public export
TransactionId : Type
TransactionId = Nat

export
data Event : Type where
  AccountCreated : Event
  TransactionHappened : (from : AccountId) -> (to : AccountId) -> (amount : Nat) -> Event

nextAccountId : History Event -> AccountId
nextAccountId Nil = 0
nextAccountId (AccountCreated :: history) = S $ nextAccountId history
nextAccountId (_ :: history) = nextAccountId history

isExistingAccount : AccountId -> History Event -> Bool
isExistingAccount accountId history = accountId < nextAccountId history

balance : AccountId -> History Event -> Integer
balance accountId Nil = 0
balance accountId (TransactionHappened from to amount :: history) =
  let fromBalance = if accountId == from then - (natToInteger amount) else 0 in
  let toBalance = if accountId == to then natToInteger amount else 0 in
  fromBalance + toBalance + balance accountId history
balance accountId (_ :: history) = balance accountId history

accountIds : History Event -> List AccountId
accountIds Nil = []
accountIds (AccountCreated :: history) = nextAccountId history :: accountIds history
accountIds (_ :: history) = accountIds history

nextTransactionId : History Event -> TransactionId
nextTransactionId [] = 0
nextTransactionId (TransactionHappened _ _ _ :: history) = S $ nextTransactionId history
nextTransactionId (_ :: history) = nextTransactionId history

transactionIds : History Event -> List TransactionId
transactionIds Nil = []
transactionIds (TransactionHappened _ _ _ :: history) = nextTransactionId history :: transactionIds history
transactionIds (_ :: history) = transactionIds history

Verified Event where
  requirements AccountCreated history = ()
  requirements (TransactionHappened from to amount) history =
    (
      So $ from /= to,
      So $ amount > 0,
      So $ isExistingAccount from history,
      So $ isExistingAccount to history,
      So $ balance from history - (natToInteger amount) >= -100
    )