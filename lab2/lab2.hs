data TransactionType = Income | Expense | Transfer
  deriving (Show, Eq)

data Transaction = Transaction Int Double TransactionType String
  deriving (Show, Eq)

exceedsThreshold :: Double -> Transaction -> Bool
exceedsThreshold threshold (Transaction _ amount _ _) = amount > threshold

classifyTransactions :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a], [a])
classifyTransactions p1 p2 list = go list ([], [], [], [])
  where
    go [] result = result
    go (x:xs) (only1, only2, both, neither) =
      let p1True = p1 x
          p2True = p2 x
      in if p1True && p2True
         then go xs (only1, only2, x:both, neither)
         else if p1True && not p2True
              then go xs (x:only1, only2, both, neither)
              else if not p1True && p2True
                   then go xs (only1, x:only2, both, neither)
                   else go xs (only1, only2, both, x:neither)

isIncomeTransaction :: Transaction -> Bool
isIncomeTransaction (Transaction _ _ ttype _) = ttype == Income

isExpenseTransaction :: Transaction -> Bool
isExpenseTransaction (Transaction _ _ ttype _) = ttype == Expense

isTransferTransaction :: Transaction -> Bool
isTransferTransaction (Transaction _ _ ttype _) = ttype == Transfer

isLargeTransaction :: Transaction -> Bool
isLargeTransaction = exceedsThreshold 1000

isSmallTransaction :: Transaction -> Bool
isSmallTransaction (Transaction _ amount _ _) = amount < 500

{-
let t1 = Transaction 1 1500.0 Income "Зарплата"
let t2 = Transaction 2 300.0 Expense "Продукты"  
let t3 = Transaction 3 2000.0 Transfer "Перевод между счетами"
let t4 = Transaction 4 450.0 Income "Премия"
let t5 = Transaction 5 1200.0 Expense "Аренда"
let t6 = Transaction 6 250.0 Transfer "Перевод другу"
let transactions = [t1, t2, t3, t4, t5, t6]

exceedsThreshold 1000 t1
exceedsThreshold 1000 t2
exceedsThreshold 500 t4
map (exceedsThreshold 1000) transactions

map isIncomeTransaction transactions
map isExpenseTransaction transactions  
map isLargeTransaction transactions
map isSmallTransaction transactions

classifyTransactions isIncomeTransaction isLargeTransaction transactions
-}

func :: (a -> a -> a) -> a -> [Either a a] -> a
func _ acc [] = acc
func f acc (Left x:xs) = func f (f acc x) xs
func f acc (Right x:xs) = func f (f acc x) xs

f :: [Maybe (a->b)] -> [Maybe a] -> [Maybe b]
f [] _ = []
f _ [] = []
f (x:xs) (Nothing:ys) = Nothing : f xs ys
f (Nothing:xs) (Just y:ys) = Nothing : f xs ys
f (Just x:xs) (Just y:ys) = Just (x y) : f xs ys
