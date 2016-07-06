data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Eq, Ord, Enum, Bounded)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Eq, Ord, Enum, Bounded)

data Date = Date { w :: Weekday
                 , d :: Int
                 , m :: Month
                 , y :: Int
                 }
    deriving (Show, Eq, Ord)

nextWeekday :: Weekday -> Weekday
nextWeekday x = if x == maxBound then Monday else succ x

nextMonth :: Month -> Month
nextMonth x = if x == maxBound then January else succ x

instance Enum Date where
    succ (Date w d m y)
        | succ d <= daysOf m y = Date (nextWeekday w) (succ d) m y
        | m      /= December   = Date (nextWeekday w) 1 (nextMonth m) y
        | otherwise            = Date (nextWeekday w) 1 January (succ y)

daysOf month year
    | elem month [September, April, June, November] = 30
    | month == February                             = if leap year then 29 else 28
    | otherwise                                     = 31

leap year
    | mod year 100 == 0 = (mod year 400 == 0)
    | mod year 4   == 0 = True
    | otherwise         = False

sol date
    | (d date, m date, y date) == (1, January, 2001) = 0
    | otherwise = sol (succ date) +
                  if (w date, d date) == (Sunday, 1) then 1
                                                     else 0

main = print $ sol $ Date Tuesday 1 January 1901
