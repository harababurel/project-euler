import Data.MemoCombinators.Class (memoize)
import Data.MemoUgly
import Data.Int

-- Needs better memoization :-??

data Letter = A | O | L
data Absence = None | One | Two
    deriving (Eq)

prev :: Absence -> Absence
prev Two = One
prev One = None

dp = dp'
    where dp' :: Int8 -> Letter -> Absence -> Bool -> Int32
          dp' 0 _ _    _     = 0
          dp' 1 A One  False = 1
          dp' 1 O None False = 1
          dp' 1 L None True  = 1
          dp' 1 _ _    _     = 0

          dp' i O absences hasL
            | absences /= None = 0
            | otherwise        = sum [dp (i-1) A     lastAbsences hasL | lastAbsences <- [None, One, Two]] +
                                 sum [dp (i-1) lastJ None         hasL | lastJ <- [L,O]]

          dp' i L absences hasL
            | absences /= None = 0
            | not hasL         = 0
            | otherwise        = sum [dp (i-1) A lastAbsences False | lastAbsences <- [None, One, Two]] +
                                      dp (i-1) O None         False

          dp' i A absences hasL
            | absences == None = 0    -- by adding an A, the absences is at least 1
            | absences == One  = sum [dp (i-1) lastJ None            hasL | lastJ <- [L,O]]
            | otherwise        = sum [dp (i-1) A     (prev absences) hasL]

main = do
    line <- getLine
    let n = read line :: Int8

    print $ sum $ [sum [dp n O None     hasL | hasL <- [False,True]]
                  ,sum [dp n A absences hasL | absences <- [One, Two], hasL <- [False,True]]
                  ,     dp n L None     True]
