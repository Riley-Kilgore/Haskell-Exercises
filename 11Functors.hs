import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor
instance Functor List where
  fmap f Empty = Empty
  fmap f (Value val (tail)) = ( Value (f val) (fmap f tail))

head':: List a -> a
head' (Value a (tail)) = a

tail':: List a -> List a
tail' (Value a (tail)) = tail

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty b = b
combineLists a Empty = a

-- Make our list a Monoid

-- Make our list an Applicative

-- Make sure that the List obeys the laws for Applicative and Monoid

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)

-- Use <$> and <*> on the lists with a binary function

-- Create some lists of binary functions

-- Use <*> on the binary functions list and the number lists
