myAppend :: [a] -> [a] -> [a]
myAppend [] list = list
myAppend (x:xs) list = (x: myAppend xs list)

myConcatHelp :: [a] -> [[a]] -> [a]
myConcatHelp currList [] = currList
myConcatHelp currList (x:xs) = myConcatHelp (myAppend currList x) xs

myConcat = myConcatHelp []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred list = [x | x <- list, pred x]

myLengthHelp :: Int -> [a] -> Int
myLengthHelp count [] = count
myLengthHelp count (x:xs) = myLengthHelp (count+1) xs

myLength = myLengthHelp 0

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x: myMap f xs)

myFoldl :: (Num b) => (b -> a -> b) -> [a] -> b -> b
myFoldl _ [] acc = acc
myFoldl f (x:xs) acc = myFoldl f xs (f acc x)

myFoldr :: (Num b) => (b -> a -> b) -> [a] -> b -> b
myFoldr _ [] acc = acc
myFoldr f (x:xs) acc = f (myFoldr f xs acc) x

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]