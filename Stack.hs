module Stack where

-- ***** STACK DATA STRUCTURE *****
-- Stacks have two attributes...
data Stack a = Stack {
	stackList :: [a],  -- a list containing all of the data in the stack stored in order of insertion...
	top :: Maybe a	   -- and the element at the top of the stack (If the stack is empty, no top exists. We use the Maybe monad to model this scenario)
	} deriving (Show, Read, Eq)

-- ***** STACK CONSTRUCTOR *****
initStack :: Stack a
initStack = Stack {
    stackList = [],
	top = Nothing
	}
	
-- ***** POP FUNCTION *****	
pushToStack :: Stack a -> a -> Stack a
pushToStack (Stack {stackList = thisStackList, top = thisTop}) elt = Stack {stackList = (thisStackList ++ [elt]), top = Just elt}

-- ***** TOP FUNCTION *****
getStackTop :: Stack a -> Maybe a
getStackTop (Stack {stackList = thisStackList, top = thisTop}) = thisTop

-- ***** POP FUNCTION *****
popStack :: Stack a -> Stack a
popStack (Stack {stackList = thisStackList, top = thisTop}) = if (length thisStackList) == 0
															      then error "popStack error: cannot pop from an empty stack"
															  else if (length thisStackList) == 1
																  then Stack {stackList = [], top = Nothing}
															  else Stack {stackList = (init thisStackList), top = Just (last (init thisStackList))}