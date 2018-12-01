module IOLib where


import Control.Monad

printOneTwoThree :: IO ()
printOneTwoThree = do
  let strings = ["one", "two", "three"]  
  forM_ strings putStrLn 

printAgain = do
  let strings = ["four", "five", "six"]  
  forM_ strings $ do { 
    \s -> 
      putStrLn s
    }
    
