module IOLib where


import DbLib
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
    
printPresents :: [Present] -> IO ()
printPresents presents = 
  forM_ presents $ do {
    \p ->
      print p
    }

printChildren :: [Child] -> IO ()
printChildren children = 
  forM_ children $ do {
    \c -> 
      print c
    }
