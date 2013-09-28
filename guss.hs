import System.Random  
import Control.Monad(when)  
  
main = do  
    gen <- getStdGen  
    askForNumber gen  
  
askForNumber :: StdGen -> IO ()  
askForNumber gen = do  
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  

    putStrLn "---------------"
    keepAsking (randNumber, newGen, 1)



keepAsking :: (Int, StdGen, Int) -> IO ()
keepAsking (randNumber, newGen, tries) = do
    -- putStrLn $ "hehaha! " ++ show randNumber
    putStrLn "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number   
            then do 
                putStrLn $ "You are correct! At " ++ show tries ++ " tries."
                askForNumber newGen  
            else do
                if number < randNumber
                    then putStrLn "go up /\\"
                    else putStrLn "go down \\/"
                keepAsking (randNumber, newGen, tries+1)
