type Level = (Integer, Integer)

levels :: [Level]
levels = concat $ map pairsForNum [3,5..12]
         where pairsForNum num = zip [2..12] $ repeat num

levelNumber :: [a] -> Int
levelNumber remainingLevels = totalLevels - levelsLeft
                              where totalLevels = length levels + 1
                                    levelsLeft = length remainingLevels

main :: IO()
main = do
  putStrLn "suddenly you wake up. Oh no, you're on .."
  putStrLn "the times-table train of terror"
  putStrLn "try to get to the end. we DARE you..!!"
  trainLoop levels

trainLoop :: [Level] -> IO()
trainLoop [] = putStrLn "You won! Well done."
trainLoop remainingLevels @ (currentLevel : levelsAfterThisOne) =
  do 
    let currentLevelNumber = levelNumber remainingLevels 
        (num1, num2) = currentLevel
    putStrLn $ "you are in a train carriage "
               ++ show currentLevelNumber
               ++ " of " ++ (show $ length levels)
    putStrLn "do you want to:"
    putStrLn "1. go to the next carriage"
    putStrLn "2. Jump out of the train"
    putStrLn "3. eat some food"
    putStrLn "q. quit"
    activity <- getLine
    case activity of 
      "1" -> do
        putStrLn "you try to go to the next carriage. the door is locked"
        putStrLn "answare this question to unlock the door:"
        putStrLn $ "what is "
                 ++ show num1
                 ++ " times "
                 ++ show num2
                 ++ "?"
        answer <- getLine
        if answer == (show $ num1 * num2)
        then do 
          putStrLn "the lock is opened!"
          trainLoop levelsAfterThisOne
        else do
          putStrLn "wrong. you try to open the lock, but i wont open"
          trainLoop remainingLevels
      "2" -> jumpingFutility
      "3" -> eatingFutility
      "q" -> putStrLn "you decide to quit. thanks for playing" 
      _   -> do
        putStrLn "that makes no sense! try again"
        trainLoop remainingLevels

jumpingFutility :: IO()
jumpingFutility = do
  putStrLn "you try to jump out of the train"
  putStrLn "you fail and die"
  trainLoop levels

eatingFutility :: IO()
eatingFutility = do
  putStrLn "you see a delicious looking cupcake"
  putStrLn "you eat it. it's time travel cupcake"
  trainLoop levels
