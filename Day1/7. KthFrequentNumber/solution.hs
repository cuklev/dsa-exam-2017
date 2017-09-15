import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Command = Add Int
             | Remove Int
             | Get Int

data Result = Added Int
            | Removed Int
            | NotRemoved Int
            | Found Int
            | NotValid Int

instance Show Result where
    show (Added x) = "Ok: " ++ show x ++ " added"
    show (Removed x) = "Ok: Number " ++ show x ++ " removed"
    show (NotRemoved x) = "Error: Number " ++ show x ++ " not found"
    show (Found x) = "Ok: Found " ++ show x
    show (NotValid x) = "Error: " ++ show x ++ " is invalid K"

readCommands :: IO [Command]
readCommands = do
    (cmd:rest) <- fmap words getLine
    if cmd == "END"
        then return []
        else let number = read $ head rest
                 constructor = case cmd of
                    "ADD" -> Add
                    "REMOVE" -> Remove
                    "GET" -> Get
                    _ -> error "Invalid command"
             in readCommands >>= \commands -> return $ constructor number : commands

processCommands :: [Command] -> S.Set (Int, Int) -> M.Map Int Int -> [Result]
processCommands [] _ _ = []
processCommands (c:cs) set map = case c of
    Add x -> let oldCount = M.lookup x map
                 newCount = maybe 1 (+1) oldCount

                 set' = maybe set (\c -> S.delete (-c, x) set) oldCount

                 newSet = S.insert (-newCount, x) set'
                 newMap = M.insert x newCount map
             in Added x : processCommands cs newSet newMap
    Remove x -> let oldCount = M.lookup x map
                    newCount = maybe 0 (\x -> x - 1) oldCount

                    set' = maybe set (\c -> S.delete (-c, x) set) oldCount

                    newSet = if newCount > 0
                                then S.insert (-newCount, x) set'
                                else set'
                    newMap = M.update (\x -> if x > 1 then Just (x - 1) else Nothing) x map

                    result = if isJust oldCount
                                then Removed x
                                else NotRemoved x
                in result : processCommands cs newSet newMap
    Get x -> let index = x - 1
                 size = S.size set

                 result = if index < 0 || index >= size
                            then NotValid x
                            else Found $ snd $ S.elemAt index set
             in result : processCommands cs set map

main = do
    commands <- readCommands
    putStr $ unlines $ map show $ processCommands commands S.empty M.empty
