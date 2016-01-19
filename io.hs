---t putStrLn "hello, world"
--putStrLn "hello, world" :: IO ()
--That's good practice, because indentation is important in Haskell.
import System.Environment
import Control.Monad
import System.IO
import System.Random
import qualified Data.ByteString.Lazy as BL
test_recursive_getline = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn line
            test_recursive_getline



main = command_copy
--mapM :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
--example
--  print :: Show a => a -> IO ()
-- [1,2,3] :: Num t => [t]
-- mapM print [1,2,3]
-- a is the type of the element Int, t is [], b is another type
-- t b is like [b], and m will get an I/O action that output [b]

test_for_m = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "which color do you associate with the number" ++ show a ++ "?"
        color <- getLine
        return color)
    print $ colors !! 1
    putStrLn "the colors that you associate with 1,2,3 and 4 are: "
    mapM putStrLn colors

test_interact = interact $ unlines . filter ((<10) . length) . lines


--forM :: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
-- it is much like mapM, just the positions are exchanged.
-- example above, forM accept a [] Int, the lambda just accept a type Int, return an IO String, so forM will return the array of
-- all getLine

--Don't think of a function like putStrLn as a function that takes a string and prints it to the screen. Think of it as a function that takes a string and returns an I/O action.




test_file = do
    handle <- openFile "123" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

my_with_file :: FilePath -> IOMode -> (Handle -> IO a) -> IO a


my_with_file filepath io_mode f = do
    handle <- openFile filepath io_mode
    result <- (f handle)
    hClose handle
    return result

test_file_v2 = do
    my_with_file "123" ReadMode  (\handle -> do
        contents <- hGetContents handle
        putStr contents)


--dispatch
dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add),
            ("view", view)
            ]
command_dispatch = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add [filename, text] = appendFile filename text

view [filename] = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle


--lazy strict bytestring
--lazy bytestring copyfile

command_copy = do
    (file1:file2:_) <- getArgs
    copy_file_lazy_bytestring file1 file2

copy_file_lazy_bytestring :: FilePath -> FilePath -> IO()

copy_file_lazy_bytestring origin dest = do
    contents <- BL.readFile origin
    BL.writeFile dest contents

--generally bytestring can get better performance than string,
--because it uses chunks with better performance not [Char]
--http://meiersi.github.io/HaskellerZ/meetups/2012%2001%2019%20-%20The%20bytestring%20library/handout.html


--define some functions here
