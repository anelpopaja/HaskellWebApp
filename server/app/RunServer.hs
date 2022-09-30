module Main where

import System.Environment (getArgs)   --OVO SU BILI PRIMJERI SA DODATNIM ARGUMENTIMA KOJI SE PROSLIJEDE, DA BI SE TAD KORISTILA BAZA REDIS ZA KESIRANJE, MOZDA CU DODATI NEKAD, PA STOJI....

import qualified BasicServer as B
--import qualified CacheServer as C
--import qualified ServerEsq as E

main :: IO ()
main = putStrLn "Running Basic Server" >> B.runServer
{-
   do
   
  args <- getArgs
  if null args
    then putStrLn "Running Basic Server" >> B.runServer
    else if head args == "cache"
      then putStrLn "Running Cache Server" >> C.runServer
        else putStrLn "Running Esqueleto Server" >> E.runServer
-}