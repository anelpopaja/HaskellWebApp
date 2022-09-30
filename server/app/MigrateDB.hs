module Main where

import System.Environment (getArgs)

import Database (localConnString, migrateDB)
--import qualified DatabaseEsq as E

main :: IO ()
main = migrateDB localConnString --do      --OVO SU BILI PRIMJERI SA DODATNIM ARGUMENTIMA KOJI SE PROSLIJEDE, DA BI SE TAD KORISTILA BAZA REDIS ZA KESIRANJE, MOZDA CU DODATI NEKAD, PA STOJI....
 -- args <- getArgs
 -- if null args || head args /= "esq"
  --  then migrateDB localConnString
--    else E.migrateDB localConnString
