{-# LANGUAGE LambdaCase #-}
module Main(main) where

import           Control.Concurrent        (Chan, forkIO, readChan)
import           Control.Monad             (forever)
import           Control.Monad.Trans.Class (lift)
import           Prelude                   hiding (print)

import           System.Console.ANSI
import           System.Console.Haskeline  as HL

import           Lib                       (externalMessages)

main :: IO ()
main = runInputT HL.defaultSettings $ do
  chan <- lift Lib.externalMessages
  print <- HL.getExternalPrint
  _ <- lift $ forkIO (suckChan chan print)
  readLoop

  where
    readLoop :: InputT IO ()
    readLoop = do
      r <- getInputLine "> "
      case r of
        Nothing -> return ()
        Just com -> do
          HL.outputStrLn $ "you reponded: " <> com
          readLoop

    suckChan :: Chan String -> (String -> IO ()) -> IO ()
    suckChan chan print = forever $ readChan chan >>= printX
      where
        printX s = print $ red s <> "\n"

red :: String -> String
red s =
  setSGRCode [SetColor Foreground Vivid Red] <> s <>
  setSGRCode [SetColor Foreground Vivid White]
