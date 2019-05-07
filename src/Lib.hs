{-# LANGUAGE LambdaCase #-}
module Lib (externalMessages) where

import           Control.Concurrent (Chan, forkIO, newChan, threadDelay,
                                     writeChan)

externalMessages :: IO (Chan String)
externalMessages = do
  chan <- newChan
  _ <- forkIO (driveChan chan)
  return chan
  where
    driveChan :: Chan String -> IO ()
    driveChan chan = loop $ map mk [1..10]
          where loop = \case
                  [] -> do return ()
                  m:messages -> do
                    pause
                    writeChan chan m
                    loop messages

                mk i = "This is external message number " <> show (i::Integer)

pause :: IO ()
pause = do
  --hFlush stdout
  threadDelay 1000000


