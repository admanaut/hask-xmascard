{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM.TBMChan
import Control.Monad
import Data.Text
import Data.Foldable
import GHC.Conc
import System.Random

data Card
  = Card
      { from    :: Text
      , to      :: Text
      , subject :: Text
      , message :: Text
      }
  deriving (Show)

data Receiver
  = Receiver
      { name  :: Text
      , email :: Text
      }
  deriving (Show)

receivers :: [Receiver]
receivers
  = [ Receiver { name = "John",   email = "john@email.com"    }
    , Receiver { name = "Leo",    email = "leo@dicaprio.com"  }
    , Receiver { name = "Tom",    email = "tom@m.com"         }
    , Receiver { name = "Simon",  email = "simon@example.com" }
    , Receiver { name = "Boris",  email = "contact@boris.com" }
    , Receiver { name = "Donald", email = "donald@duck.com"   }
    ]

-- | Reads messages from the recv channel (as long as the channel
-- is not empy), delivers them and sleeps a random amount of time.
-- Writes to the done channel when done.
elf :: String -> TBMChan Card -> TBMChan () -> IO ()
elf name recv done
  = keepDelivering =<< randomRIO (1000000, 3000000)
  where
    keepDelivering sleep = do
      empty <- atomically (isEmptyTBMChan recv)
      if not empty
        then do
          deliverCard
          threadDelay sleep
          keepDelivering sleep
        else
          atomically (writeTBMChan done ()) -- ^ I'm done

    deliverCard = do
      card <- atomically (readTBMChan recv)
      putStrLn $
           "Card delivered by "
        <> name
        <> " "
        <> (show card)

-- | Makes cards and puts them on the given channel.
sendCards :: TBMChan Card -> IO ()
sendCards chan
  = do
      let cards = mkCard <$> receivers
      _ <- atomically $ forM_ cards $ writeTBMChan chan
      -- ^ write all cards to the channel
      pure ()
  where
    mkCard r =
      Card
        { from    = "santa@lapland.io"
        , to      = email r
        , subject = "Ho Ho Ho!"
        , message = "Hi " <> name r <> ", Have a merry little christmas!"
        }

main :: IO ()
main = do
  -- | bounded channel to communicate with elves on
  chan <- atomically (newTBMChan 10)
  -- | each elf reports back when done on this channel
  done <- atomically (newTBMChan 2)
  -- ^ 2 is the number of elves

  sendCards chan
  forkIO $ elf "Snowball" chan done
  forkIO $ elf "Evergreen" chan done

  wait done -- ^ wait for all elves to finish

  putStrLn "Finished!"

  where
    -- | wait until channel is full
    wait ch = do
      full <- atomically (isFullTBMChan ch)
      unless full $ do
        threadDelay 1000000 -- 1 second.
        wait ch
