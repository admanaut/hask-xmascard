{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Data.Foldable
import Control.Monad
import Control.Concurrent.STM.TBMChan
import GHC.Conc

data Card
  = Card
      { from    :: Text
      , to      :: Text
      , subject :: Text
      , message :: Text
      }
  deriving (Show)

type CardChan = TBMChan Card

data Receiver
  = Receiver
      { name  :: Text
      , email :: Text
      }

receivers :: [Receiver]
receivers
  = [ Receiver { name = "Adrian", email = "adrian@email.com"  }
    , Receiver { name = "Leo",    email = "leo@dicaprio.com"  }
    , Receiver { name = "Tom",    email = "tom@m.com"         }
    , Receiver { name = "Simon",  email = "simon@example.com" }
    , Receiver { name = "Boris",  email = "contact@boris.com" }
    , Receiver { name = "Donald", email = "donald@duck.com"   }
    ]

-- | Reads messages from the channel and delivers them,
-- as long as the channel is not empy.
postman :: String -> TBMChan Card -> IO ()
postman name chan
  = keepDelivering
  where
    keepDelivering = do
      empty <- atomically (isEmptyTBMChan chan)
      unless empty $ do
        deliverCard
        threadDelay 1000000 -- 1 second. TODO make this random
        keepDelivering

    deliverCard = do
      card <- atomically $ do
        card <- readTBMChan chan
        pure card

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
      -- write all cards to the channel
      _ <- atomically $ forM_ cards $ writeTBMChan chan
      pure ()
  where
    mkCard r =
      Card
        { from    = "hello@yourbestfriend.co.uk"
        , to      = email r
        , subject = "hohoho!"
        , message = "Hello " <> name r <> ", Have a merry little christmas!"
        }

main :: IO ()
main = do
  -- | create a bounded channel with max bound 10
  chan <- atomically (newTBMChan 10)

  sendCards chan
  postman "Pat" chan
  postman "Bob" chan

  -- TODO: use forkIO to add concurrency

  putStrLn "Finished!"
