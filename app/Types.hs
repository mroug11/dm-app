
module Types where

import Data.Text 

data Update = NoUpdate | Header Text Update
            | StatusUpdate (Maybe Text) (Maybe Int) (Maybe Int)
            | QueueUpdate Int
        deriving (Show)