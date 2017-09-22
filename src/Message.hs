module Message (Msg) where

import qualified Data.Binary as Binary

class (Binary.Binary m) => Msg m

