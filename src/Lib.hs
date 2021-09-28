module Lib
  ( someFunc,
  )
where

import RIO
import RIO.Prelude

someFunc = 1

data L = L {a :: Text, b :: Text}

f :: L -> Text
f l = l.a