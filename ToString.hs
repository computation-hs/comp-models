{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ToString where

class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance ToString Char where
  toString x = [x]

instance (Show a) => ToString a where
  toString = show
  
