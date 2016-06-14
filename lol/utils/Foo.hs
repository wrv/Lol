{-# LANGUAGE TemplateHaskell #-}

module Foo where

import THApplyProxy

import Data.Proxy

-- baz = (*) <$> [1,2,3] <*> [4,5,6]


main' = applyProxy <$> (applyProxy [|foo|] <$> [ ''Bool, ''Int ])
        <*> [ ''Integer, ''Bool ]

foo :: Proxy s -> Proxy t -> s -> t -> Bool
foo _ _ _ _ = True
