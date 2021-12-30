{-# LANGUAGE TypeFamilies #-}

module MSFramework.Validator where

import Data.List.NonEmpty (NonEmpty)
import Validation         (Validation)

class Validator a where
   type ValidationError a
   validate :: a -> Validation (NonEmpty (ValidationError a)) a
