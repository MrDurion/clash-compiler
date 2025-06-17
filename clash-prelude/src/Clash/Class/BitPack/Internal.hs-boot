{-# LANGUAGE Safe #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

{- |
Copyright  :  (C) 2013-2016, University of Twente
                  2016-2017, Myrtle Software Ltd
                       2021, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.Class.BitPack.Internal where

import {-# SOURCE #-} Clash.Sized.Internal.BitVector (Bit)

bitToBool :: Bit -> Bool
