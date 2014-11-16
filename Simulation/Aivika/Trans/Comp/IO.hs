
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Comp.IO
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines a template-based implementation of the 'MonadComp' type class.
--
module Simulation.Aivika.Trans.Comp.IO where

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Template

-- | A template-based implementation of the 'MoandComp' type class.
instance TemplateIO m => MonadComp m
