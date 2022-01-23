-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Coffer.Util
  ( catchAndReturn
  ) where

import Polysemy ( Sem )
import Polysemy.Error ( runError, Error )

----------------------------------------------------------------------------
-- Polysemy helpers
----------------------------------------------------------------------------

-- | Takes an action that may return an @e@ or throw an @e@,
-- and turns it into an action that always returns an @e@.
catchAndReturn :: forall e r. Sem (Error e ': r) e -> Sem r e
catchAndReturn action =
  either id id <$> runError @e action
