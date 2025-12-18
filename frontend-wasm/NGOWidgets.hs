{-# LANGUAGE OverloadedStrings #-}
module NGOWidgets where

import Reflex.Dom

titleWidget :: MonadWidget t m => m ()
titleWidget = el "h2" $ text "NGOLogisticsCG Reflex Component"
