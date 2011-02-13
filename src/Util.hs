module Util (getParamNode_, getParamNodeText) where

import            Control.Monad (liftM)
import            Text.Templating.Heist (runNodeList, getParamNode, TemplateMonad)
import qualified  Text.XmlHtml as X
import qualified  Data.Text as T

-- | A version of `getParamNode` that expands child nodes
getParamNode_ :: (Monad m) => TemplateMonad m X.Node
getParamNode_ = getParamNode >>= runNodeListOnChildren
  where
    runNodeListOnChildren (X.Element t a c) = X.Element t a `liftM` runNodeList c
    runNodeListOnChildren node              = return node

getParamNodeText :: (Monad m) => TemplateMonad m String
getParamNodeText = (T.unpack . X.nodeText) `liftM` getParamNode_
