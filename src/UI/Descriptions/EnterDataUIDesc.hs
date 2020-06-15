{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Descriptions.EnterDataUIDesc where

import Control.Lens
import Control.Monad.State

data UIDesc e a b
  = Desc
      { _title :: Title,
        _insertedText :: String,
        -- | Additional custom event handler
        _acceptInputHandler :: Maybe (String -> a -> b),
        _closeHandler :: Maybe (a -> b),
        -- | Additional custom event handler
        _customEventHandler :: Maybe (e -> a -> b)
      }
  deriving (Functor)

data Title = Title {_titleText :: String}

type Builder e a b = State (UIDesc e a b)

makeLenses ''UIDesc
makeLenses ''Title

defaultTitle :: Title
defaultTitle = Title ""

defaultUIDesc :: UIDesc e a b
defaultUIDesc =
  Desc
    { _title = defaultTitle,
      _insertedText = "",
      _acceptInputHandler = Nothing,
      _closeHandler = Nothing,
      _customEventHandler = Nothing
    }

makeUI :: Builder e a b c -> UIDesc e a b
makeUI = flip execState defaultUIDesc

setTitle :: String -> Builder e a b ()
setTitle str = modify $ set title (Title str)

addAcceptInputHandler :: (String -> a -> b) -> Builder e a b ()
addAcceptInputHandler f = modify $ set acceptInputHandler (Just f)

addCloseHandler :: (a -> b) -> Builder e a b ()
addCloseHandler f = modify $ set closeHandler (Just f)

addCustomEventHandler :: (e -> a -> b) -> Builder e a b ()
addCustomEventHandler f = modify $ set customEventHandler (Just f)

getTitle :: UIDesc e a b -> String
getTitle = view (title . titleText)

getInsertedText :: UIDesc e a b -> String
getInsertedText = _insertedText

addChar :: UIDesc e a b -> Char -> UIDesc e a b
addChar desc c = over insertedText (++ [c]) desc

removeChar :: UIDesc e a b -> UIDesc e a b
removeChar desc = over insertedText (reverse . drop 1 . reverse) desc