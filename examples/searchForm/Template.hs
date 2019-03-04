{-# LANGUAGE OverloadedStrings #-}

module Template ( appStateToBrickAppState
                , Name (..)
                , searchForm
                , handleSearchChange
                ) where

import           Control.Lens
import           Data.Text

import qualified Graphics.Vty          as V
import           Reflex.Brick
import           Brick
import qualified Brick.Widgets.Border  as B
import qualified Brick.Widgets.Center  as C
import qualified Brick.Widgets.Edit    as WE
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , renderForm
  , handleFormEvent
  , focusedFormInputAttr
  , invalidFormInputAttr
  , editTextField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )

import Data 

searchForm :: Text -> Form FormState e Name
searchForm searchText = newForm [ editTextField fsf SearchField (Just 1) ] (FormState searchText)

handleSearchChange :: BrickEvent Name BrickAppEvent
                     -> AppState -> EventM Name (Next AppState)
handleSearchChange ev s = do
  fs' <- handleFormEvent ev (s ^. searchFormState)
  -- emit event for Reflex ?
  continue $ s & searchFormState .~ fs'

appStateToBrickAppState :: AppState -> ReflexBrickAppState Name
appStateToBrickAppState s = ReflexBrickAppState [window] (focusRingCursor formFocus (searchForm "placeholder")) stylesMap 
    where 
        window  =  B.borderWithLabel (str "Search on Hackage") $ 
                      hBox [ usageWidget
                           , C.hCenter $ padAll 1 $ vBox 
                                  [ renderForm $ s ^. searchFormState
                                  , hBox [ packagesListWidget ]
                                  ]
                           ] 
        packageWidget p    = hBox [ txt $ p ^. name ]
        packagesListWidget = vBox [ C.hCenter $ vBox (fmap packageWidget (s ^. packages))]
        usageWidget        = B.borderWithLabel (str "Usage") $ 
          vBox [ txt "<ENTER> : search"
               , txt "<ESC>   : quit"
               ]

stylesMap :: AttrMap
stylesMap = attrMap V.defAttr
  [ (WE.editAttr, V.white `on` V.black)
  , (WE.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]
