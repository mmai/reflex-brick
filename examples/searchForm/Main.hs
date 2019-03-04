{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad (void)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.IO.Class
import           Control.Concurrent (forkIO)
import           Control.Lens
import           Data.Text

import           Reflex
import           Reflex.Brick
import qualified Brick as B
import qualified Graphics.Vty as V

import           Data
import           APIClient
import           Template ( appStateToBrickAppState, Name(..), searchForm, handleSearchChange )

main :: IO ()
main = do
  let initial = AppState 
        -- { _search = "reflex"
        { _searchFormState = searchForm "reflex"
        , _packages = []
        }
  apiclient <- getClient
  runReflexBrickApp (pure ()) Nothing (appStateToBrickAppState initial :: ReflexBrickAppState Name) handleSearchChange $ \es -> do
    let eQuit   = select es $ RBKey V.KEsc
        eSearch = const "brick" <$> select es ( RBKey V.KEnter ) -- => laisser brick nous donner l'évenement "search launched on 'my search term'" 
        eWrite =  select es ( RBKey (V.KChar 'f') ) -- select all other keypresses ?

    -------------- trying to construct searchPackages Event

    -- -- simple : NOK, doesn't compile
    -- let eSearchPackages = liftIO . searchPackagesIO apiclient <$> eSearch

    -- performEvent : NOK events are triggered at the next "real" event 
    eSearchPackages <- performEvent $ liftIO . searchPackagesIO apiclient <$> eSearch

    -- -- performAsync : NOK  events are triggered at the next "real" event 
    -- eSearchPackages <- performAsync eSearch (searchPackagesIO apiclient)

    -- search string parameter fixed : OK, but the search term is fixed...
    -- searchPackages <- liftIO $ searchPackagesIO apiclient "reflex" 
    -- let eSearchPackages = searchPackages  <$ eSearch

    -------------- end trying to construct searchPackages Event

    -- let eUpdateForm = handleSearchForm <$> eWrite
    -- let eUpdateForm = handleSearchForm (B.VtyEvent (V.EvKey (V.KChar 'f') [])) <$ eWrite

    -- dState <- liftIO $ foldDyn (=<<) (pure initial) eUpdateForm

    dState <- foldDyn ($) initial .
                mergeWith (.) $ 
                  [ eSearchPackages
                  -- , eUpdateForm
                  ]


    let eNotQuit = difference (updated dState) eQuit 
        eOut     = appStateToBrickAppState <$> eNotQuit
        -- eOut     = fmap appStateToBrickAppState <$> eNotQuit -- :: Event t (B.EventM Name (ReflexBrickAppState Name))

    pure $ ReflexBrickApp eOut never (void eQuit)

searchPackagesIO :: APIClient -> String -> IO (AppState -> AppState)
searchPackagesIO api search = (packages .~) <$> apiSearchPackages search api 

-- Wrapper for performEventAsync where the function is called in another thread
forkEventAsync :: (MonadIO (Performable m), TriggerEvent t m, PerformEvent t m) => Event t a -> (a -> (b -> IO ()) -> IO ()) -> m (Event t b)
forkEventAsync e f = performEventAsync $ ffor e $ \a cb -> liftIO ( void $ forkIO $ f a cb )

performAsync ::  (MonadIO (Performable m), TriggerEvent t m, PerformEvent t m) => Event t a -> (a -> IO b) -> m (Event t b)
performAsync e f = forkEventAsync e (\a cb -> f a >>= cb) 
