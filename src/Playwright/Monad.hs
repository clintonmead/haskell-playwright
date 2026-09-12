module Playwright.Monad
  ( Playwright
  , askConnection
  , runPlaywrightWith
  ) where

import Control.Monad.Catch
  ( MonadThrow
  )
import Control.Monad.IO.Class
  ( MonadIO
  )
import Control.Monad.Reader
  ( ReaderT (ReaderT)
  , ask
  , runReaderT
  )
import Playwright.Connection.Types
  ( Connection
  )
import UnliftIO
  ( MonadUnliftIO
  )

newtype Playwright a = Playwright (ReaderT Connection IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadUnliftIO)

runPlaywrightWith :: Connection -> Playwright a -> IO a
runPlaywrightWith conn (Playwright action) =
  runReaderT action conn

askConnection :: Playwright Connection
askConnection =
  Playwright ask
