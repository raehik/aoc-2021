module Aoc2021.Runner where

import           Control.Monad.IO.Class
import qualified Data.Text.IO           as Text
import           Data.Text              ( Text )

runFile :: MonadIO m => (Text -> a) -> FilePath -> m a
runFile f fp = f <$> liftIO (Text.readFile fp)
