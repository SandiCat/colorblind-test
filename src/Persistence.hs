module Persistence where

import qualified Data.Aeson as Aeson
import System.Directory (listDirectory)
import System.FilePath
import CirclePacking 

saveSolutions :: Aeson.ToJSON a => FilePath -> [[Circle a]] -> IO ()
saveSolutions path solutions =
    mapM_ (\(ix,solution) -> Aeson.encodeFile
        (path </> show ix <.> "json")
        solution) $ zip [1 ..] solutions

loadSolutions ::Aeson.FromJSON a => FilePath -> IO [[Circle a]]
loadSolutions path =
    (listDirectory path
    >>= mapM (readFileLBS . (path </>)))
    <&> catMaybes . map Aeson.decode