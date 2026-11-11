{-# LANGUAGE TemplateHaskell #-}

{- | The JavaScript that every page needs, built into the hastea command so
that an application only has to bring its own HTML and styles. The files
are in @runtime\/@; see the README there.
-}
module Hastea.Cli.Runtime
  ( loader
  , wasiShim
  , devLoader
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)


-- | @index.js@: instantiates the module and starts the application.
loader :: ByteString
loader = $(makeRelativeToProject "runtime/index.js" >>= embedFile)


-- | @browser_wasi_shim.js@: the WASI system calls the module imports.
wasiShim :: ByteString
wasiShim = $(makeRelativeToProject "runtime/browser_wasi_shim.js" >>= embedFile)


{- | What the page loads in @hastea dev@ instead of @index.js@: it boots GHCi
in the page and follows what the hastea command reports.
-}
devLoader :: ByteString
devLoader = $(makeRelativeToProject "runtime/dev.js" >>= embedFile)
