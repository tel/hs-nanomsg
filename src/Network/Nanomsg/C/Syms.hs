
module Network.Nanomsg.C.Syms (

  -- * Symbol lookup

  -- While we could inline symbols from the header files using HSC,
  -- Nanomsg recommends for portability that the function @nn_symbol@
  -- is used to download the entire (constant) mapping between symbol
  -- names and numbers. We follow that method here by creating
  -- (unsafely) a constant 'symMap' (unexported) and implementing
  -- 'getSym' as an interface.

  getSym
  
  ) where

import Control.Applicative
import Control.Monad

import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import Foreign.C.String

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map as M

-- | Search through the Nanomsg symbol database via 'nn_symbol' and
-- create a mapping from names to constants. If the name isn't found,
-- raises an error as this should be a static guarantee.
symMap :: M.Map String CInt
symMap = m
  where
    m :: M.Map String CInt
    m = unsafePerformIO $ M.fromList <$> whileJustM [0..] get'
    
    get' n = alloca $ \ptr -> do
      cstr <- nn_symbol n ptr
      case cstr == nullPtr of
        True -> return Nothing
        False -> do
          val  <- peek ptr
          str  <- peekCString cstr
          return $ Just (str, val)

    whileJustM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
    whileJustM []       _   = return []
    whileJustM (x : xs) act = do
      res <- act x
      case res of
        Nothing -> return []
        Just v  -> (v:) `liftM` whileJustM xs act
{-# NOINLINE symMap #-}

getSym :: String -> CInt
getSym = (symMap M.!)
{-# NOINLINE getSym #-}


-- CCalls
---------

-- Symbols

foreign import ccall unsafe "nn.h nn_symbol"
  nn_symbol :: CInt -> Ptr CInt -> IO (Ptr CChar)
