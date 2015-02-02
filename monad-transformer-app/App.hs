-- |
--  An example monad transformer stack for a Haskell app
--
--
-- = March
--
-- Next time, we'll talk about generating and working with
-- lenses with examples of working in the Monad transformer stack.
--
--
--
--
-- = Transformer basics
--
-- - Monads provide a way to build computations with effects.
--   However, each standard Monad only specializes in one thing.
--
-- - Often, combining these effects gives us powerful abstractions
--   to model the state of some domain.
--
-- - Monad transformers provide a powerful way to abstract
--   and combine effects. e.g. we want to add error handling to
--   a State Monad or Logging to a computation inside Reader Monad.
--
-- - Monad Transformers provide significant boilerplate reduction.
--
--
-- = Mtl and Transformers
--
-- - Two "base" monad transformer libraries: @mtl@ and @transformers@
--
-- - @mtl@ builds on @transformers@ and provides other common functionality
--  through heavy use of typeclasses and functional dependencies.
--
-- - @transformers@ on the other hand would be considered more low-level
--  and uses explicit types (usually newtype) and implement the 'MonadTrans'
--  typeclass. Using @transformers@ requires a bit more boilerplate
--  (e.g. lift'ing) but is more portable to non-ghc platforms.
--
-- - Generally, prefer @mtl@
--
--
-- = Modeling an App with Monad Transformers
--
-- 1) All haskell problems have the same type require @main :: IO a@.
--   Therefore, all haskell programs have a type @:: IO a@
--   Well, what happens in between?
--
-- 2) Meaningful programs usually interact with something
--    in the outside world.
--
-- 3) We can use types to describe how our program interacts
--    with the world, and itself.
--
-- 4) Types don't just describe data, they also describe data
--    _transformations_. First class functions!
--
-- 5) Think about Monad Transformers as a way in which we can
--    control and even constrain the effects produced by our
--    program
--
--
-- = BMark
--
-- Following is an example of modeling a command line bookmark
-- program with Monad transformers. We use a simple "Command"
-- pattern.
--
-- This implementation lightly brushes on some other Haskell
-- design patterns such as Command and Interpreter pattern
--
--
-- = MonadTrans typeclass
--
------------------------------------------------------------------------
-- | The class of monad transformers.  Instances should satisfy the laws
--
-- * @'lift' . 'return' = 'return'@
--
-- * @'lift' (m >>= f) = 'lift' m >>= ('lift' . f)@
--
-- class MonadTrans t where
--       -- | Lift a computation from the argument monad to the constructed monad.
--       lift :: Monad m => m a -> t m a
--
--       -- | Monads in which 'IO' computations may be embedded.
--       -- Any monad built by applying a sequence of monad transformers to the
--       -- 'IO' monad will be an instance of this class.
--
-- class (Monad m) => MonadIO m where
--     -- | Lift a computation from the 'IO' monad.
--     liftIO :: IO a -> m a
--
-- instance MonadIO IO where
--     liftIO = id
--
------------------------------------------------------------------------



{-# LANGUAGE GeneralizedNewtypeDeriving #-}


import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           System.Environment




-- The first trick to doing this is to try to capture the
-- essence of the domain you are modelling.
--
-- In this case we are working with a csv parser. This will
-- loosely translate to a CSV Domain Specific Language


-- | Actions the program runs
data BMarkCmd
    = List
      -- ^ list bookmarks
    | Add String FilePath
      -- ^ add a bookmark by name
    | Visit String
      -- ^ Visit a bookmark



-- Next, what does our app _need_ to run that might otherwise
-- be considered global state, or impure?
--
-- Parameterizing these values can offer flexibility


-- | BMark configuration.
--
-- Holds the 'FilePath' of the config file.
data BMarkConfig = BMarkConfig FilePath


-- | BMark State
--
-- Holds an association list of bookmark names corresponding to
-- their location on disk. Could be a 'Map'
data BMarkState = BMarkState  [(String, FilePath)]




-- The standard Monad transformers, by convention, generally
-- append a T to their underlying Monad.
--
-- e.g. StateT is State which provides mutable state to the
-- underlying Monad.


-- Example of a type synonym Monad Transformer
--
-- Problem is it becomes difficult to encapsulate the functionality
--
-- type BMark = ReaderT BMarkConfig (StateT BMarkState IO ())


-- | Newtype'd Monad Transformer
--
-- We don't need to explicity derive instances due to the "isomorphism"
--
-- Newtype is simply a wrapper around another type which provides the
-- convenience of creating a distinct new type while still being able to
-- be optimized since it's restricted to a single field.
--
-- newtype provides significantly more encapsulation and gives us a
-- constructor to hide or expose
--
--
-- Notice how the 'IO' Monad is at the bottom of the "stack". We can replace
-- this with a normal Monad instance if the computations do not require 'IO'.
--
--
-- ReaaderT gives us read only configuration environment
--
--
-- StateT gives mutable state. @newtype StateT s m a@
--
-- * s - The state
--
-- * m - The inner monad
--
-- * a - result of computation
--
-- IO is the base Monad and lets us run IO actions
--
newtype BMark a
  = BMark { unBMark :: ReaderT BMarkConfig (StateT BMarkState IO) a }
    deriving ( -- We provide instances for our type so it can be derived properly
               Functor
             , Applicative
             , Monad
             -- , MonadIO  show liftIO and not
             , MonadReader BMarkConfig
             , MonadState BMarkState
             )




-- | Default config
--
-- The .bmark file could also be "read" into a more
-- complex data structure (or parsed from cmd line)
defaultBMarkConfig = BMarkConfig "~/.bmark"



-- | Initial State which is empty
initBMarkState = BMarkState []



-- |
-- In the framework that mtl provides, each monad transformer in the
-- stack makes the API of a lower level available by providing instances
-- of a host of typeclasses. We could follow this pattern, and write a
-- number of boilerplate instances.
runBMark :: BMarkConfig -> BMark a -> IO (a, BMarkState)
runBMark cfg app = runStateT (runReaderT (unBMark app) cfg) initBMarkState


-- Alternatively, initialize and deinitialize the app, saving state afterwareds
-- TODO use let ... in to initialize state from config file and write state back
-- out at the end
-- runBMark app =
--   let cfg = BMarkConfig ""
--       -- st  = BMarkState []
--   in loadState cfg
--      >>= runStateT (runReaderT (unBMark app) cfg)
--      >>= saveState



runBMarkCmd :: BMarkConfig -> BMarkCmd -> IO (a, BMarkState)
runBMarkCmd cfg cmd = runStateT (runReaderT (unBMark (go cmd)) cfg) initBMarkState
  where
    go List      = listBookmarks
    go (Add f n) = addBookmark f n
    go (Visit n) = visitBookmark n




-- | An example "run" function
--
-- Commands can be a good way to describe the transformation of an application
--
-- Applications can become vastly complex. This pattern helps us drastically
-- reduce boilerplate and enforce certain restrictions as functions propogate
-- the transformer stack
--
-- Notice that we have access to our app State and Config from any point,
-- without the need to explicity pass it around. The Monad Transformer laws
-- give us equational reasoning power.
listBookmarks :: BMark a
listBookmarks = do

    -- Now we have access to both Monad layers
    --
    -- We can, essentially

    --
    -- When using combined monads created with monad transformers, we
    -- avoid having to manage the inner monad types explicitly, and our
    -- result is clearer, simpler code. Instead of creating additional
    -- do-blocks within the computation to manipulate values in the inner
    -- monad type, we can use lifting operations to bring functions from the
    -- inner monad into the combined monad.

    BMarkConfig _ <- ask
    BMarkState  _ <- get
    error "Listing Bookmarks"


addBookmark :: String -> FilePath -> BMark a
addBookmark name fp = do
    BMarkConfig _ <- ask
    BMarkState  _ <- get
    error $ "Adding Bookmark: " ++ name ++ " -> " ++ fp



visitBookmark :: String -> BMark a
visitBookmark b = do
    BMarkConfig _ <- ask
    BMarkState  _ <- get
    error $ "Visiting Bookmark: " ++ b



main :: IO ()
main = do
  cmds <- getArgs
  case cmds of
    ["list"]       -> runBMarkCmd defaultBMarkConfig List
    ["add", f, n]  -> runBMarkCmd defaultBMarkConfig (Add f n)
    ["visit", f]   -> runBMarkCmd defaultBMarkConfig (Visit f)
    _              -> error "Uknown bmark command"
  return ()
