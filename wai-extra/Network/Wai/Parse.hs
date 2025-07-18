{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Some helpers for parsing data out of a raw WAI 'Request'.
module Network.Wai.Parse (
    parseHttpAccept,
    parseRequestBody,
    RequestBodyType (..),
    getRequestBodyType,
    sinkRequestBody,
    sinkRequestBodyEx,
    RequestParseException (..),
    BackEnd,
    lbsBackEnd,
    tempFileBackEnd,
    tempFileBackEndOpts,
    Param,
    File,
    FileInfo (..),
    parseContentType,
    ParseRequestBodyOptions,
    defaultParseRequestBodyOptions,
    noLimitParseRequestBodyOptions,
    parseRequestBodyEx,
    setMaxRequestKeyLength,
    clearMaxRequestKeyLength,
    setMaxRequestNumFiles,
    clearMaxRequestNumFiles,
    setMaxRequestFileSize,
    clearMaxRequestFileSize,
    setMaxRequestFilesSize,
    clearMaxRequestFilesSize,
    setMaxRequestParmsSize,
    clearMaxRequestParmsSize,
    setMaxHeaderLines,
    clearMaxHeaderLines,
    setMaxHeaderLineLength,
    clearMaxHeaderLineLength,
#if TEST
    Bound (..),
    findBound,
    sinkTillBound,
    killCR,
    killCRLF,
    takeLine,
#endif
) where

import Prelude hiding (lines)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import Control.Exception (catchJust)
import qualified Control.Exception as E
import Control.Monad (guard, unless, when)
import Control.Monad.Trans.Resource (
    InternalState,
    allocate,
    register,
    release,
    runInternalState,
 )
import Data.Bifunctor (bimap)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.CaseInsensitive (mk)
import Data.Function (fix, on)
import Data.IORef
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable
import Data.Word8
import Network.HTTP.Types (hContentType)
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp (InvalidRequest (..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.IO.Error (isDoesNotExistError)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

-- | Parse the HTTP accept string to determine supported content types.
parseHttpAccept :: S.ByteString -> [S.ByteString]
parseHttpAccept =
    map fst
        . sortBy (rcompare `on` snd)
        . map (addSpecificity . grabQ)
        . S.split _comma
  where
    rcompare :: (Double, Int) -> (Double, Int) -> Ordering
    rcompare = flip compare
    addSpecificity (s, q) =
        -- Prefer higher-specificity types
        let semicolons = S.count _semicolon s
            stars = S.count _asterisk s
         in (s, (q, semicolons - stars))
    grabQ s =
        -- Stripping all spaces may be too harsh.
        -- Maybe just strip either side of semicolon?
        let (s', q) = S.breakSubstring ";q=" (S.filter (/= _space) s)
            q' = S.takeWhile (/= _semicolon) (S.drop 3 q)
         in (s', readQ q')
    readQ s = case reads $ S8.unpack s of
        (x, _) : _ -> x
        _ -> 1.0

-- | Store uploaded files in memory
lbsBackEnd
    :: Monad m => ignored1 -> ignored2 -> m S.ByteString -> m L.ByteString
lbsBackEnd _ _ popper =
    loop id
  where
    loop front = do
        bs <- popper
        if S.null bs
            then return $ L.fromChunks $ front []
            else loop $ front . (bs :)

-- | Save uploaded files on disk as temporary files
--
-- Note: starting with version 2.0, removal of temp files is registered with
-- the provided @InternalState@. It is the responsibility of the caller to
-- ensure that this @InternalState@ gets cleaned up.
tempFileBackEnd
    :: InternalState -> ignored1 -> ignored2 -> IO S.ByteString -> IO FilePath
tempFileBackEnd = tempFileBackEndOpts getTemporaryDirectory "webenc.buf"

-- | Same as 'tempFileBackEnd', but use configurable temp folders and patterns.
tempFileBackEndOpts
    :: IO FilePath
    -- ^ get temporary directory
    -> String
    -- ^ filename pattern
    -> InternalState
    -> ignored1
    -> ignored2
    -> IO S.ByteString
    -> IO FilePath
tempFileBackEndOpts getTmpDir pattrn internalState _ _ popper = do
    (key, (fp, h)) <-
        flip runInternalState internalState $ allocate it (hClose . snd)
    _ <- runInternalState (register $ removeFileQuiet fp) internalState
    fix $ \loop -> do
        bs <- popper
        unless (S.null bs) $ do
            S.hPut h bs
            loop
    release key
    return fp
  where
    it = do
        tempDir <- getTmpDir
        openBinaryTempFile tempDir pattrn
    removeFileQuiet fp =
        catchJust
            (guard . isDoesNotExistError)
            (removeFile fp)
            (const $ return ())

-- | A data structure that describes the behavior of
-- the parseRequestBodyEx function.
--
-- @since 3.0.16.0
data ParseRequestBodyOptions = ParseRequestBodyOptions
    { prboKeyLength :: Maybe Int
    -- ^ The maximum length of a filename
    , prboMaxNumFiles :: Maybe Int
    -- ^ The maximum number of files.
    , prboMaxFileSize :: Maybe Int64
    -- ^ The maximum filesize per file.
    , prboMaxFilesSize :: Maybe Int64
    -- ^ The maximum total filesize.
    , prboMaxParmsSize :: Maybe Int
    -- ^ The maximum size of the sum of all parameters
    , prboMaxHeaderLines :: Maybe Int
    -- ^ The maximum header lines per mime/multipart entry
    , prboMaxHeaderLineLength :: Maybe Int
    -- ^ The maximum header line length per mime/multipart entry
    }

-- | Set the maximum length of a filename.
--
-- @since 3.0.16.0
setMaxRequestKeyLength
    :: Int -> ParseRequestBodyOptions -> ParseRequestBodyOptions
setMaxRequestKeyLength l p = p{prboKeyLength = Just l}

-- | Do not limit the length of filenames.
--
-- @since 3.0.16.0
clearMaxRequestKeyLength :: ParseRequestBodyOptions -> ParseRequestBodyOptions
clearMaxRequestKeyLength p = p{prboKeyLength = Nothing}

-- | Set the maximum number of files per request.
--
-- @since 3.0.16.0
setMaxRequestNumFiles
    :: Int -> ParseRequestBodyOptions -> ParseRequestBodyOptions
setMaxRequestNumFiles l p = p{prboMaxNumFiles = Just l}

-- | Do not limit the maximum number of files per request.
--
-- @since 3.0.16.0
clearMaxRequestNumFiles :: ParseRequestBodyOptions -> ParseRequestBodyOptions
clearMaxRequestNumFiles p = p{prboMaxNumFiles = Nothing}

-- | Set the maximum filesize per file (in bytes).
--
-- @since 3.0.16.0
setMaxRequestFileSize
    :: Int64 -> ParseRequestBodyOptions -> ParseRequestBodyOptions
setMaxRequestFileSize l p = p{prboMaxFileSize = Just l}

-- | Do not limit the maximum filesize per file.
--
-- @since 3.0.16.0
clearMaxRequestFileSize :: ParseRequestBodyOptions -> ParseRequestBodyOptions
clearMaxRequestFileSize p = p{prboMaxFileSize = Nothing}

-- | Set the maximum size of all files per request.
--
-- @since 3.0.16.0
setMaxRequestFilesSize
    :: Int64 -> ParseRequestBodyOptions -> ParseRequestBodyOptions
setMaxRequestFilesSize l p = p{prboMaxFilesSize = Just l}

-- | Do not limit the maximum size of all files per request.
--
-- @since 3.0.16.0
clearMaxRequestFilesSize :: ParseRequestBodyOptions -> ParseRequestBodyOptions
clearMaxRequestFilesSize p = p{prboMaxFilesSize = Nothing}

-- | Set the maximum size of the sum of all parameters.
--
-- @since 3.0.16.0
setMaxRequestParmsSize
    :: Int -> ParseRequestBodyOptions -> ParseRequestBodyOptions
setMaxRequestParmsSize l p = p{prboMaxParmsSize = Just l}

-- | Do not limit the maximum size of the sum of all parameters.
--
-- @since 3.0.16.0
clearMaxRequestParmsSize :: ParseRequestBodyOptions -> ParseRequestBodyOptions
clearMaxRequestParmsSize p = p{prboMaxParmsSize = Nothing}

-- | Set the maximum header lines per mime/multipart entry.
--
-- @since 3.0.16.0
setMaxHeaderLines :: Int -> ParseRequestBodyOptions -> ParseRequestBodyOptions
setMaxHeaderLines l p = p{prboMaxHeaderLines = Just l}

-- | Do not limit the maximum header lines per mime/multipart entry.
--
-- @since 3.0.16.0
clearMaxHeaderLines :: ParseRequestBodyOptions -> ParseRequestBodyOptions
clearMaxHeaderLines p = p{prboMaxHeaderLines = Nothing}

-- | Set the maximum header line length per mime/multipart entry.
--
-- @since 3.0.16.0
setMaxHeaderLineLength
    :: Int -> ParseRequestBodyOptions -> ParseRequestBodyOptions
setMaxHeaderLineLength l p = p{prboMaxHeaderLineLength = Just l}

-- | Do not limit the maximum header lines per mime/multipart entry.
--
-- @since 3.0.16.0
clearMaxHeaderLineLength :: ParseRequestBodyOptions -> ParseRequestBodyOptions
clearMaxHeaderLineLength p = p{prboMaxHeaderLineLength = Nothing}

-- | A reasonable default set of parsing options.
-- Maximum key/filename length: 32 bytes;
-- maximum files: 10; filesize unlimited; maximum
-- size for parameters: 64kbytes; maximum number of header
-- lines: 32 bytes (applies only to headers of a mime/multipart message);
-- maximum header line length: Apache's default for that is 8190 bytes
-- (http://httpd.apache.org/docs/2.2/mod/core.html#limitrequestline)
-- so we're using that here as well.
--
-- @since 3.0.16.0
defaultParseRequestBodyOptions :: ParseRequestBodyOptions
defaultParseRequestBodyOptions =
    ParseRequestBodyOptions
        { prboKeyLength = Just 32
        , prboMaxNumFiles = Just 10
        , prboMaxFileSize = Nothing
        , prboMaxFilesSize = Nothing
        , prboMaxParmsSize = Just 65336
        , prboMaxHeaderLines = Just 32
        , prboMaxHeaderLineLength = Just 8190
        }

-- | Do not impose any memory limits.
--
-- @since 3.0.21.0
noLimitParseRequestBodyOptions :: ParseRequestBodyOptions
noLimitParseRequestBodyOptions =
    ParseRequestBodyOptions
        { prboKeyLength = Nothing
        , prboMaxNumFiles = Nothing
        , prboMaxFileSize = Nothing
        , prboMaxFilesSize = Nothing
        , prboMaxParmsSize = Nothing
        , prboMaxHeaderLines = Nothing
        , prboMaxHeaderLineLength = Nothing
        }

-- | Information on an uploaded file.
data FileInfo c = FileInfo
    { fileName :: S.ByteString
    , fileContentType :: S.ByteString
    , fileContent :: c
    }
    deriving (Eq, Show)

-- | Post parameter name and value.
type Param = (S.ByteString, S.ByteString)

-- | Post parameter name and associated file information.
type File y = (S.ByteString, FileInfo y)

-- | A file uploading backend. Takes the parameter name, file name, and a
-- stream of data.
type BackEnd a =
    S.ByteString
    -- ^ parameter name
    -> FileInfo ()
    -> IO S.ByteString
    -> IO a

-- | The mimetype of the http body.
-- Depending on whether just parameters or parameters and files
-- are passed, one or the other mimetype should be used.
data RequestBodyType
    = -- | application/x-www-form-urlencoded (parameters only)
      UrlEncoded
    | -- | multipart/form-data (parameters and files)
      Multipart S.ByteString

-- | Get the mimetype of the body of an http request.
getRequestBodyType :: Request -> Maybe RequestBodyType
getRequestBodyType req = do
    ctype' <- lookup hContentType $ requestHeaders req
    let (ctype, attrs) = parseContentType ctype'
    case ctype of
        "application/x-www-form-urlencoded" -> return UrlEncoded
        "multipart/form-data" | Just bound <- lookup "boundary" attrs -> return $ Multipart bound
        _ -> Nothing

-- | Parse a content type value, turning a single @ByteString@ into the actual
-- content type and a list of pairs of attributes.
--
-- @since 1.3.2
parseContentType
    :: S.ByteString -> (S.ByteString, [(S.ByteString, S.ByteString)])
parseContentType a = do
    let (ctype, b) = S.break (== _semicolon) a
        attrs = goAttrs id $ S.drop 1 b
     in (ctype, attrs)
  where
    dq s =
        if S.length s > 2 && S.head s == _quotedbl && S.last s == _quotedbl
            then S.tail $ S.init s
            else s
    goAttrs front bs
        | S.null bs = front []
        | otherwise =
            let (x, rest) = S.break (== _semicolon) bs
             in goAttrs (front . (goAttr x :)) $ S.drop 1 rest
    goAttr bs =
        let (k, v') = S.break (== _equal) bs
            v = S.drop 1 v'
         in (strip k, dq $ strip v)
    strip = S.dropWhile (== _space) . fst . S.breakEnd (/= _space)

-- | Parse the body of an HTTP request.
-- See parseRequestBodyEx for details.
-- Note: This function does not limit the memory it allocates.
-- When dealing with untrusted data (as is usually the case when
-- receiving input from the internet), it is recommended to
-- use the 'parseRequestBodyEx' function instead.
--
-- since 3.1.15 : throws 'RequestParseException' if something goes wrong
parseRequestBody
    :: BackEnd y
    -> Request
    -> IO ([Param], [File y])
parseRequestBody = parseRequestBodyEx noLimitParseRequestBodyOptions

-- | Parse the body of an HTTP request, limit resource usage.
-- The HTTP body can contain both parameters and files.
-- This function will return a list of key,value pairs
-- for all parameters, and a list of key,a pairs
-- for filenames. The a depends on the used backend that
-- is responsible for storing the received files.
--
-- since 3.1.15 : throws 'RequestParseException' if something goes wrong
parseRequestBodyEx
    :: ParseRequestBodyOptions
    -> BackEnd y
    -> Request
    -> IO ([Param], [File y])
parseRequestBodyEx o s r =
    case getRequestBodyType r of
        Nothing -> return ([], [])
        Just rbt -> sinkRequestBodyEx o s rbt (getRequestBodyChunk r)

-- | since 3.1.15 : throws 'RequestParseException' if something goes wrong
sinkRequestBody
    :: BackEnd y
    -> RequestBodyType
    -> IO S.ByteString
    -> IO ([Param], [File y])
sinkRequestBody = sinkRequestBodyEx noLimitParseRequestBodyOptions

-- | Throws 'RequestParseException' if something goes wrong
--
-- @since 3.0.16.0
--
-- since 3.1.15 : throws 'RequestParseException' if something goes wrong
sinkRequestBodyEx
    :: ParseRequestBodyOptions
    -> BackEnd y
    -> RequestBodyType
    -> IO S.ByteString
    -> IO ([Param], [File y])
sinkRequestBodyEx o s r body = do
    ref <- newIORef ([], [])
    let add x = atomicModifyIORef ref $ \(y, z) ->
            case x of
                Left y' -> ((y' : y, z), ())
                Right z' -> ((y, z' : z), ())
    conduitRequestBodyEx o s r body add
    bimap reverse reverse <$> readIORef ref

conduitRequestBodyEx
    :: ParseRequestBodyOptions
    -> BackEnd y
    -> RequestBodyType
    -> IO S.ByteString
    -> (Either Param (File y) -> IO ())
    -> IO ()
conduitRequestBodyEx o _ UrlEncoded rbody add = do
    -- NOTE: in general, url-encoded data will be in a single chunk.
    -- Therefore, I'm optimizing for the usual case by sticking with
    -- strict byte strings here.
    let loop size front = do
            bs <- rbody
            if S.null bs
                then return $ S.concat $ front []
                else do
                    let newsize = size + S.length bs
                    case prboMaxParmsSize o of
                        Just maxSize ->
                            when (newsize > maxSize) $
                                E.throwIO $
                                    MaxParamSizeExceeded newsize
                        Nothing -> return ()
                    loop newsize $ front . (bs :)
    bs <- loop 0 id
    mapM_ (add . Left) $ H.parseSimpleQuery bs
conduitRequestBodyEx o backend (Multipart bound) rbody add =
    parsePiecesEx o backend (S8.pack "--" `S.append` bound) rbody add

-- | Take one header or subheader line.
-- Since:  3.0.26
--  Throw 431 if headers too large.
takeLine :: Maybe Int -> Source -> IO (Maybe S.ByteString)
takeLine maxlen src =
    go ""
  where
    go front = do
        bs <- readSource src
        case maxlen of
            Just maxlen' ->
                when (S.length front > maxlen') $
                    E.throwIO RequestHeaderFieldsTooLarge
            Nothing -> return ()
        if S.null bs
            then close front
            else push front bs

    close front = leftover src front >> return Nothing
    push front bs = do
        let (x, y) = S.break (== _lf) bs
         in if S.null y
                then go $ front `S.append` x
                else do
                    when (S.length y > 1) $ leftover src $ S.drop 1 y
                    let res = front `S.append` x
                    case maxlen of
                        Just maxlen' ->
                            when (S.length res > maxlen') $
                                E.throwIO RequestHeaderFieldsTooLarge
                        Nothing -> return ()
                    return . Just $ killCR res

-- | @since 3.1.15 : throws 'RequestParseException' if something goes wrong
takeLines' :: Maybe Int -> Maybe Int -> Source -> IO [S.ByteString]
takeLines' lineLength maxLines source =
    reverse <$> takeLines'' [] lineLength maxLines source

-- | @since 3.1.15 : throws 'RequestParseException' if something goes wrong
takeLines''
    :: [S.ByteString]
    -> Maybe Int
    -> Maybe Int
    -> Source
    -> IO [S.ByteString]
takeLines'' lines lineLength maxLines src = do
    case maxLines of
        Just maxLines' ->
            when (length lines > maxLines') $
                E.throwIO $
                    TooManyHeaderLines (length lines)
        Nothing -> return ()
    res <- takeLine lineLength src
    case res of
        Nothing -> return lines
        Just l
            | S.null l -> return lines
            | otherwise -> takeLines'' (l : lines) lineLength maxLines src

data Source = Source (IO S.ByteString) (IORef S.ByteString)

mkSource :: IO S.ByteString -> IO Source
mkSource f = do
    ref <- newIORef S.empty
    return $ Source f ref

readSource :: Source -> IO S.ByteString
readSource (Source f ref) = do
    bs <- atomicModifyIORef ref $ \bs -> (S.empty, bs)
    if S.null bs
        then f
        else return bs

{- HLint ignore readSource "Use tuple-section" -}

leftover :: Source -> S.ByteString -> IO ()
leftover (Source _ ref) = writeIORef ref

-- | @since 3.1.15 : throws 'RequestParseException' if something goes wrong
parsePiecesEx
    :: ParseRequestBodyOptions
    -> BackEnd y
    -> S.ByteString
    -> IO S.ByteString
    -> (Either Param (File y) -> IO ())
    -> IO ()
parsePiecesEx o sink bound rbody add =
    mkSource rbody >>= loop 0 0 0 0
  where
    loop :: Int -> Int -> Int -> Int64 -> Source -> IO ()
    loop numParms numFiles parmSize filesSize src = do
        _boundLine <- takeLine (prboMaxHeaderLineLength o) src
        res' <-
            takeLines'
                (prboMaxHeaderLineLength o)
                (prboMaxHeaderLines o)
                src
        unless (null res') $ do
            let ls' = map parsePair res'
            let x = do
                    cd <- lookup contDisp ls'
                    let ct = lookup contType ls'
                    let attrs = parseContentDispositionAttrs cd
                    name <- lookup "name" attrs
                    return (ct, name, lookup "filename" attrs)
            case x of
                Just (mct, name, Just filename) -> do
                    case prboKeyLength o of
                        Just maxKeyLength ->
                            when (S.length name > maxKeyLength) $
                                E.throwIO $
                                    FilenameTooLong name maxKeyLength
                        Nothing -> return ()
                    case prboMaxNumFiles o of
                        Just maxFiles ->
                            when (numFiles >= maxFiles) $
                                E.throwIO $
                                    MaxFileNumberExceeded numFiles
                        Nothing -> return ()
                    let ct = fromMaybe "application/octet-stream" mct
                        fi0 = FileInfo filename ct ()
                        fs =
                            catMaybes
                                [ prboMaxFileSize o
                                , subtract filesSize <$> prboMaxFilesSize o
                                ]
                        mfs = if null fs then Nothing else Just $ minimum fs
                    ((wasFound, fileSize), y) <- sinkTillBound' bound name fi0 sink src mfs
                    let newFilesSize = filesSize + fileSize
                    add $ Right (name, fi0{fileContent = y})
                    when wasFound $ loop numParms (numFiles + 1) parmSize newFilesSize src
                Just (_ct, name, Nothing) -> do
                    case prboKeyLength o of
                        Just maxKeyLength ->
                            when (S.length name > maxKeyLength) $
                                E.throwIO $
                                    ParamNameTooLong name maxKeyLength
                        Nothing -> return ()
                    let seed = id
                    let iter front bs = return $ front . (:) bs
                    ((wasFound, _fileSize), front) <-
                        sinkTillBound
                            bound
                            iter
                            seed
                            src
                            (fromIntegral <$> prboMaxParmsSize o)
                    let bs = S.concat $ front []
                    let x' = (name, bs)
                    let newParmSize = parmSize + S.length name + S.length bs
                    case prboMaxParmsSize o of
                        Just maxParmSize ->
                            when (newParmSize > maxParmSize) $
                                E.throwIO $
                                    MaxParamSizeExceeded newParmSize
                        Nothing -> return ()
                    add $ Left x'
                    when wasFound $
                        loop
                            (numParms + 1)
                            numFiles
                            newParmSize
                            filesSize
                            src
                _ -> do
                    -- ignore this part
                    let seed = ()
                        iter () _ = return ()
                    ((wasFound, _fileSize), ()) <- sinkTillBound bound iter seed src Nothing
                    when wasFound $ loop numParms numFiles parmSize filesSize src
      where
        contDisp = mk $ S8.pack "Content-Disposition"
        contType = mk $ S8.pack "Content-Type"
        parsePair s =
            let (x, y) = breakDiscard _colon s
             in (mk x, S.dropWhile (== _space) y)

-- | Things that could go wrong while parsing a 'Request'
--
-- @since 3.1.15
data RequestParseException
    = MaxParamSizeExceeded Int
    | ParamNameTooLong S.ByteString Int
    | MaxFileNumberExceeded Int
    | FilenameTooLong S.ByteString Int
    | TooManyHeaderLines Int
    deriving (Eq, Typeable)

instance E.Exception RequestParseException
instance Show RequestParseException where
    show = \case
        MaxParamSizeExceeded lmax -> unwords ["maximum parameter size exceeded:", show lmax]
        ParamNameTooLong s lmax -> unwords ["parameter name", S8.unpack s, "is too long:", show lmax]
        MaxFileNumberExceeded lmax -> unwords ["maximum number of files exceeded:", show lmax]
        FilenameTooLong fn lmax ->
            unwords ["file name", S8.unpack fn, "too long:", show lmax]
        TooManyHeaderLines nmax -> unwords ["Too many lines in mime/multipart header:", show nmax]

data Bound
    = FoundBound S.ByteString S.ByteString
    | NoBound
    | PartialBound
    deriving (Eq, Show)

findBound :: S.ByteString -> S.ByteString -> Bound
findBound b bs = handleBreak $ S.breakSubstring b bs
  where
    handleBreak (h, t)
        | S.null t = go [lowBound .. S.length bs - 1]
        | otherwise = FoundBound h $ S.drop (S.length b) t

    lowBound = max 0 $ S.length bs - S.length b

    go [] = NoBound
    go (i : is)
        | mismatch [0 .. S.length b - 1] [i .. S.length bs - 1] = go is
        | otherwise =
            let endI = i + S.length b
             in if endI > S.length bs
                    then PartialBound
                    else FoundBound (S.take i bs) (S.drop endI bs)
    mismatch [] _ = False
    mismatch _ [] = False
    mismatch (x : xs) (y : ys)
        | S.index b x == S.index bs y = mismatch xs ys
        | otherwise = True

sinkTillBound'
    :: S.ByteString
    -> S.ByteString
    -> FileInfo ()
    -> BackEnd y
    -> Source
    -> Maybe Int64
    -> IO ((Bool, Int64), y)
sinkTillBound' bound name fi sink src max' = do
    (next, final) <- wrapTillBound bound src max'
    y <- sink name fi next
    b <- final
    return (b, y)

data WTB
    = WTBWorking (S.ByteString -> S.ByteString)
    | WTBDone Bool
wrapTillBound
    :: S.ByteString
    -- ^ bound
    -> Source
    -> Maybe Int64
    -> IO (IO S.ByteString, IO (Bool, Int64))
    -- ^ Bool indicates if the bound was found
wrapTillBound bound src max' = do
    ref <- newIORef $ WTBWorking id
    sref <- newIORef (0 :: Int64)
    return (go ref sref, final ref sref)
  where
    final ref sref = do
        x <- readIORef ref
        case x of
            WTBWorking _ -> error "wrapTillBound did not finish"
            WTBDone y -> do
                siz <- readIORef sref
                return (y, siz)

    go ref sref = do
        state <- readIORef ref
        case state of
            WTBDone _ -> return S.empty
            WTBWorking front -> do
                bs <- readSource src
                cur <- atomicModifyIORef' sref $ \cur ->
                    let new = cur + fromIntegral (S.length bs) in (new, new)
                case max' of
                    Just max'' | cur > max'' -> E.throwIO PayloadTooLarge
                    _ -> return ()
                if S.null bs
                    then do
                        writeIORef ref $ WTBDone False
                        return $ front bs
                    else push $ front bs
      where
        push bs = do
            case findBound bound bs of
                FoundBound before after -> do
                    let before' = killCRLF before
                    leftover src after
                    writeIORef ref $ WTBDone True
                    return before'
                NoBound -> do
                    -- don't emit newlines, in case it's part of a bound
                    let (toEmit, front') =
                            if not (S8.null bs) && S8.last bs `elem` ['\r', '\n']
                                then
                                    let (x, y) = S.splitAt (S.length bs - 2) bs
                                     in (x, S.append y)
                                else (bs, id)
                    writeIORef ref $ WTBWorking front'
                    if S.null toEmit
                        then go ref sref
                        else return toEmit
                PartialBound -> do
                    writeIORef ref $ WTBWorking $ S.append bs
                    go ref sref

sinkTillBound
    :: S.ByteString
    -> (x -> S.ByteString -> IO x)
    -> x
    -> Source
    -> Maybe Int64
    -> IO ((Bool, Int64), x)
sinkTillBound bound iter seed0 src max' = do
    (next, final) <- wrapTillBound bound src max'
    let loop seed = do
            bs <- next
            if S.null bs
                then return seed
                else iter seed bs >>= loop
    seed <- loop seed0
    b <- final
    return (b, seed)

parseContentDispositionAttrs :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseContentDispositionAttrs = parseTokenValues
 where
    nonTokenChars = [_semicolon, _equal]
    dropSpace = S.dropWhile (== _space)
    parseTokenValues input | S.null input = []
    parseTokenValues input =
        let (token, rest) = parseToken $ dropSpace input
         in case S.uncons rest of
            Just (c, rest')
                | c == _equal -> 
                    let (value, rest'') = parseValue rest'
                     in (token, value) : parseTokenValues (S.drop 1 rest'')
                | otherwise -> (token, S.empty) : parseTokenValues rest'
            Nothing -> (token, S.empty) : parseTokenValues S.empty
    parseToken = S.break (`elem` nonTokenChars)
    parseValue input =
        case S.uncons $ dropSpace input of
            Just (c, rest) | c == _quotedbl -> parseQuotedString [] rest
            _ -> S.break (`elem` nonTokenChars) $ dropSpace input
    parseQuotedString acc input =
        let (prefix, rest) = S.break (`elem` [_quotedbl, _backslash]) input
         in case S.uncons rest of
            Just (c, rest')
                | c == _quotedbl -> (S.concat $ reverse (prefix:acc), rest')
                | c == _backslash ->
                    let (slashed, postSlash) = S.splitAt 1 rest'
                     in parseQuotedString (slashed:prefix:acc) postSlash
            _ -> (S.concat $ reverse (prefix:acc), rest)

killCRLF :: S.ByteString -> S.ByteString
killCRLF bs
    | S.null bs || S.last bs /= _lf = bs
    | otherwise = killCR $ S.init bs

killCR :: S.ByteString -> S.ByteString
killCR bs
    | S.null bs || S.last bs /= _cr = bs
    | otherwise = S.init bs
