{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Data.BridgeSupport.Parse
    ( readBridgeSupport
    , readBridgeSupport_1_0
    ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Data.BridgeSupport.Types
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Text.XML.Light

ignoreLeftovers = False

readBridgeSupport :: Element -> Either String BridgeSupport
readBridgeSupport signatures = do
    when (elName signatures /= unqual "signatures") $ do
        throwError "readBridgeSupport: root element is not <signatures>"
    
    let mbVersion = findAttr (unqual "version") signatures
        version   = fromMaybe defaultVersion mbVersion
    
    case lookup version readers of
        Just reader -> process "readBridgeSupport" (elContent signatures) reader
        Nothing     -> throwError ("Unknown BridgeSupport version: " ++ show version)

defaultVersion = "1.0"

readers = [ ("1.0", readBridgeSupport_1_0) ]

process cxt xs reader = do
    (ys, unhandled) <- runStateT (select (not . isElement) >> reader) xs
    if null unhandled || ignoreLeftovers
        then return ys
        else throwError $ unlines
                ((cxt ++ ": " ++ show (length unhandled) ++ " items left over:") 
                    : map ppContent unhandled)

selectJust p = do
    st <- get
    let (reject, accept) = partitionEithers 
            [ maybe (Left x) Right (p x) | x <- st ]
    put reject
    return accept

select p = selectJust p'
    where p' x = if p x then Just x else Nothing

isElement Elem{} = True
isElement _      = False

findAttr' attr = findAttr (unqual attr)

getRequiredAttr attr e = maybe err return (findAttr' attr e)
    where err = throwError (ppElement e ++ " element missing required " ++ show attr ++ " attribute")

getOptionalAttr attr = return . findAttr' attr

getAttr def attr = maybe (return def) return . findAttr' attr

getBoolAttr def attr e = case findAttr' attr e of
    Nothing      -> return def
    Just "true"  -> return True
    Just "false" -> return False
    Just other   -> throwError ("Invalid boolean value: " ++ show other)

getIntAttr def attr e = case findAttr' attr e of
    Nothing     -> return def
    Just str    -> case reads str of
        (i,""):_    -> return (i :: Int)
        _           -> throwError ("Invalid int value for attribute " ++ show attr ++ ": " ++ show str)

getSizedAttr getThing label e = do
    mb32 <- getThing  label          e
    mb64 <- getThing (label ++ "64") e
    
    return $! case (mb32, mb64) of
        (Just m32, Just m64) -> Just Sized{..}
        (Just m32, Nothing)  -> Just (Sized m32 m32)
        (Nothing,  Just m64) -> Just (Sized m64 m64)
        (Nothing, Nothing)   -> Nothing

selectElems name = selectJust p
    where
        p (Elem e)
            | elName e == unqual name   = Just e
        p _                             = Nothing

readBridgeSupport_1_0 :: StateT [Content] (Either String) BridgeSupport
readBridgeSupport_1_0 = do
    _ignore <- select (not . isElement)
    
    dependsOn           <- getDependencies
    structs             <- getStructs
    opaqueTypes         <- getOpaqueTypes
    constants           <- getConstants
    cfTypes             <- getCFTypes
    stringConstants     <- getStringConstants
    enumTypes           <- getEnumTypes
    functions           <- getFunctions
    informalProtocols   <- getClassInterface "informal_protocol"
    classes             <- getClassInterface "class"
    
    return BridgeSupport {..}

getDependencies = mapM (getRequiredAttr "path") =<< selectElems "depends_on"

getTypeInfo e = do
    name   <- getRequiredAttr "name" e
    m32 <- getRequiredAttr "type" e
    m64 <- getAttr m32 "type64" e
    let typeEncoding = Sized{..}
    return TypeInfo{..}

getStructs = do
    structElems <- selectElems "struct"
    sequence 
        [ do
            structType   <- getTypeInfo e
            structOpaque <- getBoolAttr False "opaque" e
            
            return Struct{..}
        | e <- structElems
        ]

getOpaqueTypes = do
    mapM getTypeInfo =<< selectElems "opaque"

getConstants = do
    constantElems <- selectElems "constant"
    sequence
        [ do
            TypeInfo constantName constantType <- getTypeInfo e
            magicCookie <- getBoolAttr False "magic_cookie" e
            return Constant{..}
        | e <- constantElems
        ]

getCFTypes = do
    cfTypeElems <- selectElems "cftype"
    sequence
        [ do
            cfTypeInfo      <- getTypeInfo e
            let getTypeID_func  = findAttr' "gettypeid_func" e
                tollFree        = findAttr' "tollfree"       e
            return CFType{..}
        | e <- cfTypeElems
        ]

getStringConstants = do
    constantElems <- selectElems "string_constant"
    sequence
        [ do
            stringName  <- getRequiredAttr "name"    e
            m32         <- getRequiredAttr "value"   e
            m64         <- getAttr m32     "value64" e
            let stringValue = Sized{..}
            stringNSString <- getBoolAttr False "nsstring" e
            return StringConstant{..}
        | e <- constantElems
        ]

getEnumTypes = do
    enumElems <- selectElems "enum"
    sequence
        [ do
            enumName        <- getRequiredAttr   "name"    e
            mbEnumValue     <- getSizedAttr getOptionalAttr "value" e
            let noValue = "No enum value specified for " ++ enumName 
            enumValue       <- maybe (throwError noValue) return mbEnumValue
            ignoreEnum      <- getBoolAttr False "ignore"  e
            enumSuggestion  <- getOptionalAttr "suggestion" e
            return EnumType{..}
        | e <- enumElems
        ]

getFunctions = do
    functionElems       <- selectElems "function"
    aliases             <- getAliases
    fns <- sequence
        [ do
            functionName <- getRequiredAttr   "name"     e
            let functionAliases = fromMaybe [] (M.lookup functionName aliases)
            isVariadic   <- getBoolAttr False "variadic" e
            isInline     <- getBoolAttr False "inline"   e
            sentinel     <- getOptionalAttr   "sentinel" e
            
            (functionArgs, functionReturn) 
                <- process (functionName ++ " function") (elContent e) getSignature
            return Function{..}
        | e <- functionElems
        ]
    
    let missingFunctions = M.keys aliases \\ map functionName fns
    when (not (null missingFunctions)) $
        throwError ("function alias(es) reference non-existent function"
            ++ if null (drop 1 missingFunctions) then "" else "s"
            ++ " " ++ intercalate ", " missingFunctions)
    
    return fns

getAliases = do
    aliasElements <- selectElems "function_alias"
    M.fromListWith (++) <$> sequence
        [ do
            name <- getRequiredAttr "name"     e
            orig <- getRequiredAttr "original" e
            return (orig, [name])
        | e <- aliasElements
        ]

getSignature :: MonadError String m => StateT [Content] m ([Arg], Maybe Return)
getSignature = liftM2 (,) getArgs getReturn

getIndex e = do
    previous <- get
    ix <- lift (getIntAttr (previous + 1) "index" e)
    put    ix
    return ix

getTypeModifier e = case findAttr' "type_modifier" e of
    Nothing     -> return Nothing
    Just "n"    -> return (Just In)
    Just "o"    -> return (Just Out)
    Just "N"    -> return (Just InOut)
    other   -> throwError ("Unknown type type_modifier: " ++ show other)

getArgs :: MonadError String m => StateT [Content] m [Arg]
getArgs = do
    argElems <- selectElems "arg"
    flip evalStateT 0 $ sequence
        [ do
            argIndex <- getIndex e
            
            lift $ do
                argDirection <- getTypeModifier e
                argArrayLengthInArg <- getOptionalAttr "c_array_length_in_arg"        e
                argArrayLengthInRet <- getOptionalAttr "c_array_length_in_retval"     e
                argArrayFixedLength <- getOptionalAttr "c_array_of_fixed_length"      e
                argArrayNullDelim   <- getBoolAttr False "c_array_delimited_by_null"  e
                argArrayVariableLen <- getBoolAttr False "c_array_of_variable_length" e
                nullAccepted        <- getBoolAttr False "null_accepted"              e
                argType             <- getSizedAttr getOptionalAttr "type"            e
                argIsFunPtr         <- getBoolAttr False "function_pointer"           e
                argIsPrintfFormat   <- getBoolAttr False "printf_format"              e
                selOfType           <- getSizedAttr getOptionalAttr "sel_of_type"     e
                
                (argArgs, argReturn)
                    <- process "function argument" (elContent e) getSignature
                return Arg{..}
        | e <- argElems
        ]

getReturn :: MonadError String m => StateT [Content] m (Maybe Return)
getReturn = do
    returnElems <- selectElems "retval"
    liftM listToMaybe $ sequence
        [ do
            retArrayLengthInArg <- getOptionalAttr "c_array_length_in_arg"        e
            retArrayFixedLength <- getOptionalAttr "c_array_of_fixed_length"      e
            retArrayNullDelim   <- getBoolAttr False "c_array_delimited_by_null"  e
            retArrayVariableLen <- getBoolAttr False "c_array_of_variable_length" e
            alreadyRetained     <- getBoolAttr False "already_retained"           e
            mbReturnType        <- getSizedAttr getOptionalAttr "type"            e
            let noRet = "No return type specified in element: " ++ show e
            returnType          <- maybe (throwError noRet) return mbReturnType
            retIsFunPtr         <- getBoolAttr False "function_pointer"           e
            
            (retArgs, retReturn)
                <- process "function argument" (elContent e) getSignature
            
            return Return{..}
        | e <- returnElems
        ]

getClassInterface elemName = do
    classElements <- selectElems elemName
    sequence
        [ do
            className       <- getRequiredAttr "name" e
            classMethods    <- process (elemName ++ " " ++ className) (elContent e) getMethods
            return ClassInterface{..}
        | e <- classElements
        ]

getMethods = do
    methodElems <- selectElems "method"
    sequence
        [ do
            selector            <- getRequiredAttr "selector"         e
            methodType          <- Sized <$> getOptionalAttr "type"   e
                                         <*> getOptionalAttr "type64" e
            isClassMethod       <- getBoolAttr False "class_method"   e
            ignoreMethod        <- getBoolAttr False "ignore"         e
            methodSuggestion    <- getOptionalAttr "suggestion"       e
            methodIsVariadic    <- getBoolAttr False "variadic"       e
            methodSentinel      <- getOptionalAttr "sentinel"         e
            
            (methodArgs, methodReturn)
                <- process "function argument" (elContent e) getSignature
            return Method{..}
        | e <- methodElems
        ]