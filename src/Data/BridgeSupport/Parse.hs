{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Data.BridgeSupport.Parse
    ( readBridgeSupport
    , readBridgeSupport_1_0
    ) where

import Control.Monad.Error
import Control.Monad.State
import Data.BridgeSupport.Types
import Data.Either
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
    (ys, unhandled) <- runStateT reader xs
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

getRequiredAttr attr e = maybe err return (findAttr attr e)
    where err = throwError (ppElement e ++ " element missing required " ++ show attr ++ " attribute")

getAttr def attr = maybe (return def) return . findAttr attr

getBoolAttr def attr e = case findAttr attr e of
    Nothing      -> return def
    Just "true"  -> return True
    Just "false" -> return False
    Just other   -> throwError ("Invalid boolean value: " ++ show other)

getIntAttr def attr e = case findAttr attr e of
    Nothing     -> return def
    Just str    -> case reads str of
        (i,""):_    -> return (i :: Int)
        _           -> throwError ("Invalid int value for attribute " ++ show attr ++ ": " ++ show str)

selectElems name = selectJust p
    where
        p (Elem e)
            | elName e == name  = Just e
        p _                     = Nothing

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
    functionAliases     <- getAliases
    informalProtocols   <- getClassInterface "informal_protocol"
    classes             <- getClassInterface "class"
    
    return BridgeSupport {..}

getDependencies = mapM (getRequiredAttr (unqual "path")) =<< selectElems (unqual "depends_on")

getTypeInfo e = do
    name   <- getRequiredAttr (unqual "name") e
    m32 <- getRequiredAttr (unqual "type") e
    m64 <- getAttr m32 (unqual "type64") e
    let typeEncoding = Sized{..}
    return TypeInfo{..}

getStructs = do
    structElems <- selectElems (unqual "struct")
    sequence 
        [ do
            structType   <- getTypeInfo e
            structOpaque <- getBoolAttr False (unqual "opaque") e
            
            return Struct{..}
        | e <- structElems
        ]

getOpaqueTypes = do
    mapM getTypeInfo =<< selectElems (unqual "opaque")

getConstants = do
    constantElems <- selectElems (unqual "constant")
    sequence
        [ do
            TypeInfo constantName constantType <- getTypeInfo e
            magicCookie <- getBoolAttr False (unqual "magic_cookie") e
            return Constant{..}
        | e <- constantElems
        ]

getCFTypes = do
    cfTypeElems <- selectElems (unqual "cftype")
    sequence
        [ do
            cfTypeInfo      <- getTypeInfo e
            let getTypeID_func  = findAttr (unqual "gettypeid_func") e
                tollFree        = findAttr (unqual "tollfree")       e
            return CFType{..}
        | e <- cfTypeElems
        ]

getStringConstants = do
    constantElems <- selectElems (unqual "string_constant")
    sequence
        [ do
            stringName  <- getRequiredAttr (unqual "name")    e
            m32         <- getRequiredAttr (unqual "value")   e
            m64         <- getAttr m32     (unqual "value64") e
            let stringValue = Sized{..}
            stringNSString <- getBoolAttr False (unqual "nsstring") e
            return StringConstant{..}
        | e <- constantElems
        ]

getEnumTypes = do
    enumElems <- selectElems (unqual "enum")
    sequence
        [ do
            enumName    <- getRequiredAttr   (unqual "name")    e
            let m32 = findAttr (unqual "value")   e
                m64 = findAttr (unqual "value64") e
                enumValue = Sized{..}
            ignoreEnum  <- getBoolAttr False (unqual "ignore")  e
            let enumSuggestion = findAttr (unqual "suggestion") e
            return EnumType{..}
        | e <- enumElems
        ]

getFunctions = do
    functionElems <- selectElems (unqual "function")
    sequence
        [ do
            functionName <- getRequiredAttr   (unqual "name")     e
            isVariadic   <- getBoolAttr False (unqual "variadic") e
            isInline     <- getBoolAttr False (unqual "inline")   e
            let sentinel = findAttr           (unqual "sentinel") e
            
            (functionArgs, functionReturn) 
                <- process (functionName ++ " function") (elContent e) getSignature
            return Function{..}
        | e <- functionElems
        ]

getSignature :: MonadError String m => StateT [Content] m ([Arg], Maybe Return)
getSignature = liftM2 (,) getArgs getReturn

getIndex e = do
    previous <- get
    ix <- lift (getIntAttr (previous + 1) (unqual "index") e)
    put    ix
    return ix

getTypeModifier e = case findAttr (unqual "type_modifier") e of
    Nothing     -> return Nothing
    Just "n"    -> return (Just In)
    Just "o"    -> return (Just Out)
    Just "N"    -> return (Just InOut)
    other   -> throwError ("Unknown type type_modifier: " ++ show other)

getArgs :: MonadError String m => StateT [Content] m [Arg]
getArgs = do
    argElems <- selectElems (unqual "arg")
    flip evalStateT 0 $ sequence
        [ do
            argIndex <- getIndex e
            
            lift $ do
                argDirection <- getTypeModifier e
                let argArrayLengthInArg = findAttr (unqual "c_array_length_in_arg")     e
                    argArrayLengthInRet = findAttr (unqual "c_array_length_in_retval")  e
                    argArrayFixedLength = findAttr (unqual "c_array_of_fixed_length")   e
                argArrayNullDelim   <- getBoolAttr False (unqual "c_array_delimited_by_null")  e
                argArrayVariableLen <- getBoolAttr False (unqual "c_array_of_variable_length") e
                nullAccepted        <- getBoolAttr False (unqual "null_accepted") e
                let argType = Sized
                        { m32 = findAttr (unqual "type")   e
                        , m64 = findAttr (unqual "type64") e
                        }
                argIsFunPtr         <- getBoolAttr False (unqual "function_pointer") e
                argIsPrintfFormat   <- getBoolAttr False (unqual "printf_format")    e
                let selOfType = Sized
                        { m32 = findAttr (unqual "sel_of_type")   e
                        , m64 = findAttr (unqual "sel_of_type64") e
                        }
                
                (argArgs, argReturn)
                    <- process "function argument" (elContent e) getSignature
                return Arg{..}
        | e <- argElems
        ]

getReturn :: MonadError String m => StateT [Content] m (Maybe Return)
getReturn = do
    returnElems <- selectElems (unqual "retval")
    liftM listToMaybe $ sequence
        [ do
            let retArrayLengthInArg = findAttr (unqual "c_array_length_in_arg")     e
                retArrayFixedLength = findAttr (unqual "c_array_of_fixed_length")   e
            retArrayNullDelim   <- getBoolAttr False (unqual "c_array_delimited_by_null")  e
            retArrayVariableLen <- getBoolAttr False (unqual "c_array_of_variable_length") e
            alreadyRetained     <- getBoolAttr False (unqual "already_retained")           e
            m32                 <- getRequiredAttr (unqual "type")   e
            m64                 <- getAttr m32     (unqual "type64") e
            let returnType = Sized{..}
            retIsFunPtr         <- getBoolAttr False (unqual "function_pointer") e
            
            (retArgs, retReturn)
                <- process "function argument" (elContent e) getSignature
            
            return Return{..}
        | e <- returnElems
        ]

getAliases = do
    aliasElements <- selectElems (unqual "function_alias")
    sequence
        [ do
            aliasName       <- getRequiredAttr (unqual "name")     e
            aliasOriginal   <- getRequiredAttr (unqual "original") e
            return Alias{..}
        | e <- aliasElements
        ]

getClassInterface elemName = do
    classElements <- selectElems (unqual elemName)
    sequence
        [ do
            className       <- getRequiredAttr (unqual "name") e
            classMethods    <- process (elemName ++ " " ++ className) (elContent e) getMethods
            return ClassInterface{..}
        | e <- classElements
        ]

getMethods = do
    methodElems <- selectElems (unqual "method")
    sequence
        [ do
            selector            <- getRequiredAttr (unqual "selector") e
            let methodType = Sized
                    { m32 = findAttr (unqual "type")   e
                    , m64 = findAttr (unqual "type64") e
                    }
            isClassMethod       <- getBoolAttr False (unqual "class_method") e
            ignoreMethod        <- getBoolAttr False (unqual "ignore")       e
            let methodSuggestion = findAttr (unqual "suggestion") e
            methodIsVariadic    <- getBoolAttr False (unqual "variadic")     e
            let methodSentinel   = findAttr (unqual "sentinel") e
            
            (methodArgs, methodReturn)
                <- process "function argument" (elContent e) getSignature
            return Method{..}
        | e <- methodElems
        ]