module Data.BridgeSupport.Types where

data BridgeSupport = BridgeSupport
    { dependsOn         :: [FilePath]
    , structs           :: [Struct]
    , opaqueTypes       :: [TypeInfo]
    , cfTypes           :: [CFType]
    , constants         :: [Constant]
    , stringConstants   :: [StringConstant]
    , enumTypes         :: [EnumType]
    , functions         :: [Function]
    , functionAliases   :: [Alias]
    , informalProtocols :: [ClassInterface]
    , classes           :: [ClassInterface]
    } deriving (Eq, Show)

data Sized t = Sized
    { m32               :: t
    , m64               :: t
    } deriving (Eq, Show)

data TypeInfo = TypeInfo
    { name              :: String
    , typeEncoding      :: Sized String
    } deriving (Eq, Show)

data Struct = Struct
    { structType        :: TypeInfo
    , structOpaque      :: Bool
    } deriving (Eq, Show)

data CFType = CFType
    { cfTypeInfo        :: TypeInfo
    , getTypeID_func    :: Maybe String
    , tollFree          :: Maybe String
    } deriving (Eq, Show)

data Constant = Constant
    { constantName      :: String
    , constantType      :: Sized String
    , magicCookie       :: Bool
    } deriving (Eq, Show)

data StringConstant = StringConstant
    { stringName        :: String
    , stringValue       :: Sized String
    , stringNSString    :: Bool
    } deriving (Eq, Show)

data EnumType = EnumType
    { enumName          :: String
    , enumValue         :: Sized (Maybe String)
    , ignoreEnum        :: Bool
    , enumSuggestion    :: Maybe String
    } deriving (Eq, Show)

data Function = Function
    { functionName      :: String
    , isVariadic        :: Bool
    , isInline          :: Bool
    , sentinel          :: Maybe String
    , functionArgs      :: [Arg]
    , functionReturn    :: Maybe Return
    } deriving (Eq, Show)

data Arg = Arg
    { argIndex              :: Int
    , argDirection          :: Maybe TypeModifier
    , argArrayLengthInArg   :: Maybe String
    , argArrayLengthInRet   :: Maybe String
    , argArrayFixedLength   :: Maybe String
    , argArrayNullDelim     :: Bool
    , argArrayVariableLen   :: Bool
    , nullAccepted          :: Bool
    , argType               :: Sized (Maybe String)
    , argIsFunPtr           :: Bool
    , argIsPrintfFormat     :: Bool
    , selOfType             :: Sized (Maybe String)
    , argArgs               :: [Arg]
    , argReturn             :: Maybe Return
    } deriving (Eq, Show)

data Return = Return
    { retArrayLengthInArg   :: Maybe String
    , retArrayFixedLength   :: Maybe String
    , retArrayNullDelim     :: Bool
    , retArrayVariableLen   :: Bool
    , alreadyRetained       :: Bool
    , returnType            :: Sized String
    , retIsFunPtr           :: Bool
    , retArgs               :: [Arg]
    , retReturn             :: Maybe Return
    } deriving (Eq, Show)

data TypeModifier
    = In
    | Out
    | InOut
    deriving (Eq, Show)

data Alias = Alias
    { aliasName             :: String
    , aliasOriginal         :: String
    } deriving (Eq, Show)

data ClassInterface = ClassInterface
    { className             :: String
    , classMethods          :: [Method]
    } deriving (Eq, Show)

data Method = Method
    { selector              :: String
    , methodType            :: Sized (Maybe String)
    , isClassMethod         :: Bool
    , ignoreMethod          :: Bool
    , methodSuggestion      :: Maybe String
    , methodIsVariadic      :: Bool
    , methodSentinel        :: Maybe String
    , methodArgs            :: [Arg]
    , methodReturn          :: Maybe Return
    } deriving (Eq, Show)