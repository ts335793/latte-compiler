{-# LANGUAGE MultiWayIf #-}

module SemanticAnalysis where

import           BNFC.AbsLatte        hiding (Type)
import qualified BNFC.AbsLatte        as Abs
import           Control.Conditional  (if')
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.State  hiding (State)
import qualified Control.Monad.State  as St
import           Data.Composition
import           Data.Foldable
import           Data.List
import           Data.List.Utils
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Prelude              hiding (error)
import           Util
import           Debug.Trace
-- position

type Position = (Int, Int)

-- error

data Error = ErrorAt Position String
           | ErrorNear Position String
           | UnlocalizedError String
           deriving Eq

instance Ord Error where
    UnlocalizedError _ <= _ = True
    _ <= UnlocalizedError _ = False
    ErrorAt p1 _ <= ErrorAt p2 _ = p1 <= p2
    ErrorNear p1 _ <= ErrorNear p2 _ = p1 <= p2
    ErrorAt p1 _ <= ErrorNear p2 _ = p1 <= p2
    ErrorNear p1 _ <= ErrorAt p2 _ = p1 < p2

instance Show Error where
    show (UnlocalizedError m) = m
    show (ErrorAt (l, c) m) = "At line: " ++ show l ++ ", position: " ++ show c ++ "\n\t" ++ m
    show (ErrorNear (l, c) m) = "Near line: " ++ show l ++ ", position: " ++ show c ++ "\n\t" ++ m

errors :: CM a -> CM [Error]
errors o = (o >> return []) `catchError` return

error :: String -> CM Error
error s = do
    p <- getLastPosition
    return $ ErrorNear p s

throwAt :: Position -> String -> CM ()
throwAt p s = throwError [ErrorAt p s]

throw :: String -> CM ()
throw s = do
    ex <- error s
    throwError [ex]

throwNotNull :: [Error] -> CM ()
throwNotNull exs = unless (null exs) (throwError exs)

checkAt :: Position -> Bool -> String -> CM ()
checkAt p b s = unless b (throwAt p s)

checkAtM :: Position -> CM Bool -> String -> CM ()
checkAtM p b s = unlessM b (throwAt p s)

check :: Bool -> String -> CM ()
check b s = unless b (throw s)

checkM :: CM Bool -> String -> CM ()
checkM b s = unlessM b (throw s)

checkUnlocalized :: Bool -> String -> CM ()
checkUnlocalized b s = unless b (throwError [UnlocalizedError s])

checkUnlocalizedM :: CM Bool -> String -> CM ()
checkUnlocalizedM b s = unlessM b (throwError [UnlocalizedError s])

interferenceError :: (Type, HandSide) -> (Type, HandSide) -> String
interferenceError (et, ehs) (it, ihs) = "Couldn't match expected " ++ show ehs ++ " " ++ show et ++ " against interfered " ++ show ihs ++ " " ++ show it ++ "."

invalidTypeError :: Type -> String
invalidTypeError t = show t ++ " is not proper type."

notCreateableError :: Type -> String
notCreateableError t = show t ++ " is not createable."

repeatedError :: String
repeatedError = "Repeated argument names."

existsError :: String -> String
existsError s = s ++ " already defined."

existsInClassError :: String -> String -> String
existsInClassError c s = s ++ " already defined in " ++ c ++ "."

-- type

data HandSide = LHS | RHS
              deriving Eq

instance Show HandSide where
    show LHS = "l-value"
    show RHS = "r-value"

data Type = TInt
          | TString
          | TBool
          | TVoid
          | TObj String
          | TArr Type
          | TFun [Type] Type
          | TAny
          | TAnyRef
          deriving Eq

instance Show Type where
    show TInt = "int"
    show TString = "string"
    show TBool = "boolean"
    show TVoid = "void"
    show (TObj c) = c
    show (TArr t) = show t ++ "[]"
    show (TFun argts rt) = foldr (\argt s -> show argt ++ " -> " ++ s) (show rt) argts
    show TAny = "any"
    show TAnyRef = "any reference"

instance Read Type where
    readsPrec _ "int" = [(TInt, "")]
    readsPrec _ "string" = [(TString, "")]
    readsPrec _ "boolean" = [(TBool, "")]
    readsPrec _ "void" = [(TVoid, "")]
    readsPrec _ s
        | endswith "[]" s = [(TArr $ read $ take (length s - 2) s, "")]
        | otherwise = [(TObj s, "")]

bnfcTypeToTypePosition :: Abs.Type -> (Type, Position)
bnfcTypeToTypePosition (TSingle (PIdent (p, i))) = (read i, p)
bnfcTypeToTypePosition (TArray (PIdent (p, i))) = (read $ i ++ "[]", p)

bnfcTypeToType :: Abs.Type -> Type
bnfcTypeToType bt = fst $ bnfcTypeToTypePosition bt

isCallable :: Type -> Bool
isCallable (TFun _ _) = True
isCallable _ = False

isCreateable :: Type -> Bool
isCreateable TVoid = False
isCreateable _ = True

isArray :: Type -> Bool
isArray (TArr _) = True
isArray _ = False

arrayFieldType :: Type -> Type
arrayFieldType (TArr t) = t

isProperType :: Type -> CM Bool
isProperType TInt = return True
isProperType TString = return True
isProperType TBool = return True
isProperType TVoid = return True
isProperType (TObj c) = isClass c
isProperType (TArr t) = isProperType t
isProperType (TFun argts rt) = allM isProperType argts &&^ isProperType rt

isSubclass :: String -> String -> CM Bool
isSubclass c1 c2 = S.member c2 <$> getSuperclasses c1

isSubtype :: Type -> Type -> CM Bool
isSubtype TInt TInt = return True
isSubtype TString TString = return True
isSubtype TBool TBool = return True
isSubtype TVoid TVoid = return True
isSubtype (TObj c1) (TObj c2) = c1 `isSubclass` c2
isSubtype (TArr t1) (TArr t2) = t1 `isSubtype` t2
isSubtype (TFun args1 r1) (TFun args2 r2) =
    pure (length args1 == length args2) &&^
    allM (\(t1, t2) -> t2 `isSubtype` t1) (zip args1 args2) &&^
    (r1 `isSubtype` r2)
isSubtype _ TAny = return True
isSubtype TAny _ = return True
isSubtype TAnyRef (TArr _) = return True
isSubtype (TArr _) TAnyRef = return True
isSubtype TAnyRef (TObj _) = return True
isSubtype (TObj _) TAnyRef = return True
isSubtype _ _ = return False

-- state

data Field = Field {
    fieldType     :: Type,
    fieldPosition :: Position
} deriving Show

data Function = Function {
    arguments        :: [(String, Field)],
    body             :: Block,
    functionType     :: Type,
    functionPosition :: Position
} deriving Show

data Virtual = Virtual {
    virtualType     :: Type,
    virtualPosition :: Position
} deriving Show

data Class = Class {
    superclass    :: String,
    fields        :: Map String Field,
    methods       :: Map String Function,
    virtuals      :: Map String Virtual,
    classPosition :: Position
} deriving Show

getSuperclass :: String -> CM String
getSuperclass c = superclass <$> getClass c

getSuperclasses :: String -> CM (Set String)
getSuperclasses "object" = return $ S.singleton "object"
getSuperclasses c = do
    s <- getSuperclass c
    S.insert c <$> getSuperclasses s

getFields :: String -> CM (Map String Field)
getFields c = fields <$> getClass c

hasField :: String -> String -> CM Bool
hasField c f = M.member f <$> getFields c

setField :: String -> String -> Type -> Position -> CM ()
setField c f t p = do
    (Class s fs ms vs cp) <- getClass c
    setClass c s (M.insert f (Field t p) fs) ms vs cp

getFieldsT :: Type -> CM (Map String Field)
getFieldsT (TObj c) = getFields c
getFieldsT (TArr c) = return $ M.singleton "length" (Field TInt (-1, -1))
getFieldsT _ = return M.empty

getFieldT :: Type -> String -> CM Field
getFieldT t f = (M.! f) <$> getFieldsT t

hasFieldT :: Type -> String -> CM Bool
hasFieldT t f = M.member f <$> getFieldsT t

getMethods :: String -> CM (Map String Function)
getMethods c = methods <$> getClass c

hasMethod :: String -> String -> CM Bool
hasMethod c f = M.member f <$> getMethods c

setMethod :: String -> String -> [(String, Field)] -> Block -> Type -> Position -> CM ()
setMethod c m a b t p = do
    (Class s fs ms vs cp) <- getClass c
    setClass c s fs (M.insert m (Function a b t p) ms) vs cp

getVirtuals :: String -> CM (Map String Virtual)
getVirtuals c = virtuals <$> getClass c

getVirtual :: String -> String -> CM Virtual
getVirtual c v = (M.! v) <$> getVirtuals c

hasVirtual :: String -> String -> CM Bool
hasVirtual c v = M.member v <$> getVirtuals c

setVirtual :: String -> String -> Type -> Position -> CM ()
setVirtual c v t p = do
    (Class s fs ms vs cp) <- getClass c
    setClass c s fs ms (M.insert v (Virtual t p) vs) cp

getVirtualsT :: Type -> CM (Map String Virtual)
getVirtualsT (TObj c) = getVirtuals c
getVirtualsT _ = return M.empty

getVirtualT :: Type -> String -> CM Virtual
getVirtualT t v = (M.! v) <$> getVirtualsT t

hasVirtualT :: Type -> String -> CM Bool
hasVirtualT t f = M.member f <$> getVirtualsT t

getCurrentClassVirtual :: String -> CM Virtual
getCurrentClassVirtual v = do
    Just c <- getCurrentClass
    getVirtual c v

hasCurrentClassVirtual :: String -> CM Bool
hasCurrentClassVirtual v = do
    Just c <- getCurrentClass
    c `hasVirtual` v

data State = State {
    lastPosition :: Position,
    currentClass :: Maybe String,
    frames       :: [Map String Field],
    functions    :: Map String Function,
    classes      :: Map String Class
} deriving Show

getLastPosition :: CM Position
getLastPosition = lastPosition <$> get

updateLastPosition :: Position -> CM ()
updateLastPosition p = modify (\st -> st { lastPosition = p })

getCurrentClass :: CM (Maybe String)
getCurrentClass = currentClass <$> get

inClass :: CM Bool
inClass = (Nothing /=) <$> getCurrentClass

getFrames :: CM [Map String Field]
getFrames = frames <$> get

pushFrame :: CM ()
pushFrame = modify (\st -> st { frames = M.empty : frames st })

popFrame :: CM ()
popFrame = modify (\st -> st { frames = tail $ frames st })

frame :: CM a -> CM a
frame b = do
    pushFrame
    r <- b
    popFrame
    return r

getBlock :: CM (Map String Field)
getBlock = head <$> getFrames

isBlock :: String -> CM Bool
isBlock l = M.member l <$> getBlock

getLocal :: String -> CM Field
getLocal l = do
    Just m <- find (M.member l) <$> getFrames
    return $ m M.! l

isLocal :: String -> CM Bool
isLocal l = any (M.member l) <$> getFrames

setLocal :: String -> Type -> Position -> CM ()
setLocal l t p = modify (\st -> st { frames = M.insert l (Field t p) (head $ frames st) : tail (frames st) })

getFunctions :: CM (Map String Function)
getFunctions = functions <$> get

getFunction :: String -> CM Function
getFunction f = (M.! f) <$> getFunctions

isFunction :: String -> CM Bool
isFunction f = M.member f <$> getFunctions

setFunction :: String -> [(String, Field)] -> Block -> Type -> (Int, Int) -> CM ()
setFunction f a b t p = modify (\st -> st { functions = M.insert f (Function a b t p) (functions st) })

getClasses :: CM (Map String Class)
getClasses = classes <$> get

getClass :: String -> CM Class
getClass c = (M.! c) <$> getClasses

isClass :: String -> CM Bool
isClass c = M.member c <$> getClasses

setClass :: String -> String -> Map String Field -> Map String Function -> Map String Virtual -> Position -> CM ()
setClass c s fs ms vs p = modify (\st -> st { classes = M.insert c (Class s fs ms vs p) (classes st) })

type CM = ExceptT [Error] (St.State State)

-- check expression type

checkTypeHelperAt :: Position -> Expr -> Type -> HandSide -> CM ()
checkTypeHelperAt p e t LHS = do
    updateLastPosition p
    (t', hs') <- getType e
    checkAtM p (pure (hs' == LHS) &&^ (t' `isSubtype` t)) (interferenceError (t, LHS) (t', hs'))
checkTypeHelperAt p e t RHS = do
    (t', hs') <- getType e
    checkAtM p (t' `isSubtype` t) (interferenceError (t, RHS) (t', hs'))

checkTypeHelper :: Expr -> Type -> HandSide -> CM ()
checkTypeHelper e t LHS = do
    (t', hs') <- getType e
    checkM (pure (hs' == LHS) &&^ (t' `isSubtype` t)) (interferenceError (t, LHS) (t', hs'))
checkTypeHelper e t RHS = do
    (t', hs') <- getType e
    checkM (t' `isSubtype` t) (interferenceError (t, RHS) (t', hs'))

checkType :: Expr -> Type -> HandSide -> CM ()
checkType e@(EVar (PIdent (p, _))) = checkTypeHelperAt p e
checkType e@ELitInt {} = checkTypeHelper e
checkType e@EString {} = checkTypeHelper e
checkType e@ELitTrue = checkTypeHelper e
checkType e@ELitFalse = checkTypeHelper e
checkType e@ENull = checkTypeHelper e
checkType e@(ESelect _ (PIdent (p, _))) = checkTypeHelperAt p e
checkType e@(EMetCall _ (PIdent (p, _)) _) = checkTypeHelperAt p e
checkType e@EAt {} = checkTypeHelper e
checkType e@(EApp (PIdent (p, _)) _) = checkTypeHelperAt p e
checkType e@ENeg {} = checkTypeHelper e
checkType e@ENot {} = checkTypeHelper e
checkType e@EIncr {} = checkTypeHelper e
checkType e@EDecr {} = checkTypeHelper e
checkType e@(ENewVar (PIdent (p, _))) = checkTypeHelperAt p e
checkType e@(ENewArr (PIdent (p, _)) _) = checkTypeHelperAt p e
checkType e@(ECastVar (PIdent (p, _)) _) = checkTypeHelperAt p e
checkType e@(ECastArr (PIdent (p, _)) _) = checkTypeHelperAt p e
checkType e@EMul {} = checkTypeHelper e
checkType e@EAdd {}= checkTypeHelper e
checkType e@ERel {} = checkTypeHelper e
checkType e@EAnd {} = checkTypeHelper e
checkType e@EOr {} = checkTypeHelper e

-- get expression type

checkFunctionCallAt :: Position -> String -> [Type] -> [Expr] -> CM ()
checkFunctionCallAt p i argts es = do
    updateLastPosition p
    if' (length argts == length es)
        (do exs <- concatMapM (errors . uncurry3 checkType) (zip3 es argts (repeat RHS))
            throwNotNull exs)
        (do let ex = ErrorAt p ("Wrong number of parameters in " ++ i ++ " call.")
            exs <- concatMapM (errors . getType) es
            throwError $ ex : exs)

getNewTypeAt :: Position -> String -> CM (Type, HandSide)
getNewTypeAt p i = do
    updateLastPosition p
    checkAtM p (isProperType t) (invalidTypeError t)
    checkAt p (isCreateable t) (notCreateableError t)
    return (t, LHS)
    where
        t = read i

getCastTypeAt :: Position -> Type -> Expr -> CM (Type, HandSide)
getCastTypeAt p nt e = do
    updateLastPosition p
    ifM (isProperType nt)
        (do (t, hs) <- getType e
            checkAtM p ((nt `isSubtype` t) ||^ (t `isSubtype` nt)) ("Cannot cast " ++ show t ++ " to " ++ show nt ++ ".")
            return (nt, hs))
        (do let ex = ErrorAt p (invalidTypeError nt)
            exs <- errors $ getType e
            throwError $ ex : exs)

getTypeHelper :: [(Expr, Type, HandSide)] -> (Type, HandSide) -> CM (Type, HandSide)
getTypeHelper xs r = do
    exs <- concatMapM (errors . uncurry3 checkType) xs
    throwNotNull exs
    return r

getType :: Expr -> CM (Type, HandSide)
getType (EVar (PIdent (p, i))) = do
    updateLastPosition p
    checkAtM p (isLocal i) (i ++ " is not defined.")
    t <- fieldType <$> getLocal i
    return (t, LHS)
getType (ELitInt i) = do
    check (toInteger (minBound :: Int) <= i && i <= toInteger (maxBound :: Int)) (show i ++ " exceeds int bounds.")
    return (TInt, RHS)
getType EString {} = return (TString, RHS)
getType ELitTrue = return (TBool, RHS)
getType ELitFalse = return (TBool, RHS)
getType ENull = return (TAnyRef, RHS)
getType (ESelect e (PIdent (p, i))) = do
    (t, _) <- getType e
    updateLastPosition p
    checkAtM p (t `hasFieldT` i) (show t ++ " has no field " ++ i ++ ".")
    it <- fieldType <$> getFieldT t i
    return (it, LHS)
getType (EMetCall e (PIdent (p, i)) es) = do
    (t, _) <- getType e `catchError` (\exs1 -> do
        updateLastPosition p
        exs2 <- concatMapM (errors . getType) es
        throwError $ exs2 ++ exs1)
    updateLastPosition p
    ifM (t `hasVirtualT` i)
        (do TFun argts rt <- virtualType <$> getVirtualT t i
            checkFunctionCallAt p i argts es
            return (rt, RHS))
        (do let ex = ErrorAt p (show t ++ " has no method " ++ i ++ ".")
            exs <- concatMapM (errors . getType) es
            throwError $ ex : exs)
getType (EAt e1 e2) = do
    (t, _) <- getType e1 `catchError` (\exs1 -> do
        exs2 <- errors $ checkType e2 TInt RHS
        throwError $ exs2 ++ exs1)
    if' (isArray t)
        (do checkType e2 TInt RHS
            return (arrayFieldType t, LHS))
        (do ex <- error (show t ++ " is not array type.")
            exs <- errors $ checkType e2 TInt RHS
            throwError $ ex : exs)
getType (EApp (PIdent (p, i)) es) = do
    updateLastPosition p
    ifM (inClass &&^ hasCurrentClassVirtual i)
        (do TFun argts rt <- virtualType <$> getCurrentClassVirtual i
            checkFunctionCallAt p i argts es
            return (rt, RHS))
        (ifM (isFunction i)
            (do TFun argts rt <- functionType <$> getFunction i
                checkFunctionCallAt p i argts es
                return (rt, RHS))
            (do let ex = ErrorAt p (i ++ " function is not defined.")
                exs <- concatMapM (errors . getType) es
                throwError $ ex : exs))
getType (ENeg e) = checkType e TInt RHS >> return (TInt, RHS)
getType (ENot e) = checkType e TBool RHS >> return (TBool, RHS)
getType (EIncr e) = checkType e TInt LHS >> return (TInt, RHS)
getType (EDecr e) = checkType e TInt LHS >> return (TInt, RHS)
getType (ENewVar (PIdent (p, i))) = getNewTypeAt p i
getType (ENewArr (PIdent (p, i)) e) = do
    (t, hs) <- getNewTypeAt p (i ++ "[]") `catchError` (\exs1 -> do
        exs2 <- errors $ checkType e TInt RHS
        throwError $ exs2 ++ exs1)
    checkType e TInt RHS
    return (t, hs)
getType (ECastVar (PIdent (p, i)) e) = getCastTypeAt p (read i) e
getType (ECastArr (PIdent (p, i)) e) = getCastTypeAt p (read $ i ++ "[]") e
getType (EMul e1 _ e2) = getTypeHelper [(e1, TInt, RHS), (e2, TInt, RHS)] (TInt, RHS)
getType (EAdd e1 Plus e2) = do
    exsI <- concatMapM (errors . uncurry3 checkType) [(e1, TInt, RHS), (e2, TInt, RHS)]
    exsS <- concatMapM (errors . uncurry3 checkType) [(e1, TString, RHS), (e2, TString, RHS)]
    case (exsI, exsS) of
        ([], _) -> return (TInt, RHS)
        (_, []) -> return (TString, RHS)
        _ -> throwError exsI
getType (EAdd e1 _ e2) = getTypeHelper [(e1, TInt, RHS), (e2, TInt, RHS)] (TInt, RHS)
getType (ERel e1 EQU e2) = do
    exs <- concatMapM (errors . getType) [e1, e2]
    throwNotNull exs
    (t1, _) <- getType e1
    (t2, _) <- getType e2
    checkM ((t1 `isSubtype` t2) ||^ (t2 `isSubtype` t1)) ("== cannot be used with " ++ show t1 ++ " and " ++ show t2 ++ ".")
    return (TBool, RHS)
getType (ERel e1 NE e2) = do
    exs <- concatMapM (errors . getType) [e1, e2]
    throwNotNull exs
    (t1, _) <- getType e1
    (t2, _) <- getType e2
    checkM ((t1 `isSubtype` t2) ||^ (t2 `isSubtype` t1)) ("!= cannot be used with " ++ show t1 ++ " and " ++ show t2 ++ ".")
    return (TBool, RHS)
getType (ERel e1 _ e2) = do
    exs <- concatMapM (errors . getType) [e1, e2]
    throwNotNull exs
    (t1, _) <- getType e1
    (t2, _) <- getType e2
    check ((t1, t2) == (TInt, TInt) || (t1, t2) == (TString, TString)) ("Cannot use <, <=, =>, > with " ++ show t1 ++ " and " ++ show t2)
    return (TBool, RHS)
    --getTypeHelper [(e1, TInt, RHS), (e2, TInt, RHS)] (TBool, RHS)
getType (EAnd e1 e2) = getTypeHelper [(e1, TBool, RHS), (e2, TBool, RHS)] (TBool, RHS)
getType (EOr e1 e2) = getTypeHelper [(e1, TBool, RHS), (e2, TBool, RHS)] (TBool, RHS)

-- check statements

checkMethod :: String -> Function -> CM ()
checkMethod c f@(Function _ _ _ p) =
    frame (do setLocal "self" (TObj c) p
              frame (do mapM_ (\(i, Field t p) -> setLocal i t p) . M.toList =<< getFields c
                        checkFunction f))

checkFunction :: Function -> CM ()
checkFunction (Function args b (TFun argts rt) p) = do
    updateLastPosition p
    frame (do forM_ args (\(i, Field t p) -> setLocal i t p)
              checkBlock rt b)

checkBlock :: Type -> Block -> CM ()
checkBlock rt (Block xs) = do
    exs <- frame $ concatMapM (errors . checkStmt rt) xs
    throwNotNull exs

checkItem :: Type -> Item -> CM ()
checkItem t (NoInit (PIdent (p, i))) = do
    checkAtM p (notM $ isBlock i) (i ++ " already defined in scope.")
    setLocal i t p
checkItem t (Init (PIdent (p, i)) e) = do
    whenM (isBlock i)
        (do let ex = ErrorAt p (i ++ " already defined in scope.")
            exs <- errors $ checkType e t RHS
            throwError $ ex : exs)
    checkType e t RHS
    setLocal i t p

checkStmtHelper :: [(Expr, Type, HandSide)] -> [(Type, Stmt)] -> CM ()
checkStmtHelper es ss = do
    exsE <- concatMapM (errors . uncurry3 checkType) es
    exsS <- concatMapM (errors . frame . uncurry checkStmt) ss
    let exs = exsE ++ exsS
    throwNotNull exs

checkStmt :: Type -> Stmt -> CM ()
checkStmt _ Empty = return ()
checkStmt rt (BStmt b) = checkBlock rt b
checkStmt _ (Decl bt is) = do
    exs1 <- errors $ checkM (isProperType t) (invalidTypeError t)
    exs2 <- errors $ check (isCreateable t) (notCreateableError t)
    let t' = if' (null $ exs1 ++ exs2) t TAny
    exs3 <- concatMapM (errors . checkItem t') is
    throwNotNull $ exs1 ++ exs2 ++ exs3
    where
        t = bnfcTypeToType bt
checkStmt _ (Ass e1 e2) = do
    (t1, hs1) <- getType e1 `catchError` (\exs1 -> do
        exs2 <- errors $ getType e2
        throwError $ exs2 ++ exs1)
    if' (hs1 == LHS)
        (checkType e2 t1 RHS)
        (do ex <- error ("Cannot assign to " ++ show RHS ++ ".")
            exs <- errors $ getType e2
            throwError $ ex : exs)
checkStmt TVoid (Ret e) = do
    exs <- errors $ checkType e TVoid RHS
    ex <- error $ notCreateableError TVoid
    throwError $ ex : exs
checkStmt rt (Ret e) = checkType e rt RHS
checkStmt TVoid VRet = return ()
checkStmt rt VRet = throw (interferenceError (rt, RHS) (TVoid, RHS))
checkStmt rt (Cond e s) = checkStmtHelper [(e, TBool, RHS)] [(rt, s)]
checkStmt rt (CondElse e s1 s2) = checkStmtHelper [(e, TBool, RHS)] [(rt, s1), (rt, s2)]
checkStmt rt (While e s) = checkStmtHelper [(e, TBool, RHS)] [(rt, s)]
checkStmt rt (For bt (PIdent (p, i)) e s) = do
    exs1 <- errors $ checkM (isProperType t) (invalidTypeError t)
    exs2 <- errors $ check (isCreateable t) (notCreateableError t)
    let t' = if' (null $ exs1 ++ exs2) t TAny
    exs3 <- errors $ checkType e (TArr t') RHS
    exs4 <- errors $ frame $ setLocal i t' p >> checkStmt rt s
    throwError $ exs1 ++ exs2 ++ exs3 ++ exs4
    where
        t = bnfcTypeToType bt
checkStmt _ (SExp e) = void $ getType e

-- collect definitions

isUnique :: (Eq a) => [a] -> Bool
isUnique l = length (nub l) == length l

getDefsInProgram :: Program -> CM ()
getDefsInProgram (Program xs) = do
    exs <- concatMapM (errors . getDefsInTopDef) xs
    throwNotNull exs

getDefsInTopDef :: TopDef -> CM ()
getDefsInTopDef (FnDef brt (PIdent (p, i)) args b) = do
    exs1 <- errors $ checkAtM p (notM $ isFunction i) (existsError i)
    exs2 <- errors $ checkAt p (isUnique argns) repeatedError
    exs3 <- errors $ when (i == "main") (checkAt p (null args) "Main function must have zero arguments.")
    exs4 <- errors $ when (i == "main") (checkAt p (rt == TInt || rt == TInt) ("Main function cannot return " ++ show rt ++ "."))
    throwNotNull $ exs1 ++ exs2 ++ exs3 ++ exs4
    setFunction i args' b (TFun argts rt) p
    where
        rt = bnfcTypeToType brt
        args' = map (\(Arg t (PIdent (p, a))) -> (a, Field (bnfcTypeToType t) p)) args
        argts = map (\(_, f) -> fieldType f) args'
        argns = map fst args'
getDefsInTopDef (TopClsDef (PIdent (p, c)) xs) = do
    checkAtM p (notM $ isClass c) (existsError c)
    setClass c "object" M.empty M.empty M.empty p
    exs <- concatMapM (errors . getDefsInClsDef c) xs
    throwNotNull exs

getDefsInTopDef (ExtClsDef (PIdent (p, c)) (PIdent (_, s)) xs) = do
    checkAtM p (notM $ isClass c) (existsError c)
    setClass c s M.empty M.empty M.empty p
    exs <- concatMapM (errors . getDefsInClsDef c) xs
    throwNotNull exs

getDefsInClsDef :: String -> ClsDef -> CM ()
getDefsInClsDef c (VarDef bt props) = do
    exs <- concatMapM (errors . collectField) props
    throwNotNull exs
    where
        t = bnfcTypeToType bt
        collectField (PIdent (p, i)) = do
            checkAtM p (notM $ c `hasField` i) (existsInClassError c i)
            setField c i t p
getDefsInClsDef c (MetDef brt (PIdent (p, i)) args b) = do
    exs1 <- errors $ checkAtM p (notM $ c `hasMethod` i) (existsInClassError c i)
    exs2 <- errors $ checkAt p (isUnique argns) repeatedError
    throwNotNull $ exs1 ++ exs2
    setMethod c i args' b (TFun argts rt) p
    where
        rt = bnfcTypeToType brt
        args' = map (\(Arg t (PIdent (p, i))) -> (i, Field (bnfcTypeToType t) p)) args
        argts = map (\(_, f) -> fieldType f) args'
        argns = map fst args'

-- check definitions consistency

checkTypesInFunction :: Function -> CM ()
checkTypesInFunction (Function _ _ (TFun argts rt) p) = do
    exs1 <- concatMapM (errors . (\t -> checkAtM p (isProperType t) (invalidTypeError t))) (rt : argts)
    exs2 <- concatMapM (errors . (\t -> checkAt p (isCreateable t) (notCreateableError t))) argts
    throwNotNull $ exs1 ++ exs2

checkTypesInClass :: Class -> CM ()
checkTypesInClass (Class s fs ms _ p) = do
    exs1 <- errors $ checkAtM p (isClass s) (existsError s)
    exs2 <- concatMapM (errors . (\(Field t p) -> checkAtM p (isProperType t) (invalidTypeError t))) (M.elems fs)
    exs3 <- concatMapM (errors . (\(Field t p) -> checkAt p (isCreateable t) (notCreateableError t))) (M.elems fs)
    exs4 <- concatMapM (errors . checkTypesInFunction) (M.elems ms)
    throwNotNull $ exs1 ++ exs2 ++ exs3 ++ exs4

checkTypes :: CM ()
checkTypes = do
    exs1 <- concatMapM (errors . checkTypesInFunction) . M.elems =<< getFunctions
    exs2 <- concatMapM (errors . checkTypesInClass) . M.elems =<< getClasses
    throwNotNull $ exs1 ++ exs2

-- check inheritance cycles

findInheritanceCycles :: CM ()
findInheritanceCycles = do
    cl <- M.keys <$> getClasses
    (_, exs) <- foldM (\(prevVis, exs1) c ->
        if' (S.member c prevVis)
            (   return (prevVis, exs1))
            (do (currVis, exs2)<- dfs c prevVis S.empty
                return (S.union prevVis currVis, exs2 ++ exs1))) (S.empty, []) cl
    throwNotNull exs
    where
        dfs :: String -> Set String -> Set String -> CM (Set String, [Error])
        dfs v prevVis currVis = do
            let currVis' = S.insert v currVis
            s <- getSuperclass v
            case s of
                "object" -> return (currVis', [])
                _ ->
                    if | S.member s currVis' -> do
                            p <- classPosition <$> getClass v
                            return (currVis', [ErrorAt p (v ++ " is on cycle in inheritance tree.")])
                       | S.member s prevVis -> return (currVis', [])
                       | otherwise -> dfs s prevVis currVis'

-- create virtuals

createVirtuals :: CM ()
createVirtuals = throwNotNull =<< concatMapM (errors . dfs) =<< filter (/= "object") <$> neighbours "object"
    where
        neighbours :: String -> CM [String]
        neighbours s = do
            cs <- filter ((== s) . superclass . snd) . M.toList <$> getClasses
            return $ map fst cs

        checkMethod :: String -> String -> Function -> CM ()
        checkMethod s f (Function _ _ t p) =
            whenM (s `hasVirtual` f)
                (do Virtual vt vp <- getVirtual s f
                    checkAtM p (t `isSubtype` vt) (show t ++ " is not subtype of " ++ show vt ++ " (inherited constraint)."))

        dfs :: String -> CM ()
        dfs c = do
            s <- getSuperclass c
            cms <- getMethods c
            exs1 <- concatMapM (errors . uncurry (checkMethod s)) (M.toList cms)
            svs <- getVirtuals s
            forM_ (M.toList svs) (\(v, Virtual t p) -> setVirtual c v t p)
            forM_ (M.toList cms) (\(m, Function _ _ t p) -> setVirtual c m t p)
            exs2 <- concatMapM (errors . dfs) =<< neighbours c
            throwNotNull $ exs1 ++ exs2

-- check return statements

data Value = VInt Int
           | VString String
           | VBool Bool
           | VNullRef
           deriving (Eq, Ord)

getConstExpr :: Expr -> Maybe Value
getConstExpr EVar {} = Nothing
getConstExpr (ELitInt i) = Just $ VInt $ fromInteger i
getConstExpr (EString s) = Just $ VString s
getConstExpr ELitTrue = Just $ VBool True
getConstExpr ELitFalse = Just $ VBool False
getConstExpr ENull = Just VNullRef
getConstExpr ESelect {} = Nothing
getConstExpr EMetCall {} = Nothing
getConstExpr EAt {} = Nothing
getConstExpr EApp {} = Nothing
getConstExpr (ENeg e) =
    case getConstExpr e of
        Just (VInt i) -> Just $ VInt (-i)
        _ -> Nothing
getConstExpr (ENot e) =
    case getConstExpr e of
        Just (VBool b) -> Just $ VBool (not b)
        _ -> Nothing
getConstExpr (EIncr e) = getConstExpr e
getConstExpr (EDecr e) = getConstExpr e
getConstExpr ENewVar {} = Nothing
getConstExpr ENewArr {} = Nothing
getConstExpr (ECastVar _ e) = getConstExpr e
getConstExpr (ECastArr _ e) = getConstExpr e
getConstExpr (EMul e1 Times e2) = liftM2 (\(VInt a) (VInt b) -> VInt $ a * b) (getConstExpr e1) (getConstExpr e2)
getConstExpr (EMul e1 Div e2) = liftM2 (\(VInt a) (VInt b) -> VInt $ a `div` b) (getConstExpr e1) (getConstExpr e2)
getConstExpr (EMul e1 Mod e2) = liftM2 (\(VInt a) (VInt b) -> VInt $ a `rem` b) (getConstExpr e1) (getConstExpr e2)
getConstExpr (EAdd e1 Plus e2) =
    case (getConstExpr e1, getConstExpr e2) of
        (Just (VInt a), Just (VInt b)) -> Just $ VInt $ a + b
        (Just (VString a), Just (VString b)) -> Just $ VString $ a ++ b
        _ -> Nothing
getConstExpr (EAdd e1 Minus e2) = liftM2 (\(VInt a) (VInt b) -> VInt $ a - b) (getConstExpr e1) (getConstExpr e2)
getConstExpr (ERel e1 LTH e2) = liftM2 (VBool .: (<)) (getConstExpr e1) (getConstExpr e2)
getConstExpr (ERel e1 LE e2) = liftM2 (VBool .: (<=)) (getConstExpr e1) (getConstExpr e2)
getConstExpr (ERel e1 GTH e2) = liftM2 (VBool .: (>)) (getConstExpr e1) (getConstExpr e2)
getConstExpr (ERel e1 GE e2) = liftM2 (VBool .: (>=)) (getConstExpr e1) (getConstExpr e2)
getConstExpr (ERel e1 EQU e2) = liftM2 (VBool .: (==)) (getConstExpr e1) (getConstExpr e2)
getConstExpr (ERel e1 NE e2) = liftM2 (VBool .: (/=)) (getConstExpr e1) (getConstExpr e2)
getConstExpr (EAnd e1 e2) = liftM2 (\(VBool a) (VBool b) -> VBool $ a && b) (getConstExpr e1) (getConstExpr e2)
getConstExpr (EOr e1 e2) =
    case (getConstExpr e1, getConstExpr e2) of
        (_, Just (VBool True)) -> Just $ VBool True
        (Just (VBool True), _) -> Just $ VBool True
        (Just (VBool False), Just (VBool False)) -> Just $ VBool False
        _ -> Nothing

blockAlwaysReturns :: Block -> Bool
blockAlwaysReturns (Block xs) = any stmtAlwaysReturns xs

stmtAlwaysReturns :: Stmt -> Bool
stmtAlwaysReturns Empty = False
stmtAlwaysReturns (BStmt b) = blockAlwaysReturns b
stmtAlwaysReturns Decl {} = False
stmtAlwaysReturns Ass {} = False
stmtAlwaysReturns Ret {} = True
stmtAlwaysReturns VRet = True
stmtAlwaysReturns (Cond e s) =
    case getConstExpr e of
        Just (VBool True) -> stmtAlwaysReturns s
        _ -> False
stmtAlwaysReturns (CondElse e s1 s2) =
    case getConstExpr e of
        Just (VBool True) -> stmtAlwaysReturns s1
        Just (VBool False) -> stmtAlwaysReturns s2
        _ -> stmtAlwaysReturns s1 && stmtAlwaysReturns s2
stmtAlwaysReturns (While e s) =
    case getConstExpr e of
        Just (VBool True) -> True
        _ -> False
stmtAlwaysReturns For {} = False
stmtAlwaysReturns SExp {} = False

functionAlwaysReturns :: Function -> Bool
functionAlwaysReturns (Function _ _ (TFun _ TVoid) _) = True
functionAlwaysReturns (Function _ b _ _) = blockAlwaysReturns b

-- semantic analysis

semanticAnalysis :: Program -> CM ()
semanticAnalysis p = do
    exs1 <- errors $ getDefsInProgram p
    exs2 <- errors checkTypes
    throwNotNull $ exs1 ++ exs2
    throwNotNull =<< errors findInheritanceCycles
    exs3 <- errors createVirtuals
    exs4 <- concatMapM (errors . checkFunction) . M.elems =<< getFunctions
    exs5 <- concatMapM (\c -> concatMapM (errors . checkMethod c) . M.elems =<< getMethods c) . M.keys =<< getClasses
    exs6 <- errors $ checkUnlocalizedM (isFunction "main") "No main function."
    throwNotNull $ exs3 ++ exs4 ++ exs5 ++ exs6
    exs7 <- concatMapM (errors . (\(n, f) -> checkUnlocalized (functionAlwaysReturns f) (n ++ " does not always return."))) . M.toList =<< getFunctions
    exs8 <- concatMapM (\c -> concatMapM (errors . (\(n, f) -> checkUnlocalized (functionAlwaysReturns f) (n ++ " in " ++ c ++ " does not always return."))) . M.toList =<< getMethods c) . M.keys =<< getClasses
    throwNotNull $ exs7 ++ exs8

-- run

initialState :: State
initialState = State {
    lastPosition = (0, 0),
    currentClass = Nothing,
    frames = [],
    functions = M.fromList [("printString", Function [("s", Field TString (0, 0))] (Block [VRet]) (TFun [TString] TVoid) (0, 0)),
                            ("printInt",    Function [("i", Field TInt (0, 0))]    (Block [VRet]) (TFun [TInt] TVoid)    (0, 0)),
                            ("readInt",     Function []                            (Block [Ret (ELitInt 0)]) (TFun [] TInt)         (0, 0)),
                            ("readString",  Function []                            (Block [Ret (EString "")]) (TFun [] TString)      (0, 0)),
                            ("error",       Function []                            (Block [VRet]) (TFun [] TVoid)        (0, 0))],
    classes = M.singleton "object" (Class "object" M.empty M.empty M.empty (0, 0))
}

runSemanticAnalysis :: Program -> Either [Error] ()
runSemanticAnalysis p = evalState (runExceptT (semanticAnalysis p)) initialState

runSemanticAnalysis' :: Program -> State
runSemanticAnalysis' p = execState (runExceptT (semanticAnalysis p)) initialState
