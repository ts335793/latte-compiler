{-# LANGUAGE MultiWayIf #-}

module SemanticAnalysis where

import           BNFC.AbsLatte        hiding (Type)
import qualified BNFC.AbsLatte        as Abs
import           Control.Conditional  (if')
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.State  hiding (State)
import qualified Control.Monad.State  as St
import           Data.Foldable
import           Data.List.Utils
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S
import Util

-- position

type Position = (Int, Int)

-- error

data Error = ErrorAt Position String
           | ErrorNear Position String
           deriving Eq

instance Ord Error where
     x <= y = errorPosition x <= errorPosition y

instance Show Error where
    show (ErrorAt (l, c) m) = "At line: " ++ show l ++ ", position: " ++ show c ++ "\n\t" ++ m
    show (ErrorNear (l, c) m) = "Near line: " ++ show l ++ ", position: " ++ show c ++ "\n\t" ++ m

errorPosition :: Error -> Position
errorPosition (ErrorAt p _) = p
errorPosition (ErrorNear p _) = p

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
          deriving Eq

instance Show Type where
    show TInt = "int"
    show TString = "string"
    show TBool = "boolean"
    show TVoid = "void"
    show (TObj c) = c
    show (TArr t) = show t ++ "[]"
    show (TFun argts rt) = foldr (\argt s -> show argt ++ " -> " ++ s) (show rt) argts

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

object :: Type
object = TObj "object"

isCallable :: Type -> Bool
isCallable (TFun _ _) = True
isCallable _ = False

isCreateable :: Type -> Bool
isCreateable TVoid = True
isCreateable _ = False

isArray :: Type -> Bool
isArray (TArr _) = True
isArray _ = False

arrayFieldType :: Type -> Type
arrayFieldType (TArr t) = t

-- state

data Field = Field {
    fieldType     :: Type,
    fieldPosition :: Position
}

data Function = Function {
    arguments        :: [(String, Field)],
    body             :: Block,
    functionType     :: Type,
    functionPosition :: Position
}

data Virtual = Virtual {
    virtualType     :: Type,
    virtualPosition :: Position
}

data Class = Class {
    superclass    :: Maybe String,
    fields        :: Map String Field,
    methods       :: Map String Function,
    virtuals      :: Map String Virtual,
    classPosition :: Position
}

data State = State {
    lastPosition :: Position,
    currentClass :: Maybe String,
    frames       :: [Map String Field],
    functions    :: Map String Function,
    classes      :: Map String Class
}

type CM = ExceptT [Error] (St.State State)

-- getters

errorNear :: String -> CM Error
errorNear s = do
    p <- getLastPosition
    return $ ErrorNear p s

getLastPosition :: CM Position
getLastPosition = lastPosition <$> get

getCurrentClass :: CM (Maybe String)
getCurrentClass = currentClass <$> get

getFrames :: CM [Map String Field]
getFrames = frames <$> get

getLocal :: String -> CM Field
getLocal l = do
    Just m <- find (M.member l) <$> getFrames
    return $ m M.! l

getFunctions :: CM (Map String Function)
getFunctions = functions <$> get

getFunction :: String -> CM Function
getFunction f = (M.! f) <$> getFunctions

getClasses :: CM (Map String Class)
getClasses = classes <$> get

getClass :: String -> CM Class
getClass c = (M.! c) <$> getClasses

getSuperclass :: String -> CM (Maybe String)
getSuperclass c = superclass <$> getClass c

getSuperclasses :: String -> CM (Set String)
getSuperclasses c = do
    ms <- getSuperclass c
    case ms of
        Nothing -> return $ S.singleton c
        Just s -> S.insert c <$> getSuperclasses s

getFields :: String -> CM (Map String Field)
getFields c = fields <$> getClass c

getFieldsT :: Type -> CM (Map String Field)
getFieldsT (TObj c) = getFields c
getFieldsT (TArr c) = return $ M.singleton "length" (Field TInt (-1, -1))
getFieldsT _ = return M.empty

getFieldT :: Type -> String -> CM Field
getFieldT t f = (M.! f) <$> getFieldsT t

getVirtuals :: String -> CM (Map String Virtual)
getVirtuals c = virtuals <$> getClass c

getVirtualsT :: Type -> CM (Map String Virtual)
getVirtualsT (TObj c) = getVirtuals c
getVirtualsT _ = return M.empty

getVirtual :: String -> String -> CM Virtual
getVirtual c v = (M.! v) <$> getVirtuals c

getVirtualT :: Type -> String -> CM Virtual
getVirtualT t v = (M.! v) <$> getVirtualsT t

getCurrentClassVirtual :: String -> CM Virtual
getCurrentClassVirtual v = do
    Just c <- getCurrentClass
    getVirtual c v

-- setters

-- conditions

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
isSubtype (TObj c1) (TObj c2) = isSubclass c1 c2
isSubtype (TArr t1) (TArr t2) = isSubtype t1 t2
isSubtype (TFun args1 r1) (TFun args2 r2) =
    pure (length args1 == length args2) &&^
    allM (\(t1, t2) -> isSubtype t2 t1) (zip args1 args2) &&^
    isSubtype r1 r2
isSubtype _ _ = return False

inClass :: CM Bool
inClass = (Nothing /=) <$> getCurrentClass

isLocal :: String -> CM Bool
isLocal l = any (M.member l) <$> getFrames

isFunction :: String -> CM Bool
isFunction f = M.member f <$> getFunctions

isClass :: String -> CM Bool
isClass c = M.member c <$> getClasses

hasFieldT :: Type -> String -> CM Bool
hasFieldT t f = M.member f <$> getFieldsT t

hasVirtual :: String -> String -> CM Bool
hasVirtual c v = M.member v <$> getVirtuals c

hasVirtualT :: Type -> String -> CM Bool
hasVirtualT t f = M.member f <$> getVirtualsT t

hasCurrentClassVirtual :: String -> CM Bool
hasCurrentClassVirtual v = do
    Just c <- getCurrentClass
    hasVirtual c v

-- modifiers

updateLastPosition :: Position -> CM ()
updateLastPosition p = modify (\st -> st { lastPosition = p })

at :: Position -> String -> CM ()
at p s = throwError [ErrorAt p s]

near :: String -> CM ()
near s = do
    ex <- errorNear s
    throwError [ex]

-- checkers

checkAt :: Position -> Bool -> String -> CM ()
checkAt p b s = unless b (at p s)

checkAtM :: Position -> CM Bool -> String -> CM ()
checkAtM p b s = unlessM b (at p s)

checkNear :: Bool -> String -> CM ()
checkNear b s = unless b (near s)

checkNearM :: CM Bool -> String -> CM ()
checkNearM b s = unlessM b (near s)

-- check expression type

typeError :: (Type, HandSide) -> (Type, HandSide) -> String
typeError (et, ehs) (it, ihs) = "Couldn't match expected " ++ show ehs ++ " " ++ show et ++ " against interfered " ++ show ihs ++ " " ++ show it ++ "."

checkExprTypeErrors :: Expr -> Type -> HandSide -> CM [Error]
checkExprTypeErrors e hs t = (checkExprType e hs t >> return []) `catchError` return

checkExprTypeHelperAt :: Position -> Expr -> Type -> HandSide -> CM ()
checkExprTypeHelperAt p e t LHS = do
    updateLastPosition p
    (t', hs') <- getExprType e
    checkAtM p (pure (hs' == LHS) &&^ isSubtype t' t) (typeError (t, LHS) (t', hs'))
checkExprTypeHelperAt p e t RHS = do
    (t', hs') <- getExprType e
    checkAtM p (isSubtype t' t) (typeError (t, RHS) (t', hs'))

checkExprTypeHelperNear :: Expr -> Type -> HandSide -> CM ()
checkExprTypeHelperNear e t LHS = do
    (t', hs') <- getExprType e
    checkNearM (pure (hs' == LHS) &&^ isSubtype t' t) (typeError (t, LHS) (t', hs'))
checkExprTypeHelperNear e t RHS = do
    (t', hs') <- getExprType e
    checkNearM (isSubtype t' t) (typeError (t, RHS) (t', hs'))

checkExprType :: Expr -> Type -> HandSide -> CM ()
checkExprType e@(EVar (PIdent (p, _))) = checkExprTypeHelperAt p e
checkExprType e@(ELitInt _) = checkExprTypeHelperNear e
checkExprType e@(EString _) = checkExprTypeHelperNear e
checkExprType e@(ELitTrue) = checkExprTypeHelperNear e
checkExprType e@(ELitFalse) = checkExprTypeHelperNear e
checkExprType e@(ENull) = checkExprTypeHelperNear e
checkExprType e@(ESelect _ (PIdent (p, _))) = checkExprTypeHelperAt p e
checkExprType e@(EMetCall _ (PIdent (p, _)) _) = checkExprTypeHelperAt p e
checkExprType e@(EAt _ _) = checkExprTypeHelperNear e
checkExprType e@(EApp (PIdent (p, _)) _) = checkExprTypeHelperAt p e
checkExprType e@(ENeg _) = checkExprTypeHelperNear e
checkExprType e@(ENot _) = checkExprTypeHelperNear e
checkExprType e@(EIncr _) = checkExprTypeHelperNear e
checkExprType e@(EDecr _) = checkExprTypeHelperNear e
checkExprType e@(ENewVar (PIdent (p, _))) = checkExprTypeHelperAt p e
checkExprType e@(ENewArr (PIdent (p, _)) _) = checkExprTypeHelperAt p e
checkExprType e@(ECastVar (PIdent (p, _)) _) = checkExprTypeHelperAt p e
checkExprType e@(ECastArr (PIdent (p, _)) _) = checkExprTypeHelperAt p e
checkExprType e@(EMul _ _ _) = checkExprTypeHelperNear e
checkExprType e@(EAdd _ _ _) = checkExprTypeHelperNear e
checkExprType e@(ERel _ _ _) = checkExprTypeHelperNear e
checkExprType e@(EAnd _ _) = checkExprTypeHelperNear e
checkExprType e@(EOr _ _) = checkExprTypeHelperNear e

-- get expression type

getExprTypeErrors :: Expr -> CM [Error]
getExprTypeErrors x = (getExprType x >> return []) `catchError` return

checkFunctionCallAt :: Position -> String -> [Type] -> [Expr] -> CM ()
checkFunctionCallAt p i argts es = do
    updateLastPosition p
    if' (length argts == length es)
        (do exs <- concatMapM (uncurry3 checkExprTypeErrors) (zip3 es argts (repeat RHS))
            case exs of
                [] -> return ()
                _ -> throwError exs)
        (do let ex = ErrorAt p ("Wrong number of parameters in " ++ i ++ " call.")
            exs <- concatMapM getExprTypeErrors es
            throwError $ ex : exs)

getNewTypeAt :: Position -> String -> CM (Type, HandSide)
getNewTypeAt p i = do
    updateLastPosition p
    let t = read i
    checkAtM p (isProperType t) (show t ++ " is not proper type.")
    checkAt p (isCreateable t) (show t ++ " cannot be created.")
    return (t, LHS)

getCastTypeAt :: Position -> Type -> Expr -> CM (Type, HandSide)
getCastTypeAt p nt e = do
    updateLastPosition p
    ifM (isProperType nt)
        (do (t, hs) <- getExprType e
            checkAtM p (isSubtype nt t ||^ isSubtype t nt) ("Cannot cast " ++ show t ++ " to " ++ show nt ++ ".")
            return (nt, hs))
        (do let ex = ErrorAt p (show nt ++ " is not a proper type.")
            exs <- getExprTypeErrors e
            throwError $ ex : exs)

checkEqualTypeExprs :: Expr -> Expr -> CM Bool
checkEqualTypeExprs e1 e2 = do
    exs <- concatMapM getExprTypeErrors [e1, e2]
    unless (null exs) (throwError exs)
    (t1, _) <- getExprType e1
    (t2, _) <- getExprType e2
    return $ t1 == t2

getExprTypeHelper :: [(Expr, Type, HandSide)] -> (Type, HandSide) -> CM (Type, HandSide)
getExprTypeHelper xs r = do
    exs <- concatMapM (uncurry3 checkExprTypeErrors) xs
    unless (null exs) (throwError exs)
    return r

getExprType :: Expr -> CM (Type, HandSide)
getExprType (EVar (PIdent (p, i))) = do
    updateLastPosition p
    checkAtM p (isLocal i) (i ++ " is not defined.")
    t <- fieldType <$> getLocal i
    return (t, LHS)
getExprType (ELitInt _) = return (TInt, RHS)
getExprType (EString _) = return (TString, RHS)
getExprType ELitTrue = return (TBool, RHS)
getExprType ELitFalse = return (TBool, RHS)
getExprType ENull = return (object, RHS)
getExprType (ESelect e (PIdent (p, i))) = do
    (t, _) <- getExprType e
    updateLastPosition p
    checkAtM p (hasFieldT t i) (show t ++ " has no field " ++ i ++ ".")
    it <- fieldType <$> getFieldT t i
    return (it, LHS)
getExprType (EMetCall e (PIdent (p, i)) es) = do
    (t, _) <- getExprType e `catchError` (\exs1 -> do
        updateLastPosition p
        exs2 <- concatMapM getExprTypeErrors es
        throwError $ exs2 ++ exs1)
    updateLastPosition p
    ifM (hasVirtualT t i)
        (do TFun argts rt <- virtualType <$> getVirtualT t i
            checkFunctionCallAt p i argts es
            return (rt, RHS))
        (do let ex = ErrorAt p (show t ++ " has no method " ++ i ++ ".")
            exs <- concatMapM getExprTypeErrors es
            throwError $ ex : exs)
getExprType (EAt e1 e2) = do
    (t, _) <- getExprType e1 `catchError` (\exs1 -> do
        exs2 <- checkExprTypeErrors e2 TInt RHS
        throwError $ exs2 ++ exs1)
    if' (isArray t)
        (do checkExprType e2 TInt RHS
            return (arrayFieldType t, LHS))
        (do ex <- errorNear (show t ++ " is not array type.")
            exs <- checkExprTypeErrors e2 TInt RHS
            throwError $ ex : exs)
getExprType (EApp (PIdent (p, i)) es) = do
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
                exs <- concatMapM getExprTypeErrors es
                throwError $ ex : exs))
getExprType (ENeg e) = checkExprType e TInt RHS >> return (TInt, RHS)
getExprType (ENot e) = checkExprType e TBool RHS >> return (TBool, RHS)
getExprType (EIncr e) = checkExprType e TInt LHS >> return (TInt, RHS)
getExprType (EDecr e) = checkExprType e TInt LHS >> return (TInt, RHS)
getExprType (ENewVar (PIdent (p, i))) = getNewTypeAt p i
getExprType (ENewArr (PIdent (p, i)) e) = do
    (t, hs) <- getNewTypeAt p (i ++ "[]") `catchError` (\exs1 -> do
        exs2 <- checkExprTypeErrors e TInt RHS
        throwError $ exs2 ++ exs1)
    checkExprType e TInt RHS
    return (t, hs)
getExprType (ECastVar (PIdent (p, i)) e) = getCastTypeAt p (read i) e
getExprType (ECastArr (PIdent (p, i)) e) = getCastTypeAt p (read $ i ++ "[]") e
getExprType (EMul e1 _ e2) = getExprTypeHelper [(e1, TInt, RHS), (e2, TInt, RHS)] (TInt, RHS)
getExprType (EAdd e1 Plus e2) = do
    exsI <- concatMapM (uncurry3 checkExprTypeErrors) [(e1, TInt, RHS), (e2, TInt, RHS)]
    exsS <- concatMapM (uncurry3 checkExprTypeErrors) [(e1, TString, RHS), (e2, TString, RHS)]
    case (exsI, exsS) of
        ([], _) -> return (TInt, RHS)
        (_, []) -> return (TString, RHS)
        _ -> throwError exsI
getExprType (ERel e1 EQU e2) = do
    checkNearM (checkEqualTypeExprs e1 e2) "== operator applied to different types."
    return (TBool, RHS)
getExprType (ERel e1 NE e2) = do
    checkNearM (checkEqualTypeExprs e1 e2) "!= operator applied to different types."
    return (TBool, RHS)
getExprType (ERel e1 _ e2) = getExprTypeHelper [(e1, TInt, RHS), (e2, TInt, RHS)] (TBool, RHS)
getExprType (EAnd e1 e2) = getExprTypeHelper [(e1, TBool, RHS), (e2, TBool, RHS)] (TBool, RHS)
getExprType (EOr e1 e2) = getExprTypeHelper [(e1, TBool, RHS), (e2, TBool, RHS)] (TBool, RHS)

{-import           BNFC.AbsLatte       hiding (Type)
import qualified BNFC.AbsLatte       as Abs
import           Control.Monad.Extra
import           Control.Monad.State hiding (State)
import qualified Control.Monad.State as St
import           Data.Foldable
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Set            (Set)
import qualified Data.Set            as S

data Type = Var String
          | Arr String
          | Fun [Type] Type
          deriving (Show)

data LR = L | R deriving (Eq)

data Field = Field {
    fieldType     :: Type,
    fieldPosition :: (Int, Int)
}

data Function = Function {
    arguments        :: [(String, Field)],
    body             :: Block,
    functionType     :: Type,
    functionPosition :: (Int, Int)
}

data Virtual = Virtual {
    virtualType     :: Type,
    virtualPosition :: (Int, Int)
}

data Class = Class {
    superclass    :: Maybe String,
    fields        :: Map String Field,
    methods       :: Map String Function,
    virtuals      :: Map String Virtual,
    classPosition :: (Int, Int)
}

data State = State {
    errors       :: Map (Int, Int, Bool) String,
    lastPosition :: (Int, Int),
    frames       :: [Map String Field],
    functions    :: Map String Function,
    classes      :: Map String Class
}

type IM = St.State State

-- helpers

bnfcTypeToTypePosition :: Abs.Type -> (Type, (Int, Int))
bnfcTypeToTypePosition (TVar (PIdent (p, i))) = (Var i, p)
bnfcTypeToTypePosition (TArr (PIdent (p, i))) = (Arr i, p)

bnfcTypeToType :: Abs.Type -> Type
bnfcTypeToType t = fst $ bnfcTypeToTypePosition t

unknownType = Var "unknown"
voidType = Var "void"
boolType = Var "boolean"
intType = Var "int"
stringType = Var "string"
anyType = Var "null"

-- getters

getLastPosition :: IM (Int, Int)
getLastPosition = lastPosition <$> get

getFrames :: IM [Map String Field]
getFrames = frames <$> get

getBlock :: IM (Map String Field)
getBlock = head <$> getFrames

getLocal :: String -> IM Field
getLocal l = do
    Just m <- find (M.member l) <$> getFrames
    return $ m M.! l

getFunctions :: IM (Map String Function)
getFunctions = functions <$> get

getFunction :: String -> IM Function
getFunction f = (M.! f) <$> getFunctions

getClasses :: IM (Map String Class)
getClasses = classes <$> get

getClass :: String -> IM Class
getClass c = (M.! c) <$> getClasses

getSuperclass :: String -> IM (Maybe String)
getSuperclass c = superclass <$> getClass c

getSuperclasses :: String -> IM (Set String)
getSuperclasses c = do
    ms <- getSuperclass c
    case ms of
        Nothing -> return $ S.singleton c
        Just s -> S.insert c <$> getSuperclasses s

getFields :: String -> IM (Map String Field)
getFields c = fields <$> getClass c

getTypeFields :: Type -> IM (Map String Field)
getTypeFields (Var c) = getFields c
getTypeFields (Arr c) = M.insert "length" (Field intType (-1, -1)) <$> getFields c

getField :: String -> String -> IM Field
getField c f = (M.! f) <$> getFields c

getTypeField :: Type -> String -> IM Field
getTypeField t f = (M.! f) <$> getTypeFields t

getMethods :: String -> IM (Map String Function)
getMethods c = methods <$> getClass c

getMethod :: String -> String -> IM Function
getMethod c f = (M.! f) <$> getMethods c

getVirtuals :: String -> IM (Map String Virtual)
getVirtuals c = virtuals <$> getClass c

getTypeVirtuals :: Type -> IM (Map String Virtual)
getTypeVirtuals (Var c) = getVirtuals c
getTypeVirtuals (Arr c) = M.empty

getVirtual :: String -> String -> IM Virtual
getVirtual c f = (M.! f) <$> getVirtuals c

getTypeVirtual :: Type -> String -> IM Virtual
getTypeVirtual t f = (M.! f) <$> getTypeVirtuals t

getClassPosition :: String -> IM (Int, Int)
getClassPosition s = classPosition <$> getClass s

-- conditions

isBlock :: String -> IM Bool
isBlock l = M.member l <$> getBlock

isLocal :: String -> IM Bool
isLocal l = any (M.member l) <$> getFrames

isFunction :: String -> IM Bool
isFunction f = M.member f <$> getFunctions

isClass :: String -> IM Bool
isClass c = M.member c <$> getClasses

isField :: String -> String -> IM Bool
isField c f = M.member f <$> getFields c

isTypeField :: Type -> String -> IM Bool
isTypeField t f = M.member f <$> getTypeFields t

isMethod :: String -> String -> IM Bool
isMethod c f = M.member f <$> getMethods c

isVirtual :: String -> String -> IM Bool
isVirtual c f = M.member f <$> getVirtuals c

isTypeVirtual :: Type -> String -> IM Bool
isTypeVirtual t f = M.member f <$> getTypeVirtuals

isProperType :: Type -> IM Bool
isProperType (Var c) = isClass c
isProperType (Arr c) = isClass c
isProperType (Fun args r) = isProperType r &&^ allM isProperType args

isSubclass :: String -> String -> IM Bool
isSubclass c1 c2 = S.member c2 <$> getSuperclasses c1

isSubtype :: Type -> Type -> IM Bool
isSubtype (Var c1) (Var c2) = isSubclass c1 c2
isSubtype (Arr c1) (Arr c2) = isSubclass c1 c2
isSubtype (Fun args1 r1) (Fun args2 r2) =
    pure (length args1 == length args2) &&^
    allM (\(t1, t2) -> isSubtype t2 t1) (zip args1 args2) &&^
    isSubtype r1 r2
isSubtype _ _ = return False

-- setters

notify :: String -> (Int, Int) -> IM ()
notify s (c, l) = modify (\st -> st { errors = M.insert (c, l, True) s $ errors st })

notify' :: String -> IM ()
notify' s = do
    (c, l) <- getLastPosition
    modify (\st -> st { errors = M.insert (c, l, False) s $ errors st })

check :: Bool -> String -> (Int, Int) -> IM ()
check b s p = unless b $ notify s p

check' :: Bool -> String -> IM ()
check' b s = unless b $ notify' s

checkM :: IM Bool -> String -> (Int, Int) -> IM ()
checkM b s p = unlessM b $ notify s p

checkM' :: IM Bool -> String -> IM ()
checkM' b s = unlessM b $ notify' s

setLastPosition :: (Int, Int) -> IM ()
setLastPosition p = modify (\st -> st { lastPosition = p })

pushFrame :: IM ()
pushFrame = modify (\st -> st { frames = M.empty : frames st })

popFrame :: IM ()
popFrame = modify (\st -> st { frames = tail $ frames st })

frame :: IM a -> IM a
frame b = do
    pushFrame
    r <- b
    popFrame
    return r

setLocal :: String -> Type -> (Int, Int) -> IM ()
setLocal l t p = modify (\st -> st { frames = M.insert l (Field t p) (head $ frames st) : tail (frames st) })

setFunction :: String -> [(String, Field)] -> Block -> Type -> (Int, Int) -> IM ()
setFunction f a b t p = modify (\st -> st { functions = M.insert f (Function a b t p) (functions st) })

setClass :: String -> Maybe String -> Map String Field -> Map String Function -> Map String Virtual -> (Int, Int) -> IM ()
setClass c s fs ms vs p = modify (\st -> st { classes = M.insert c (Class s fs ms vs p) (classes st) })

setField :: String -> String -> Type -> (Int, Int) -> IM ()
setField c f t p = do
    (Class s fs ms vs cp) <- getClass c
    setClass c s (M.insert f (Field t p) fs) ms vs cp

setMethod :: String -> String -> [(String, Field)] -> Block -> Type -> (Int, Int) -> IM ()
setMethod c m a b t p = do
    (Class s fs ms vs cp) <- getClass c
    setClass c s fs (M.insert m (Function a b t p) ms) vs cp

setVirtual :: String -> String -> Type -> (Int, Int) -> IM ()
setVirtual c v t p = do
    (Class s fs ms vs cp) <- getClass c
    setClass c s fs ms (M.insert v (Virtual t p) vs) cp

-- collect definitions

collectDefinitionsInProgram :: Program -> IM ()
collectDefinitionsInProgram (Program xs) = forM_ xs collectDefinitionsInTopDef

collectDefinitionsInTopDef :: TopDef -> IM ()
collectDefinitionsInTopDef (FnDef brt (PIdent (p, f)) args b) = do
    checkM (notM $ isFunction f) ("Function " ++ f ++ " already exists.") p
    setFunction f args' b (Fun argts rt) p
    where
        rt = bnfcTypeToType brt
        args' = map (\(Arg t (PIdent (p, a))) -> (a, Field (bnfcTypeToType t) p)) args
        argts = map (\(_, f) -> fieldType f) args'
collectDefinitionsInTopDef (TopClsDef (PIdent (p, c)) xs) = do
    checkM (notM $ isClass c) ("Class " ++ c ++ " already exists.") p
    setClass c Nothing M.empty M.empty M.empty p
    forM_ xs $ collectDefinitionsInClsDef c
collectDefinitionsInTopDef (ExtClsDef (PIdent (p, c)) (PIdent (_, s)) xs) = do
    checkM (notM $ isClass c) ("Class " ++ c ++ " already exists.") p
    setClass c (Just s) M.empty M.empty M.empty p
    forM_ xs $ collectDefinitionsInClsDef c

collectDefinitionsInClsDef :: String -> ClsDef -> IM ()
collectDefinitionsInClsDef c (VarDef bt props) =
    forM_ props (\(PIdent (p, prop)) -> do
        checkM (notM $ isField c prop) ("Property " ++ prop ++ " already exists in class " ++ c ++ ".") p
        setField c prop t p)
    where
        t = bnfcTypeToType bt
collectDefinitionsInClsDef c (MetDef brt (PIdent (p, f)) args b) = do
    checkM (notM $ isMethod c f) ("Method " ++ f ++ " already exists in class " ++ c ++ ".") p
    setMethod c f args' b (Fun argts rt) p
    where
        rt = bnfcTypeToType brt
        args' = map (\(Arg t (PIdent (p, a))) -> (a, Field (bnfcTypeToType t) p)) args
        argts = map (\(_, f) -> fieldType f) args'

-- check type names

checkType :: Type -> IM ()
checkType (Var c) = checkM' (isClass c) ("Class " ++ c ++ " is not defined.")
checkType (Arr c) = checkM' (isClass c) ("Class " ++ c ++ " is not defined.")
checkType (Fun args r) = checkType r >> forM_ args checkType

checkTypesInExpr :: Expr -> IM ()
checkTypesInExpr (EVar (PIdent (p, _))) = setLastPosition p
checkTypesInExpr (ELitInt _) = return ()
checkTypesInExpr (EString _) = return ()
checkTypesInExpr ELitTrue = return ()
checkTypesInExpr ELitFalse = return ()
checkTypesInExpr ENull = return ()
checkTypesInExpr (ESelect e (PIdent (p, _))) = checkTypesInExpr e >> setLastPosition p
checkTypesInExpr (EMetCall e (PIdent (p, _)) es) = checkTypesInExpr e >> setLastPosition p >> forM_ es checkTypesInExpr
checkTypesInExpr (EAt e1 e2) = forM_ [e1, e2] checkTypesInExpr
checkTypesInExpr (EApp (PIdent (p, _)) es) = setLastPosition p >> forM_ es checkTypesInExpr
checkTypesInExpr (ENeg e) = checkTypesInExpr e
checkTypesInExpr (ENot e) = checkTypesInExpr e
checkTypesInExpr (EIncr e) = checkTypesInExpr e
checkTypesInExpr (EDecr e) = checkTypesInExpr e
checkTypesInExpr (ENewVar (PIdent (p, c))) = setLastPosition p >> checkType (Var c)
checkTypesInExpr (ENewArr (PIdent (p, c)) e) = setLastPosition p >> checkType (Arr c) >> checkTypesInExpr e
checkTypesInExpr (ECastArr (PIdent (p, c)) e) = setLastPosition p >> checkType (Arr c) >> checkTypesInExpr e
checkTypesInExpr (ECastVar (PIdent (p, c)) e) = setLastPosition p >> checkType (Var c) >> checkTypesInExpr e
checkTypesInExpr (EMul e1 _ e2) = forM_ [e1, e2] checkTypesInExpr
checkTypesInExpr (EAdd e1 _ e2) = forM_ [e1, e2] checkTypesInExpr
checkTypesInExpr (ERel e1 _ e2) = forM_ [e1, e2] checkTypesInExpr
checkTypesInExpr (EAnd e1 e2) = forM_ [e1, e2] checkTypesInExpr
checkTypesInExpr (EOr e1 e2) = forM_ [e1, e2] checkTypesInExpr

checkTypesInItem :: Item -> IM ()
checkTypesInItem (NoInit (PIdent (p, _))) = setLastPosition p
checkTypesInItem (Init (PIdent (p, _)) e) = setLastPosition p >> checkTypesInExpr e

checkTypesInStmt :: Stmt -> IM ()
checkTypesInStmt Empty = return ()
checkTypesInStmt (BStmt b) = checkTypesInBlock b
checkTypesInStmt (Decl bt xs) = checkType (bnfcTypeToType bt) >> forM_ xs checkTypesInItem
checkTypesInStmt (Ass e1 e2) = checkTypesInExpr e1 >> checkTypesInExpr e2
checkTypesInStmt (Ret e) = checkTypesInExpr e
checkTypesInStmt VRet = return ()
checkTypesInStmt (Cond e s) = checkTypesInExpr e >> checkTypesInStmt s
checkTypesInStmt (CondElse e s1 s2) = checkTypesInExpr e >> checkTypesInStmt s1 >> checkTypesInStmt s2
checkTypesInStmt (While e s) = checkTypesInExpr e >> checkTypesInStmt s
checkTypesInStmt (For bt (PIdent (p, _)) e s) = setLastPosition p >> checkType (bnfcTypeToType bt) >> checkTypesInExpr e >> checkTypesInStmt s
checkTypesInStmt (SExp e) = checkTypesInExpr e

checkTypesInBlock :: Block -> IM ()
checkTypesInBlock (Block xs) = forM_ xs checkTypesInStmt

checkTypesInFunction :: Function -> IM ()
checkTypesInFunction (Function _ b t p) = setLastPosition p >> checkType t >> checkTypesInBlock b

checkTypes :: IM ()
checkTypes = do
    fs <- getFunctions
    forM_ fs checkTypesInFunction
    cs <- getClasses
    forM_ cs (\(Class s' fs ms _ p) -> do
        setLastPosition p
        case s' of
            Nothing -> return ()
            Just s -> checkM' (isClass s) ("Class " ++ s ++ " is not defined.")
        forM_ fs (\(Field t p) -> setLastPosition p >> checkType t)
        forM_ ms checkTypesInFunction)

-- check inheritance cycles

checkInheritanceCycles :: IM ()
checkInheritanceCycles = do
    cl <- M.keys <$> getClasses
    foldM_ (\prevVis c ->
        if S.member c prevVis
            then return prevVis
            else S.union prevVis <$> dfs c prevVis S.empty) S.empty cl
    where
        dfs :: String -> Set String -> Set String -> IM (Set String)
        dfs v prevVis currVis = do
            let currVis' = S.insert v currVis
            ms <- getSuperclass v
            case ms of
                Nothing -> return currVis'
                Just s ->
                    if | S.member s currVis' -> do
                            p <- getClassPosition v
                            notify ("Class " ++ v ++ " is on cycle in inheritance tree.") p
                            return currVis'
                       | S.member s prevVis -> return currVis'
                       | otherwise -> dfs s prevVis currVis'

-- create virtuals

createVirtuals :: IM ()
createVirtuals = do
    ns <- neighbours Nothing
    forM_ ns dfs
    where
        neighbours :: Maybe String -> IM [String]
        neighbours ms = do
            cs <- M.keys <$> getClasses
            ss <- mapM getSuperclass cs
            return $ map fst $ filter (\(_, s) -> s == ms) (zip cs ss)

        dfs :: String -> IM ()
        dfs c = do
            ms <- getSuperclass c
            case ms of
                Nothing -> return ()
                Just s -> do
                    cms <- getMethods c
                    forM_ (M.toList cms) (\(m, Function _ _ t p) ->
                        whenM (isVirtual s m) (do
                            Virtual st (sl, sc) <- getVirtual s m
                            checkM (isSubtype t st) ("Method " ++ m ++ "'s type " ++ show t ++ " is not subtype of " ++ show st
                                                     ++ ". Method previously defined in line " ++ show sl ++ ", column " ++ show sc ++ ".") p))
                    sms <- getVirtuals s
                    forM_ (M.toList sms) (\(m, Virtual t p) -> setVirtual c m t p)
                    forM_ (M.toList cms) (\(m, Function _ _ t p) -> setVirtual c m t p)
            ns <- neighbours (Just c)
            forM_ ns dfs

-- check

getExprType :: Expr -> IM (Type, LR)
getExprType (EVar (PIdent (p, l))) = do
    setLastPosition p
    unlessM (isLocal l)
        (do notify ("Variable " ++ l ++ " is not defined.") p
            setLocal l unknownType p)
    Field t _ <- getLocal l
    return (t, L)
getExprType (ELitInt _) = return (intType, R)
getExprType (EString _) = return (stringType, R)
getExprType ELitTrue = return (boolType, R)
getExprType ELitFalse = return (boolType, R)
getExprType ENull = return (anyType, R)
getExprType (ESelect e (PIdent (p, f))) = do
    setLastPosition p
    (et, _) <- getExprType e
    ifM (isTypeField et f)
        (do ft <- getTypeField et f
            return (ft, L))
        (do notify ("Type " ++ show et ++ " has no field " ++ show f ++ ".") p
            return (unknownType, L))
getExprType (EMetCall e (PIdent (p, f)) es) = do
    setLastPosition p
    (et, _) <- getExprType e
    ifM (isTypeVirtual et f)
        (do Virtual t _ <- getTypeVirtual et f
            return (t, R))
        (do notify ("Type " ++ show et ++ " has no method " ++ show f ++ ".") p
            return (unknownType, L))
getExprType (EAt e1 e2) = do
    (e1t, _) <- getExprType e1
    (e2t, _) <- getExprType e2
    case (e1t, e2t) of
        (Arr c, Var "int") -> return (Var c, L)
        (Arr c, _) -> do
            notify' ("Cannot use " ++ show e2t ++ " as index.")
            return (Var c, L)
        _ -> do
            notify' ("Cannot access index of " ++ show e1t ++ ".")
            return (unknownType, L)
getExprType (EApp (PIdent (p, f)) es) = do
    setLastPosition p
    ets <- fst <$> mapM getExprType es
    ifM (isLocal "self" &&^ (do Field st _ <- getLocal "self"; isTypeVirtual st f))
        (do st <- getLocal "self"
            Virtual ft@(Fun fts frt) _ <- getTypeVirtual st f
            checkM (isSubtype (Fun ets frs) ft) ("Wrong type parameters.") p
                (do ))

{-
Expr
    = EVar PIdent
    | ELitInt Integer
    | EString String
    | ELitTrue
    | ELitFalse
    | ENull
    | ESelect Expr PIdent
    | EMetCall Expr PIdent [Expr]
    | EAt Expr Expr
    | EApp PIdent [Expr]
    | ENeg Expr
    | ENot Expr
    | EIncr Expr
    | EDecr Expr
    | ENewVar PIdent
    | ENewArr PIdent Expr
    | ECastArr PIdent Expr
    | ECastVar PIdent Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr-}

checkItem :: Type -> Item -> IM ()
checkItem t (NoInit (PIdent (p, l))) = setLastPosition p >> setLocal l t p
checkItem t (Init (PIdent (p, l)) e) = do
    setLastPosition p
    (et, _) <- getExprType e
    checkM (isSubtype et t) ("Cannot assign " ++ show et ++ " to " ++ show t) p
    setLocal l t p

checkConditionExpr :: Expr -> IM ()
checkConditionExpr e = do
    (et, _) <- getExprType e
    check' (et == booleanType) ("Cannot use " ++ show et ++ " as condition.")

checkStmt :: Type -> Stmt -> IM ()
checkStmt _ Empty = return ()
checkStmt rt (BStmt b) = checkBlock rt b
checkStmt _ (Decl t xs) = forM_ xs $ checkItem (bnfcTypeToType t)
checkStmt _ (Ass el er) = do
    (elt, lr) <- getExprType el
    check' (lr == L) "Cannot assign to left-hand side expression."
    (ert, _) <- getExprType er
    checkM' (isSubtype ert elt) ("Cannot assign " ++ show ert ++ " to " ++ show elt ++ ".")
checkStmt rt (Ret e) = do
    (et, _) <- getExprType e
    checkM' (isSubtype et rt) ("Cannot return " ++ show et ++ " in function returning " ++ show rt ++ ".")
checkStmt rt VRet = check' (rt == voidType) ("Cannot return " ++ show (Var "void") ++ " in function returning " ++ show rt ++ ".")
checkStmt rt (Cond e s) = do
    checkConditionExpr e
    checkStmt rt
checkStmt rt (CondElse e st sf) = do
    checkConditionExpr e
    checkStmt rt st
    checkStmt rt sf
checkStmt rt (While e s) = do
    checkConditionExpr e
    checkStmt rt s
checkStmt rt (For bt (PIdent (p, i)) e s) = do
    setLastPosition p
    (et, _) <- getExprType e
    checkM (isSubtype et t) ("Cannot assign " ++ show et ++ " to " ++ show t ++ ".") p
    checkStmt rt s
    where
        t = bnfcTypeToType bt
checkStmt rt (SExp e) = void $ getExprType e


checkBlock :: Type -> Block -> IM ()
checkBlock rt (Block xs) = frame $ forM_ xs (checkStmt rt)

checkFunction :: Function -> IM ()
checkFunction (Function args b (Fun _ rt) fp) =
    frame (do
        forM_ args (\(l, Field lt lp) -> do
            checkM (notM $ isBlock l) ("Parameter name " ++ l ++  "already used.") lp
            setLocal l lt fp)
        checkBlock rt b)

checkAll :: IM ()
checkAll = do
    fs <- getFunctions
    forM_ fs checkFunction
    cs <- M.toList <$> getClasses
    forM_ cs (\(c, Class _ fs ms vs cp) ->
        frame (do
            setLocal "self" (Var c) cp
            forM_ ms checkFunction))

{-
Expr
    = EVar PIdent
    | ELitInt Integer
    | EString String
    | ELitTrue
    | ELitFalse
    | ENull
    | ESelect Expr PIdent
    | EMetCall Expr PIdent [Expr]
    | EAt Expr Expr
    | EApp PIdent [Expr]
    | ENeg Expr
    | ENot Expr
    | EIncr Expr
    | EDecr Expr
    | ENewVar PIdent
    | ENewArr PIdent Expr
    | ECastArr PIdent Expr
    | ECastVar PIdent Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr



checkTypesInProgram :: Program -> IM ()
checkTypesInProgram (Program xs) = forM_ xs checkTypesInTopDef

checkTypesInTopDef :: TopDef -> IM ()
checkTypesInTopDef (FnDef brt (PIdent (_, f)) args b) =
    frame (do
        forM_ args (\(Arg bt, PIdent (p, a)) -> do
            checkM (notM $ isBlock a) ("Parameter name " ++ a ++ " in function " ++ f ++ " already used.") p
            setLocal a (bnfcTypeToType bt) p)
        checkTypesInBlock rt b)
    where
        rt = bnfcTypeToType brt
checkTypesInTopDef (TopClsDef (PIdent (_, c)) xs) = forM_ (checkTypesInClsDef c) xs
checkTypesInTopDef (ExtClsDef (PIdent (_, c)) _ xs) = forM_ (checkTypesInClsDef c) xs

checkTypesInClsDef c (MetDef brt (PIdent (fp, f)) args b) =
    frame (do
        setLocal "self" (Var c) fp
        frame (do
            forM_ args (\(Arg bt, PIdent (p, a)) -> do
                checkM (notM $ isBlock a) ("Parameter name " ++ a ++ " in method " ++ f ++ " already used.") p
                setLocal a (bnfcTypeToType bt) p)
            checkTypesInBlock rt b))
    where
        rt = bnfcTypeToType brt
checkTypesInClsDef _ _ = return ()

checkTypesInBlock :: Type -> Block -> IM ()
checkTypesInBlock rt (Block xs) = frame $ forM_ xs (checkTypesInStmt rt)

checkTypesInStmt :: Type -> Stmt -> IM ()
checkTypesInStmt _ Empty = return ()
checkTypesInStmt rt (BStmt b) = checkTypesInBlock rt b
checkTypesInStmt _ (Decl bt ls) = forM_ ls (checkTypesInItem (bnfcTypeToType t))
checkTypesInStmt _ (Ass el er) = do
    (elt, lr) <- getExprType el
    case lr of
        R -> notify ("Cannot assign to the right-hand side.") (-1, -1) -- TODO getExprPosition
        L -> return ()
    (ert, _) <- getExprType er
    checkM (isSubtype ert elt) ("Type " ++ elt ++ " is not subtype of " ++ ert ++ ".") (-1, -1) -- TODO
checkTypesInStmt rt (Ret e) = do
    (et, _) <- getExprType e
    checkM (isSubtype et rt)
checkTypesInStmt rt VRet = isSubtype (Var "void") rt
checkTypesInStmt rt (Cond e s) = do
    (et, _) <- getExprType e
    checkM (isSubtype et (Var "boolean")) ("Type " ++ show elt ++ " cannot be used in condition.") (-1, -1) -- TODO
    checkTypesInStmt rt s
checkTypesInStmt rt (CondElse e s1 s2) = do
    (et, _) <- getExprType e
    checkM (isSubtype et (Var "boolean")) ("Type " ++ show elt ++ " cannot be used in condition.") (-1, -1) -- TODO
    checkTypesInStmt rt s1
    checkTypesInStmt rt s2
checkTypesInStmt rt (While e s) = do
    (et, _) <- getExprType e
    checkM (isSubtype et (Var "boolean")) ("Type " ++ show elt ++ " cannot be used in condition.") (-1, -1) -- TODO
    checkTypesInStmt rt s
checkTypesInStmt rt (For blt (PIdent (p, l) e s) = do
    (et, _) <- getExprType e
    checkM (isSubtype et )
    where
        lt <- bnfcTypeToType blt
                        | SExp Expr


checkTypesInItem :: Type -> Item -> IM ()
checkTypesInItem t (NoInit (PIdent (p, l))) = do
    checkM (notM $ isBlock l) ("Variable " ++ l ++ " already defined in block.") p
    setLocal l t p
checkTypesInItem t (Init (PIdent (p, l)) e) = do
    checkM (notM $ isBlock l) ("Variable " ++ l ++ " already defined in block.") p
    (et, _) <- getExprType e
    checkM (isSubtype et t) ("Cannot assign value of type " ++ show t ++ " to variable " ++ l
                             ++ " of type " ++ show t ++ ".") p
    setLocal l t p-}
-}
