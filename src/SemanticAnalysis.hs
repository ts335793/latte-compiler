{-# LANGUAGE MultiWayIf #-}

module SemanticAnalysis where

import           BNFC.AbsLatte       hiding (Type)
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

data Class = Class {
    super   :: Maybe String,
    fields  :: Map String (Type, (Int, Int)),
    methods :: Map String (Type, (Int, Int))
}

data State = State {
    errors    :: [(String, (Int, Int))],
    frames    :: [Map String (Type, (Int, Int))],
    functions :: Map String (Type, (Int, Int)),
    classes   :: Map String (Class, (Int, Int))
}

type IM = St.State State

-- helpers

bnfcTypeToTypePosition :: Abs.Type -> (Type, (Int, Int))
bnfcTypeToTypePosition (TVar (PIdent (p, i))) = (Var i, p)
bnfcTypeToTypePosition (TArr (PIdent (p, i))) = (Arr i, p)

bnfcTypeToType :: Abs.Type -> Type
bnfcTypeToType t = fst $ bnfcTypeToTypePosition t

-- getters

getFrames :: IM [Map String (Type, (Int, Int))]
getFrames = frames <$> get

getBlock :: IM (Map String (Type, (Int, Int)))
getBlock = head <$> getFrames

getLocal :: String -> IM (Type, (Int, Int))
getLocal l = do
    Just m <- find (M.member l) <$> getFrames
    return $ m M.! l

getFunctions :: IM (Map String (Type, (Int, Int)))
getFunctions = functions <$> get

getFunction :: String -> IM (Type, (Int, Int))
getFunction f = (M.! f) <$> getFunctions

getFunctionType :: String -> IM Type
getFunctionType f = fst <$> getFunction f

getFunctionPosition :: String -> IM (Int, Int)
getFunctionPosition f = snd <$> getFunction f

getClasses :: IM (Map String (Class, (Int, Int)))
getClasses = classes <$> get

getClass :: String -> IM (Class, (Int, Int))
getClass c = (M.! c) <$> getClasses

getSuper :: String -> IM (Maybe String)
getSuper c = super . fst <$> getClass c

getClassClasses :: String -> IM (Set String)
getClassClasses c = do
    ms <- getSuper c
    case ms of
        Nothing -> return S.empty
        Just s -> S.insert s <$> getClassClasses s

getFields :: String -> IM (Map String (Type, (Int, Int)))
getFields c = fields . fst <$> getClass c

getField :: String -> String -> IM (Type, (Int, Int))
getField c f = (M.! f) <$> getFields c

getMethods :: String -> IM (Map String (Type, (Int, Int)))
getMethods c = methods . fst <$> getClass c

getMethod :: String -> String -> IM (Type, (Int, Int))
getMethod c f = (M.! f) <$> getMethods c

getClassPosition :: String -> IM (Int, Int)
getClassPosition c = snd <$> getClass c

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

isMethod :: String -> String -> IM Bool
isMethod c f = M.member f <$> getMethods c

isProperType :: Type -> IM Bool
isProperType (Var c) = isClass c
isProperType (Arr c) = isClass c
isProperType (Fun args r) = isProperType r &&^ allM isProperType args

isSubclass :: String -> String -> IM Bool
isSubclass c1 c2 = S.member c2 <$> getClassClasses c1

isSubtype :: Type -> Type -> IM Bool
isSubtype (Var c1) (Var c2) = isSubclass c1 c2
isSubtype (Arr c1) (Arr c2) = isSubclass c1 c2
isSubtype (Fun args1 r1) (Fun args2 r2) =
    return (length args1 == length args2) &&^
    allM (\(t1, t2) -> isSubtype t2 t1) (zip args1 args2) &&^
    isSubtype r1 r2
isSubtype _ _ = return False

-- setters

pushFrame :: IM ()
pushFrame = modify (\st -> st { frames = M.empty : frames st })

popFrame :: IM ()
popFrame = modify (\st -> st { frames = tail $ frames st })

setLocal :: String -> Type -> (Int, Int) -> IM ()
setLocal l t p = modify (\st -> st { frames = M.insert l (t, p) (head $ frames st) : tail (frames st) })

setFunction :: String -> Type -> (Int, Int) -> IM ()
setFunction f t p = modify (\st -> st { functions = M.insert f (t, p) (functions st) })

setClass :: String -> Maybe String -> Map String (Type, (Int, Int)) -> Map String (Type, (Int, Int)) -> (Int, Int) -> IM ()
setClass c s fs ms p = modify (\st -> st { classes = M.insert c (Class s fs ms, p) (classes st) })

setField :: String -> String -> Type -> (Int, Int) -> IM ()
setField c f t p = do
    (Class s fs ms, cp) <- getClass c
    setClass c s (M.insert f (t, p) fs) ms cp

setMethod :: String -> String -> Type -> (Int, Int) -> IM ()
setMethod c m t p = do
    (Class s fs ms, cp) <- getClass c
    setClass c s fs (M.insert m (t, p) ms) cp

-- errors

notify :: String -> (Int, Int) -> IM ()
notify s p = modify (\st -> st { errors = (s, p) : errors st })

check :: Bool -> String -> (Int, Int) -> IM ()
check b s p = unless b $ notify s p

checkM :: IM Bool -> String -> (Int, Int) -> IM ()
checkM b s p = unlessM b $ notify s p

-- collect definitions

collectDefinitionsInProgram :: Program -> IM ()
collectDefinitionsInProgram (Program xs) = forM_ xs collectDefinitionsInTopDef

collectDefinitionsInTopDef :: TopDef -> IM ()
collectDefinitionsInTopDef (FnDef brt (PIdent (p, f)) args _) = do
    checkM (notM $ isFunction f) ("Function " ++ f ++ " already exists.") p
    setFunction f (Fun argts rt) p
    where
        rt = bnfcTypeToType brt
        argts = map (\(Arg t _) -> bnfcTypeToType t) args
collectDefinitionsInTopDef (TopClsDef (PIdent (p, c)) xs) = do
    checkM (notM $ isClass c) ("Class " ++ c ++ " already exists.") p
    setClass c Nothing M.empty M.empty p
    forM_ xs $ collectDefinitionsInClsDef c
collectDefinitionsInTopDef (ExtClsDef (PIdent (p, c)) (PIdent (_, s)) xs) = do
    checkM (notM $ isClass c) ("Class " ++ c ++ " already exists.") p
    setClass c (Just s) M.empty M.empty p
    forM_ xs $ collectDefinitionsInClsDef c

collectDefinitionsInClsDef :: String -> ClsDef -> IM ()
collectDefinitionsInClsDef c (VarDef bt props) =
    forM_ props (\(PIdent (p, prop)) -> do
        checkM (notM $ isField c prop) ("Property " ++ prop ++ " already exists in class " ++ c ++ ".") p
        setField c prop t p)
    where
        t = bnfcTypeToType bt
collectDefinitionsInClsDef c (MetDef brt (PIdent (p, f)) args _) = do
    checkM (notM $ isMethod c f) ("Method " ++ f ++ " already exists in class " ++ c ++ ".") p
    setMethod c f (Fun argts rt) p
    where
        rt = bnfcTypeToType brt
        argts = map (\(Arg t _) -> bnfcTypeToType t) args

-- check type consistence in definitions

checkDefinitionsConsistency :: IM ()
checkDefinitionsConsistency = do
    fs <- M.toList <$> getFunctions
    forM_ fs (\(f, (t, p)) -> checkM (isProperType t) ("Type " ++ show t ++ " of functions " ++ f ++ " is incorrect.") p)
    cs <- M.toList <$> getClasses
    forM_ cs (\(c, (_, p)) -> do
        ms <- getSuper c
        case ms of
            Nothing -> return ()
            Just s -> checkM (isClass s) ("Class " ++ s ++ " is not defined.") p
        fs <- M.toList <$> getFields c
        forM_ fs (\(f, (t, p)) -> checkM (isProperType t) ("Type " ++ show t ++ " of property " ++ f ++ " is incorrect.") p)
        ms <- M.toList <$> getMethods c
        forM_ ms (\(m, (t, p)) -> checkM (isProperType t) ("Type " ++ show t ++ " of method " ++ m ++ " is incorrect.") p))

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
            ms <- getSuper v
            case ms of
                Nothing -> return currVis'
                Just s ->
                    if | S.member s currVis' -> do
                            p <- getClassPosition v
                            notify ("Class " ++ v ++ " is on cycle in inheritance tree.") p
                            return currVis'
                       | S.member s prevVis -> return currVis'
                       | otherwise -> dfs s prevVis currVis'

-- propagate virtual methods

propagateMethods :: IM ()
propagateMethods = do
    ns <- neighbours Nothing
    forM_ ns dfs
    where
        neighbours :: Maybe String -> IM [String]
        neighbours ms = do
            cs <- M.keys <$> getClasses
            ss <- mapM getSuper cs
            return $ map fst $ filter (\(_, s) -> s == ms) (zip cs ss)

        dfs :: String -> IM ()
        dfs c = do
            ms <- getSuper c
            cms <- getMethods c
            case ms of
                Nothing -> return ()
                Just s -> do
                    forM_ (M.toList cms) (\(cm, (ct, cp)) ->
                        whenM (isMethod s cm) (do
                            (st, (l, c)) <- getMethod s cm
                            checkM (isSubtype ct st) ("Type " ++ show ct ++ " of method " ++ cm ++ " is not subtype of "
                                                      ++ show st ++ " inherited from line " ++ show l ++ ", column "
                                                      ++ show c ++ ".") cp))
                    sms <- getMethods s
                    forM_ (M.toList sms) (\(m, (t, p)) -> setMethod c m t p)
                    forM_ (M.toList cms) (\(m, (t, p)) -> setMethod c m t p)
            ns <- neighbours (Just c)
            forM_ ns dfs

