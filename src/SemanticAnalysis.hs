{-# LANGUAGE MultiWayIf #-}

module SemanticAnalysis where

import           BNFC.AbsLatte       hiding (Type)
import qualified BNFC.AbsLatte       as Abs
import           Control.Bool
import           Control.Monad.State hiding (State)
import qualified Control.Monad.State as St
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Set            (Set)
import qualified Data.Set            as S

data Type = Var String
          | Arr String
          | Fun [Type] Type

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

isFunction :: String -> IM Bool
isFunction f = M.member f <$> getFunctions

isClass :: String -> IM Bool
isClass c = M.member c <$> getClasses

isField :: String -> String -> IM Bool
isField c f = M.member f <$> getFields c

isMethod :: String -> String -> IM Bool
isMethod c f = M.member f <$> getMethods c

-- setters

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

notify :: String -> (Int, Int) -> IM ()
notify s p = modify (\st -> st { errors = (s, p) : errors st })

check :: Bool -> String -> (Int, Int) -> IM ()
check b s p = unless b $ notify s p

checkM :: IM Bool -> String -> (Int, Int) -> IM ()
checkM b s p = unlessM b $ notify s p

checkCycles :: IM ()
checkCycles = do
    cs <- getClasses
    let cl = M.keys cs
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
                            notify "Found cycle in inheritance tree." p
                            return currVis'
                       | S.member s prevVis -> return currVis'
                       | otherwise -> dfs s prevVis currVis'

collectDefinitionsProgram :: Program -> IM ()
collectDefinitionsProgram (Program xs) = forM_ xs collectDefinitionsTopDef

collectDefinitionsTopDef :: TopDef -> IM ()
collectDefinitionsTopDef (FnDef brt (PIdent (p, f)) args _) = do
    checkM (notM $ isFunction f) ("Function " ++ f ++ " already exists.") p
    setFunction f (Fun argts rt) p
    where
        rt = bnfcTypeToType brt
        argts = map (\(Arg t _) -> bnfcTypeToType t) args
collectDefinitionsTopDef (TopClsDef (PIdent (p, c)) xs) = do
    checkM (notM $ isClass c) ("Class " ++ c ++ " already exists.") p
    setClass c Nothing M.empty M.empty p
    forM_ xs $ collectDefinitionsClsDef c
collectDefinitionsTopDef (ExtClsDef (PIdent (p, c)) (PIdent (_, s)) xs) = do
    checkM (notM $ isClass c) ("Class " ++ c ++ " already exists.") p
    setClass c (Just s) M.empty M.empty p
    forM_ xs $ collectDefinitionsClsDef c

collectDefinitionsClsDef :: String -> ClsDef -> IM ()
collectDefinitionsClsDef c (VarDef bt props) =
    forM_ props (\(PIdent (p, prop)) -> do
        checkM (notM $ isField c prop) ("Property " ++ prop ++ " already exists in class " ++ c ++ ".") p
        setField c prop t p)
    where
        t = bnfcTypeToType bt
collectDefinitionsClsDef c (MetDef brt (PIdent (p, f)) args _) = do
    checkM (notM $ isMethod c f) ("Method " ++ f ++ " already exists in class " ++ c ++ ".") p
    setMethod c f (Fun argts rt) p
    where
        rt = bnfcTypeToType brt
        argts = map (\(Arg t _) -> bnfcTypeToType t) args
