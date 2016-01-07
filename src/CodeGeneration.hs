module CodeGeneration where

import           BNFC.AbsLatte        hiding (BinOp, Block, RelOp, Type)
import qualified BNFC.AbsLatte        as Abs
import           Control.Arrow
import           Control.Conditional  (if')
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.State  hiding (State)
import qualified Control.Monad.State  as St
import           Data.Foldable
import           Data.List
import           Data.List.Utils
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Traversable
import           Debug.Trace
import           Prelude              hiding (error)
import           Util

type ID = Int

data Type = TInt
          | TChar
          | TString
          | TBool
          | TVoid
          | TLabel
          | TLLVMArr Int Type
          | TObj String
          | TArr Type
          | TFun [Type] Type
          | TRef Type
          | TAnyRef
          deriving (Eq, Ord)

instance Show Type where
    show TInt = "i32"
    show TChar = "i8"
    show TString = "i8*"
    show TBool = "i1"
    show TVoid = "void"
    show TLabel = "label"
    show (TLLVMArr s t) = "[" ++ show s ++ " x " ++ show t ++ "]"
    show (TObj c) = "%class." ++ c
    show (TRef t) = show t ++ "*"
    show (TFun argts rt) = foldr (\argt s -> show argt ++ " -> " ++ s) (show rt) argts

instance Read Type where
    readsPrec _ "int" = [(TInt, "")]
    readsPrec _ "string" = [(TString, "")]
    readsPrec _ "boolean" = [(TBool, "")]
    readsPrec _ "void" = [(TVoid, "")]
    readsPrec _ s
        | endswith "[]" s = [(TArr $ read $ take (length s - 2) s, "")]
        | otherwise = [(TObj s, "")]

bnfcTypeToType :: Abs.Type -> Type
bnfcTypeToType (TSingle (PIdent (_, i))) = read i
bnfcTypeToType (TArray (PIdent (_, i))) = read $ i ++ "[]"

data Value = VInt Int
           | VStringGlobalConstant ID
           | VBool Bool
           | VVoid
           | VLabel ID
           | VReg ID
           | VNull
           deriving (Eq, Ord)

showLabel :: Value -> String
showLabel (VLabel i) = "L." ++ show i

instance Show Value where
    show (VInt i) = show i
    show (VStringGlobalConstant i) = "@str." ++ show i
    show (VBool True) = show 1
    show (VBool False) = show 0
    show VVoid = ""
    show l@VLabel {} = "%" ++ showLabel l
    show (VReg i) = "%r." ++ show i
    show VNull = "null"

defaultValue :: Type -> Value
defaultValue TInt = VInt 0
defaultValue TString = VNull
defaultValue TBool = VBool False
defaultValue TVoid = VVoid

type TypeValue = (Type, Value)

data BinOp = BAdd | BSub | BMul | BDiv | BMod

instance Show BinOp where
    show BAdd = "add"
    show BSub = "sub"
    show BMul = "mul"
    show BDiv = "sdiv"
    show BMod = "srem"

data RelOp = RLt | RLe | REq | RNe | RGe | RGt

instance Show RelOp where
    show RLt = "slt"
    show RLe = "sle"
    show REq = "eq"
    show RNe = "ne"
    show RGe = "sge"
    show RGt = "sgt"

data Instruction = IBin TypeValue TypeValue BinOp TypeValue
                 | ICmp TypeValue TypeValue RelOp TypeValue
                 | IBr [TypeValue]
                 | ICall TypeValue String [TypeValue]
                 | IAssign TypeValue TypeValue
                 | IReturn TypeValue
                 | IAllocate TypeValue
                 | IGetPointer TypeValue [TypeValue]
                 | ILabel TypeValue
                 | IFunDefBegin Type String [TypeValue]
                 | IFunDefEnd
                 | IStringConstant TypeValue String
                 | IDeclare Type String [Type]
                 | IPhi TypeValue [(TypeValue, TypeValue)]

showTypeValues :: [TypeValue] -> String
showTypeValues [] = ""
showTypeValues [(t, v)] = show t ++ " " ++ show v
showTypeValues ((t, v) : xs) = showTypeValues [(t, v)] ++ ", " ++ showTypeValues xs

instance Show Instruction where
    show (IBin (ta, va) (tb, vb) op (tc, vc)) = "\t" ++ show va ++ " = " ++ show op ++ " " ++ show tb ++ " " ++ show vb ++ ", " ++  show vc
    show (ICmp (ta, va) (tb, vb) op (tc, vc)) = "\t" ++ show va ++ " = icmp " ++ show op ++ " " ++ show tb ++ " " ++ show vb ++ ", " ++ show vc
    show (IBr xs) = "\tbr " ++ showTypeValues xs
    show (ICall (TVoid, v) b xs) = "\tcall " ++ show TVoid ++ " @" ++ b ++ "(" ++ showTypeValues xs ++ ")"
    show (ICall (t, v) b xs) = "\t" ++ show v ++ " = call " ++ show t ++ " @" ++ b ++ "(" ++ showTypeValues xs ++ ")"
    show (IAssign (ta, va) (tb, vb)) = "\tWRONG " ++ show ta ++ " " ++ show va ++ " = " ++ show tb ++ " " ++ show vb
    show (IReturn (t, v)) = "\tret " ++ show t ++ " " ++ show v
    show (IAllocate (TRef t, v)) = "\t" ++ show v ++ " = alloca " ++ show t
    show (IGetPointer (t, v) xs@((TRef xt, _):_)) = "\t" ++ show v ++ " = getelementptr " ++ show xt ++ ", " ++ showTypeValues xs
    show (ILabel (t, v)) = showLabel v ++ ":"
    show (IFunDefBegin t f xs) = "define " ++ show t ++ " @" ++ f ++ "(" ++ showTypeValues xs ++ ") {"
    show IFunDefEnd = "}"
    show (IStringConstant (TRef t, v) s) = show v ++ " = internal constant " ++ show t ++ " c\"" ++ s ++ "\\00\""
    show (IDeclare t f xs) = "declare " ++ show t ++ " @" ++ f ++ "(" ++ showTypes xs ++ ")"
        where
            showTypes [] = ""
            showTypes [t] = show t
            showTypes (t : ts) = showTypes [t] ++ ", " ++ showTypes ts
    show (IPhi (t, v) xs) = "\t" ++ show v ++ " = phi " ++ show t ++ " " ++ showPhiArgs xs
        where
            showPhiArgs [] = ""
            showPhiArgs [((_, v), (_, l))] = "[" ++ show v ++ ", " ++ show l ++ "]"
            showPhiArgs (x : xs) = showPhiArgs [x] ++ ", " ++ showPhiArgs xs

data Function = Function {
    functionArgs :: [(String, Type)],
    functionBody :: Abs.Block,
    functionType :: Type,
    functionCode :: [Instruction]
} deriving (Show)

data Block = Block {
    blockInputs       :: Set TypeValue,
    blockInstructions :: [Instruction]
} deriving (Show)

blockToInstructions :: TypeValue -> Block -> [Instruction]
blockToInstructions l (Block _ is) = ILabel l : is

blockOutputs :: Block -> [TypeValue]
blockOutputs (Block _ is) =
    case last is of
        IBr [a] -> [a]
        IBr [_, b, c] -> [b, c]
        _ -> []

getBlockInstructions :: TypeValue -> GM [Instruction]
getBlockInstructions l = blockInstructions <$> getGraphBlock l

setBlockInstructions :: TypeValue -> [Instruction] -> GM ()
setBlockInstructions l is = do
    b <- getGraphBlock l
    setGraphBlock l b { blockInstructions = is }

addEdge :: TypeValue -> TypeValue -> GM ()
addEdge l1 l2 = do
    b2 <- getGraphBlock l2
    setGraphBlock l2 b2 { blockInputs = S.insert l1 (blockInputs b2) }

removeEdge :: TypeValue -> TypeValue -> GM ()
removeEdge l1 l2 = do
    b2 <- getGraphBlock l2
    setGraphBlock l2 b2 { blockInputs = S.delete l1 (blockInputs b2) }

data Graph = Graph {
    graphParameters :: [TypeValue],
    graphReturnType :: Type,
    graphSource     :: TypeValue,
    graphBlocks     :: Map TypeValue Block
} deriving (Show)

graphToInstructions :: String -> Graph -> [Instruction]
graphToInstructions f (Graph ps rt l bs) = a ++ b ++ c ++ d
    where
        a = [IFunDefBegin rt f ps]
        b = blockToInstructions l (bs M.! l)
        c = concatMap (uncurry blockToInstructions) (M.toList (M.delete l bs))
        d = [IFunDefEnd]

getGraphBlocks :: GM (Map TypeValue Block)
getGraphBlocks = graphBlocks <$> getGraph

getGraphBlock :: TypeValue -> GM Block
getGraphBlock l = (M.! l) <$> getGraphBlocks

setGraphBlock :: TypeValue -> Block -> GM ()
setGraphBlock l b = do
    g <- getGraph
    setGraph g { graphBlocks = M.insert l b (graphBlocks g) }

data State = State {
    lastID          :: ID,
    instructions    :: [Instruction],
    graph           :: Graph,
    frames          :: [Map String TypeValue],
    stringConstants :: Map String TypeValue,
    functions       :: Map String Function
} deriving (Show)

newID :: GM ID
newID = do
    i <- lastID <$> get
    modify (\st -> st { lastID = i + 1 })
    return i

newRegister :: Type -> GM TypeValue
newRegister t = do
    i <- newID
    return (t, VReg i)

newLabel :: GM TypeValue
newLabel = do
    i <- newID
    return (TLabel, VLabel i)

newStringGlobalConstant :: Int -> GM TypeValue
newStringGlobalConstant s = do
    i <- newID
    return (TRef $ TLLVMArr s TChar, VStringGlobalConstant i)

emit :: Instruction -> GM ()
emit i = do
    is <- instructions <$> get
    modify (\st -> st { instructions = i : is })

cleanInstructions :: GM ()
cleanInstructions = modify (\st -> st { instructions = [] })

getGraph :: GM Graph
getGraph = graph <$> get

setGraph :: Graph -> GM ()
setGraph g = modify (\st -> st { graph = g })

getInstructions :: GM [Instruction]
getInstructions = reverse . instructions <$> get

getFrames :: GM [Map String TypeValue]
getFrames = frames <$> get

pushFrame :: GM ()
pushFrame = modify (\st -> st { frames = M.empty : frames st })

popFrame :: GM ()
popFrame = modify (\st -> st { frames = tail $ frames st })

frame :: GM a -> GM a
frame b = do
    pushFrame
    r <- b
    popFrame
    return r

getStringConstants :: GM (Map String TypeValue)
getStringConstants = stringConstants <$> get

getStringConstant :: String -> GM TypeValue
getStringConstant s = do
    unlessM (M.member s <$> getStringConstants)
        (do c <- newStringGlobalConstant $ length s + 1
            modify (\st -> st { stringConstants = M.insert s c (stringConstants st) }))
    (M.! s) . stringConstants <$> get

getLocal :: String -> GM TypeValue
getLocal l = do
    Just m <- find (M.member l) <$> getFrames
    return $ m M.! l

setLocal :: String -> TypeValue -> GM ()
setLocal i tv = modify (\st -> st { frames = M.insert i tv (head $ frames st) : tail (frames st) })

getFunctions :: GM (Map String Function)
getFunctions = functions <$> get

getFunction :: String -> GM Function
getFunction f = (M.! f) <$> getFunctions

setFunction :: String -> [(String, Type)] -> Abs.Block -> Type -> [Instruction] -> GM ()
setFunction f a b t is = modify (\st -> st { functions = M.insert f (Function a b t is) (functions st) })

type GM = ExceptT String (St.State State)

notImplemented :: String -> GM a
notImplemented m = throwError $ m ++ " - not implemented."

-- generate condition

genRelCond :: Expr -> RelOp -> Expr -> TypeValue -> TypeValue -> GM ()
genRelCond e1 op e2 lTrue lFalse = do
    tv1 <- genExpr e1
    tv2 <- genExpr e2
    tvc <- newRegister TBool
    emit $ ICmp tvc tv1 op tv2
    emit $ IBr [tvc, lTrue, lFalse]

genCond :: Expr -> TypeValue -> TypeValue -> GM ()
genCond ELitTrue lTrue lFalse = emit $ IBr [lTrue]
genCond ELitFalse lTrue lFalse = emit $ IBr [lFalse]
genCond (ENot e) lTrue lFalse = genCond e lFalse lTrue
genCond (ERel e1 LTH e2) lTrue lFalse = genRelCond e1 RLt e2 lTrue lFalse
genCond (ERel e1 LE e2) lTrue lFalse = genRelCond e1 RLe e2 lTrue lFalse
genCond (ERel e1 GTH e2) lTrue lFalse = genRelCond e1 RGt e2 lTrue lFalse
genCond (ERel e1 GE e2) lTrue lFalse = genRelCond e1 RGe e2 lTrue lFalse
genCond (ERel e1 EQU e2) lTrue lFalse = genRelCond e1 REq e2 lTrue lFalse
genCond (ERel e1 NE e2) lTrue lFalse = genRelCond e1 RNe e2 lTrue lFalse
genCond (EAnd e1 e2) lTrue lFalse = do
    lMid <- newLabel
    genCond e1 lMid lFalse
    emit $ ILabel lMid
    genCond e2 lTrue lFalse
genCond (EOr e1 e2) lTrue lFalse = do
    lMid <- newLabel
    genCond e1 lTrue lMid
    emit $ ILabel lMid
    genCond e2 lTrue lFalse

-- generate expression

genCondWithValue :: Expr -> GM TypeValue
genCondWithValue e = do
    lTrue <- newLabel
    lFalse <- newLabel
    lEnd <- newLabel
    r <- newRegister TBool
    genCond e lTrue lFalse
    emit $ ILabel lTrue
    emit $ IAssign r (TBool, VBool True)
    emit $ IBr [lEnd]
    emit $ ILabel lFalse
    emit $ IAssign r (TBool, VBool False)
    emit $ IBr [lEnd]
    emit $ ILabel lEnd
    return r

genBinExpr :: Expr -> BinOp -> Expr -> GM TypeValue
genBinExpr e1 op e2 = do
    tv1 <- genExpr e1
    tv2 <- genExpr e2
    r <- newRegister TInt
    emit $ IBin r tv1 op tv2
    return r

genIncrDecrExpr :: Expr -> BinOp -> GM TypeValue
genIncrDecrExpr e op = do
    etv <- genExpr e
    r <- newRegister TInt
    emit $ IAssign r etv
    emit $ IBin etv etv op (TInt, VInt 1)
    return r

genExpr :: Expr -> GM TypeValue
genExpr (EVar (PIdent (_, i))) = getLocal i
genExpr (ELitInt i) = return (TInt, VInt $ fromInteger i)
genExpr (EString s) = do
    c <- getStringConstant s
    r <- newRegister TString
    emit $ IGetPointer r [c, (TInt, VInt 0), (TInt, VInt 0)]
    return r
genExpr ELitTrue = return (TBool, VBool True)
genExpr ELitFalse = return (TBool, VBool False)
genExpr ENull = notImplemented "ENull"
genExpr (ESelect e (PIdent (_, i))) = notImplemented "ESelect"
genExpr (EMetCall e (PIdent (_, m)) args) = notImplemented "ESelect"
genExpr (EAt e1 e2) = notImplemented "EAt"
genExpr (EApp p@(PIdent (_, f)) args) = do -- no arguments cast and methods call
    TFun argts rt <- functionType <$> getFunction f
    r <- newRegister rt
    argtvs <- mapM genExpr args
    emit $ ICall r f argtvs
    return r
genExpr (ENeg e) = do
    etv <- genExpr e
    r <- newRegister TInt
    emit $ IBin r (TInt, VInt 0) BSub etv
    return r
genExpr e@ENot {} = genCondWithValue e
genExpr (EIncr e) = genIncrDecrExpr e BAdd -- only for TInt (not TRef TInt)
genExpr (EDecr e) = genIncrDecrExpr e BSub -- only for TInt (not TRef TInt)
genExpr (ENewVar (PIdent (_, i))) = notImplemented "ENewVar"
genExpr (ENewArr (PIdent (_, i)) e) = notImplemented "ENewArr"
genExpr (ECastVar (PIdent (_, i)) e) = notImplemented "ECastVar"
genExpr (ECastArr (PIdent (_, i)) e) = notImplemented "ECastArr"
genExpr (EMul e1 Times e2) = genBinExpr e1 BMul e2
genExpr (EMul e1 Div e2) = genBinExpr e1 BDiv e2
genExpr (EMul e1 Mod e2) = genBinExpr e1 BMod e2
genExpr (EAdd e1 Plus e2) = do --genBinExpr e1 BAdd e2
    tv1@(t1, _) <- genExpr e1
    tv2@(t2, _) <- genExpr e2
    case (t1, t2) of
        (TInt, TInt) -> do
            r <- newRegister TInt
            emit $ IBin r tv1 BAdd tv2
            return r
        (TString, TString) -> do
            l1r <- newRegister TInt
            l2r <- newRegister TInt
            emit $ ICall l1r "_strlen" [tv1]
            emit $ ICall l2r "_strlen" [tv2]
            a <- newRegister TInt
            emit $ IBin a l1r BAdd l2r
            b <- newRegister TInt
            emit $ IBin b a BAdd (TInt, VInt 1)
            c <- newRegister TString
            emit $ ICall c "_malloc" [b]
            d <- newRegister TString
            emit $ ICall d "_strcpy" [c, tv1]
            e <- newRegister TString
            emit $ ICall e "_strcat" [d, tv2]
            return e
genExpr (EAdd e1 Minus e2) = genBinExpr e1 BSub e2
genExpr e@EAnd {} = genCondWithValue e
genExpr e@EOr {} = genCondWithValue e

-- generate statement

genBlock :: Abs.Block -> GM ()
genBlock (Abs.Block xs) = frame $ forM_ xs genStmt

genStmt :: Stmt -> GM ()
genStmt Empty = return ()
genStmt (BStmt b) = genBlock b
genStmt (Decl bt xs) = forM_ xs help
    where
        t = bnfcTypeToType bt
        help :: Item -> GM ()
        help (NoInit (PIdent (_, i))) = do
            r <- newRegister t
            emit $ IAssign r (t, defaultValue t)
            setLocal i r
        help (Init (PIdent (_, i)) e) = do
            r <- newRegister t
            tv <- genExpr e
            emit $ IAssign r tv -- no cast
            setLocal i r
genStmt (Ass e1 e2) = do -- only registers
    tv1 <- genExpr e1
    tv2 <- genExpr e2
    emit $ IAssign tv1 tv2
genStmt (Ret e) = do -- no cast
    tv <- genExpr e
    emit $ IReturn tv
genStmt VRet = emit $ IReturn (TVoid, VVoid)
genStmt (Cond e s) = do
    lTrue <- newLabel
    lEnd <- newLabel
    genCond e lTrue lEnd
    emit $ ILabel lTrue
    genStmt s
    emit $ IBr [lEnd]
    emit $ ILabel lEnd
genStmt (CondElse e s1 s2) = do
    lTrue <- newLabel
    lFalse <- newLabel
    lEnd <- newLabel
    genCond e lTrue lFalse
    emit $ ILabel lTrue
    genStmt s1
    emit $ IBr [lEnd]
    emit $ ILabel lFalse
    genStmt s2
    emit $ IBr [lEnd]
    emit $ ILabel lEnd
genStmt (While e s) = do
    lCond <- newLabel
    lBody <- newLabel
    lEnd <- newLabel
    emit $ IBr [lCond]
    emit $ ILabel lCond
    genCond e lBody lEnd
    emit $ ILabel lBody
    genStmt s
    emit $ IBr [lCond]
    emit $ ILabel lEnd
genStmt (For bt (PIdent (_, i)) e s) = notImplemented "For"
genStmt (SExp e) = void $ genExpr e

-- generate function

genFunction :: String -> Function -> GM [Instruction]
genFunction f (Function args b (TFun _ rt) _) = do
    cleanInstructions
    frame (do
        rs <- mapM (newRegister . snd) args
        forM_ (zip args rs) (\((n, _), r) -> setLocal n r)
        emit $ IFunDefBegin rt f rs
        lEntry <- newLabel
        emit $ ILabel lEntry
        genBlock b
        emit $ ICall (TVoid, VVoid) "_no_return" []
        emit $ IReturn (rt, defaultValue rt)
        emit IFunDefEnd)
    getInstructions

-- convert instructions to graph

removeUnreachableInstructions :: [Instruction] -> [Instruction]
removeUnreachableInstructions is = takeWhile (not . isFinal) is ++ [head (dropWhile (not .isFinal) is)]
    where
        isFinal :: Instruction -> Bool
        isFinal IBr {} = True
        isFinal IReturn {} = True
        isFinal _ = False

createGraphFromInstructions :: [Instruction] -> GM ()
createGraphFromInstructions is' = do
    setGraph $ Graph ps rt sl M.empty
    forM_ iss2 (\(l, bis) -> setGraphBlock l (Block S.empty bis))
    mapM_ (\(l, b) -> forM_ (blockOutputs b) (\x -> addEdge l x)) . M.toList =<< getGraphBlocks
    where
        IFunDefBegin rt f ps = head is'
        is = tail $ take (length is' - 1) is' -- function body
        ILabel sl = head is
        iss1 = splitInstructions is
        iss2 = map (second removeUnreachableInstructions) iss1

        isLabel :: Instruction -> Bool
        isLabel (ILabel l) = True
        isLabel _ = False

        splitInstructions :: [Instruction] -> [(TypeValue, [Instruction])]
        splitInstructions [] = []
        splitInstructions (ILabel l : is) = (l, b) : bs
            where
                b = takeWhile (not . isLabel) is
                is' = drop (length b) is
                bs = splitInstructions is'

-- find alive variables

filterRegisters :: Set TypeValue -> Set TypeValue
filterRegisters = S.filter isRegister
    where
        isRegister :: TypeValue -> Bool
        isRegister (_, VReg _) = True
        isRegister (_, _) = False

killI :: Instruction -> Set TypeValue
killI (IBin l _ _ _) = S.singleton l
killI (ICmp l _ _ _) = S.singleton l
killI (IBr _) = S.empty
killI (ICall l _ _) = filterRegisters $ S.singleton l
killI (IAssign l _) = S.singleton l
killI (IReturn _) = S.empty
killI (IAllocate l) = S.singleton l
killI (IGetPointer l _) = S.singleton l

useI :: Instruction -> Set TypeValue
useI (IBin _ r1 _ r2) = filterRegisters $ S.fromList [r1, r2]
useI (ICmp _ r1 _ r2) = filterRegisters $ S.fromList [r1, r2]
useI (IBr xs) = filterRegisters $ S.fromList xs
useI (ICall _ _ xs) = filterRegisters $ S.fromList xs
useI (IAssign _ r) = filterRegisters $ S.singleton r
useI (IReturn r) = filterRegisters $ S.singleton r
useI (IAllocate _) = S.empty
useI (IGetPointer _ xs) = filterRegisters $ S.fromList xs

inI :: Instruction -> Set TypeValue -> Set TypeValue
inI i out = (out `S.difference` killI i) `S.union` useI i

inB :: Block -> Set TypeValue -> Set TypeValue
inB b out = foldr inI out (blockInstructions b)

inG :: Graph -> Map TypeValue (Set TypeValue) -> Map TypeValue (Set TypeValue)
inG g outs = M.fromList $ map help (M.toList $ graphBlocks g)
    where
        help :: (TypeValue, Block) -> (TypeValue, Set TypeValue)
        help (l, b) =
            let out = foldl (\acc n -> acc `S.union` (outs M.! n)) S.empty (blockOutputs b)
            in (l, inB b out)

aliveVariables :: Graph -> Map TypeValue (Set TypeValue)
aliveVariables g = fix $ inG g

-- insert phi

generatePhiCalls :: GM ()
generatePhiCalls = do
    g <- getGraph
    let av = aliveVariables g
    for_ (M.toList (trace ("FIX POINT TO: " ++ show av ++ "\n\n") av)) (\(l, s) -> do
        bi <- getBlockInstructions l
        setBlockInstructions l ((map (\x -> IPhi x []) (S.toList s)) ++ bi))

-- all function

getFunctionCode :: String -> Function -> GM [Instruction]
getFunctionCode n f = do
    is <- genFunction n f
    createGraphFromInstructions is
    generatePhiCalls
    graphToInstructions n <$> getGraph

-- collect definitions

getDefsInProgram :: Program -> GM ()
getDefsInProgram (Program xs) = forM_ xs getDefsInTopDef

getDefsInTopDef :: TopDef -> GM ()
getDefsInTopDef (FnDef brt (PIdent (_, i)) args b) = setFunction i args' b (TFun argts rt) []
    where
        rt = bnfcTypeToType brt
        args' = map (\(Arg t (PIdent (p, a))) -> (a, bnfcTypeToType t)) args
        argts = map snd args'
getDefsInTopDef (TopClsDef (PIdent (_, c)) xs) = notImplemented "TopClsDef"
getDefsInTopDef (ExtClsDef (PIdent (_, c)) (PIdent (_, s)) xs) = notImplemented "ExtClsDef"

getDefsInClsDef :: String -> ClsDef -> GM ()
getDefsInClsDef c (VarDef bt props) = notImplemented "VarDef"
getDefsInClsDef c (MetDef brt (PIdent (_, i)) args b) = notImplemented "MetDef"

-- code generation

codeGeneration :: Program -> GM [Instruction]
codeGeneration p = do
    getDefsInProgram p
    mapM_ (uncurry genFunction) . filter (\(k,_) -> not (M.member k builtInFunctions)) . M.toList =<< getFunctions
    mapM_ (\(s, tv) -> emit $ IStringConstant tv s) . M.toList =<< getStringConstants
    mapM_ (\(s, Function _ _ (TFun argts rt) _) -> emit $ IDeclare rt s argts) (M.toList builtInFunctions)
    getInstructions

codeGeneration2 :: Program -> GM [Instruction]
codeGeneration2 p = do
    getDefsInProgram p
    is1 <- concatMapM (uncurry getFunctionCode) . filter (\(k,_) -> not (M.member k builtInFunctions)) . M.toList =<< getFunctions
    is2 <- map (\(s, tv) -> IStringConstant tv s) . M.toList <$> getStringConstants
    let is3 = map (\(s, Function _ _ (TFun argts rt) _) -> IDeclare rt s argts) (M.toList builtInFunctions)
    return $ is1 ++ is2 ++ is3

builtInFunctions = M.fromList [("printString", Function [("s", TString)] (Abs.Block []) (TFun [TString] TVoid) []),
                               ("printInt",    Function [("i", TInt)]    (Abs.Block []) (TFun [TInt] TVoid)    []),
                               ("readInt",     Function []               (Abs.Block []) (TFun [] TInt)         []),
                               ("readString",  Function []               (Abs.Block []) (TFun [] TString)      []),
                               ("error",       Function []               (Abs.Block []) (TFun [] TVoid)        []),
                               ("_no_return",  Function []               (Abs.Block []) (TFun [] TVoid)        []),
                               ("_strlen",     Function [("s", TString)] (Abs.Block []) (TFun [TString] TInt)  []),
                               ("_malloc",     Function [("i", TInt)]    (Abs.Block []) (TFun [TInt] TString)  []),
                               ("_strcat",     Function [("i", TString), ("j", TString)] (Abs.Block [Empty]) (TFun [TString, TString] TString) []),
                               ("_strcpy",     Function [("i", TString), ("j", TString)] (Abs.Block [Empty]) (TFun [TString, TString] TString) [])]

initialState = State {
    lastID = 0,
    instructions = [],
    frames = [],
    stringConstants = M.empty,
    functions = builtInFunctions,
    graph = Graph [] TVoid (TVoid, VVoid) M.empty
}

runCodeGeneration :: Program -> Either String [Instruction]
runCodeGeneration p = evalState (runExceptT (codeGeneration p)) initialState

runCodeGeneration2 :: Program -> Either String [Instruction]
runCodeGeneration2 p = evalState (runExceptT (codeGeneration2 p)) initialState

runCodeGeneration' :: Program -> State
runCodeGeneration' p = execState (runExceptT (codeGeneration p)) initialState

runCodeGeneration2' :: Program -> State
runCodeGeneration2' p = execState (runExceptT (codeGeneration2 p)) initialState
