module CodeGeneration where

import           BNFC.AbsLatte        hiding (BinOp, Block, RelOp, Type)
import qualified BNFC.AbsLatte        as Abs
import           Control.Conditional  (if')
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.State  hiding (State)
import qualified Control.Monad.State  as St
import           Data.Char
import           Data.Foldable
import           Data.Key             hiding (zip)
import           Data.List
import           Data.List.Utils
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Monoid
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Traversable
import           Data.Tuple.Extra
import           Debug.Trace
import           Numeric
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

isRegisterValue :: Value -> Bool
isRegisterValue VReg {} = True
isRegisterValue _ = False

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
    show (IAssign (ta, va) (tb, vb)) = "\tDEBUG " ++ show ta ++ " " ++ show va ++ " = " ++ show tb ++ " " ++ show vb
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

liftInstruction :: (TypeValue -> TypeValue) -> (TypeValue -> TypeValue) -> ((TypeValue, TypeValue) -> (TypeValue, TypeValue)) -> Instruction -> Instruction
liftInstruction f g _ (IBin l r1 op r2) = IBin (f l) (g r1) op (g r2)
liftInstruction f g _ (ICmp l r1 op r2) = ICmp (f l) (g r1) op (g r2)
liftInstruction _ g _ (IBr rs) = IBr (map g rs)
liftInstruction f g _ (ICall l x rs) = ICall (f l) x (map g rs)
liftInstruction f g _ (IAssign l r) = IAssign (f l) (g r)
liftInstruction _ g _ (IReturn r) = IReturn (g r)
liftInstruction f _ _ (IAllocate l) = IAllocate (f l)
liftInstruction f g _ (IGetPointer l rs) = IGetPointer (f l) (map g rs)
liftInstruction f g h (IPhi l rs) = IPhi (f l) (map h rs)
liftInstruction f _ _ (ILabel l) = ILabel (f l)

reduceInstruction :: Monoid m => (TypeValue -> m) -> (TypeValue -> m) -> ((TypeValue, TypeValue) -> m) -> Instruction -> m
reduceInstruction f g _ (IBin l r1 op r2) = f l <> g r1 <> g r2
reduceInstruction f g _ (ICmp l r1 op r2) = f l <> g r1 <> g r2
reduceInstruction _ g _ (IBr rs) = mconcat (map g rs)
reduceInstruction f g _ (ICall l x rs) = f l <> mconcat (map g rs)
reduceInstruction f g _ (IAssign l r) = f l <> g r
reduceInstruction _ g _ (IReturn r) = g r
reduceInstruction f _ _ (IAllocate l) = f l
reduceInstruction f g _ (IGetPointer l rs) = f l <> mconcat (map g rs)
reduceInstruction f g h (IPhi l rs) = f l <> mconcat (map h rs)
reduceInstruction f _ _ (ILabel l) = f l

isJumpInstruction :: Instruction -> Bool
isJumpInstruction IBr {} = True
isJumpInstruction IReturn {} = True
isJumpInstruction _ = False

isLabelInstruction :: Instruction -> Bool
isLabelInstruction (ILabel l) = True
isLabelInstruction _ = False

leftSideInstruction :: Instruction -> [TypeValue]
leftSideInstruction = filter (isRegisterValue . snd) . reduceInstruction (\x -> [x]) (const []) (const [])

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

blockOutputs :: Block -> [TypeValue]
blockOutputs (Block _ is) =
    case last is of
        IBr [a] -> [a]
        IBr [_, b, c] -> [b, c]
        _ -> []

blockToInstructions :: TypeValue -> Block -> [Instruction]
blockToInstructions l (Block _ is) = ILabel l : is

getBlockInputs :: TypeValue -> GM (Set TypeValue)
getBlockInputs l = blockInputs <$> getGraphBlock l

getBlockOutputs :: TypeValue -> GM [TypeValue]
getBlockOutputs l = blockOutputs <$> getGraphBlock l

getBlockInstructions :: TypeValue -> GM [Instruction]
getBlockInstructions l = blockInstructions <$> getGraphBlock l

setBlockInstructions :: TypeValue -> [Instruction] -> GM ()
setBlockInstructions l is = do
    b <- getGraphBlock l
    setGraphBlock l b { blockInstructions = is }

data Graph = Graph {
    graphParams     :: [TypeValue],
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

getGraphSource :: GM TypeValue
getGraphSource = graphSource <$> getGraph

getGraphParams :: GM [TypeValue]
getGraphParams = graphParams <$> getGraph

getGraphBlocks :: GM (Map TypeValue Block)
getGraphBlocks = graphBlocks <$> getGraph

getGraphBlock :: TypeValue -> GM Block
getGraphBlock l = (M.! l) <$> getGraphBlocks

setGraphBlock :: TypeValue -> Block -> GM ()
setGraphBlock l b = do
    g <- getGraph
    setGraph g { graphBlocks = M.insert l b (graphBlocks g) }

addGraphEdge :: TypeValue -> TypeValue -> GM ()
addGraphEdge l1 l2 = do
    b2 <- getGraphBlock l2
    setGraphBlock l2 b2 { blockInputs = S.insert l1 (blockInputs b2) }

removeGraphEdge :: TypeValue -> TypeValue -> GM ()
removeGraphEdge l1 l2 = do
    b2 <- getGraphBlock l2
    setGraphBlock l2 b2 { blockInputs = S.delete l1 (blockInputs b2) }

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
getStringConstant rawS = do
    unlessM (M.member hexS <$> getStringConstants)
        (do c <- newStringGlobalConstant $ length rawS + 1
            modify (\st -> st { stringConstants = M.insert hexS c (stringConstants st) }))
    (M.! hexS) . stringConstants <$> get
    where
        hexS = concatMap (\c ->
            let hexC = showHex (ord c) ""
            in "\\" ++ replicate (2 - length hexC) '0' ++ hexC) rawS

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

setFunction :: String -> Function -> GM ()
setFunction n f = modify (\st -> st { functions = M.insert n f (functions st) })

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

genExprCond :: Expr -> TypeValue -> TypeValue -> GM ()
genExprCond e lTrue lFalse = do
    r <- genExpr e
    emit $ IBr [r, lTrue, lFalse]

genCond :: Expr -> TypeValue -> TypeValue -> GM ()
genCond e@EVar {} lTrue lFalse = genExprCond e lTrue lFalse
genCond ELitTrue lTrue lFalse = emit $ IBr [lTrue]
genCond ELitFalse lTrue lFalse = emit $ IBr [lFalse]
genCond e@ESelect {} lTrue lFalse = genExprCond e lTrue lFalse
genCond e@EMetCall {} lTrue lFalse = genExprCond e lTrue lFalse
genCond e@EAt {} lTrue lFalse = genExprCond e lTrue lFalse
genCond e@EApp {} lTrue lFalse = genExprCond e lTrue lFalse
genCond (ENot e) lTrue lFalse = genCond e lFalse lTrue
genCond e@ECastVar {} lTrue lFalse = genCond e lFalse lTrue
genCond e@ECastArr {} lTrue lFalse = genCond e lFalse lTrue
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
genExpr (EMetCall e (PIdent (_, m)) args) = notImplemented "EMetCall"
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
genExpr e@ERel {} = genCondWithValue e
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
        if' (rt == TVoid)
            (   emit $ IReturn (TVoid, VVoid))
            (do emit $ ICall (TVoid, VVoid) "_no_return" []
                emit $ IReturn (rt, defaultValue rt))
        emit IFunDefEnd)
    getInstructions

-- convert instructions to graph

removeUnreachableInstructions :: [Instruction] -> [Instruction]
removeUnreachableInstructions is =
    takeWhile (not . isJumpInstruction) is ++ [head (dropWhile (not . isJumpInstruction) is)]

createGraphFromInstructions :: [Instruction] -> GM ()
createGraphFromInstructions is' = do
    setGraph $ Graph ps rt sl M.empty
    forM_ iss2 (\(l, bis) -> setGraphBlock l (Block S.empty bis))
    mapWithKeyM_ (\l b -> forM_ (blockOutputs b) (addGraphEdge l)) =<< getGraphBlocks
    where
        IFunDefBegin rt f ps = head is'
        is = tail $ take (length is' - 1) is' -- function body
        ILabel sl = head is
        iss1 = splitInstructions is
        iss2 = map (second removeUnreachableInstructions) iss1

        splitInstructions :: [Instruction] -> [(TypeValue, [Instruction])]
        splitInstructions [] = []
        splitInstructions (ILabel l : is) = (l, b) : bs
            where
                b = takeWhile (not . isLabelInstruction) is
                is' = drop (length b) is
                bs = splitInstructions is'

-- find alive variables

filterRegisters :: Set TypeValue -> Set TypeValue
filterRegisters x = S.empty -- TODO S.filter isRegisterValue

killSetInstruction :: Instruction -> Set TypeValue
killSetInstruction = S.fromList . leftSideInstruction

useSetInstruction :: Instruction -> Set TypeValue
useSetInstruction = S.fromList . filter (isRegisterValue . snd) . reduceInstruction (const []) (\x -> [x]) (const [])

inSetInstruction :: Instruction -> Set TypeValue -> Set TypeValue
inSetInstruction i o = (o `S.difference` killSetInstruction i) `S.union` useSetInstruction i

inSetBlock :: Block -> Set TypeValue -> Set TypeValue
inSetBlock b o = foldr inSetInstruction o (blockInstructions b)

inSetsGraph :: Graph -> Map TypeValue (Set TypeValue) -> Map TypeValue (Set TypeValue)
inSetsGraph g os = M.unionWith S.union os newOs
    where
        newOs = M.fromList $ map help (M.toList $ graphBlocks g)

        help :: (TypeValue, Block) -> (TypeValue, Set TypeValue)
        help (l, b) =
            let o = mconcat $ map (os M.!) (blockOutputs b)
            in (l, inSetBlock b o)

aliveVariables :: Graph -> Map TypeValue (Set TypeValue)
aliveVariables g = help $ M.map (const S.empty) (graphBlocks g)
    where
        help :: Map TypeValue (Set TypeValue) -> Map TypeValue (Set TypeValue)
        help o = let o' = inSetsGraph g o
                 in if' (o == o') o (help o')

-- insert phi

insertPhiCalls :: GM ()
insertPhiCalls = do
    g <- getGraph
    sl <- getGraphSource
    (flip mapWithKeyM_) (aliveVariables g) (\l avs ->
        unless (l == sl)
            (do bis <- getBlockInstructions l
                inputs <- S.toList <$> getBlockInputs l
                let bis' = (map (\av -> IPhi av (map (\i -> (av, i)) inputs)) (S.toList avs)) ++ bis
                setBlockInstructions l bis'))

-- reassign registers to make ssa

replaceIfEqual :: Eq a => a -> a -> a -> a
replaceIfEqual a b c = if' (c == a) b c

replaceAllUses :: Instruction -> TypeValue -> TypeValue -> Instruction
replaceAllUses i a b = liftInstruction (replaceIfEqual a b) (replaceIfEqual a b) (both $ replaceIfEqual a b) i

replaceLeftSides :: Instruction -> TypeValue -> TypeValue -> Instruction
replaceLeftSides i a b = liftInstruction (replaceIfEqual a b) id id i

replaceAllUsesButPhiParams :: Instruction -> TypeValue -> TypeValue -> Instruction
replaceAllUsesButPhiParams i a b = liftInstruction (replaceIfEqual a b) (replaceIfEqual a b) id i

updatePhiParams :: TypeValue -> Map TypeValue TypeValue -> [(TypeValue, TypeValue)] -> [(TypeValue, TypeValue)]
updatePhiParams l m [] = []
updatePhiParams l m ((a, b):xs)
    | l == b && M.member a m = (m M.! a, l) : updatePhiParams l m xs
    | otherwise = (a, b) : updatePhiParams l m xs

updatePhiParamsInBlock :: TypeValue -> TypeValue -> Map TypeValue TypeValue -> GM ()
updatePhiParamsInBlock l n m = do
    bi <- getBlockInstructions l
    let bi' = foldr help [] bi
    setBlockInstructions l bi'
    where
        help :: Instruction -> [Instruction] -> [Instruction]
        help (IPhi nx xs) acc = IPhi nx (updatePhiParams n m xs) : acc
        help x acc = x:acc

reassignRegistersInBlock :: TypeValue -> GM ()
reassignRegistersInBlock l = do
    is <- getBlockInstructions l
    sl <- getGraphSource
    m0 <- if' (l == sl) (M.fromList . map dupe <$> getGraphParams) (return M.empty)
    (m, revis) <- foldlM (\(m1, is) i -> do
        (m2, i1) <-
            case leftSideInstruction i of
                [] -> return (m1, i)
                [r@(t, _)] -> do
                    nr <- newRegister t
                    return (M.insert r nr m1, replaceLeftSides i r nr)
        let i3 = foldl (\i2 (r, nr) -> replaceAllUsesButPhiParams i2 r nr) i1 (M.toList m1)
        return (m2, i3 : is)) (m0, []) is
    setBlockInstructions l (reverse revis)
    os <- getBlockOutputs l
    for_ os (\o -> updatePhiParamsInBlock o l m)

-- remove assignments

removeAssignments :: TypeValue -> GM ()
removeAssignments l = do
    bi <- getBlockInstructions l
    let (bi', m) = help bi M.empty
    setBlockInstructions l bi'
    os <- getBlockOutputs l
    for_ os (\o -> updatePhiParamsInBlock o l m)
    where
        help :: [Instruction] -> Map TypeValue TypeValue -> ([Instruction], Map TypeValue TypeValue)
        help [] m = ([], m)
        help ((IAssign l r) : is) m
            | M.member r m = help is (M.insert l (m M.! r) m)
            | otherwise = help is (M.insert l r m)
        help (i : is) m =
            let i2 = foldlWithKey replaceAllUses i m
                (is1, m1) = help is m
            in (i2 : is1, m1)

-- all function

getFunctionCode :: String -> Function -> GM [Instruction]
getFunctionCode n f = do
    is <- genFunction n f
    createGraphFromInstructions is
    insertPhiCalls
    mapM_ reassignRegistersInBlock . M.keys =<< getGraphBlocks
    mapM_ removeAssignments . M.keys =<< getGraphBlocks
    graphToInstructions n <$> getGraph

-- collect definitions

getDefsInProgram :: Program -> GM ()
getDefsInProgram (Program xs) = forM_ xs getDefsInTopDef

getDefsInTopDef :: TopDef -> GM ()
getDefsInTopDef (FnDef brt (PIdent (_, i)) args b) = setFunction i (Function args' b (TFun argts rt) [])
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

runCodeGeneration' :: Program -> State
runCodeGeneration' p = execState (runExceptT (codeGeneration p)) initialState
