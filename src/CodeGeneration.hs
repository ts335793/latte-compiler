module CodeGeneration where

import Data.List.Utils

data Type = TInt
          | TString
          | TBool
          | TVoid
          | TLabel
          | TObj String
          | TArr Type
          | TFun [Type] Type
          | TRef Type
          | TAnyRef
          deriving Eq

instance Show Type where
    show TInt = "i32"
    show TString = "i8*"
    show TBool = "i1"
    show TVoid = "void"
    show TLabel = "label"
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

data Value = VInt Int
           | VString String
           | VBool Bool
           | VVoid
           | VLabel Int
           | VReg Int
           | VNull

instance Show Value where
    show (VInt i) = show i
    show (VString s) = "\"" ++ s ++ "\""
    show (VBool True) = show 1
    show (VBool False) = show 0
    show VVoid = ""
    show (VLabel i) = "%L." ++ show i
    show (VReg i) = "%r." ++ show i
    show VNull = "null"

type TypeValue = (Type, Value)

data BinOp = BOAdd | BOSub | BOMul | BODiv | BOMod

instance Show BinOp where
    show BOAdd = "add"
    show BOSub = "sub"
    show BOMul = "mul"
    show BODiv = "sdiv"
    show BOMod = "srem"

data RelOp = BOLt | BOLe | BOEq | BONe | BOGe | BOGt

instance Show RelOp where
    show BOLt = "slt"
    show BOLe = "sle"
    show BOEq = "eq"
    show BONe = "ne"
    show BOGe = "sge"
    show BOGt = "sgt"

data Instruction = IBin TypeValue TypeValue BinOp TypeValue
                 | INot TypeValue TypeValue
                 | INeg TypeValue TypeValue
                 | ICmp TypeValue TypeValue RelOp TypeValue
                 | IJmp TypeValue TypeValue TypeValue
                 | IFunCall TypeValue String [TypeValue]
                 | ICast TypeValue TypeValue Type
                 | IRet TypeValue

instance Show Instruction where
    show (IBin (ta, va) (tb, vb) op (tc, vc)) = show va ++ " = " ++ show op ++ " " ++ show tb ++ " " ++ show vb ++ ", " ++  show vc
    show (INot a b) = show (IBin a (TBool, VInt 0) BOAdd b)
    show (INeg a b) = show (IBin a (TInt, VInt 0) BOSub b)
    show (ICmp (ta, va) (tb, vb) op (tc, vc)) = show va ++ " = " ++ show op ++ " " ++ show tb ++ " " ++ show vb ++ ", " ++ show vc
    show (IJmp (ta, va) (tb, vb) (tc, vc)) = "br " ++ show ta ++ " " ++ show va ++ ", " ++ show tb ++ " " ++ show vb ++ ", " ++ show tc ++ " " ++ show vc
    show (IFunCall (t, v) b tvs) = show v ++ " = call " ++ show t ++ " " ++ show b ++ "(" ++ showArgs tvs ++ ")"
        where
            showArgs [] = ""
            showArgs [(t, v)] = show t ++ " " ++ show v
            showArgs ((t, v):xs) = show t ++ " " ++ show v ++ ", " ++ showArgs xs
    show (IRet (t, v)) = "ret " ++ show t ++ " " ++ show v

{-virtualName :: String -> String -> String
virtualName c m = c ++ "_" ++ m

genGetFieldCode :: TypeValue -> String -> CM ([Instruction], TypeValue)
genGetFieldCode (TObj c, VReg i) f = throwNotImplementedException "GetField"
genGetFieldCode (TArr c, VReg i) f = throwNotImplementedException "GetField"

genCastCode :: TypeValue -> Type -> ([Instruction], TypeValue)
genCastCode (_, VNull) t = throwNotImplementedException "Cast"
genCastCode (VObj c, VReg i) t = throwNotImplementedException "Cast"
genCastCode (VArr c, VReg i) t = throwNotImplementedException "Cast"
genCastCode v _ = ([], v)

genNewCode :: Type -> CM ([Instruction], TypeValue)
genNewCode (VObj c) = throwNotImplementedException "New"
getNewCode (VArr c) = throwNotImplementedException "New"

genExprCode :: Expr -> CM ([Instruction], TypeValue)
genExprCode (EVar (PIdent (_, i))) = do
    tv <- getLocal i
    return ([], tv)
genExprCode (ELitInt i) = return ([], (TInt, VConstInt i))
genExprCode (EString s) = return ([], (TString, VConstString s))
genExprCode ELitTrue = return ([], (TBool, VConstBool True))
genExprCode ELitFalse = return ([], (TBool, VConstBool False))
genExprCode ENull = return ([], (TAnyRef, VNull))
genExprCode (ESelect e (PIdent (_, i))) = do
    (eis, etv) <- genExprCode e
    (iis, itv) <- genGetFieldCode etv i
    return (eis ++ iis, itv)
genExprCode (EMetCall e (PIdent (_, m)) args) = do
    (eis, (et@(TObj ec), ev)) <- genExprCode e

    TFun margts mrt <- virtualType <$> getVirtual ec m -- TODO ma miec typ self w argts
    let vn = (virtualName ec m)
    nv <- VRet mrt <$> newLoc

    (argis, argvs) <- unzip <$> mapM genExprCode es
    (cis, cvs) <- mapM (uncurry genCastCode) (zip argvs margts)

    return (eis ++ concat argis ++ concat cis ++ [IFunCall nv vn cvs], nv)
genExprCode (EAt e1 e2) = throwNotImplementedException
genExprCode (EApp p@(PIdent (_, f))) args) = do
    ifM (hasCurrentClassVirtual f)
        (   genExprCode (EMetCall (EVar "self") p args))
        (do TFun fargts frt <- functionType <$> getFunction f
            nv <- VRet frt <$> newLoc

            (argis, argvs) <- unzip <$> mapM genExprCode es
            (cis, cvs) <- mapM genCastCode (zip argvs margts)

            return (concat argis ++ concat cis ++ [IFunCall nv vn cvs], nv))
genExprCode (ENeg e) = do
    (eis, ev) <- getExprCode e
    nv <- VRet TInt <$> newLoc
    return (eis ++ [INeg nv ev], nv)
genExprCode (ENot e) = do
    (eis, ev) <- getExprCode e
    nv <- VRet TBool <$> newLoc
    return (eis ++ [INot nv ev], nv)
genExprCode (EIncr e) = throwNotImplementedException 6
genExprCode (EDecr e) = throwNotImplementedException 7
genExprCode (ENewVar (PIdent (_, c))) = throwNotImplementedException 8
genExprCode (ENewArr (PIdent (_, c)) e) = throwNotImplementedException 9
genExprCode (ECastVar (PIdent (_, c)) e) = do
    (eis, ev) <- getExprCode e
    (cis, cv) <- genCastCode ev (TObj c)
    return (eis ++ cis, cv)
genExprCode (ECastArr (PIdent (_, i)) e) = do
    (eis, ev) <- getExprCode e
    (cis, cv) <- genCastCode ev (TArr c)
    return (eis ++ cis, cv)
genExprCode (Mul e1 Times e2) = do
    (e1is, e1v@()) <- getExprCode e1
    (e2is, e2v) <- getExprCode e2
    case
    (VReg,
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr

{-import SemanticAnalysis

data Place = Reg Int
           | Loc Int

data TypeValue = VInt Int
           | VBool Bool
           | VLabel String
           | VRegister Int
           deriving Show

data Instruction = IAdd Value Value Value
                 | ISub Value Value Value
                 | IMul Value Value Value
                 | IDiv Value Value Value
                 | IMod Value Value Value
                 | INot Value Value
                 | INeg Value Value
                 | ICmpLt Value Value Value
                 | ICmpLe Value Value Value
                 | ICmpEq Value Value Value
                 | ICmpNe Value Value Value
                 | ICmpGe Value Value Value
                 | ICmpGt Value Value Value
                 | IJmp Value Value Value
                 | IFunCall String [Value]
                 | IRet Value
                 deriving Show

data Block = Block {
    instructions :: [Instruction]
}

genExprInstructions :: Expr -> CM ([Instruction], Value)
genExprInstructions (EVar i) = do
    Field t p <- getLocal i
    case t of

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
    | ECastVar PIdent Expr
    | ECastArr PIdent Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr-}-}