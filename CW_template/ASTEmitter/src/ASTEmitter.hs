{-
Implement the following functions:

    ppStencilDef,
    ppArgDecl,
    ppFSig,
    ppLHSExpr,
    ppRHSExpr

Leave everything else as it is.
-}
module ASTEmitter
  ( ppProgram,
    ppBindings,
    ppAST,
    ppExprTup,
    ppFSig,
    ppArgDecl,
    ppStencilDef,
    ppMainTypeDecl,
    ppMainArgDef,
    ppMainReturnDef,
  )
where

import AST
import Data.List (intercalate)

ppProgram :: ASTInstance -> IO ()
ppProgram astInstance =
  let (instanceName, ast, functionSignaturesList, stencilDefinitionsList, mainArgDeclsList) = astInstance
      (mainArgDeclsInArgs, mainArgDeclsOutArgs) = mainArgDeclsList
      stencilDefs = map ppStencilDef stencilDefinitionsList
      inArgDecls = map ppArgDecl mainArgDeclsInArgs
      outArgDecls = map ppArgDecl mainArgDeclsOutArgs
      -- inArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsInArgs
      -- outArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsOutArgs
      functionDecls = map ppFSig functionSignaturesList
      mainTypeDecl = ppMainTypeDecl mainArgDeclsList
      mainArgDef = ppMainArgDef mainArgDeclsList
      mainReturnDef = ppMainReturnDef mainArgDeclsList
      mainExprs = map ("    " ++) (ppAST ast)
      mainDef = [mainArgDef] ++ mainExprs ++ [mainReturnDef]
   in mapM_
        putStrLn
        ( ["-- " ++ instanceName ++ "\n"]
            ++ ["-- Stencil definitions"]
            ++ stencilDefs
            ++ ["\n-- Argument type declarations"]
            ++ ["---- Input arguments"]
            ++ inArgDecls
            ++ ["---- Output arguments"]
            ++ outArgDecls
            ++ ["\n-- Function type declarations"]
            ++ functionDecls
            ++ ["\n-- Main function type declaration"]
            ++ [mainTypeDecl]
            ++ ["\n-- Main function definition"]
            ++ mainDef
        )

ppBindings :: AST -> String
ppBindings = unlines . ppAST

ppAST :: AST -> [String]
ppAST = map ppExprTup

ppExprTup :: (Expr, Expr) -> String
ppExprTup (lhs, rhs) = ppLHSExpr lhs ++ " = " ++ ppRHSExpr rhs

ppLHSExpr :: Expr -> String
ppLHSExpr (Vec _ exp) = ppLHSExpr exp
ppLHSExpr (SVec _ exp) = ppLHSExpr exp
ppLHSExpr (Scalar _ _ name) = name
ppLHSExpr (Tuple explist) = "(" ++ intercalate "," (map ppLHSExpr explist) ++ ")"

ppRHSExpr :: Expr -> String
ppRHSExpr (Map a b) = "map" ++ " " ++ ppRHSExpr a ++ " " ++ ppRHSExpr b
ppRHSExpr (Function name list) = if null list then name else "(" ++ name ++ " (" ++ intercalate "," (map ppRHSExpr list) ++ "))"
ppRHSExpr (ZipT list) = "(zipt (" ++ intercalate "," (map ppRHSExpr list) ++ "))"
ppRHSExpr (Stencil a b) = "stencil " ++ ppRHSExpr a ++ " " ++ ppRHSExpr b
ppRHSExpr (UnzipT exp) = "unzipt (" ++ ppRHSExpr exp ++ ")"
ppRHSExpr (Vec _ exp) = ppRHSExpr exp
ppRHSExpr (SVec _ exp) = ppRHSExpr exp
ppRHSExpr (FVec _ exp) = ppRHSExpr exp
ppRHSExpr (Scalar _ _ name) = name

-- Pretty-printer for the function signatures
ppFSig :: FunctionSignature -> String
ppFSig (n, l) = n ++ " :: " ++ applyTry l

applyTry :: [Expr] -> String
applyTry [] = ""
applyTry (x : xs)
  | null xs = try x
  | checkEMPTupl x = applyTry xs
  | otherwise = try x ++ " -> " ++ applyTry xs

checkEMPTupl :: Expr -> Bool
checkEMPTupl (Tuple []) = True
checkEMPTupl (Tuple _) = False
checkEMPTupl _ = False

try :: Expr -> String
try (FVec os expression) = "FVec" ++ " " ++ "[" ++ show (head os) ++ "]" ++ try expression
try (Tuple explist) = "(" ++ intercalate ", " (map try explist) ++ ")"
try (Vec _ exp) = "Vec" ++ " " ++ try exp
try (Function _ fexp) = concatMap try fexp
try (SVec s sexp) = "SVec" ++ " " ++ show s ++ " " ++ try sexp
try (Scalar _ dt _) = ppDType dt
try (ZipT zlist) = concatMap try zlist
try (UnzipT uexp) = try uexp
try (Const ci) = show ci
try (Stencil s1 s2) = try s1 ++ " " ++ try s2
try (Map m1 m2) = try m1 ++ " " ++ try m2

-- Pretty-printer for the argument data types
ppDType :: DType -> String
ppDType DInteger = "Int"
ppDType DInt = "Int"
ppDType DReal = "Float"
ppDType DFloat = "Float"
ppDType (DSVec sz dt) = "SVec " ++ show sz ++ " " ++ ppDType dt
ppDType (DVec sz dt) = "Vec " ++ show sz ++ " " ++ ppDType dt
ppDType (DFVec dims dt) = "FVec " ++ show dims ++ " " ++ ppDType dt
ppDType (DTuple dts) = "(" ++ intercalate ", " (map ppDType dts) ++ ")"
ppDType DDC = show DDC

ppArgDecl :: (String, DType) -> String
ppArgDecl a = ppArgName a ++ " :: " ++ ppArgDeclType a

ppArgDeclType :: (String, DType) -> String
ppArgDeclType (_, argType) = ppDType argType

ppArgName :: (String, DType) -> String
ppArgName (argName, _) = argName

ppArgs pp argDecls
  | length argDecls == 1 = pp (head argDecls)
  | otherwise = "(" ++ intercalate "," (map pp argDecls) ++ ")"

-- Pretty-printer for stencil definitions
ppStencilDef :: StencilDefinition -> String
ppStencilDef (name, lint) = name ++ " = " ++ show lint

ppMainTypeDecl :: ([(String, DType)], [(String, DType)]) -> String
ppMainTypeDecl mainArgDeclsList_ =
  let (mainArgDeclsInArgs, mainArgDeclsOutArgs) = mainArgDeclsList_
      inArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsInArgs
      outArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsOutArgs
   in "main :: " ++ inArgDeclTypes ++ " -> " ++ outArgDeclTypes

ppMainArgDef :: ([(String, DType)], [(String, DType)]) -> String
ppMainArgDef (mainArgDeclsInArgs, mainArgDeclsOutArgs) = "main " ++ ppArgs ppArgName mainArgDeclsInArgs ++ " = let "

ppMainReturnDef :: ([(String, DType)], [(String, DType)]) -> String
ppMainReturnDef (mainArgDeclsInArgs, mainArgDeclsOutArgs) = "  in\n      " ++ ppArgs ppArgName mainArgDeclsOutArgs