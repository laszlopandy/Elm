{-# LANGUAGE PatternGuards #-}
module Types.Alias (dealias, get, mistakes) where

import Ast
import Located
import Control.Arrow (second)
import Data.List (group,sort)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types.Substitutions (subst)
import Types.Types

builtins :: [(String,([X],Type))]
builtins =
    let touch = ("t0", time) : map (flip (,) int) ["x","y","x0","y0","id"]
        state = [("string", string), ("selectionStart", int), ("selectionEnd", int)]
        line  = [("color", tipe "Color"), ("width", float),
                 ("cap", tipe "LineCap"), ("join", tipe "LineJoin"),
                 ("miterLimit", float),   ("dashing", listOf int),
                 ("dashOffset", int)]
        makeRecord fields =
            RecordT (Map.fromList $ map (second (:[])) fields) EmptyRecord
    in  [ ("String", ([], listOf char)),
          ("Time", ([], float)),
          ("KeyCode", ([], int)),
          ("Touch", ([], makeRecord touch)),
          ("FieldState", ([], makeRecord state)),
          ("LineStyle", ([], makeRecord line))
        ]

get :: [Statement] -> Map.Map String ([X],Type)
get stmts = Map.fromList (builtins ++ concatMap getAlias stmts)
    where getAlias stmt = case stmt of
                            TypeAlias alias xs t -> [(alias, (xs,t))]
                            _ -> []

dealias :: Map.Map String ([X],Type) -> Type -> Type
dealias aliases t =
  let f = dealias aliases in
  case t of
    ADT name ts -> case Map.lookup name aliases of
                     Just (xs,t) -> f (subst (zip xs ts) t)
                     Nothing -> ADT name (map f ts)
    LambdaT t u -> LambdaT (f t) (f u)
    RecordT r t -> RecordT (Map.map (map f) r) (f t)
    _ -> t

mistakes :: [Statement] -> [String]
mistakes stmts = badKinds stmts ++ dups stmts ++ badOrder stmts

badKinds :: [Statement] -> [String]
badKinds stmts = [msg tname | t <- getAllTypes stmts, tname <- badT t]
  where
    msg x = "Type Error: Type alias '" ++ x ++
            "' was given the wrong number of arguments."

    badT :: Type -> [String]
    badT (ADT name ts)
      | Just (xs, _) <- Map.lookup name (get stmts),
        length xs /= length ts = [name]
    badT _ = []

getAllTypes :: [Statement] -> [Type]
getAllTypes [] = []
getAllTypes (stmt:xs) = case stmt of
      (Definition def) -> fromDef def ++ rest
      (Datatype _ _ strTypeListList) -> (concatMap snd strTypeListList) ++ rest
      (TypeAlias _ _ t) -> t : rest
      (ImportEvent _ _ _ t) -> t : rest
      (ExportEvent _ _ t) -> t : rest
    where rest = getAllTypes xs
          fromDef def = case def of
              (FnDef _ _ _) -> []
              (OpDef _ _ _ _) -> []
              (TypeAnnotation _ t) -> [t]

annotation :: Def -> Maybe String
annotation s =
    case s of
      TypeAnnotation name _ -> Just name
      _ -> Nothing

definition :: Def -> Maybe String
definition s =
    case s of
      FnDef name _ _ -> Just name
      OpDef name _ _ _ -> Just name
      _ -> Nothing

checkTopLevelAndLets :: [Statement] -> (String -> [Def] -> [a]) -> [a]
checkTopLevelAndLets stmts fcheck =
  fcheck "at top-level" topLevelDefs ++
  concatMap (fcheck "in let binding") allLetDefs
  where
    topLevelDefs = mapMaybe maybeDef stmts
    maybeDef (Definition d) = Just d
    maybeDef _ = Nothing
    allLetDefs = getAllLets stmts

getAllLets :: [Statement] -> [[Def]]
getAllLets [] = []
getAllLets (stmt:xs) = case stmt of
      (Definition def) -> (fromDef def) : rest
      (Datatype _ _ _) -> rest
      (TypeAlias _ _ _) -> rest
      (ImportEvent _ expr _ _) -> (fromExpr expr) : rest
      (ExportEvent _ _ _) -> rest
    where rest = getAllLets xs
          fromDef def = case def of
              (FnDef _ _ expr) -> fromExpr expr
              (OpDef _ _ _ expr) -> fromExpr expr
              (TypeAnnotation _ _) -> []
          fromExpr (L _ _ e) = case e of
              (IntNum _) -> []
              (FloatNum _) -> []
              (Chr _) -> []
              (Str _) -> []
              (Boolean _) -> []
              (Range expr1 expr2) -> fromExpr expr1 ++ fromExpr expr2
              (Access expr _) -> fromExpr expr
              (Remove expr _) -> fromExpr expr
              (Insert expr1 _ expr2) -> fromExpr expr1 ++ fromExpr expr2
              (Modify expr strExprList) -> fromExpr expr ++ (concatMap (fromExpr . snd) strExprList)
              (Record strStrListExprList) -> concatMap (\(x,y,z) -> fromExpr z) strStrListExprList
              (Binop _ expr1 expr2) -> fromExpr expr1 ++ fromExpr expr2
              (Lambda _ expr) -> fromExpr expr
              (App expr1 expr2) -> fromExpr expr1 ++ fromExpr expr2
              (If expr1 expr2 expr3) ->  fromExpr expr1 ++ fromExpr expr2 ++ fromExpr expr3
              (MultiIf exprExprList) -> concatMap (\(e1,e2) -> fromExpr e1 ++ fromExpr e2) exprExprList
              (Let defs expr) -> defs ++ (concatMap fromDef defs) ++ fromExpr expr
              (Var _) -> []
              (Case expr patternExprList) -> fromExpr expr ++ concatMap (fromExpr . snd) patternExprList
              (Data _ exprs) -> concatMap fromExpr exprs
              (Markdown _) -> []


dups :: [Statement] -> [String]
dups stmts = checkTopLevelAndLets stmts $ \ctxt defs ->
  let
      dup :: (Def -> Maybe String) -> [String]
      dup f = map head . filter ((>1) . length) . group . sort $ mapMaybe f defs

      msg = "Syntax Error: There can only be one "
      defMsg x = msg ++ "definition of '" ++ x ++ "' " ++ ctxt ++ "."
      annMsg x = msg ++ "type annotation for '" ++ x ++ "'" ++ ctxt ++ "."
  in
    map defMsg (dup definition) ++ map annMsg (dup annotation)

badOrder :: [Statement] -> [String]
badOrder stmts = checkTopLevelAndLets stmts $ \ctxt defs ->
  let
      msg x = "Syntax Error: The type annotation for '" ++ x ++
              "' must be directly above its definition in " ++ ctxt ++ "."
      as = sort $ mapMaybe annotation defs
      ds = sort $ mapMaybe definition defs

      expectedPairs :: [String] -> [String] -> [String]
      expectedPairs as ds =
          case (as,ds) of
            (x:xs, y:ys) -> case compare x y of
                              LT -> expectedPairs xs (y:ys)
                              EQ -> x : expectedPairs xs ys
                              GT -> expectedPairs (x:xs) ys
            ( _  ,  _  ) -> []

      actualPairs :: [Def] -> [String]
      actualPairs stmts =      
          case stmts of
            TypeAnnotation n _ : rest@(d : _) ->
                (if Just n == definition d
                 then [n]
                 else []
                ) ++ actualPairs rest
            t:s:rest -> actualPairs (s:rest)
            _ -> []

      missings :: [String] -> [String] -> [String]
      missings expected actual =
          case (expected, actual) of
            (e:es, a:as) -> case compare e a of
                              LT -> e : missings es (a:as)
                              EQ -> missings es as
                              GT -> a : missings (e:es) as
            ( [] ,  _  ) -> actual
            (  _ ,  [] ) -> expected
  in
   map msg $ missings (sort $ expectedPairs as ds) (sort $ actualPairs defs)
