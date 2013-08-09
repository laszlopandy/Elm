module Parse.Expr (def,term) where

import Ast
import Located
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (isSymbol, isDigit)
import Data.List (foldl')
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent
--import qualified Text.Pandoc as Pan

import Parse.Library
import Parse.Patterns
import Parse.Binops
import Parse.Types

import Guid
import Types.Types (Type (VarT), Scheme (Forall))


--------  Basic Terms  --------

numTerm :: IParser Expr
numTerm = toExpr <$> (preNum <?> "number")
    where toExpr n | '.' `elem` n = FloatNum (read n)
                   | otherwise = IntNum (read n)
          preNum  = (++) <$> many1 digit <*> option "" postNum
          postNum = do try $ lookAhead (string "." >> digit)
                       string "."
                       ('.':) <$> many1 digit

strTerm :: IParser Expr
strTerm = liftM Str . expecting "string" . betwixt '"' '"' . many $
          backslashed <|> satisfy (/='"')

varTerm :: IParser Expr
varTerm = toVar <$> var <?> "variable"

toVar v = case v of "True"  -> Boolean True
                    "False" -> Boolean False
                    _       -> Var v

chrTerm :: IParser Expr
chrTerm = Chr <$> betwixt '\'' '\'' (backslashed <|> satisfy (/='\''))
          <?> "character"

accessor :: IParser Expr
accessor = do
  start <- getPosition
  lbl <- try (string "." >> rLabel)
  end <- getPosition
  let loc e = addLoc ("." ++ lbl) (pos start end e)
  return (Lambda "_" (loc $ Access (loc $ Var "_") lbl))


--------  Complex Terms  --------

listTerm :: IParser Expr
listTerm =
      (do { try $ string "[markdown|"
          ; md <- filter (/='\r') <$> manyTill anyChar (try $ string "|]")
          ; return . Markdown $ "<markdown>" }) --Pan.readMarkdown Pan.def md })
  <|> (braces $ choice
       [ try $ do { lo <- expr; whitespace; string ".." ; whitespace
                  ; Range lo <$> expr }
       , do (L _ _ e) <- list <$> commaSep expr
            return e
       ])

parensTerm :: IParser CExpr
parensTerm = parens $ choice
             [ do start <- getPosition
                  op <- try anyOp
                  end <- getPosition
                  let loc = pos start end
                  return . loc . Lambda "x" . loc . Lambda "y" . loc $
                         Binop op (loc $ Var "x") (loc $ Var "y")
             , do start <- getPosition
                  let comma = char ',' <?> "comma ','"
                  commas <- comma >> many (whitespace >> comma)
                  end <- getPosition
                  let vars = map (('v':) . show) [ 0 .. length commas + 1 ]
                      loc = pos start end
                  return $ foldr (\x e -> loc $ Lambda x e)
                             (loc . tuple $ map (loc . Var) vars) vars
             , do start <- getPosition
                  es <- commaSep expr
                  end <- getPosition
                  return $ case es of [e] -> e
                                      _   -> pos start end (tuple es)
             ]

recordTerm :: IParser CExpr
recordTerm = brackets $ choice [ misc, addLocation record ]
    where field = do
              fDefs <- (:) <$> (PVar <$> rLabel) <*> spacePrefix patternTerm
              whitespace
              e <- string "=" >> whitespace >> expr
              n <- sourceLine <$> getPosition
              runAt (1000 * n) $ flattenPatterns fDefs e
          extract [ FnDef f args exp ] = return (f,args,exp)
          extract _ = fail "Improperly formed record field."
          record = Record <$> (mapM extract =<< commaSep field)
          change = do
              lbl <- rLabel
              whitespace >> string "<-" >> whitespace
              (,) lbl <$> expr
          remove r = addLocation (string "-" >> whitespace >> Remove r <$> rLabel)
          insert r = addLocation $ do
                       string "|" >> whitespace
                       Insert r <$> rLabel <*>
                           (whitespace >> string "=" >> whitespace >> expr)
          modify r = addLocation
                     (string "|" >> whitespace >> Modify r <$> commaSep1 change)
          misc = try $ do
            record <- addLocation (Var <$> rLabel)
            whitespace
            opt <- optionMaybe (remove record)
            whitespace
            case opt of
              Just e  -> try (insert e) <|> return e
              Nothing -> try (insert record) <|> try (modify record)
                        

term :: IParser CExpr
term =  addLocation (choice [ numTerm, strTerm, chrTerm, listTerm, accessor ])
    <|> accessible (addLocation varTerm <|> parensTerm <|> recordTerm)
    <?> "basic term (4, x, 'c', etc.)"

--------  Applications  --------

appExpr :: IParser CExpr
appExpr = do
  tlist <- spaceSep1 term
  return $ case tlist of
             t:[] -> t
             t:ts -> foldl' (\f x -> epos f x $ App f x) t ts

--------  Normal Expressions  --------

binaryExpr :: IParser CExpr
binaryExpr = binops [] appExpr anyOp

ifExpr :: IParser Expr
ifExpr = reserved "if" >> whitespace >> (normal <|> multiIf)
    where normal = do e1 <- expr ; whitespace
                      reserved "then" ; whitespace ; e2 <- expr
                      whitespace <?> "an 'else' branch"
                      reserved "else" <?> "an 'else' branch" ; whitespace
                      If e1 e2 <$> expr
          multiIf = (MultiIf <$> spaceSep1 iff)
              where iff = do string "|" ; whitespace
                             b <- expr ; whitespace ; string "->" ; whitespace
                             (,) b <$> expr

lambdaExpr :: IParser CExpr
lambdaExpr = do char '\\' <|> char '\x03BB' <?> "anonymous function"
                whitespace
                pats <- spaceSep1 patternTerm
                whitespace ; arrow ; whitespace
                e <- expr
                return . run $ makeLambda pats e

defSet :: IParser [Def]
defSet = concat <$> block (do d <- anyDef ; whitespace ; return d)

letExpr :: IParser Expr
letExpr = do
  reserved "let" ; whitespace
  defs <- defSet
  whitespace ; reserved "in" ; whitespace
  Let defs <$> expr

caseExpr :: IParser Expr
caseExpr = do
  reserved "case"; whitespace; e <- expr; whitespace; reserved "of"; whitespace
  Case e <$> (with <|> without)
    where case_ = do p <- patternExpr; whitespace; arrow; whitespace
                     (,) p <$> expr
          with    = brackets (semiSep1 (case_ <?> "cases { x -> ... }"))
          without = block (do c <- case_ ; whitespace ; return c)

expr = addLocation (choice [ ifExpr, letExpr, caseExpr ])
    <|> lambdaExpr
    <|> binaryExpr 
    <?> "an expression"

funcDef = try (do p1 <- try patternTerm ; infics p1 <|> func p1)
          <|> ((:[]) <$> patternExpr)
          <?> "the definition of a variable (x = ...)"
    where func p@(PVar v) = (p:) <$> spacePrefix patternTerm
          func p          = do try (lookAhead (whitespace >> string "="))
                               return [p]
          infics p1 = do
            o:p <- try (whitespace >> anyOp)
            p2  <- (whitespace >> patternTerm)
            return $ if o == '`' then [ PVar $ takeWhile (/='`') p, p1, p2 ]
                                 else [ PVar (o:p), p1, p2 ]

assignExpr :: IParser [Def]
assignExpr = withPos $ do
  fDefs <- funcDef
  whitespace
  e <- string "=" >> whitespace >> expr
  n <- sourceLine <$> getPosition
  runAt (1000 * n) $ flattenPatterns fDefs e

anyDef = 
  ((\d -> [d]) <$> typeAnnotation) <|>
  assignExpr

def = map Definition <$> anyDef

parseDef str =
    case iParse def "" str of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
