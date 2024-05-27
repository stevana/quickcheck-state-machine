-- | Utilities to pretty print 'Expr' and 'EditExpr'
module Test.StateMachine.TreeDiff.Pretty (
    -- * Explicit dictionary
    Pretty (..),
    ppExpr,
    ppEditExpr,
    ppEditExprCompact,
    -- * pretty
    prettyPretty,
    prettyExpr,
    prettyEditExpr,
    prettyEditExprCompact,
    -- * prettyprinter
    ansiWlPretty,
    ansiWlExpr,
    ansiWlEditExpr,
    ansiWlEditExprCompact,
    -- ** background
    ansiWlBgPretty,
    ansiWlBgExpr,
    ansiWlBgEditExpr,
    ansiWlBgEditExprCompact,
    -- * Utilities
    escapeName,
    ) where

import           Data.Char
                   (isAlphaNum, isPunctuation, isSymbol, ord)
import           Data.Either
                   (partitionEithers)
import           Numeric
                   (showHex)
import           Test.StateMachine.TreeDiff.Expr
import           Text.Read
                   (readMaybe)


import qualified Data.Map                        as Map
import qualified Prettyprinter                   as PP
import qualified Prettyprinter.Render.Terminal   as PP
import qualified Text.PrettyPrint                as HJ

-- | Because we don't want to commit to single pretty printing library,
-- we use explicit dictionary.
data Pretty doc = Pretty
    { ppCon    :: ConstructorName -> doc
    , ppRec    :: [(FieldName, doc)] -> doc
    , ppLst    :: [doc] -> doc
    , ppCpy    :: doc -> doc
    , ppIns    :: doc -> doc
    , ppDel    :: doc -> doc
    , ppSep    :: [doc] -> doc
    , ppParens :: doc -> doc
    , ppHang   :: doc -> doc -> doc
    }

-- | Escape field or constructor name
--
-- >>> putStrLn $ escapeName "Foo"
-- Foo
--
-- >>> putStrLn $ escapeName "_×_"
-- _×_
--
-- >>> putStrLn $ escapeName "-3"
-- `-3`
--
-- >>> putStrLn $ escapeName "kebab-case"
-- kebab-case
--
-- >>> putStrLn $ escapeName "inner space"
-- `inner space`
--
-- >>> putStrLn $ escapeName $ show "looks like a string"
-- "looks like a string"
--
-- >>> putStrLn $ escapeName $ show "tricky" ++ "   "
-- `"tricky"   `
--
-- >>> putStrLn $ escapeName "[]"
-- `[]`
--
-- >>> putStrLn $ escapeName "_,_"
-- `_,_`
--
escapeName :: String -> String
escapeName n
    | null n                      = "``"
    | isValidString n             = n
    | all valid' n && headNotMP n = n
    | otherwise                   = "`" ++ concatMap e n ++ "`"
  where
    e '`'               = "\\`"
    e '\\'              = "\\\\"
    e ' '               = " "
    e c | not (valid c) = "\\x" ++ showHex (ord c) ";"
    e c                 = [c]

    valid c = isAlphaNum c || isSymbol c || isPunctuation c
    valid' c = valid c && c `notElem` "[](){}`\","

    headNotMP ('-' : _) = False
    headNotMP ('+' : _) = False
    headNotMP _         = True

    isValidString s@('"':_:_)
        | last s == '"' =
            case readMaybe s :: Maybe String of
                Just _  -> True
                Nothing -> False
    isValidString _         = False

-- | Pretty print an 'Expr' using explicit pretty-printing dictionary.
ppExpr :: Pretty doc -> Expr -> doc
ppExpr p = ppExpr' p False

ppExpr' :: Pretty doc -> Bool -> Expr -> doc
ppExpr' p = impl where
    impl _ (App x []) = ppCon p (escapeName x)
    impl b (App x xs) = ppParens' b $ ppHang p (ppCon p (escapeName x)) $
        ppSep p $ map (impl True) xs
    impl _ (Rec x xs) = ppHang p (ppCon p (escapeName x)) $ ppRec p $
        map ppField' $ Map.toList xs
    impl _ (Lst xs)   = ppLst p (map (impl False) xs)

    ppField' (n, e) = (escapeName n, impl False e)

    ppParens' True  = ppParens p
    ppParens' False = id

-- | Pretty print an @'Edit' 'EditExpr'@ using explicit pretty-printing dictionary.
ppEditExpr :: Pretty doc -> Edit EditExpr -> doc
ppEditExpr = ppEditExpr' False

-- | Like 'ppEditExpr' but print unchanged parts only shallowly
ppEditExprCompact :: Pretty doc -> Edit EditExpr -> doc
ppEditExprCompact = ppEditExpr' True

ppEditExpr' :: Bool -> Pretty doc -> Edit EditExpr -> doc
ppEditExpr' compact p = ppSep p . ppEdit False
  where
    ppEdit b (Cpy (EditExp expr)) = [ ppCpy p $ ppExpr' p b expr ]
    ppEdit b (Cpy expr) = [ ppEExpr b expr ]
    ppEdit b (Ins expr) = [ ppIns p (ppEExpr b expr) ]
    ppEdit b (Del expr) = [ ppDel p (ppEExpr b expr) ]
    ppEdit b (Swp x y) =
        [ ppDel p (ppEExpr b x)
        , ppIns p (ppEExpr b y)
        ]

    ppEExpr _ (EditApp x []) = ppCon p (escapeName x)
    ppEExpr b (EditApp x xs) = ppParens' b $ ppHang p (ppCon p (escapeName x)) $
        ppSep p $ concatMap (ppEdit True) xs
    ppEExpr _ (EditRec x xs) = ppHang p (ppCon p (escapeName x)) $ ppRec p $
        justs ++ [ (n, ppCon p "...") | n <- take 1 nothings ]
      where
        xs' = map ppField' $ Map.toList xs
        (nothings, justs) = partitionEithers xs'

    ppEExpr _ (EditLst xs)   = ppLst p (concatMap (ppEdit False) xs)
    ppEExpr b (EditExp x)    = ppExpr' p b x

    ppField' (n, Cpy (EditExp e)) | compact, not (isScalar e) = Left n
    ppField' (n, e) = Right (escapeName n, ppSep p $ ppEdit False e)

    ppParens' True  = ppParens p
    ppParens' False = id

    isScalar (App _ []) = True
    isScalar _          = False

-------------------------------------------------------------------------------
-- pretty
-------------------------------------------------------------------------------

-- | 'Pretty' via @pretty@ library.
prettyPretty :: Pretty HJ.Doc
prettyPretty = Pretty
    { ppCon    = HJ.text
    , ppRec    = HJ.braces . HJ.sep . HJ.punctuate HJ.comma
               . map (\(fn, d) -> HJ.text fn HJ.<+> HJ.equals HJ.<+> d)
    , ppLst    = HJ.brackets . HJ.sep . HJ.punctuate HJ.comma
    , ppCpy    = id
    , ppIns    = \d -> HJ.char '+' HJ.<> d
    , ppDel    = \d -> HJ.char '-' HJ.<> d
    , ppSep    = HJ.sep
    , ppParens = HJ.parens
    , ppHang   = (`HJ.hang` 2)
    }

-- | Pretty print 'Expr' using @pretty@.
--
-- >>> prettyExpr $ Rec "ex" (Map.fromList [("[]", App "bar" [])])
-- ex {`[]` = bar}
prettyExpr :: Expr -> HJ.Doc
prettyExpr = ppExpr prettyPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @pretty@.
prettyEditExpr :: Edit EditExpr -> HJ.Doc
prettyEditExpr = ppEditExpr prettyPretty

-- | Compact 'prettyEditExpr'.
prettyEditExprCompact :: Edit EditExpr -> HJ.Doc
prettyEditExprCompact = ppEditExprCompact prettyPretty

-------------------------------------------------------------------------------
-- prettyprinter
-------------------------------------------------------------------------------

-- | 'Pretty' via @prettyprinter@ library (with colors).
ansiWlPretty :: Pretty (PP.Doc PP.AnsiStyle)
ansiWlPretty = Pretty
    { ppCon    = PP.pretty
    , ppRec    = PP.encloseSep PP.lbrace PP.rbrace PP.comma
               . map (\(fn, d) -> PP.pretty fn PP.<+> PP.equals <> PP.softline <> d)
    , ppLst    = PP.list
    , ppCpy    = PP.annotate (PP.colorDull PP.White)
    , ppIns    = \d -> PP.annotate (PP.color PP.Green) $ PP.unAnnotate $ PP.pretty '+' PP.<> d
    , ppDel    = \d -> PP.annotate (PP.color PP.Red)   $ PP.unAnnotate $ PP.pretty '-' PP.<> d
    , ppSep    = PP.sep
    , ppParens = PP.parens
    , ppHang   = \d1 d2 -> PP.hang 2 (d1 <> PP.softline <> d2)
    }

-- | Pretty print 'Expr' using @prettyprinter@.
ansiWlExpr :: Expr -> PP.Doc PP.AnsiStyle
ansiWlExpr = ppExpr ansiWlPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @prettyprinter@.
ansiWlEditExpr :: Edit EditExpr -> PP.Doc PP.AnsiStyle
ansiWlEditExpr = ppEditExpr ansiWlPretty

-- | Compact 'ansiWlEditExpr'
ansiWlEditExprCompact :: Edit EditExpr -> PP.Doc PP.AnsiStyle
ansiWlEditExprCompact = ppEditExprCompact ansiWlPretty

-------------------------------------------------------------------------------
-- Background
-------------------------------------------------------------------------------

-- | Like 'ansiWlPretty' but color the background.
ansiWlBgPretty :: Pretty (PP.Doc PP.AnsiStyle)
ansiWlBgPretty = ansiWlPretty
    { ppIns    = \d -> PP.annotate (PP.bgColorDull PP.Green <> PP.color PP.White) $ PP.unAnnotate $ PP.pretty '+' PP.<> d
    , ppDel    = \d -> PP.annotate (PP.bgColorDull PP.Red   <> PP.color PP.White) $ PP.unAnnotate $ PP.pretty '-' PP.<> d
    }

-- | Pretty print 'Expr' using @prettyprinter@.
ansiWlBgExpr :: Expr -> PP.Doc PP.AnsiStyle
ansiWlBgExpr = ppExpr ansiWlBgPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @prettyprinter@.
ansiWlBgEditExpr :: Edit EditExpr -> PP.Doc PP.AnsiStyle
ansiWlBgEditExpr = ppEditExpr ansiWlBgPretty

-- | Compact 'ansiWlBgEditExpr'.
ansiWlBgEditExprCompact :: Edit EditExpr -> PP.Doc PP.AnsiStyle
ansiWlBgEditExprCompact = ppEditExprCompact ansiWlBgPretty
