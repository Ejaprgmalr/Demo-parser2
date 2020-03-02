module ElaboratorImpl where

import Absyn
-- add other imports
import Data.Char

import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Applicative




lookres :: [Resource] -> RName -> Either ErrMsg Resource
lookres [] _ = Left "not found resource"
lookres  ((R curr) : rest) target =
    case curr == target of
        True -> Right (R $ toUpperString curr)
        False -> lookres rest target


toUpperString :: [Char] -> [Char]
toUpperString = map toUpper





-- Int: provides, useds, requireds
type REnv = Map Resource (Int, Int, Int)
type REnvUnit = (Resource, (Int, Int, Int))

type CEnv = Map CName REnv

newtype Elaba a = Elaba {runElaba :: CEnv -> (Either ErrMsg (a, CEnv))}

instance Monad Elaba where
  return a = Elaba $ \env -> (Right (a, env))
  m >>= f = Elaba (\env -> (do
                              (a, env') <- runElaba m env
                              runElaba (f a) env'))
  fail s = Elaba (\_ -> Left s)

instance Functor Elaba where
  fmap = liftM
instance Applicative Elaba where
  pure = return; (<*>) = ap


type ContentUnit = (RName, Int)
type Content = [ContentUnit]

type StandardClauseUnit = (CKind, RName, Int)
type StandardClause = [StandardClauseUnit]


type StandardComponentUnit = CName StandardClause
type StandardComponent = [StandardComponentUnit]



buildResourceMap :: StandardClause -> REnv -> REnv
buildResourceMap [] container = container
buildResourceMap ((ckind, rn, i):rest) container = 
    case Map.lookup (R rn) container of
        Nothing -> 
            case ckind of
                CKProvides -> let newContainer = Map.insert (R rn) (i, 0, 0) container in
                              buildResourceMap rest newContainer 
                CKUses     -> let newContainer = Map.insert (R rn) (0, i, 0) container in
                              buildResourceMap rest newContainer
                CKRequires -> let newContainer = Map.insert (R rn) (0, 0, i) container in
                              buildResourceMap rest newContainer

        Just (proi, usei, reqi) ->
            case ckind of
                CKProvides -> let newContainer = Map.insert (R rn) (proi+i, usei, reqi) container in
                              buildResourceMap rest newContainer
                CKUses     -> let newContainer = Map.insert (R rn) (proi, usei+i, reqi) container in
                              buildResourceMap rest newContainer
                CKRequires -> let newContainer = Map.insert (R rn) (proi, usei, reqi+i) container in
                              buildResourceMap rest newContainer

mergeRMap :: [REnvUnit] -> REnv -> REnv
mergeRMap [] renv = renv
mergeRMap ((resource, (proi, usei, reqi)) : rest) renv =
    case Map.lookup resource renv of
        Nothing -> 
                let newRenv = Map.insert resource (proi, usei, reqi) renv in
                mergeRMap rest newRenv
        Just (proi1, usei1, reqi1) -> 
                let newRenv = Map.insert resource (proi + proi1, usei + usei1, reqi + reqi1) renv in
                mergeRMap rest newRenv



-- buildCompMap :: [IComp] -> CEnv -> CEnv
-- buildCompMap [] container = container
-- buildCompMap ((IC cn clause_list):rest) container = 
--         case Map.lookup cn container of
--             Nothing    -> let newContainer = Map.insert cn (buildResourceMap clause_list) container in
--                           buildCompMap rest newContainer
--             Just renv  -> let thisREnv = buildResourceMap clause_list
--                               newREnv = mergeRMap (Map.toList thisREnv) renv
--                               newContainer = Map.insert cn newREnv container
--                           in  buildCompMap rest newContainer



-- putIComp :: [IComp] -> Elaba ()
-- putIComp icomp_list =
--     Elaba(\env -> Right (buildcompMap icomp_list))









elabName :: RSpec -> Either ErrMsg Content
elabName (RSNum i (RSRes rn)) = Right [(rn, i)]
elabName (RSRes rn) = Right [(rn, 1)]
elabName (RSAnd rsp1 rsp2) = 
    case (elabName rsp1, elabName rsp2) of
        (Right fst_list, Right snd_list) -> Right (fst_list ++ snd_list)  
        (Left _, _) -> Left "err"
        (_, Left _)   -> Left " err"
elabName (RSOr rsp1 rsp2) = 
    case (elabName rsp1, elabName rsp2) of
        (Right [(r1, 1)], Right [(r2, 1)]) -> Right [(r1++"or"++r2, 1)]
        (Right [(r1, i1)], Right [(r2, i2)]) -> Right [(show i1 ++ r1++"or"++show i2 ++r2, 1)]
        (Left _, _) -> Left "err"
        (_, Left _)   -> Left "err"
elabName _ = Left "invalid form of RSpec"



-- elabClauses :: [Clause] -> Either ErrMsg StandardClause
-- elabClauses ((ckind, rspec):rest)  =
--     case (elabName rspec) of
--         (Right content_list) -> Right ((add_ckind ckind content_list [])++ (elabClauses rest) )
--         (Left err) -> Left err

elabClauses :: [Clause] -> StandardClause -> Either ErrMsg StandardClause
elabClauses [] standardClause = Right standardClause
elabClauses ((ckind, rspec):rest) standardClause =
    case (elabName rspec) of
        (Right content_list) -> 
               elabClauses rest (standardClause ++ (add_ckind ckind content_list []))
        (Left err) -> Left err


add_ckind :: CKind -> Content -> StandardClause -> StandardClause
add_ckind _ [] container = container
add_ckind ckind ((rn, i):rest) container = add_ckind ckind rest ((ckind, rn, i):container)



-- elabIComp :: [IComp] -> StandardComponent ->Either ErrMsg StandardComponent
-- elabIComp [] standardComponent =Right standardComponent
-- elabIComp ((IC cn clause_list):rest) standardComponent =
--        case elabClauses clause_list of
--            (Right standardclause) ->
--                   elabIComp rest ()


-- elabIComp :: IComp -> Elaba RProf 
-- elabIComp (IC cname clause_list) =
--     case (elabClauses clause_list []) of
--         (Right standardclause) -> 



-- elabClauses :: Either ErrMsg 





elaborate :: IDB -> Either ErrMsg DB
elaborate = undefined






-- type IDB = ([RName], [IComp])
-- data IComp = IC CName [Clause]
-- type Clause = (CKind, RSpec)



-- type DB = ([Resource], [(CName, RProf)])
-- type RProf = [(Resource, (Int, Int))] -- (net contribution, requirement)




-- eg. input : ([str, str....], [IC str [(CKind, RSRpec)], IC str [], .....])
--     output: ([R str...R str],   [str [R str, (int, int)], []])


-- input of parser
-- component c: provides r2, 5 r1, r3; uses 2 r1; requires 4 r3, 7 r1; uses 1 r2.


-- output of parser also the input of elaborator
-- [IC "c" [(CKProvides,RSAnd (RSRes "r2") (RSAnd (RSNum 5 (RSRes "r1")) (RSRes "r3"))),(CKUses,RSNum 2 (RSRes "r1")),(CKRequires,RSAnd (RSNum 4 (RSRes "r3")) (RSNum 7 (RSRes "r1"))),(CKUses,RSNum 1 (RSRes "r2"))]]


-- output of elaborator
-- ("c", [(R "r1", (3,7)), (R "r3", (1, 4))]



--     dbi = (["r"], [IC "c" [(CKProvides, RSRes "r")]])
--     dbf = ([R "r"], [("c", [(R "r", (1,0))])])

-- [(CKProvides,RSAnd (RSNum 1 (RSRes "a")) (RSNum 2 (RSRes "b"))),(CKUses,RSNum 1 (RSRes "b")),(CKRequires,RSNum 1 (RSRes "d")),(CKUses,RSNum 1 (RSRes "a"))]])
-- [(R "b" (1, 0)), (R "d" (0, 1))]



                        
                  