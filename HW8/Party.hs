{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons (Emp name fun) (GL xs funSum) = GL ((Emp name fun) : xs) (fun + funSum)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL xs funx) (GL ys funy) = (GL (xs ++ ys) (funx + funy))
    
moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL xs funx) (GL ys funy)
    | funx > funy   = (GL xs funx)
    | otherwise     = (GL ys funy)

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f initVal (Node rootVal children) = foldr (\x y -> treeFold f y x) newInitVal children
            where  newInitVal = f rootVal initVal

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss employees = (withBoss, (glCons boss withoutBoss))
            where
                    withBoss       = mconcat (map (\(x,y) -> y) employees)
                    withoutBoss    = mconcat (map (\(x,y) -> moreFun x y) employees)
maxFunBothOptions :: Tree Employee -> (GuestList, GuestList)
--The case of Leaf
maxFunBothOptions (Node (Emp name fun) []) = ((GL [] 0), (GL [(Emp name fun)] fun))
maxFunBothOptions (Node rootVal children) = nextLevel rootVal (map maxFunBothOptions children)

maxFun :: Tree Employee -> GuestList
maxFun t = moreFun withBoss withoutBoss
            where (withBoss, withoutBoss) = (maxFunBothOptions t)

getName :: Employee -> String
getName (Emp name _) = name

convertToFormat :: GuestList -> [String]
convertToFormat (GL guests fun) = ("Total fun: " ++ (show fun)) : (map getName guests)

readEmployeeTree :: String -> Tree Employee
readEmployeeTree s = read s :: Tree Employee

main = interact $ unlines . convertToFormat . maxFun . readEmployeeTree
