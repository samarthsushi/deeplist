module DeepList where

import Control.Monad (foldM)

data DeepList a = Elem a | List [DeepList a] deriving (Show)

instance Functor DeepList where
    fmap f (Elem x) = Elem (f x)
    fmap f (List xs) = List (map (fmap f) xs)

instance Applicative DeepList where
    pure = Elem 
    (Elem f) <*> dl = fmap f dl
    (List fs) <*> dl = List (map (<*> dl) fs)

instance Monad DeepList where
    return = pure
    (Elem x) >>= f = f x
    (List xs) >>= f = List (map (>>= f) xs)

nextDeep :: DeepList a -> Maybe (a, DeepList a)
-- If it's a single element, return it
nextDeep (Elem x) = Just (x, List []) 
-- If it's an empty list, return Nothing                              
nextDeep (List []) = Nothing   
-- If the first element is an Elem, return it                                     
nextDeep (List (Elem x : xs)) =                                     
    Just (x, List xs)
-- If we encounter a nested List, go deeper
nextDeep (List (List ys : xs)) =                                    
    case nextDeep (List ys) of
        -- If the nested list is empty, continue with xs
        Nothing -> nextDeep (List xs)                               
        Just (deepElem, rest) -> Just (deepElem, List (rest : xs))

chainNextDeep :: Int -> DeepList a -> Maybe (a, DeepList a)
chainNextDeep n dl = foldM (\(_, rest) _ -> nextDeep rest) (undefined, dl) [1..n]


