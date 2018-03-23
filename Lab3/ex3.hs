impartireMaybe :: Maybe Int -> Maybe Int -> Maybe Int
impartireMaybe e1 e2 = case (e1, e2) of
                    (_, Just 0) -> Nothing
                    (Just n, Just m) -> Just (quot n m)
                    (_,_) -> Nothing
                    
sumaMaybe :: Maybe Int -> Maybe Int -> Maybe Int
sumaMaybe e1 e2 = case (e1, e2) of
            (Just n, Just m) -> Just ( n + m )
            (_,_) -> Nothing