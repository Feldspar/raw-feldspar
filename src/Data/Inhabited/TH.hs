module Data.Inhabited.TH where



import Language.Haskell.TH

import Language.Syntactic.TH


inhabitedTupleInstances :: Int -> DecsQ
inhabitedTupleInstances n = return
    [ instD
        [AppT (ConT (mkName "Inhabited")) (VarT a) | a <- take w varSupply]
        ( AppT
            (ConT (mkName "Inhabited"))
            (foldl AppT (TupleT w) (map VarT $ take w varSupply))
        )
        [ FunD
            (mkName "example")
            [ Clause
                []
                (NormalB $ TupE $ map VarE $ replicate w (mkName "example"))
                []
            ]
        ]
      | w <- [2..n]
    ]
{-

inhabitedTupleInstances :: Int -> DecsQ
inhabitedTupleInstances n = return
    [ InstanceD
        [AppT (ConT (mkName "Inhabited")) (VarT a) | a <- take w varSupply]
        ( AppT
            (ConT (mkName "Inhabited"))
            (foldl AppT (TupleT w) (map VarT $ take w varSupply))
        )
        [ FunD
            (mkName "example")
            [ Clause
                []
                (NormalB $ TupE $ map VarE $ replicate w (mkName "example"))
                []
            ]
        ]
      | w <- [2..n]
    ]
-}
