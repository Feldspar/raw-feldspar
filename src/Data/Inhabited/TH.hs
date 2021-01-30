{-# LANGUAGE CPP #-}

module Data.Inhabited.TH where



import Language.Haskell.TH

import Language.Syntactic.TH



mkTupE :: [Exp] -> Exp
#if __GLASGOW_HASKELL__ >= 810
mkTupE = TupE . map Just
#else
mkTupE = TupE
#endif

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
                (NormalB $ mkTupE $ map VarE $ replicate w (mkName "example"))
                []
            ]
        ]
      | w <- [2..n]
    ]

