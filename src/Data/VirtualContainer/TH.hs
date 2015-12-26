module Data.VirtualContainer.TH where



import Language.Haskell.TH

import Language.Syntactic.TH



-- | Construct a definition of a virtual container type with tuples up to the
-- given width:
--
-- > data Virtual p con a
-- >   where
-- >     -- An actual container
-- >     Actual :: p a => con a -> Virtual p con a
-- >     -- A pair of virtual containers
-- >     VTup2  :: Virtual p con a -> Virtual p con b
-- >            -> Virtual p con (a,b)
-- >     ...
mkVirtualType :: Int -> DecsQ
mkVirtualType n = return
    [ DataD
        []
        (mkName "Virtual")
        (map (PlainTV . mkName) ["p","con","res"])
        ( concat
            [ [ ForallC
                  []
                  [AppT (VarT (mkName "p")) (VarT (mkName "res"))]
                  (NormalC (mkName "Actual") [(NotStrict, AppT (VarT (mkName "con")) (VarT (mkName "res")))])
              ]
            , [ ForallC
                  (map PlainTV $ take w varSupply)
                  [foldl1 AppT [EqualityT, VarT (mkName "res"), foldl AppT (TupleT w) (map VarT $ take w varSupply)]]
                  ( NormalC
                      (mkName ("VTup" ++ show w))
                      [ ( NotStrict
                        , foldl1 AppT [ConT (mkName "Virtual"), VarT (mkName "p"), VarT (mkName "con"), VarT a]
                        )
                        | a <- take w varSupply
                      ]
                  )
                | w <- [2..n]
              ]
            ]
        )
        []
    ]

-- | Construct selector functions for virtual containers:
--
-- > vsel1 :: Select1 tup => Virtual p c tup -> Virtual p c (Sel1 tup)
-- > vsel1 (VTup2 a _)   = a
-- > vsel1 (VTup3 a _ _) = a
-- > ...
-- >
-- > vsel2 :: Select2 tup => Virtual p c tup -> Virtual p c (Sel2 tup)
-- > vsel2 (VTup2 _ b)   = b
-- > vsel2 (VTup3 _ b _) = b
-- > ...
-- >
-- > ...
mkVSel :: Int -> DecsQ
mkVSel n = return $ concatMap mkVSelS [1..n]
  where
    mkVSelS s =
      [ SigD
          (mkName ("vsel" ++ show s))
          ( ForallT
              (map (PlainTV . mkName) ["p","c","tup"])
              [AppT (ConT (mkName ("Select" ++ show s))) (VarT (mkName "tup"))]
              (foldl1 AppT
                  [ ArrowT
                  , foldl1 AppT
                      [ ConT (mkName "Virtual")
                      , VarT (mkName "p")
                      , VarT (mkName "c")
                      , VarT (mkName "tup")
                      ]
                  , foldl1 AppT
                      [ ConT (mkName "Virtual")
                      , VarT (mkName "p")
                      , VarT (mkName "c")
                      , AppT (ConT (mkName ("Sel" ++ show s))) (VarT (mkName "tup"))
                      ]
                  ]
              )
          )
      , FunD (mkName ("vsel" ++ show s))
          [ Clause
              [ConP (mkName ("VTup" ++ show w)) $ map VarP $ take w varSupply]
              (NormalB $ VarE (varSupply !! (s-1)))
              []
            | w <- [max 2 s .. n]
          ]
      ]

applyN
    :: Int           -- ^ Arity
    -> Exp           -- ^ N-ary function
    -> (Int -> Exp)  -- ^ Argument constructor
    -> ExpQ
applyN n fun mkArg = return $ foldl AppE fun $ map mkArg [1..n]

virtualCases
    :: Int                    -- ^ Max tuple width
    -> Exp                    -- ^ Virtual structure to examine
    -> (Int -> [Exp] -> Exp)  -- ^ Result (arg #1 is the tuple width; arg #2 is the sub-structures)
    -> Q Exp
virtualCases n tup body = return $ CaseE tup
    [ Match lhs (NormalB rhs) []
      | w <- [2..n]
      , let lhs = ConP (mkName ("VTup" ++ show w)) $ map VarP $ take w varSupply
      , let rhs = body w $ map VarE $ take w varSupply
    ]

virtualCases2
    :: Int                            -- ^ Max tuple width
    -> (Exp,Exp)                      -- ^ Virtual structures to examine
    -> (Int -> ([Exp],[Exp]) -> Exp)  -- ^ Result (arg #1 is the tuple width; arg #2 is the sub-structures)
    -> Maybe Exp                      -- ^ Fall through case
    -> Q Exp
virtualCases2 n (tup1,tup2) body fall = return $ CaseE (TupE [tup1,tup2]) $
    [ Match (TupP [lhs1,lhs2]) (NormalB rhs) []
      | w <- [2..n]
      , let vars1 = take w varSupply
      , let vars2 = take w $ drop w varSupply
      , let lhs1 = ConP (mkName ("VTup" ++ show w)) $ map VarP vars1
      , let lhs2 = ConP (mkName ("VTup" ++ show w)) $ map VarP vars2
      , let rhs = body w (map VarE vars1, map VarE vars2)
    ] ++ fallThrough
  where
    fallThrough = case fall of
        Just e -> [Match WildP (NormalB e) []]
        _ -> []

