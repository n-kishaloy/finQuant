{-# LANGUAGE DeriveAnyClass #-}

-- | Module      : Finance.Statements
-- Description : Implement __Statements__ modules for the __finz__ library
-- Copyright   : (c) 2024 Kishaloy Neogi
-- License     : MIT
-- Maintainer  : Kishaloy Neogi
-- Email       : nkishaloy@yahoo.com
--
-- The module implements the Statements module which defines all the items that
-- go in preparing Account Statements, with definition of items that go in Balance
-- Sheet, Income Statements and Cash Flow Statements.
--
-- The module also implements the individual items that go in each of these
-- statements like Cash, Income, Tax etc.
--
-- Analysis of all these Statements as well as way to convert to and from JSON
-- is also implemented.
--
-- You may see the github repository at <https://github.com/n-kishaloy/finQuant>
module Finance.Statements
  ( FinType (..),
    BsType (..),
    BsMap,
    PlType (..),
    PlMap,
    CfType (..),
    CfMap,
    debitType,
    BalanceSheetEntry (..),
    transact,
  )
where

import Data.Approx ((=~))
import Data.HashMap.Strict qualified as Hm
import Data.HashSet qualified as Hs
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Time (Day, defaultTimeLocale, fromGregorian, parseTimeM, toGregorian)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Records (HasField (..))

calcNode :: (Hashable a) => Hm.HashMap a Double -> [a] -> [a] -> Double
calcNode h y0 y1 =
  let adder q = foldl' (\t w -> t + Hm.lookupDefault 0.0 w q) 0.0
   in adder h y0 - adder h y1

finMapAdd :: (Hashable a) => Double -> a -> Hm.HashMap a Double -> Hm.HashMap a Double
finMapAdd y z h = if y =~ 0.0 then Hm.delete z h else Hm.insert z y h

class (Show a, Eq a, Hashable a, Ord a, Enum a) => FinType a where
  -- | @calcElem x h = Calculate and set the derived items in statements@
  --   The calulated elements includes Current Assets, Assets, Equity,
  calcElem :: Hm.HashMap a Double -> Hm.HashMap a Double
  calcElem p = foldl' (\h (z, (s, t)) -> finMapAdd (calcNode h s t) z h) p calcComb

  calcComb :: [(a, ([a], [a]))]

  calcSet :: Hs.HashSet a
  calcSet = Hs.fromList $ fst <$> calcComb

  calcVec :: V.Vector a
  calcVec = V.fromList $ Hs.toList calcSet

  isCalc :: a -> Bool
  isCalc = flip Hs.member calcSet
  {-# INLINE isCalc #-}

  checkCalc :: a -> b -> b
  checkCalc t x = if isCalc t then error ("Calc item : " ++ show t) else x
  {-# INLINE checkCalc #-}

  sanitizeCalc :: [(a, Double)] -> [(a, Double)]
  sanitizeCalc = ((\x -> checkCalc (fst x) x) <$>)

  removeCalc :: [(a, Double)] -> [(a, Double)]
  removeCalc = filter (not . isCalc . fst)

  setMap :: Maybe (Hm.HashMap a Double) -> Maybe (Hm.HashMap a Double)
  setMap = (calcElem <$>)

  scaleMap :: Double -> Hm.HashMap a Double -> Hm.HashMap a Double
  scaleMap r x = Hm.fromList $ (\(k, v) -> (k, v * r)) <$> Hm.toList x

  clean :: Hm.HashMap a Double -> Hm.HashMap a Double
  clean = Hm.filter (=~ 0.0)

  commonSize :: Double -> Maybe (Hm.HashMap a Double) -> Maybe (Hm.HashMap a Double)
  commonSize xdiv p = p >>= \x -> return $ (/ xdiv) <$> x

data BsType
  = Cash
  | CurrentReceivables
  | CurrentLoans
  | CurrentAdvances
  | OtherCurrentAssets
  | CurrentInvestments
  | Inventories
  | RawMaterials
  | WorkInProgress
  | FinishedGoods
  | CurrentAssets
  | AccountReceivables
  | LongTermLoanAssets
  | LongTermAdvances
  | LongTermInvestments
  | OtherLongTermAssets
  | PlantPropertyEquipment
  | AccumulatedDepreciation
  | NetPlantPropertyEquipment
  | LeasingRentalAssets
  | AccumulatedAmortizationLease
  | NetLeaseRentalAssets
  | Goodwill
  | CapitalWip
  | OtherTangibleAssets
  | IntangibleAssets
  | IntangibleAssetsDevelopment
  | AccumulatedAmortization
  | NetIntangibleAssets
  | LongTermAssets
  | Assets
  | CurrentPayables
  | CurrentBorrowings
  | CurrentNotesPayable
  | OtherCurrentLiabilities
  | InterestPayable
  | CurrentProvisions
  | CurrentTaxPayables
  | LiabilitiesSaleAssets
  | CurrentLeasesLiability
  | CurrentLiabilities
  | AccountPayables
  | LongTermBorrowings
  | BondsPayable
  | DeferredTaxLiabilities
  | LongTermLeasesLiability
  | DeferredCompensation
  | DeferredRevenues
  | CustomerDeposits
  | OtherLongTermLiabilities
  | PensionProvision
  | TaxProvision
  | LongTermProvision
  | LongTermLiabilities
  | Liabilities
  | CommonStock
  | PreferredStock
  | PdInCapAbovePar
  | PdInCapTreasuryStock
  | RevaluationReserves
  | Reserves
  | RetainedEarnings
  | AccumulatedOCI
  | MinorityInterests
  | Equity
  | BalanceSheetCheck
  deriving (Eq, Show, Ord, Generic, Enum, Bounded, Hashable)

data PlType
  = OperatingRevenue
  | NonOperatingRevenue
  | ExciseStaxLevy
  | OtherIncome
  | Revenue
  | CostMaterial
  | DirectExpenses
  | COGS
  | Salaries
  | AdministrativeExpenses
  | ResearchNDevelopment
  | OtherOverheads
  | OtherOperativeExpenses
  | OtherExpenses
  | ExceptionalItems
  | GrossProfit
  | EBITDA
  | Depreciation
  | TaxDepreciation
  | AssetImpairment
  | LossDivestitures
  | Amortization
  | EBITX
  | InterestRevenue
  | InterestExpense
  | CostDebt
  | OtherFinancialRevenue
  | EBTX
  | ExtraordinaryItems
  | PriorYears
  | EBT
  | TaxesCurrent
  | TaxesDeferred
  | EAT
  | NetIncomeDiscontinuedOps
  | NetIncome
  | Dividends
  | ContributionRetainedEarnings
  | GainsLossesForex
  | GainsLossesActurial
  | GrossSalesPPE
  | GrossSalesLeaseRentalAssets
  | GrossSalesIntangibleAssets
  | AccAmortSalesPPE
  | AccAmortSalesLeaseRental
  | AccAmortSalesIntangible
  | SalesAmountPPE
  | SalesAmountLeaseRentalAssets
  | SalesAmountIntangibleAssets
  | GainsLossesSales
  | FvChangeAvlSale
  | OtherDeferredTaxes
  | OtherComprehensiveIncome
  | TotalComprehensiveIncome
  deriving (Eq, Show, Ord, Generic, Enum, Bounded, Hashable)

data CfType
  = ChangeCurrentAssets
  | ChangeLongTermAssets
  | ChangeCurrentLiabilities
  | ChangeLongTermLiabilities
  | ChangeProvisions
  | ChangeRetainedEarnings
  | AdjustmentsRetainedEarnings
  | ChangeAccumulatedOci
  | OtherCashFlowOperations
  | CashFlowOperations
  | ChangePPE
  | ChangeReserves
  | AdjustmentsSalesAssets
  | InvestmentsCapDevp
  | InvestmentsLoans
  | ChangeEquityAssets
  | ChangeInvestments
  | OtherCashFlowInvestments
  | CashFlowInvestments
  | StockSalesAndPurchase
  | ChangeDebt
  | CashFlowInterests
  | CashFlowDividends
  | DonorContribution
  | OtherCashFlowFinancing
  | CashFlowFinancing
  | NetCashFlow
  | FreeCashFlowFirm
  | CashFlowTaxShield
  | FreeCashFlowEquity
  | CashFlowDebt
  deriving (Eq, Show, Ord, Generic, Enum, Bounded, Hashable)

data FinOthersTyp
  = CorporateTaxRate
  | GrossProfitTaxRate
  | RevenueTaxRate
  | CurrentRatio
  | AcidRatio
  | DaysOfInventory
  | InventoryTurnoverRatio
  deriving (Eq, Show, Ord, Generic, Enum, Bounded, Hashable)

instance FinType BsType where
  calcComb :: [(BsType, ([BsType], [BsType]))]
  calcComb =
    [ ( Inventories,
        ([RawMaterials, WorkInProgress, FinishedGoods], [])
      ),
      ( CurrentAssets,
        ( [ Cash,
            CurrentReceivables,
            CurrentLoans,
            CurrentAdvances,
            OtherCurrentAssets,
            CurrentInvestments,
            Inventories
          ],
          []
        )
      ),
      ( NetPlantPropertyEquipment,
        ([PlantPropertyEquipment], [AccumulatedDepreciation])
      ),
      ( NetLeaseRentalAssets,
        ( [LeasingRentalAssets],
          [AccumulatedAmortizationLease]
        )
      ),
      ( NetIntangibleAssets,
        ( [IntangibleAssets, IntangibleAssetsDevelopment],
          [AccumulatedAmortization]
        )
      ),
      ( LongTermAssets,
        ( [ AccountReceivables,
            LongTermLoanAssets,
            LongTermAdvances,
            LongTermInvestments,
            OtherLongTermAssets,
            NetPlantPropertyEquipment,
            NetLeaseRentalAssets,
            Goodwill,
            CapitalWip,
            OtherTangibleAssets,
            NetIntangibleAssets
          ],
          []
        )
      ),
      (Assets, ([CurrentAssets, LongTermAssets], [])),
      ( CurrentLiabilities,
        ( [ CurrentPayables,
            CurrentBorrowings,
            CurrentNotesPayable,
            OtherCurrentLiabilities,
            InterestPayable,
            CurrentProvisions,
            CurrentTaxPayables,
            LiabilitiesSaleAssets,
            CurrentLeasesLiability
          ],
          []
        )
      ),
      ( LongTermLiabilities,
        ( [ AccountPayables,
            LongTermBorrowings,
            BondsPayable,
            DeferredTaxLiabilities,
            LongTermLeasesLiability,
            DeferredCompensation,
            DeferredRevenues,
            CustomerDeposits,
            OtherLongTermLiabilities,
            PensionProvision,
            TaxProvision,
            LongTermProvision
          ],
          []
        )
      ),
      ( Liabilities,
        ([CurrentLiabilities, LongTermLiabilities], [])
      ),
      ( Equity,
        ( [ CommonStock,
            PreferredStock,
            PdInCapAbovePar,
            PdInCapTreasuryStock,
            RevaluationReserves,
            Reserves,
            RetainedEarnings,
            AccumulatedOCI,
            MinorityInterests
          ],
          []
        )
      ),
      (BalanceSheetCheck, ([Assets], [Liabilities, Equity]))
    ]

instance FinType PlType where
  calcComb :: [(PlType, ([PlType], [PlType]))]
  calcComb =
    [ ( Revenue,
        ( [OperatingRevenue, NonOperatingRevenue],
          [ExciseStaxLevy]
        )
      ),
      (COGS, ([CostMaterial, DirectExpenses], [])),
      (GrossProfit, ([Revenue], [COGS])),
      ( EBITDA,
        ( [GrossProfit, OtherIncome],
          [ Salaries,
            AdministrativeExpenses,
            ResearchNDevelopment,
            OtherOverheads,
            OtherOperativeExpenses,
            OtherExpenses,
            ExceptionalItems
          ]
        )
      ),
      ( EBITX,
        ( [EBITDA],
          [ Depreciation,
            AssetImpairment,
            LossDivestitures,
            Amortization
          ]
        )
      ),
      ( EBTX,
        ( [EBITX, InterestRevenue, OtherFinancialRevenue],
          [InterestExpense, CostDebt]
        )
      ),
      (EBT, ([EBTX], [ExtraordinaryItems, PriorYears])),
      (EAT, ([EBT], [TaxesCurrent, TaxesDeferred])),
      (NetIncome, ([EAT, NetIncomeDiscontinuedOps], [])),
      ( ContributionRetainedEarnings,
        ([NetIncome], [Dividends])
      ),
      ( GainsLossesSales,
        ( [ SalesAmountPPE,
            SalesAmountLeaseRentalAssets,
            SalesAmountIntangibleAssets,
            AccAmortSalesPPE,
            AccAmortSalesLeaseRental,
            AccAmortSalesIntangible
          ],
          [ GrossSalesPPE,
            GrossSalesLeaseRentalAssets,
            GrossSalesIntangibleAssets
          ]
        )
      ),
      ( OtherComprehensiveIncome,
        ( [ GainsLossesForex,
            GainsLossesActurial,
            GainsLossesSales,
            FvChangeAvlSale
          ],
          [OtherDeferredTaxes]
        )
      ),
      ( TotalComprehensiveIncome,
        ([NetIncome, OtherComprehensiveIncome], [])
      )
    ]

instance FinType CfType where
  calcComb :: [(CfType, ([CfType], [CfType]))]
  calcComb =
    [ ( CashFlowOperations,
        ( [ ChangeCurrentLiabilities,
            ChangeLongTermLiabilities,
            ChangeProvisions,
            ChangeRetainedEarnings,
            AdjustmentsRetainedEarnings,
            ChangeAccumulatedOci,
            OtherCashFlowOperations
          ],
          [ChangeCurrentAssets, ChangeLongTermAssets]
        )
      ),
      ( CashFlowInvestments,
        ( [ OtherCashFlowInvestments,
            AdjustmentsSalesAssets,
            ChangeReserves
          ],
          [ ChangePPE,
            InvestmentsCapDevp,
            InvestmentsLoans,
            ChangeEquityAssets,
            ChangeInvestments
          ]
        )
      ),
      ( CashFlowFinancing,
        ( [ StockSalesAndPurchase,
            ChangeDebt,
            DonorContribution,
            OtherCashFlowFinancing
          ],
          [CashFlowInterests, CashFlowDividends]
        )
      ),
      ( NetCashFlow,
        ( [CashFlowOperations, CashFlowInvestments, CashFlowFinancing],
          []
        )
      ),
      ( FreeCashFlowEquity,
        ( [ CashFlowOperations,
            ChangeDebt,
            ChangeReserves,
            AdjustmentsSalesAssets
          ],
          [CashFlowInterests, ChangePPE]
        )
      ),
      (CashFlowDebt, ([CashFlowInterests], [ChangeDebt])),
      ( FreeCashFlowFirm,
        ( [FreeCashFlowEquity, CashFlowDebt],
          [CashFlowTaxShield]
        )
      )
    ]

cashFlowBalanceSheet :: [(CfType, ([BsType], [BsType]))]
cashFlowBalanceSheet =
  [ ( ChangeCurrentAssets,
      ( [ CurrentReceivables,
          CurrentLoans,
          CurrentAdvances,
          OtherCurrentAssets,
          CurrentInvestments,
          RawMaterials,
          WorkInProgress,
          FinishedGoods
        ],
        []
      )
    ),
    ( ChangeLongTermAssets,
      ( [AccountReceivables, LongTermAdvances, CapitalWip],
        [ AccumulatedDepreciation,
          AccumulatedAmortizationLease,
          AccumulatedAmortization
        ]
      )
    ),
    ( ChangeCurrentLiabilities,
      ( [ CurrentPayables,
          CurrentBorrowings,
          CurrentNotesPayable,
          OtherCurrentLiabilities,
          InterestPayable,
          CurrentProvisions,
          CurrentTaxPayables,
          LiabilitiesSaleAssets,
          CurrentLeasesLiability
        ],
        []
      )
    ),
    ( ChangeLongTermLiabilities,
      ( [ AccountPayables,
          DeferredTaxLiabilities,
          DeferredCompensation,
          DeferredRevenues,
          CustomerDeposits,
          OtherLongTermLiabilities
        ],
        []
      )
    ),
    ( ChangeProvisions,
      ( [PensionProvision, TaxProvision, LongTermProvision],
        []
      )
    ),
    (ChangeRetainedEarnings, ([RetainedEarnings], [])),
    (ChangeAccumulatedOci, ([AccumulatedOCI], [])),
    ( ChangePPE,
      ([PlantPropertyEquipment, LeasingRentalAssets], [])
    ),
    ( ChangeReserves,
      ([RevaluationReserves, Reserves], [])
    ),
    ( InvestmentsCapDevp,
      ([IntangibleAssetsDevelopment], [])
    ),
    (InvestmentsLoans, ([LongTermLoanAssets], [])),
    (ChangeEquityAssets, ([IntangibleAssets], [])),
    ( ChangeInvestments,
      ([LongTermInvestments, Goodwill], [])
    ),
    ( OtherCashFlowInvestments,
      ([], [OtherLongTermAssets, OtherTangibleAssets])
    ),
    ( StockSalesAndPurchase,
      ( [ CommonStock,
          PreferredStock,
          PdInCapAbovePar,
          PdInCapTreasuryStock
        ],
        []
      )
    ),
    ( ChangeDebt,
      ( [LongTermBorrowings, BondsPayable, LongTermLeasesLiability],
        []
      )
    ),
    (OtherCashFlowFinancing, ([MinorityInterests], []))
  ]

cashFlowProfitLoss :: [(CfType, ([PlType], [PlType]))]
cashFlowProfitLoss =
  [ (CashFlowInterests, ([InterestExpense, CostDebt], [])),
    (CashFlowDividends, ([Dividends], [])),
    ( AdjustmentsRetainedEarnings,
      ( [ InterestExpense,
          CostDebt,
          Dividends,
          AccAmortSalesPPE,
          AccAmortSalesLeaseRental,
          AccAmortSalesIntangible
        ],
        [GainsLossesSales]
      )
    ),
    ( AdjustmentsSalesAssets,
      ( [GainsLossesSales],
        [ AccAmortSalesPPE,
          AccAmortSalesLeaseRental,
          AccAmortSalesIntangible
        ]
      )
    )
  ]

data BalanceSheetEntry
  = AssetEntry
  | AssetContra
  | LiabilityEntry
  | LiabilityContra
  | EquityEntry
  | EquityContra
  deriving (Eq, Show, Ord, Generic, Enum, Bounded, Hashable)

balanceSheetMap :: Hm.HashMap BsType ([BsType], [BsType])
balanceSheetMap = Hm.fromList calcComb

debitMapping :: BsType -> BalanceSheetEntry -> BalanceSheetEntry -> Hm.HashMap BsType BalanceSheetEntry -> Hm.HashMap BsType BalanceSheetEntry
debitMapping ct calPos calNeg dm =
  let (a, b) = balanceSheetMap Hm.! ct
      adder :: [BsType] -> BalanceSheetEntry -> BalanceSheetEntry -> Hm.HashMap BsType BalanceSheetEntry -> Hm.HashMap BsType BalanceSheetEntry
      adder [] _ _ xm = xm
      adder (x : v) cP cN xm =
        if isCalc x
          then adder v cP cN $ debitMapping x cP cN xm
          else adder v cP cN $ Hm.insert x cP xm
   in adder b calNeg calPos $ adder a calPos calNeg dm

debitType :: Hm.HashMap BsType BalanceSheetEntry
debitType =
  debitMapping Equity EquityEntry EquityContra $
    debitMapping Liabilities LiabilityEntry LiabilityContra $
      debitMapping Assets AssetEntry AssetContra Hm.empty

type BsMap = Hm.HashMap BsType Double

type PlMap = Hm.HashMap PlType Double

type CfMap = Hm.HashMap CfType Double

type Transaction = (BsType, BsType, Double)

debit :: BsMap -> BsType -> Double -> BsMap
debit hm tp val =
  if isCalc tp
    then error "Calculated value"
    else
      let pos = Hm.insertWith (+) tp val hm
          neg = Hm.insertWith (+) tp (-val) hm
       in case debitType Hm.! tp of
            AssetEntry -> pos
            LiabilityContra -> pos
            EquityContra -> pos
            _ -> neg

credit :: BsMap -> BsType -> Double -> BsMap
credit hm tp val = debit hm tp (-val)

transact :: BsMap -> Transaction -> BsMap
transact hm (deb, crd, val) = credit (debit hm deb val) crd val

transactList :: BsMap -> [Transaction] -> BsMap
transactList = foldl' transact
