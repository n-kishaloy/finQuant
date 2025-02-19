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
    FinOthersTyp (..),
    FinOthMap,
    debitType,
    BalanceSheetEntry (..),
    transact,
    transactList,
    calcCashFlow,
    Param (..),
    BalanceSheet (..),
    ProfitLoss (..),
    CashFlow (..),
    FinOthers (..),
    FinancialReport (..),
  )
where

import Data.Approx ((=~))
import Data.Function ((&))
import Data.HashMap.Strict qualified as Hm
import Data.HashSet qualified as Hs
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Map.Strict qualified as Bm
import Data.Time (Day)
import Data.Vector qualified as V
import Finance as F
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

  infixl 9 !~
  (!~) :: Hm.HashMap a Double -> a -> Double
  (!~) h k = Hm.lookupDefault 0.0 k h

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

cfBL :: [(CfType, ([BsType], [BsType]))]
cfBL =
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

cfPL :: [(CfType, ([PlType], [PlType]))]
cfPL =
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

type FinOthMap = Hm.HashMap FinOthersTyp Double

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

deprTaxAdjust :: PlMap -> Double
deprTaxAdjust pl = case pl Hm.!? TaxDepreciation of
  Just x -> pl !~ Depreciation - x
  _ -> 0.0

-- | Derives the non-calc items of the Cash Flow statement from
--  the beginning and ending Balance Sheets given as BsMap and Profit Loss statement
--  given as PlMap.

-- | Note that you have to still run the calc_elements function to compute the full
--  Cash Flow statement. This function just provides an easy way to derive them of
--  a Balance Sheet.
calcCashFlow :: BsMap -> BsMap -> PlMap -> Double -> Double -> Double -> CfMap
calcCashFlow b0 b1 pl corpTax gpTax revTax =
  let cf0 =
        foldl' (\h (z, (s, t)) -> 
          finMapAdd (calcNode pl s t) z h) Hm.empty cfPL
          & \cf -> foldl' (\h (z, (s, t)) -> 
            finMapAdd (calcNode b1 s t - calcNode b0 s t) z h) cf cfBL
      cfInt = cf0 !~ CashFlowInterests
      ebitTx = corpTax * (pl !~ EBT + cfInt + deprTaxAdjust pl)
      grTx = gpTax * pl !~ GrossProfit
      revTx = revTax * pl !~ Revenue
   in Hm.insert
        CashFlowTaxShield
        (min (corpTax * cfInt) (max 0.0 (ebitTx + grTx - revTx)))
        cf0

data Param = Param
  { unlevered :: Double,
    shieldTax :: Double,
    equity :: Double,
    debt :: Double,
    valuation :: Double
  }
  deriving (Show)

data BalanceSheet = BalanceSheet
  { date :: Day,
    value :: BsMap
  }
  deriving (Show)

data ProfitLoss = ProfitLoss
  { period :: F.Period,
    value :: PlMap
  }
  deriving (Show)

data CashFlow = CashFlow
  { period :: F.Period,
    value :: CfMap
  }
  deriving (Show)

data FinOthers = FinOthers
  { period :: F.Period,
    value :: FinOthMap
  }
  deriving (Show)

data FinancialReport = FinancialReport
  { period :: F.Period,
    balanceSheetBegin :: Maybe BsMap,
    balanceSheetEnd :: Maybe BsMap,
    profitLoss :: Maybe PlMap,
    cashFlow :: Maybe CfMap,
    others :: Maybe FinOthMap
  }

instance HasField "bal_Sheet_Begin" FinancialReport (Maybe BalanceSheet) where
  getField :: FinancialReport -> Maybe BalanceSheet
  getField fr =
    fr.balanceSheetBegin >>= \bl ->
      return BalanceSheet {date = fst fr.period, value = bl}

instance HasField "bal_Sheet_End" FinancialReport (Maybe BalanceSheet) where
  getField :: FinancialReport -> Maybe BalanceSheet
  getField fr =
    fr.balanceSheetEnd >>= \bl ->
      return BalanceSheet {date = snd fr.period, value = bl}

instance HasField "profit_loss_Stat" FinancialReport (Maybe ProfitLoss) where
  getField :: FinancialReport -> Maybe ProfitLoss
  getField fr =
    fr.profitLoss >>= \pl -> return ProfitLoss {period = fr.period, value = pl}

instance HasField "cash_flow_Stat" FinancialReport (Maybe CashFlow) where
  getField :: FinancialReport -> Maybe CashFlow
  getField fr =
    fr.cashFlow >>= \cf -> return CashFlow {period = fr.period, value = cf}

instance HasField "fin_others" FinancialReport (Maybe FinOthers) where
  getField :: FinancialReport -> Maybe FinOthers
  getField fr =
    fr.others >>= \fo -> return FinOthers {period = fr.period, value = fo}

instance HasField "eps" FinancialReport Double where
  getField :: FinancialReport -> Double
  getField _fr = error "Not implemented"

instance HasField "diluted_eps" FinancialReport (Double -> Double -> Double -> Double -> Double -> Double) where
  getField :: FinancialReport -> Double -> Double -> Double -> Double -> Double -> Double
  getField _fr _earn _pref_div _shares _share_price _options =
    error "Not implemented"

instance HasField "currentRatio" FinancialReport Double where
  getField :: FinancialReport -> Double
  getField _fr = error "Not implemented"
