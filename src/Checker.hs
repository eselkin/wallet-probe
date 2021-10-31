{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Checker(endpoints, CheckerSchema) where

import           Control.Lens           (view)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import qualified Data.Map               as Map hiding (empty)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Issuer      as I hiding (endpoints)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Builtins.Class
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.Contract    (ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..) )
import           Wallet.Emulator.Wallet (Wallet, walletAddress, walletPubKey)

data FindParam = FindParam
    { nftTokenName :: String
    , issuerWallet :: Wallet
    , holderWallet :: Wallet
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data FindAllNFTParam = FindAllNFTParam 
    { hW :: Wallet
    , checkMethod :: String
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type CheckerSchema =  Endpoint "findNFT" FindParam 
                  .\/ Endpoint "findAllNFT" FindAllNFTParam

findNFT :: forall w s e. AsContractError e => FindParam -> Contract w s e ()
findNFT param = do
    pkh <- pubKeyHash <$> ownPubKey
    logInfo @String $ "INSIDE"
    let w  = holderWallet param
        iw = issuerWallet param
        tn = TokenName $ stringToBuiltinByteString (nftTokenName param)
        nftAssetClass = AssetClass (I.issuerCS $ (pubKeyHash . walletPubKey) iw, tn)
    os  <- map snd . Map.toList <$> utxosAt (walletAddress w)
    let nftVal = mconcat [view ciTxOutValue o | o <- os, nf (view ciTxOutValue o) nftAssetClass]
        qty = assetClassValueOf (nftVal) nftAssetClass
    logInfo @String $ "Searching for NFT " <> (show nftAssetClass)
    logInfo @String $ "Find NFT result - " ++ (if qty == 0 then "NOT FOUND" else "FOUND")
    where
      nf val ac = assetClassValueOf val ac == 1

findAllNFT :: forall w s e. AsContractError e => FindAllNFTParam -> Contract w s e ()
findAllNFT allParam = do
    logInfo @String $ "Checking for all NFTs"
    let h = hW allParam
        opType = checkMethod allParam
    os  <- map snd . Map.toList <$> utxosAt (walletAddress h)
    let nftVal = mconcat [ flattenValue $ view ciTxOutValue o | o <- os, fv <- flattenValue $ view ciTxOutValue o, operation opType fv]
    logInfo @String $ "Searching for all NFT " <> (show nftVal)
    where 
        thirdIsSingular :: (CurrencySymbol, b, Integer) -> Bool
        thirdIsSingular (_,_,x) = x == 1
        nonAdaToken :: (CurrencySymbol, b, Integer) -> Bool
        nonAdaToken (x,y,z) = x /= ""
        operation :: String -> (CurrencySymbol, b, Integer) -> Bool
        operation "third" = thirdIsSingular
        operation       _ = nonAdaToken

findNFT' :: Promise () CheckerSchema Text ()
findNFT' = endpoint @"findNFT" findNFT

findAllNFT' :: Promise () CheckerSchema Text ()
findAllNFT' = endpoint @"findAllNFT" findAllNFT

endpoints :: AsContractError e => Contract () CheckerSchema Text e
endpoints = do
    logInfo @String "Waiting for request."
    selectList [findNFT', findAllNFT'] >>  endpoints

mkSchemaDefinitions ''CheckerSchema
mkKnownCurrencies []
