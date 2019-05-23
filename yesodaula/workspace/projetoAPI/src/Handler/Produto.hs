{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Produto where

import Import
import Network.HTTP.Types
import Database.Persist.Postgresql

postProdutoR :: Handler TypedContent
postProdutoR = do 
    produto <- requireJsonBody :: Handler Produto
    pid <- runDB $ insert produto 
    sendStatusJSON created201 (object ["resp" .= pid])

getProdutosR :: Handler TypedContent
getProdutosR = do 
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    sendStatusJSON ok200 (object ["resp" .= produtos])