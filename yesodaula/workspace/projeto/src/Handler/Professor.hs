{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Professor where

import Import
import Database.Persist.Postgresql

-- <$>, <*> CAP 7.4 do LIVRO!
formProf :: Form Professor
formProf = renderBootstrap $ Professor 
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Titulo: " Nothing
    <*> areq intField  "Carga: " Nothing

-- ^ coloca outro html, no caso, os inputs
getProfR :: Handler Html
getProfR = do 
    (widget,enctype) <- generateFormPost formProf
    defaultLayout $ do
        [whamlet|
            <form action=@{ProfR} method=post>
                ^{widget}
                <input type="submit" value="cadastrar">
        |]

postProfR :: Handler Html
postProfR = do
    ((res,_),_) <- runFormPost formProf
    case res of
        FormSuccess prof -> do
            runDB $ insert prof
            redirect ProfR
        _ -> redirect HomeR
    
-- SELECT * FROM Professor
getTodosProfR :: Handler Html
getTodosProfR = do 
    profs <- runDB $ selectList [] [Asc ProfessorNome]
    defaultLayout $(whamletFile "templates/professor.hamlet")
