{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Database.Persist.Postgresql

-- SHAKESPEAREAN TEMPLATES
-- whamlet => html
-- julius => javascript
-- lucius|cassius => css
getHomeR :: Handler Html
getHomeR = do
    sess <- lookupSession "_ID"
    defaultLayout $ do
        -- pasta: static/css/bootstrap.css
        -- / e . sao trocados por _
        addStylesheet $ StaticR css_bootstrap_css
        toWidgetHead [julius|
            function teste(){
                alert("BORA JOGAR!");
            }
        |]
        toWidget [lucius|
            h1 {
                color : green;
            }
        |]
        [whamlet|
            $maybe sessao <- sess    
                Ola #{sessao}
            $nothing    
                <h1 class>
                    SIGA
            <ul>
                <li>
                    <a href=@{AlunoR}>
                        Cadastro de aluno
                <li>
                    <a href=@{TodosAlunosR}>
                        Listar aluno
                <li>
                    <a href=@{ProfR}>
                        Cadastro de Professor
                
                <li>
                    <a href=@{UsuarioR}>
                        Cadastro de Usuario
                $maybe _ <- sess 
                    <li>
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Sair">
                $nothing
                    <li>
                        <a href=@{LoginR}>
                            Entrar
               <img src=@{StaticR imgs_champions1_jpg}>
                        
            <button onclick="teste()" class="btn btn-primary">
                OK
            
        |]

getPage1R :: Handler Html
getPage1R = do 
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 1
            
            <a href=@{HomeR}>
                Voltar
        |]

getPage2R :: Handler Html
getPage2R = do 
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 2
        |]

getPage3R :: Handler Html
getPage3R = do 
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 3
        |]