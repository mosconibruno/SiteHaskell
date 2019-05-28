{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Database.Persist.Postgresql

-- <$>, <*> CAP 7.4 do LIVRO!
formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap $ (,)
    <$> (Usuario 
        <$> areq emailField "E-mail:" Nothing
        <*> areq passwordField "Senha: " Nothing)
    <*> areq passwordField "Confirmacao: " Nothing  

getUsuarioR :: Handler Html
getUsuarioR = do 
    (widget,enctype) <- generateFormPost formUsuario
    msg <- getMessage
    defaultLayout $ do
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
            <form action=@{UsuarioR} method=post>
                ^{widget}
                <input type="submit" value="cadastrar">
        |]

postUsuarioR :: Handler Html
postUsuarioR = do
    ((res,_),_) <- runFormPost formUsuario
    case res of
        FormSuccess (usuario,confirmacao) -> do
            if (usuarioSenha usuario) == confirmacao then do 
                runDB $ insert usuario
                redirect HomeR
            else do
                setMessage [shamlet|
                    <h1>
                        Usuario e senha n batem
                |]
                redirect UsuarioR
        _ -> redirect HomeR
    