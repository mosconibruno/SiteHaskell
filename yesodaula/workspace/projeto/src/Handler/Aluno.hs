{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Aluno where

import Import
import Database.Persist.Postgresql

-- <$>, <*> CAP 7.4 do LIVRO!
formAluno :: Maybe Aluno -> Form Aluno
formAluno mAluno = renderBootstrap $ Aluno 
    <$> areq textField "Nome: " (fmap alunoNome mAluno)
    <*> areq textField "RA: " (fmap alunoRa mAluno)
    <*> areq intField  "Idade: " (fmap alunoIdade mAluno)

-- ^ coloca outro html, no caso, os inputs
getAlunoR :: Handler Html
getAlunoR = do 
    (widget,enctype) <- generateFormPost (formAluno Nothing)
    defaultLayout $ do
        [whamlet|
            <form action=@{AlunoR} method=post>
                ^{widget}
                <input type="submit" value="cadastrar">
        |]

postAlunoR :: Handler Html
postAlunoR = do
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formAluno Nothing)
    case res of
        FormSuccess aluno -> do
            runDB $ insert aluno
            redirect AlunoR
        _ -> redirect HomeR
    
-- SELECT * FROM Aluno
getTodosAlunosR :: Handler Html
getTodosAlunosR = do 
    alunos <- runDB $ selectList [] [Asc AlunoNome]
    defaultLayout $(whamletFile "templates/aluno.hamlet")

getAlunoPerfilR :: AlunoId -> Handler Html
getAlunoPerfilR aluid = do 
    aluno <- runDB $ get404 aluid
    defaultLayout $ do 
        [whamlet|
            <h1>
                Aluno #{alunoNome aluno}
            <div>
                RA: #{alunoRa aluno}
            <div>
                Idade: #{alunoIdade aluno}
        |]

postAlunoApagarR :: AlunoId -> Handler Html
postAlunoApagarR aluid = do
    runDB $ get404 aluid
    runDB $ delete aluid
    redirect TodosAlunosR

getAlunoAlteraR :: AlunoId -> Handler Html
getAlunoAlteraR aluid = do
    aluno <- runDB $ get404 aluid
    (widget,enctype) <- generateFormPost (formAluno $ Just aluno)
    defaultLayout $ do
        [whamlet|
            <form action=@{AlunoAlteraR aluid} method=post>
                ^{widget}
                <input type="submit" value="atualizar">
        |]

postAlunoAlteraR :: AlunoId -> Handler Html
postAlunoAlteraR aluid = do
    aluno <- runDB $ get404 aluid
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formAluno $ Just aluno) 
    case res of
        FormSuccess alunoNovo -> do
            runDB $ replace aluid alunoNovo
            redirect TodosAlunosR
        _ -> redirect HomeR