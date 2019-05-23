{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Disciplina where

import Import

formDisc :: ProfessorId -> Form Disciplina
formDisc profid = renderBootstrap $ Disciplina
    <$> areq textField "Nome: " Nothing
    <*> areq (selectField aluLista) "Aluno" Nothing
    <*> pure profid
    <*> areq dayField "Começo" Nothing

aluLista = do
       entidades <- runDB $ selectList [] [Asc AlunoNome] 
       optionsPairs $ fmap (\ent -> (alunoNome $ entityVal ent, entityKey ent)) entidades



getDiscR :: ProfessorId -> Handler Html
getDiscR profid = do
    runDB $ get404 profid
    msg <- getMessage
    (widget,enctype) <- generateFormPost (formDisc profid)
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
            <form action=@{DiscR profid} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postDiscR :: ProfessorId -> Handler Html
postDiscR profid = do 
    ((res,_),_) <- runFormPost (formDisc profid)
    case res of 
        FormSuccess disc -> do 
            _ <- runDB $ insert disc
            setMessage [shamlet|
                <h2>
                    Disciplina CADASTRADA COM SUCESSO!
            |]
            redirect (DiscR profid)
        _ -> redirect HomeR

getChamadaR :: ProfessorId -> Handler Html
getChamadaR profid = do 
    runDB $ get404 profid
    profs <- runDB $ selectList [DisciplinaProfid ==. profid] []
    listaAluId <- return $ map (\(Entity _ disc) -> disciplinaAluid disc) profs
    alunos <- runDB $ selectList [AlunoId <-. listaAluId] [] 
    defaultLayout $(whamletFile "templates/aluno.hamlet")

getMatriculaR :: AlunoId -> Handler Html
getMatriculaR aluid = do 
    runDB $ get404 aluid
    alunos <- runDB $ selectList [DisciplinaAluid ==. aluid] []
    listaProfId <- return $ map (\(Entity _ disc) -> disciplinaProfid disc ) alunos
    profs <- runDB $ selectList [ProfessorId <-. listaProfId] [] 
    defaultLayout $(whamletFile "templates/professor.hamlet")
