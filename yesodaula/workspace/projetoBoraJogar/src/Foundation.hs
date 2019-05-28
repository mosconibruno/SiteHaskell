{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
    makeLogger = return . appLogger
    authRoute _ = Just $ LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR  _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized AdminR _ = isAdmin 
    isAuthorized _ _ = isUser
    
isAdmin :: Handler AuthResult
isAdmin = do 
    sess <- lookupSession "_ID"
    case sess of 
        Just "root" -> return Authorized
        Just _ -> return $ Unauthorized "N eh admin!"
        Nothing -> return AuthenticationRequired

isUser :: Handler AuthResult
isUser = do 
    sess <- lookupSession "_ID"
    case sess of 
        Just _ -> return Authorized
        Nothing -> return AuthenticationRequired

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
-- UMA VEZ
type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
