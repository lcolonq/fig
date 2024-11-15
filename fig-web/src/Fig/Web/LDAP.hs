module Fig.Web.LDAP where

import Fig.Prelude

import System.Exit (ExitCode(..))
import qualified System.Process as Proc

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Text as Text

import Fig.Web.Utils

-- | Reset the password in LDAP for the specified user (creating the user if necessary)
resetUserPassword :: MonadIO m => Config -> Text -> Text -> m (Maybe Text)
resetUserPassword cfg user uid = do
  let login = Text.toLower user
  password <- UUID.toText <$> liftIO UUID.nextRandom
  (exitCode, out0, err0) <- liftIO . flip Proc.readCreateProcessWithExitCode ""
    . Proc.proc cfg.lldapCli $ unpack <$>
    [ "-H", cfg.lldapHost
    , "-D", cfg.lldapUser
    , "-w", cfg.lldapPassword
    , "user", "add", login, login <> "@users.colonq.computer"
    , "-p", password
    , "-f", uid
    ]
  (_, out1, err1) <- liftIO . flip Proc.readCreateProcessWithExitCode ""
    . Proc.proc cfg.lldapCli $ unpack <$>
    [ "-H", cfg.lldapHost
    , "-D", cfg.lldapUser
    , "-w", cfg.lldapPassword
    , "user", "group", "add", login, "fig_users"
    ]
  case exitCode of
    ExitSuccess -> pure $ Just password
    ExitFailure _ -> do
      log . pack $ mconcat
        [ "LDAP CLI error:\n"
        , out0, err0
        , out1, err1
        ]
      pure Nothing
