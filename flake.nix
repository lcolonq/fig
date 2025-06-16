{
  description = "fig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      lldap-cli = pkgs.stdenv.mkDerivation {
        name = "lldap-cli";
        src = pkgs.fetchFromGitHub {
          owner = "Zepmann";
          repo = "lldap-cli";
          rev = "2a80dc4";
          sha256 = "uk7SOiQmUYtoJnihSnPsu/7Er4wXX4xvPboJaNSMjkM=";
        };
        buildPhase = "";
        installPhase = ''
          mkdir -p $out/bin
          cp lldap-cli $out/bin
        '';
      };
      lldap-cli-wrapped = pkgs.writeShellScriptBin "lldap-cli" ''
        export PATH=${pkgs.lldap}/bin:${pkgs.curl}/bin:${pkgs.jq}/bin:$PATH
        ${lldap-cli}/bin/lldap-cli "$@"
      '';

      haskellOverrides = self: super: {
        scotty = self.callHackageDirect {
          pkg = "scotty";
          ver = "0.21";
          sha256 = "sha256-coeQZJT7COSmoyA1eiykoMFv3+xNnxkF5tX4mlFcd84=";
        } {};
        discord-haskell = self.callCabal2nix "discord-haskell" ./deps/discord-haskell {};
        irc-conduit = self.callCabal2nix "irc-conduit" ./deps/irc-conduit {};
        irc-client = self.callCabal2nix "irc-client" ./deps/irc-client {};
        fig-utils = self.callCabal2nix "fig-utils" ./fig-utils {};
        fig-bus = self.callCabal2nix "fig-bus" ./fig-bus {};
        fig-monitor-twitch = self.callCabal2nix "fig-monitor-twitch" ./fig-monitor-twitch {};
        fig-monitor-discord = self.callCabal2nix "fig-monitor-discord" ./fig-monitor-discord {};
        fig-monitor-irc = self.callCabal2nix "fig-monitor-irc" ./fig-monitor-irc {};
        fig-bridge-irc-discord = self.callCabal2nix "fig-bridge-irc-discord" ./fig-bridge-irc-discord {};
        fig-web = self.callCabal2nix "fig-web" ./fig-web {};
        };
      haskellPackages = pkgs.haskell.packages.ghc94.override {
        overrides = haskellOverrides;
      };

      figBusSExpModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.fig-bus-sexp;
        in {
          options.colonq.services.fig-bus-sexp = {
            enable = lib.mkEnableOption "Enable the fig message bus";
            host = lib.mkOption {
              type = lib.types.str;
              default = "127.0.0.1";
              description = "The host bound by the fig server";
            };
            port = lib.mkOption {
              type = lib.types.port;
              default = 32050;
              description = "The port bound by the fig server";
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.fig-bus-sexp" = {
              wantedBy = ["network-online.target"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-bus}/bin/fig-bus sexp --host ${cfg.host} --port ${toString cfg.port}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.fig-bus-sexp";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.fig-bus-sexp";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.fig-bus-sexp";
                CacheDirectoryMode = "0750";
              };
            };
          };
        };
      figBusBinaryModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.fig-bus-binary;
        in {
          options.colonq.services.fig-bus-binary = {
            enable = lib.mkEnableOption "Enable the fig message bus";
            host = lib.mkOption {
              type = lib.types.str;
              default = "127.0.0.1";
              description = "The host bound by the fig server";
            };
            port = lib.mkOption {
              type = lib.types.port;
              default = 32051;
              description = "The port bound by the fig server";
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.fig-bus-binary" = {
              wantedBy = ["network-online.target"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-bus}/bin/fig-bus binary --host ${cfg.host} --port ${toString cfg.port}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.fig-bus-binary";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.fig-bus-binary";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.fig-bus-binary";
                CacheDirectoryMode = "0750";
              };
            };
          };
        };
      figMonitorTwitchLiveWatcherModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.fig-monitor-twitch-live-watcher;
        in {
          options.colonq.services.fig-monitor-twitch-live-watcher = {
            enable = lib.mkEnableOption "Enable the fig Twitch live watcher";
            busHost = lib.mkOption {
              type = lib.types.str;
              default = "127.0.0.1";
              description = "Message bus port";
            };
            busPort = lib.mkOption {
              type = lib.types.port;
              default = 32051;
              description = "Address of message bus";
            };
            configFile = lib.mkOption {
              type = lib.types.path;
              description = "Path to config file";
              default = pkgs.writeText "fig-monitor-twitch.toml" ''
                client_id = ""
                user_token = ""
                user_login = ""
                monitor = []
              '';
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.fig-monitor-twitch-live-watcher" = {
              wantedBy = ["multi-user.target"];
              after = ["colonq.fig-bus.service"];
              unitConfig = {
                StartLimitInterval = "3600";
                StartLimitBurst = "5";
              };
              serviceConfig = {
                Restart = "on-failure";
                RestartSec = "300";
                ExecStart = "${haskellPackages.fig-monitor-twitch}/bin/fig-monitor-twitch live-checker --bus-host ${cfg.busHost} --bus-port ${toString cfg.busPort} --config ${cfg.configFile}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.fig-monitor-twitch-live-watcher";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.fig-monitor-twitch-live-watcher";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.fig-monitor-twitch-live-watcher";
                CacheDirectoryMode = "0750";
              };
            };
          };
        };
      figMonitorDiscordModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.fig-monitor-discord;
        in {
          options.colonq.services.fig-monitor-discord = {
            enable = lib.mkEnableOption "Enable the fig Discord monitor";
            busHost = lib.mkOption {
              type = lib.types.str;
              default = "127.0.0.1";
              description = "Message bus port";
            };
            busPort = lib.mkOption {
              type = lib.types.port;
              default = 32050;
              description = "Address of message bus";
            };
            configFile = lib.mkOption {
              type = lib.types.path;
              description = "Path to config file";
              default = pkgs.writeText "fig-monitor-discord.toml" ''
                auth_token = ""
              '';
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.fig-monitor-discord" = {
              wantedBy = ["multi-user.target"];
              after = ["colonq.fig-bus.service"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-monitor-discord}/bin/fig-monitor-discord --bus-host ${cfg.busHost} --bus-port ${toString cfg.busPort} --config ${cfg.configFile}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.fig-monitor-discord";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.fig-monitor-discord";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.fig-monitor-discord";
                CacheDirectoryMode = "0750";
              };
            };
          };
        };
      figMonitorIRCModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.fig-monitor-irc;
        in {
          options.colonq.services.fig-monitor-irc = {
            enable = lib.mkEnableOption "Enable the fig IRC monitor";
            busHost = lib.mkOption {
              type = lib.types.str;
              default = "127.0.0.1";
              description = "Message bus port";
            };
            busPort = lib.mkOption {
              type = lib.types.port;
              default = 32050;
              description = "Address of message bus";
            };
            configFile = lib.mkOption {
              type = lib.types.path;
              description = "Path to config file";
              default = pkgs.writeText "fig-monitor-irc.toml" ''
                host = "colonq.computer"
                port = 26697
                nick = "discord"
                channels = ["#cyberspace", "#geiserzone", "#jakerealm"]
              '';
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.fig-monitor-irc" = {
              wantedBy = ["multi-user.target"];
              after = ["colonq.fig-bus.service"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-monitor-irc}/bin/fig-monitor-irc --bus-host ${cfg.busHost} --bus-port ${toString cfg.busPort} --config ${cfg.configFile}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.fig-monitor-irc";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.fig-monitor-irc";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.fig-monitor-irc";
                CacheDirectoryMode = "0750";
              };
            };
          };
        };
      figBridgeIRCDiscordModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.fig-bridge-irc-discord;
        in {
          options.colonq.services.fig-bridge-irc-discord = {
            enable = lib.mkEnableOption "Enable the fig IRC/Discord bridge";
            busHost = lib.mkOption {
              type = lib.types.str;
              default = "127.0.0.1";
              description = "Message bus port";
            };
            busPort = lib.mkOption {
              type = lib.types.port;
              default = 32050;
              description = "Address of message bus";
            };
            configFile = lib.mkOption {
              type = lib.types.path;
              description = "Path to config file";
              default = pkgs.writeText "fig-bridge-irc-discord.toml" ''
                [[mapping]]
                irc = "#cyberspace"
                discord = 1064660360533135551 # the-computer in clonkcord
                
                [[mapping]]
                irc = "#geiserzone"
                discord = 1117224697914990662 # bot-test in clonkcord
                
                [[mapping]]
                irc = "#jakerealm"
                discord = 1135088202114412628 # general in jakecord
              '';
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.fig-bridge-irc-discord" = {
              wantedBy = ["multi-user.target"];
              after = ["colonq.fig-bus.service"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-bridge-irc-discord}/bin/fig-bridge-irc-discord --bus-host ${cfg.busHost} --bus-port ${toString cfg.busPort} --config ${cfg.configFile}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.fig-bridge-irc-discord";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.fig-bridge-irc-discord";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.fig-bridge-irc-discord";
                CacheDirectoryMode = "0750";
              };
            };
          };
        };
      figWebModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.fig-web;
        in {
          options.colonq.services.fig-web = {
            enable = lib.mkEnableOption "Enable the fig web server";
            busHost = lib.mkOption {
              type = lib.types.str;
              default = "127.0.0.1";
              description = "Message bus port";
            };
            busPort = lib.mkOption {
              type = lib.types.port;
              default = 32051;
              description = "Address of message bus";
            };
            configFile = lib.mkOption {
              type = lib.types.path;
              description = "Path to config file";
              default = pkgs.writeText "fig-web.toml" ''
                port = 8000
                asset_path = "/var/lib/fig-web-assets"
                client_id = ""
                auth_token = ""
                db_host = ""
              '';
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.fig-web" = {
              wantedBy = ["multi-user.target"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-web}/bin/fig-web public --bus-host ${cfg.busHost} --bus-port ${toString cfg.busPort} --config ${cfg.configFile}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.fig-web";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.fig-web";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.fig-web";
                CacheDirectoryMode = "0750";
              };
            };
          };
        };
      figWebSecureModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.fig-web-secure;
        in {
          options.colonq.services.fig-web-secure = {
            enable = lib.mkEnableOption "Enable the fig web server (secure)";
            busHost = lib.mkOption {
              type = lib.types.str;
              default = "127.0.0.1";
              description = "Message bus port";
            };
            busPort = lib.mkOption {
              type = lib.types.port;
              default = 32051;
              description = "Address of message bus";
            };
            configFile = lib.mkOption {
              type = lib.types.path;
              description = "Path to config file";
              default = pkgs.writeText "fig-web-secure.toml" ''
                port = 8000
                asset_path = "/var/lib/fig-web-assets"
                client_id = ""
                auth_token = ""
                db_host = ""
              '';
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.fig-web-secure" = {
              wantedBy = ["multi-user.target"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-web}/bin/fig-web secure --bus-host ${cfg.busHost} --bus-port ${toString cfg.busPort} --config ${cfg.configFile}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.fig-web-secure";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.fig-web-secure";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.fig-web-secure";
                CacheDirectoryMode = "0750";
              };
            };
          };
        };
    in {
      devShells.x86_64-linux.default = haskellPackages.shellFor {
        packages = hspkgs: with hspkgs; [
          fig-utils
          fig-bus
          fig-monitor-twitch
          fig-monitor-discord
          fig-monitor-irc
          fig-bridge-irc-discord
          fig-web
        ];
        withHoogle = true;
        buildInputs = [
          lldap-cli-wrapped
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          pkgs.nodejs
          pkgs.guile
          pkgs.pkg-config
        ];
      };
      packages.x86_64-linux = {
        default = haskellPackages.fig-bus;
        figUtils = haskellPackages.fig-utils;
        figBus = haskellPackages.fig-bus;
        figMonitorTwitch = haskellPackages.fig-monitor-twitch;
        figMonitorDiscord = haskellPackages.fig-monitor-discord;
        figMonitorIRC = haskellPackages.fig-monitor-irc;
        figBridgeIRCDiscord = haskellPackages.fig-bridge-irc-discord;
        figWeb = haskellPackages.fig-web;
      };
      apps.x86_64-linux.default = {
        type = "app";
        program = "${haskellPackages.fig-bus}/bin/fig-bus";
      };
      nixosModules = {
        figBusSExp = figBusSExpModule;
        figBusBinary = figBusBinaryModule;
        figMonitorTwitchLiveWatcher = figMonitorTwitchLiveWatcherModule;
        figMonitorDiscord = figMonitorDiscordModule;
        figMonitorIRC = figMonitorIRCModule;
        figBridgeIRCDiscord = figBridgeIRCDiscordModule;
        figWeb = figWebModule;
        figWebSecure = figWebSecureModule;
      };
      overlay = self: super: {
        fig = {
          lldap-cli = lldap-cli-wrapped;
        };
      };
    };
}
