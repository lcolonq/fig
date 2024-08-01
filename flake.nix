{
  description = "fig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    ps-tools.follows = "purs-nix/ps-tools";
    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      ps-tools = inputs.ps-tools.legacyPackages.${system};
      purs-nix = inputs.purs-nix { inherit system; };

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
        fig-monitor-bullfrog = self.callCabal2nix "fig-monitor-bullfrog" ./fig-monitor-bullfrog {};
        fig-bridge-irc-discord = self.callCabal2nix "fig-bridge-irc-discord" ./fig-bridge-irc-discord {};
        fig-bless = self.callCabal2nix "fig-bless" ./fig-bless {};
        fig-emulator-gb = self.callCabal2nix "fig-emulator-gb" ./fig-emulator-gb {};
        fig-frontend = self.callCabal2nix "fig-frontend" ./fig-frontend {};
        };
      haskellPackages = pkgs.haskell.packages.ghc94.override {
        overrides = haskellOverrides;
      };

      purescript = purs-nix.purs {
        dependencies = [
          "console"
          "effect"
          "prelude"
          "random"
          "refs"
          "web-html"
          "web-dom"
          "web-uievents"
          "canvas"
          "argonaut"
          "fetch"
          "fetch-argonaut"
        ];
        dir = ./fig-frontend-client;
        srcs = [ "src" ];
      };
      fig-frontend-client = purescript.bundle {};

      figBusModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.fig-bus;
        in {
          options.colonq.services.fig-bus = {
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
            systemd.services."colonq.fig-bus" = {
              after = ["network-online.target"];
              wantedBy = ["network-online.target"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-bus}/bin/fig-bus --host ${cfg.host} --port ${toString cfg.port}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.fig-bus";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.fig-bus";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.fig-bus";
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
              default = 32050;
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
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-monitor-twitch-live-watcher}/bin/fig-monitor-twitch live-checker --bus-host ${cfg.busHost} --bus-port ${toString cfg.busPort} --config ${cfg.configFile}";
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
      figFrontendModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.fig-frontend;
        in {
          options.colonq.services.fig-frontend = {
            enable = lib.mkEnableOption "Enable the fig web frontend";
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
              default = pkgs.writeText "fig-frontend.toml" ''
                port = 8000
                asset_path = "./fig-frontend-assets"
                client_id = ""
                auth_token = ""
                db_host = ""
              '';
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.fig-frontend" = {
              wantedBy = ["multi-user.target"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-frontend}/bin/fig-frontend --bus-host ${cfg.busHost} --bus-port ${toString cfg.busPort} --config ${cfg.configFile}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.fig-frontend";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.fig-frontend";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.fig-frontend";
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
          fig-monitor-bullfrog
          fig-bridge-irc-discord
          fig-bless
          fig-frontend
          fig-emulator-gb
        ];
        withHoogle = true;
        buildInputs = [
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          pkgs.nodejs
          (purescript.command {})
          ps-tools.for-0_15.purescript-language-server
          purs-nix.esbuild
          purs-nix.purescript
          pkgs.m4
          pkgs.dhall
          pkgs.dhall-json
        ];
      };
      packages.x86_64-linux = {
        default = haskellPackages.fig-bus;
        figBus = haskellPackages.fig-bus;
        figMonitorTwitch = haskellPackages.fig-monitor-twitch;
        figMonitorDiscord = haskellPackages.fig-monitor-discord;
        figMonitorIRC = haskellPackages.fig-monitor-irc;
        figMonitorBullfrog = haskellPackages.fig-monitor-bullfrog;
        figBridgeIRCDiscord = haskellPackages.fig-bridge-irc-discord;
        figBless = haskellPackages.fig-bless;
        # figBlessStatic = haskellPackagesStatic.fig-bless;
        figEmulatorGB = haskellPackages.fig-emulator-gb;
        figFrontend = haskellPackages.fig-frontend;
      };
      apps.x86_64-linux.default = {
        type = "app";
        program = "${haskellPackages.fig-bus}/bin/fig-bus";
      };
      nixosModules = {
        figBus = figBusModule;
        figMonitorDiscord = figMonitorDiscordModule;
        figMonitorIRC = figMonitorIRCModule;
        figBridgeIRCDiscord = figBridgeIRCDiscordModule;
        figFrontend = figFrontendModule;
      };
    };
}
