{
  description = "fig";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-23.05;

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      haskellPackages = pkgs.haskell.packages.ghc94.override {
        overrides = self: super: {
          discord-haskell = self.callCabal2nix "discord-haskell" ./deps/discord-haskell {};
          irc-conduit = self.callCabal2nix "irc-conduit" ./deps/irc-conduit {};
          irc-client = self.callCabal2nix "irc-client" ./deps/irc-client {};
          fig-utils = self.callCabal2nix "fig-utils" ./fig-utils {};
          fig-bus = self.callCabal2nix "fig-bus" ./fig-bus {};
          fig-monitor-discord = self.callCabal2nix "fig-monitor-discord" ./fig-monitor-discord {};
          fig-monitor-irc = self.callCabal2nix "fig-monitor-irc" ./fig-monitor-irc {};
          fig-monitor-bullfrog = self.callCabal2nix "fig-monitor-bullfrog" ./fig-monitor-bullfrog {};
          fig-bridge-irc-discord = self.callCabal2nix "fig-bridge-irc-discord" ./fig-bridge-irc-discord {};
          fig-frontend = self.callCabal2nix "fig-frontend" ./fig-frontend {};
        };
      };
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
                channel = 1064660360533135551
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
                sendchannel = "#cyberspace"
                channels = ["#cyberspace"]
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
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.fig-bridge-irc-discord" = {
              wantedBy = ["multi-user.target"];
              after = ["colonq.fig-bus.service"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.fig-bridge-irc-discord}/bin/fig-bridge-irc-discord --bus-host ${cfg.busHost} --bus-port ${toString cfg.busPort}";
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
    in {
      devShells.x86_64-linux.default = haskellPackages.shellFor {
        packages = hspkgs: with hspkgs; [
          fig-utils
          fig-bus
          fig-monitor-discord
          fig-monitor-irc
          fig-monitor-bullfrog
          fig-bridge-irc-discord
          fig-frontend
        ];
        withHoogle = true;
        buildInputs = [
          haskellPackages.haskell-language-server
        ];
      };
      packages.x86_64-linux = {
        default = haskellPackages.fig-bus;
        figBus = haskellPackages.fig-bus;
        figMonitorDiscord = haskellPackages.fig-monitor-discord;
        figMonitorIRC = haskellPackages.fig-monitor-irc;
        figMonitorBullfrog = haskellPackages.fig-monitor-bullfrog;
        figBridgeIRCDiscord = haskellPackages.fig-bridge-irc-discord;
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
      };
    };
}
