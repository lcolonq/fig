{ manifest_version = 2
, name = "computerspotting"
, version = "1.0"
, description = "spot the computer"
, icons =
  [ { mapKey = "48", mapValue = "assets/mrgreen.png" }
  ]
, web_accessible_resources =
  [ "mrgreen.png"
  , "mrblue.png"
  ]
, permissions =
  [ "webRequest"
  , "webRequestBlocking"
  , "tabs"
  , "cookies"
  , "*://*.twitch.tv/*"
  , "*://api.colonq.computer/*"
  ]
, background =
  { scripts = ["background.js"]
  }
, content_scripts =
  [ { matches = ["*://*.twitch.tv/*"]
    , js = ["config.js", "main.js"]
    , css = ["main.css"]
    , run_at = "document_end"
    }
  ]
}