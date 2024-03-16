;;; fig-clone --- Activate the cloning device -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

;; swab DNA
;; (-non-nil
;;  (--map-indexed
;;   (when (s-equals? (car it) "Must_Broke_")
;;     (cons
;;      (nth (- it-index 1) fig//incoming-chat-history)
;;      it))
;;   fig//incoming-chat-history)))
;; (-non-nil (--map-indexed (when (s-equals? (car it) "pnutonium") (cons (nth (- it-index 1) test-messages-parsed) it)) test-messages-parsed))
(defconst fig//dna-must_broke_
  '((("Hexadigital" . "Hi MODCLONK! hexadiHello") "Must_Broke_" .
     "modclonk is the only person i know that still playing the fruit game...")
    (("JDDoesDev" . "shebang shebang... oh baby") "Must_Broke_" .
     "enjoy your nap modclonk")
    (("FairchildVT" . "Must!") "Must_Broke_" . "Fair! lcolonHi")
    (("Lokiharth" . "I am extra buttoms for mouse enjoyer peepoHappy")
     "Must_Broke_" . "FeelsStrongMan")
    (("imgeiser" .
      "YOU SHOUL PLAY NIER AUTOMATA NOW! *thunder* (game is depression inducing)")
     "Must_Broke_" . "bless u")
    (("pwnedary" . "Joel") "Must_Broke_" . "Joel")
    (("GenDude" . "Heo mod! Joel") "Must_Broke_" . "lcolonHi modclonk")
    (("prodzpod" . "we are at least 15 thousand dollars indeed")
     "Must_Broke_" . "the 50K is already worth it, we're at NASA level")
    (("imgeiser" . "ask uwu for the furnace") "Must_Broke_" .
     "only 1K now wwparaSad")
    (("DoctorGlitchy" .
      "You can't make a perfect recreation unless you have nasa grade supercritical furnaces ;-;")
     "Must_Broke_" . "show us your clones doctor wwparaStare")
    (("saferq" . "@DoctorGlitchy so you admit they were clones")
     "Must_Broke_" . "beautiful clonkheads")
    (("NikolaRadHristov" . "wat") "Must_Broke_" .
     "we cant say here wwparaStare")
    (("GenDude" . "Germans? More like GERMans") "Must_Broke_" .
     "get momomo here")
    (("saferq" .
      "I mean how often do you sequence dna without blowing it apart and putting it back together")
     "Must_Broke_" . "unipiuScared")
    (("DoctorGlitchy" . "Not cloneing just science") "Must_Broke_" .
     "the socks baby...")
    (("dwinkley_" . "@DoctorGlitchy this is too science for me")
     "Must_Broke_" . "do i need to give you my spit? wwparaStare")
    (("Azrhyga" . "DNA is an helix, or the government lied to me?")
     "Must_Broke_" . "can you see it")
    (("divorce_jonZe" .
      "i put the hot scope on my left eye and the cold scope on my right")
     "Must_Broke_" . "$20,000 clonkhead vs $1 clonkhead")
    (("imgeiser" . "VAI BRASIL PORRA") "Must_Broke_" . "kino.")
    (("CrazyKitty357" .
      "just spend more points jetsLUL [i](this was sent from godot)[/i]")
     "Must_Broke_" . "those 4K extra points...")
    (("GenDude" .
      "Its a fake clone to distract you from the doxxing. He is outside your house.")
     "Must_Broke_" . "CRAZY KITTY IS CHEATING")
    (("pablo_pepe_69" . "PERFECTION") "Must_Broke_" .
     "doubters cant get it")
    (("saferq" . "clone, clonq, connect the dots sheeple") "Must_Broke_"
     . "i will not lick anything ever again")
    (("DoctorGlitchy" . "OH GOD") "Must_Broke_" .
     "what type of avant-garde shit is this")
    (("LeadenGin" . "He's selling kanye clones for $1?? wow")
     "Must_Broke_" . "uwutooConcern")
    (("fabibiusus" .
      "Milliards is used for billions in a few European languages.")
     "Must_Broke_" . "24K is a scary number")
    (("Bezelea" . "was the lever count intentional?") "Must_Broke_" .
     "wwparaStare wwparaStare")
    (("prodzpod" . "oh i was talking about the wikipedia donation popup")
     "Must_Broke_" .
     "https://cdn.discordapp.com/attachments/779073687546232892/1184230427649642556/GBF2_iLWgAAQ9ID.png already posted on twitter but sending again cus the funny")
    (("fn_lumi" .
      "toast is too fancy??? i guess amazon not doing so great rn")
     "Must_Broke_" . "based mr green enjoyer")
    (("imgeiser" . "wait") "Must_Broke_" . "sex")
    (("steeledshield" . "📈") "Must_Broke_" .
     "i only need boosts in my wallet")
    (("pwnedary" . "Joel?") "Must_Broke_" . "Joel?")
    (("DoctorGlitchy" . "Joel") "Must_Broke_" . "Joel")
    (("CodeSpace0x25" . "so just dm me") "Must_Broke_" . "jesas")
    (("pablo_pepe_69" . "100k fuck me sideways") "Must_Broke_" .
     "EZ Clap")
    (("CodeSpace0x25" . "I’ll try to help with what I can @oakspirit")
     "Must_Broke_" . "imagine")
    (("ZenyaHima" . "very cool slideshow clonk") "Must_Broke_" .
     "uwutooConcern")
    (("ZenyaHima" . "INSANECAT") "Must_Broke_" . ":3")
    (("prodzpod" .
      "i played oub today, the map got bigger compared to the last time")
     "Must_Broke_" . "wwparaDance prog wwparaDance")
    (("pablo_pepe_69" . "[0.0]") "Must_Broke_" . "Christ.")
    (("DoctorGlitchy" . "Clueless") "Must_Broke_" . "ancient clonk stuff")
    (("pablo_pepe_69" . "streamer > mods > VIPs > soil > viewers")
     "Must_Broke_" . "us? wwparaSad")
    (("pablo_pepe_69" . "<3 <3 <3 MC") "Must_Broke_" .
     "i need to star calling her MC for now on")
    (("liquidcake1" . "WTF is going on here?") "Must_Broke_" .
     "classic clonkhead")
    (("DigbyCat" . "I was expecting number 1 to be modclonq")
     "Must_Broke_" . "forsenE")
    (("prodzpod" . "the integration award") "Must_Broke_" .
     "FORSEN MAH MAN")
    (("pablo_pepe_69" . "LUL LUL LUL LUL") "Must_Broke_" . "Clap")
    (("flyann" . "holy crap lois its huey hexadecimal") "Must_Broke_" .
     "unipiuScared")
    (("pablo_pepe_69" .
      "Kreygasm Kreygasm Kreygasm Kreygasm POWERPOINT STREAM")
     "Must_Broke_" . "wwparaPog numbers?")
    (("prodzpod" . "wrapup!!") "Must_Broke_" . "wwparaPog")
    (("imgeiser" . "I was in some people top, I am shocked")
     "Must_Broke_" . "lcolonCool numbers cool")
    (("Wina" . "archive.org is top 5 websites of all time tbh")
     "Must_Broke_" . "wwparaShake")
    (("eudemoniac" . "ogre build") "Must_Broke_" . "im an archive freak")
    (("DoctorGlitchy" . "Huh???") "Must_Broke_" . "🔥🔥🔥")
    (("eudemoniac" . "we should make our entry into the rap space, boss")
     "Must_Broke_" . "will we get a funny badge for this recap?")
    (("shindigs" . "CLONKING") "Must_Broke_" . "lcolonHi")))

(defconst fig//dna-tyumici
  '((("Must_Broke_" . "the dopples are evolving Clap") "Tyumici" .
     "where's the stream readme")
    (("prodzpod" . "oh did tyumici clone redeemed") "Tyumici" .
     "dopple is good")
    (("carlossss333" . "live cloning") "Tyumici" . "I'm 90% cloneable")
    (("Tyumici" . "I'm 90% cloneable") "Tyumici" . "clone me up")
    (("prodzpod" . "im more of a shapez guy though") "Tyumici" .
     "aw hell yea, same")
    (("DigbyCat" . "Well \"not shit\" relative to windows") "Tyumici" .
     "no way 11 is not shit")
    (("ocuxw" . "is pacman have a issues?") "Tyumici" .
     "I love being able to say I use Arch (btw)")
    (("pwnedary" . "goodnight computer") "Tyumici" .
     "midi can do everything")
    (("ConditionBleen" . "lmao") "Tyumici" .
     "it's practically stolen valor")
    (("steeledshield" .
      "web dev often involves a lot of idiot proofing and telling it NOT to do certain stuff")
     "Tyumici" . "It pays the bills tho")
    (("Crane0001" .
      "I’m at work and can’t hear you but still wanted to say hi")
     "Tyumici" .
     "I wanted to do that as well, but yea work aoc balance was not feasible")
    (("Azrhyga" .
      "LCOLONQ is not any anime girl, is an ASCII anime girl inside of a computer with friend")
     "Tyumici" . "Oh no I meant for next year, the anarchy arc lol")
    (("steeledshield" . "yay!") "Tyumici" .
     "2024: Clonq does wrong things?")
    (("terriakijerky" . "this year has sucked for me jnero1LoamPensive")
     "Tyumici" . "this year was a lot")))

(defconst fig//dna-pnutonium
  '((("baxtercrook" . "lcolonHi chubohHello") "pnutonium" .
     "this isnt singing this is yappin FR")
    (("lcolonq" .
      "votsirHdaRalokiN: -12, O87OP: -9, eiwets_yzarc: -7, YTT1KYZ4RC: -6, ynnuBnwotknaD: -6")
     "pnutonium" . "oh you did a good job though")
    (("liquidcake1" . "Well done, Friend!") "pnutonium" .
     "jol jol jol jol jol jol joljoljoljol")
    (("steeledshield" . "*yo") "pnutonium" .
     "i could probably manually pitch that to mariah carey vocals")
    (("liquidcake1" . "What's on the B-side?") "pnutonium" .
     "number of songs")
    (("steeledshield" .
      "I watched the batman animated series episode that has that version of jingle bells in it recently")
     "pnutonium" . "@liquidcake1 robin laid an egg")
    (("lcolonq" . "Not even a nibble...") "pnutonium" . "discord irc")
    (("lcolonq" . "https://discord.gg/f4JTbgN7St") "pnutonium" . "IRC")
    (("practicalnpc" . "are you excited for this upcoming year?")
     "pnutonium" . "cant promise oit")
    (("zom_danni" . "hope you are having a great day dude!") "pnutonium"
     . "happy virtual holidays frien (:")
    (("a_tension_span" .
      "I just got ads for cat food. It's good to know that twitch has so little data on me to still don't get me the right ads.")
     "pnutonium" . "we 80% in for the shits and gigs anyways")
    (("pwnedary" . "modclonk spotted?") "pnutonium" . "hi modclonk")
    (("gendude" . "I've seen where you lurk") "pnutonium" .
     "my pc just bluescreened D:")
    (("gendude" . "Happy U-Haul-iDays") "pnutonium" . "merry cridmuh")
    (("a_tension_span" .
      "vtube studio can do custom shaders only now? How is that not a day1 feature?")
     "pnutonium" . "its joever for colonq")
    (("h_ingles" . "Every vtuber knows you! A vtuber's vtuber.")
     "pnutonium" . "x list soon <3")
    (("harrisamapon" . "Stopping by from Sciants shout out to you")
     "pnutonium" .
     "I'll influence you to delete your OS files")))

(defconst fig//dna-ellg
  '((("prodzpod" . "what the fuck") "ellg" .
     "every song is better when its 20% faster")
    (("Ricardo_Stryki" . "clonk clones") "ellg" .
     "remember the make the clone really into javascript (web) and nightcore")
    (("WUOTE" . "the only thing that brings me true joy is ACID") "ellg"
     . "ill take a ellg and a ellg (gay) clone")
    (("WUOTE" .
      "i love backseating tho! especially world record holders, becomes progressively harder over time rem2emWoozy")
     "ellg" . "you owe me like 2 clones at this point tbh")
    (("ellg" . "you owe me like 2 clones at this point tbh") "ellg" .
     "NOW: clone (real)")
    (("MODCLONK" . "lmao") "ellg" . "its ok to look hot clonk")
    (("prodzpod" . "he earthbends on gofd") "ellg" .
     "i dont think thats true")
    (("prodzpod" . "@yiffweed Factorio is such a good game") "ellg" .
     "do people ever not believe modclonk when she says shes married")
    (("Tomaterr" . "do you have a day of butlerian jihad") "ellg" .
     "lol i knew you were that kind of guy")
    (("MODCLONK" . ">:(") "ellg" .
     "whens the last time you got a haircut")
    (("ZedZark" . "you say \"abandoned\" but you still have them") "ellg"
     . "it was on the docket last week!")
    (("ellg" . "it was on the docket last week!") "ellg" .
     "speaking of add, dont forget to clone today >:)")
    (("ZedZark" .
      "maybe you can convince yourself that playing a game is productive work")
     "ellg" .
     "ya for sure, im just saying, its a good thing to at least try")
    (("babanana_7" . "relatable") "ellg" .
     "theres a lot of great resources on how to start that whole thing")
    (("stoicmana" . "imagine just having fun") "ellg" . "start there")
    (("prodzpod" .
      "i saw something like that happen in a lot of my friend who love mmos")
     "ellg" . "do you meditate at all clonk")
    (("KuromaruOniisan" . "by violating the intersate commerce clause")
     "ellg" .
     "modclonk seems the type that could get you some dank nugs (in minecraft)")
    (("khargoosh" . "The kids call it Doom Scrolling.") "ellg" . "lmao")
    (("khargoosh" . "I think the kids fall it Doom scrollibg.") "ellg" .
     "doing fat bong rips fixes a lot of things in life")
    (("Deep_field" . "err same*") "ellg" .
     "kinda like getting into meditation")
    (("ellg" . "kinda like getting into meditation") "ellg" .
     "\"why am i playing this game i could be coding something cool \" took me a while to get over that")
    (("prodzpod" . "nethack.") "ellg" . "i have the same issue")
    (("Tomaterr" . "idk how weed would interact with your Psyche") "ellg"
     .
     "not even joking, you just need to slightly turn down the \"why am i wasting time\" part of your brain")
    (("prodzpod" .
      "but if you want to write a story and weave it into the game making a game with solid gameplay (something you would like playing) that hints or gestures to the story (something you would like making) can be an answer")
     "ellg" . "then you can get into the gaming zone")
    (("zulleyy3" .
      "I am certain that Frag is a big reason why people play Mario")
     "ellg" .
     "i think you just need to get into drugs and also always having a slight buzz")
    (("Tyumici" . "god gtfo is hard") "ellg" . "he wanted to play the og")
    (("bobbehs" .
      "have you tried GTFO. thats the real title. its a hardcore survival horror shooter with great atmosphere")
     "ellg" . "you wont make it 2 hours in")
    (("prodzpod" .
      "so you can just make up wild shit about the game but there is definitely some kind of story the game eludes to")
     "ellg" .
     "didnt you say you wanted to do a modded ff7 playthrough lol")
    (("bobbehs" .
      "i find the longevity in my games the being the skill ceiling. games where you can constantly get better")
     "ellg" . "impressive")
    (("yiffweed" . "My roguelike is a roguelike") "ellg" .
     "its super impressibe")
    (("prodzpod" .
      "inscryption was a mid story game but then it became a twisted roguelike through the dlc")
     "ellg" . "ya")
    (("khargoosh" .
      "My first plunge into games was choose your own story book on a computer. After that I was in love. I want an awesome story and great engagement.")
     "ellg" .
     "https://rheavenstudio.itch.io/heaven-studio have you seen this clonk")
    (("Deep_field" . "Tunnet was the game. Its pretty cool") "ellg" .
     "very good cheese")
    (("ellg" . "very good cheese") "ellg" .
     "i have a giant block of tillamook black label cheddar in my fridge")
    (("Deep_field" .
      "its cool to see rust in games. I remember playing some kind of networking game that was written in rust")
     "ellg" . "@Tomaterr their smoked cheddar is so good")
    (("mcollardx" . "Aged steel cheddar. /me makes a note.") "ellg" .
     "tyumici is so right")
    (("Spaecplex" . "add fucked up physics so we can strafejump") "ellg"
     . "https://creamery.wsu.edu/cougar-cheese/ get on this")
    (("LeadenGin" . "moon jumping") "ellg" . "comes in a can")
    (("khargoosh" . "You on Linux?") "ellg" . "best cheese on earth")
    (("wetslugs" . "i like the low friction grass") "ellg" .
     "you need to get some cougar cheddar @MODCLONK")
    (("Deep_field" . "sexy dithering") "ellg" . "smh")
    (("Spaecplex" . "woh") "ellg" .
     "$15 stick of butter fans in this channel")
    (("Faeliore" . "I'm a big raylib enjoyer") "ellg" .
     "i think im kinda becoming a raylib guy over sdl")
    (("Tomaterr" . "Kerrygold Sponsor Me") "ellg" .
     "raylib also good to look at")
    (("prodzpod" . "holy fuck the kanban board") "ellg" .
     "its really nice with rust enums and pattern matching stuff")
    (("khargoosh" . "Egui is great for Native Apps. I use it.") "ellg" .
     "you might like Iced clonk, its based around elm / fp ui patterns")
    (("ZedZark" . "(probably not immediate mode)") "ellg" .
     "but imgui has a lot of really cool 3rd party stuff and looks a lot nicer too imo")
    (("CamuiCh" . "Cute stumpy walking stumps") "ellg" .
     "the c bindings are really simple you just have to do some annoying rust -> c string stuff")
    (("prodzpod" .
      "hello can you briefly flash the second season official new model for a second")
     "ellg" . "i think id probably still just use imgui even in rust")
    (("ZamielPayne" . "you look good with redhair") "ellg" .
     "immediate mode guis are cool for debugging but idk if id ever build something complex, you basically half implement retained ui to get something useful")
    (("Crane0001" . "well that resolves it") "ellg" .
     "i mean for something that simple ya itll be fine, its more like running that on phones and batterys and stuff kinda get weird")
    (("MODCLONK" . "I forget also you can't hear me lmao") "ellg" .
     "its updating the ui once every like 60 fps")
    (("MODCLONK" . "@Crane0001 lmao I guess I am too") "ellg" .
     "ya, and itll kill your battery too")
    (("khargoosh" . "Iced or egui") "ellg" . "theres a few")
    (("WUOTE" . "probably this https://jsoncanvas.org/") "ellg" .
     "like where you draw boxes and text on")
    (("Faeliore" . "its just a format for infinite canvas apps") "ellg" .
     "infinite canvas")
    (("JDDoesDev" . "is Jason Canvas related to John Twitch?") "ellg" .
     "i have not")
    (("ShyRyan" . "phoenix live view shyryaPhoenix") "ellg" .
     "im gonna become an enterprise golang developer and stop all ui dev now")
    (("ZamielPayne" . "google ads is in dart") "ellg" .
     "dart only exists becuase of flutter")))

(defconst fig//dna-hexadigital
  '((("abipolarcarp123" .
      "We must accept the limitless capacity to forget, like math operations. We have been given gifts to recall what we forget and to have learned at all is something to cherish tdogSmile")
     "hexadigital" .
     "I have absolutely no idea why but for some reason I associate the number 29 with you, and today is the 29th hexadiBirdbrain")
    (("zulleyy3" .
      "or rather... why have sync / async code in the first place")
     "hexadigital" .
     "LCOLONQ! LCOLONQ! LCOLONQ! Today is a LCOLONQ! day hexadiCoding")
    (("setolyx" . "bogaHey catch y'all later!") "hexadigital" .
     "Thanks for the stream, L:Q hexadiCoding")
    (("tomaterr" . "they're just trying things out") "hexadigital" .
     "It's weird because they only rolled back the artistic portion, the rest of the stuff for IRL streamers is still allowed")
    (("fighting_annelids" . "nice, good luck clonk") "hexadigital" .
     "Biochar is interesting since it doesn't add any nutrition to soil, but it has a ton of surface space for microbes (a teaspoon of biochar dust has the surface area of a football field), so it improves the soil health and lasts hundreds of years")
    (("azrhyga" .
      "LCOLONQ having LCOLONQ stream in it hair, breaking the matrix")
     "hexadigital" .
     "Yeah - I've been dehydrating and grinding most of my compost supplies to take with me whenever I move, but bones and meat tends to be a bit too greasy for that to work well, so I'm turning it into biochar and processing it in small portions alongside the rest of the pre-compost")
    (("blazynights" . "ovo") "hexadigital" .
     "It's a bit different from your birds but I've been making biochar out of chicken bones all day today so far hexadiGardening")
    (("azrhyga" . "Wow @Hexadigital, that was fast :0") "hexadigital" .
     "The power of alerts hexadiCoding")
    (("hexadigital" . "birds!") "hexadigital" . "hexadiHeart")
    (("lcolonq" . "test") "hexadigital" . "birds!")
    (("liquidcake1" .
      "I'm note sure if I we have Prime, though. Is it like Supreme?")
     "hexadigital" .
     "I fear for the day that Amazon doesn't think I am a student and Prime costs more than a Twitch sub - last time I told it that I am forever learning, always discovering new things, so hopefully that's an infinite student discount")
    (("liquidcake1" . "I'm an Ingerlander, yes.") "hexadigital" .
     "prime time")
    (("danktownbunny" . "These clones better fight to the death")
     "hexadigital" . "Friend doesn't know the composition of water...")
    (("wadderdragen" . "The Raiders whomst just joined") "hexadigital" .
     "unhydrogenated water")
    (("wadderdragen" . "Fried Homer Simpson") "hexadigital" .
     "is it taco tuesday or terabyte tuesday? can you eat a terabyte of tacos?")
    (("pnutonium" .
      "while Im less busy Im gonna say that clonq is my 2 most watched with 170 hours <3")
     "hexadigital" . "Still looking for a morse code streamer")
    (("wadderdragen" . "Sauce that makes you say \"mmmm sauce mmm\"")
     "hexadigital" .
     "The only other non-verbal streamer I know of is one I don't watch because of that - no chat history, and no way to know when they're speaking")
    (("lokiharth" . "the strongest lurker") "hexadigital" .
     "Top selling point: you can walk away for 10 minutes and read the chat when you get back")
    (("hexadigital" .
      "I lurk more than I chat, so I would never complain about a lurker!")
     "hexadigital" . "hexadiLurk")
    (("inspectordiameter" . "I spent 47hrs watching @LCOLONQ in 2023.")
     "hexadigital" .
     "I lurk more than I chat, so I would never complain about a lurker!")
    (("wadderdragen" . "Sauce that makes you generate a windows 10 key")
     "hexadigital" . "You showed up in ninth hexadiHello")
    (("liquidcake1" .
      "Maybe it's just the ones with the brightest avatars.")
     "hexadigital" .
     "The first person is the top viewer, top chatter, and top redeemer, I think - they've never bought a sub or bits or anything though")
    (("michaelhunt1122" . "maybe highest volume on stream") "hexadigital"
     .
     "The fifth \"loudest fan\" on mine hasn't been in the stream for about half a year, I think")
    (("hexadigital" .
      "It showed top viewers last year, now it's top chatters")
     "hexadigital" . "Maybe most redeems?")
    (("prodzpod" . ":gasp:") "hexadigital" .
     "It showed top viewers last year, now it's top chatters")
    (("vesdeg" .
      "nixos is reproducible? how about you reproduce with some bitches")
     "hexadigital" .
     "I wish Twitch would show who your top viewers were, rather than just chatters")
    (("yellowberryhn" . "what fruit game") "hexadigital" . "hexadiShock")
    (("must_broke_" .
      "modclonk is the only person i know that still playing the fruit game...")
     "hexadigital" . "Hi MODCLONK! hexadiHello")
    (("steeledshield" . "mods are sleeping, post arch propaganda")
     "hexadigital" .
     "Hey LCOLONQ! Hope you're having a Telemetry-free Tuesday hexadiCoding")))

;; put DNA in chemical grade tube
(defun fig//clone-put-in-chemical-grade-tube (dna)
  "Take DNA and put it into a chemical grade tube."
  (--map (cons (cdar it) (cddr it)) dna))

;; hydrochloric acid
(defun fig//clone-apply-hydrochloric-acid (tube)
  "Pour hydrochloric acid into TUBE."
  (cons
   (-map #'car tube)
   (-map #'cdr tube)))

;; G5 50 solution
(defun fig//clone-g5-50-solution (acidtube keyphrase prompt k)
  "Mix ACIDTUBE with G5 50 solution.
KEYPHRASE is incorporated into the mix.
PROMPT defines the clone's personality.
Send the result to lab station K."
  (fig/ask
   keyphrase
   k
   prompt
   (car acidtube)
   (cdr acidtube)
   ))

;; self fusion machine
(defun fig//dna-to-fake-chatter-profile (dna username color prompt)
  "Clone from DNA with USERNAME and COLOR and PROMPT in the self fusion machine."
  (let ((acidtube (fig//clone-apply-hydrochloric-acid (fig//clone-put-in-chemical-grade-tube dna))))
    (fig//make-fake-chatter-profile
     :username username
     :color color
     :compute-likeliness (lambda (_) 0.1)
     :send-message
     (lambda (st)
       (fig//clone-g5-50-solution
        acidtube
        (fig//build-fake-chat-prompt st)
        prompt
        (lambda (msg)
          (fig//fake-chatter-send st msg)))))))

;; NASA grade supercritical furnaces

(provide 'fig-clone)
;;; fig-clone.el ends here
