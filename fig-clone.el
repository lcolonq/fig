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
;;   fig//incoming-chat-history))
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
