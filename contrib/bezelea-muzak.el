; still a bit messy, and uwu bells sound very different
(setq muzak-music-alist '(
                   ("Major Scale" . "CDEFGABc")
                   ("Minor Key Test" . "EB//CGcBEB//CGBGEB") ; Bezelea
                   ("Take Me Out To The Ball Game" . "C/cAGEG//D//C/cAGEG") ; Bezelea
                   ("Harry Potter" . "AcBA/ED//B//AcBG/AE///") ; Bezelea
                   ("Leaving on a Jet Plane" . "G///E///A/GF/G///G/E/G/F/GF/EC") ; Bezelea
                   ("Fly Me To The Moon" . "c//BAG/E///EGc/B//AGF/EDE//////AGFE/D/E/F/A/G//FED/C//////DDAA///////c/B/G////////CCFF//////A/G/F//E/") ; Bezelea
                   ("Fly Me To The Moon (Short)" . "c//BAG/E///EGc/B//AGF/EDE") ; Bezelea
                   ("Crazy Train" . "EEBEcEBEAG/GAG/D") ; Bezelea (Sounds ok at first, but uses "/" for F#)
                   ("Crazy Train (Accurate)" . "A/A/E/A/F/A/E/A/D/c/B/c/D/c/B/G/A/A/E/A/F/A/E/A/D/c/B/c/D/c/B/G/") ; Bezelea (Bad)
                   ("Santa Claus Is Coming To Town" . "EFG/G///ABc/c///EFG/G/G/AGF/F///E/G/C/E/D/F///B/c") ; Bezelea
                   ("Silent Night" . "G//AG/E/////G//AG/E/////D///D/B/////c///c/G") ; Bezelea
                   ("Do Your Ears Hang Low" . "AGF/F/F/AACDCAc/FGAAAAAAFGAGG/G/AGFFFFFFAACDCAc/FGA///G///F") ; Bezelea
                   ("King of the Hill" . "C/CCCCCE/CCCCCF/FFFA/G///CCCCECCCF/E/D/DEDCCCCCECCCFFFAG//c/A/G/F/EEEFEDCcc") ; TODO FIXME
                   ("Thunderstruck" . "EGEBEGEBEGEBEGEBEGEcEGEcEGEcEGEc") ; Bezelea FIXME Bad
                   ("Take On Me" . "BBGE/E/A/A/ABBCDccGE/E/A/A/AGGAG") ; Bezelea FIXME
                   ("Take Me Out" . "c/A///G/GA////c/A//G/GA////c/A/////G//G/A") ; Bezelea FIXME
                   ("Smells Like Teen Spirit" . "E/EE//AAAAAG/GG//c/cccc") ; Bezelea FIXME
                   ("Wish You Were Here" . "C/DEG/A///c/////A/c/A/G///////C/DEG/A///c//////A/c/A/G//////C/DEG/A//////A/G/E/D//////C/DEG/A//////A/G/E/D") ; Bezelea FIXME
                   ("Beethoven's 5th" . "AAAF////GGGE////") ; Bezelea
                   ("Star Spangled Banner" . "") ; Bezelea
                   ("Eye Of The Tiger" . "A/B/c///c/cc/B//A/G/G/A/B/A///A//B//c///c/cc/B//AG/B/A") ; Bezelea
                   ("Saria's Song" . "////FFAABB//FFAABB//FFAAEEDD////BBCCBBGGEE////") ; muz_e
                   ("Saria's Song 2" . "////FFAABB//FFAABB//FFAAEEDD////BBCCBBGGEE////") ; muz_e
                   ("Saria's Song (Fixed)" . "FAB/FAB/FABED/BcBGE//DEGE")
                   ("Song of Time?" . "A//D///F//A//D//F//A//B//G//F/G/A/D") ; McFishTM
                   ("Song of Time (Fixed)" . "A/D//F/A/D//F/AcB/G/FGA/D/CED")
                   ("Coffin Dance (Fixed)" . "D/DAG/F/E/EFG/FED/DcBcBcDDDcBcBc") ; Unknown
                   ("Do Re Mi" . "C//DE/C/E/C/E//D//EFFEDF//E//FG/E/G/E/G/GG") ;DaiyaDiamandis
                   ("Do Re Mi 2" . "C//DE//C/E/C/E///D//EFFEDF///E//FG//E/G/E/G/G") ; (Edited)
                   ("Do Re Mi 3" . "C//DE/C/E/C/E//D//EFFEDF//E//FG/E/G/E/G/G/F/GAAGFA") ; DaiyaDiamandis
                   ("Do Re Mi 4" . "C//DE/C/E/C/E//D//EFFEDF//E//FG/E/G/E/G/G/F/GAAGFAA") ; DaiyaDiamandis
                   ("Do Re Mi 5" . "C//DE/C/E/C/E//D//EFFEDF//E//FG/E/G/E/G/G/F/GAAGFA////G/CDEFGA/A/DEFGABB") ; DaiyaDiamandis
                   ("Frere Jacques" . "CDECCDECEFG/EFG/") ; Crane30
                   ("Frere Jacques (Slow)" . "C/D/E/C/C/D/E/C/E/F/G///E/F/G///GAGFE/C/GAGFE/C/G/C///C/G/C///") ; Crane30
                   ("Crane's Melody"   . "F/EED///EF/G/A////B/C///CB/A/E////G/A//A//G/F//D//C/D///") ; Crane30
                   ("Crane's Melody 2" . "F/EED///EF/G/A///B/C///CB/A/E////G/A//A//G/F//D//C/D///") ; Crane30
                   ("Crane's Melody 3" . "F/EED///EF/G/A///B/c///cB/A/E////G/A//A//G/F//D//C/D///") ; Crane30
                   ("State Anthem of the Russian Federation" . "//G/c///G//AB///E/E/A///G//FG///C/C/D///D/E/F///F/G/A///B/c/DD") ; Scottulism
                   ("Pizza Tower" . "A/E/G///A/E/G///ABABAGEDE") ; Scottulism
                   ("Scott's Melody 2" . "//A/E/G///A/E/G///ABABAGEDEE") ; Scottulism
                   ("???" . "E/DEFEDC/C/DEFFF//FFEDD/") ; mister_magister
                   ("???" . "E//D/E/F/E/D/C//C//D/E/F/F/F///F/F/E/D/D//E//D/E/F/E/D/C//C//D/E/F/F/F///F/F/E/D/D//E//D/E/F/E/D/C//C//D/E/F/F/F///F/F/E/D/D//") ; mister_magister
                   ("??? 2" . "E//D/E/F/E/D/C//C//D/E/F/F/F///F/F/E/D/D//////////E//D/E/F/E/D/C//C//D/E/F/F/F///F/F/E/D/D//") ; mister_magister
                   ("???" . "E/DEFEDC/C/DEFFF//FFEDD/E/DEFEDC/C/DEFFF//FFEDD/E/DEFEDC/C/DEFFF//FFEDD") ; mister_magister (Edited)
                   ("???" . "GABCDGACBGACBGACBGACBGACBGACBGACBGACBGACBGACBG") ; mister_magister
                   ("Magister's Melody" . "CDCECFCGCACBCDCECFCGCACBCDCECFCGCACBCDCECFCGCACB") ; mister_magister
                   ("???" . "C//E//G//C//E//G//D//F//D//F/") ; mister_magister
                   ("???" . "GGG/BB/EE/F/G/A/D/FF/EEEE/B/A/D") ; itsbitsybones
                   ("???" . "AAAFEEEDDDAAADDADADADDDBBAADDDDDDDDAAAA") ; itsbitsybones
                   ("???" . "GGGBBAAFCEBGEBFCBECBGCBEGEDCG") ; itsbitsybones
                   ("Mary Aborted a Little Lamb?" . "GGGABBBAAAEEEEDDDGGGBBBADDAADDDDBBBADDDBBBAAAGGGGFFEE") ; itsbitsybones
                   ("???" . "AAGGBABCAGABCGABCGFADEFGABCABGACGACGACGABAC") ; itsybitsybones
                   ("???" . "ABCABADCBADBCBADBCBADBCBADBC") ; itsybitsybones
                   ("JC's Melody" . "ACDF//BAC//FEDED//ACDF") ; JCRouzer
                   ("JC's 2nd Melody"   . "///////E///FG///G///A/F//E//E//F/G///D//D/D/A/E/E///F/G/E///G/A/F/E/D/E/E/F/G/G") ; JCRouzer
                   ("JC's 2nd Melody 2" . "///////E///FG///G///A/F//E//E//F/G///D//D/D/A/E/E///F/G/E///G/A/F/E/D/E/E/F/G/E/D//A/AA/B/A/G/A//B/G/A//A/A/B/A/D/E/F/G//E//AB/A///D/E/G/E/B//B//A/D/E/G/E/A//A//GG") ; JCRouzer
                   ("JC's 2nd Melody 3" . "E/FG/G/AF/E/E/FG//D/DDAEE//FGE/GAFEDEEFGED/AAABAGA/B/G/A//A/A/B/A/D/E/F/G//E//AB/A///D/E/G/E/B//B//A/D/E/G/E/A//A//GG") ; JCRouzer
                   ("???" . "B/CDDECBA/B/BCDBAAAE") ; beartherawr
                   ("Sounds like Marco Polo" . "DDCC/BBAA/DDCC/BBAA/DDCC/BBAA") ; beartherawr
                   ("???" . "C/CBCBCBCGF//FGCCGCD//CBCBC") ; jbent02
                   ("???" . "B//C//D/D//E/BBCDBAAAEBBCDBDECBCBAABBCDBAEEEFE") ;jbent02
                   ("???" . "FEDAGCBAGFEFEFDGGBCABGABFEGABEFFGABDEDE") ; dr_susboi
                   ("???" . "AAAACCCCAAAABBBBGGGGBBBBACBGACBGGGGGACBGACBG") ; Must_Broke_
                   ("???" . "B//B//B//D//C//B//B//B//D//C//B//B//B//D//C//B//B//E//B//E//B") ; netgeiser
                   ("???" . "GG/G/G/F/G/GF/G/AB/A/G/G/GG/F/G/G/D") ; Oronar
                   ("???" . "B//C/DD/E/CBA//B//B/CDB/A/AAEFGGAFEEFGDDDAEEFGEGAFEDEEFG") ;Oronar
                   ("Unknown Melody" . "E//FG//G//AF/E/E/FG//D/DDAEE//FGE//GAFEDEEFGG")
                   ("McFishTM Melody" . "D//F//D////D//F//D////E//F//E//F//E//C//A////A//D//F//G//EA") ; McFishTM
                   ("Song of Healing?" . "B//A//F////B//A//F////B//A//E//D//E//B//A//F//B") ; McFishTM
                   ("Mario" . "A/A///A//F/A///c/////C/") ; MrSoffit
                   ("Mario (Fast)" . "AA/A/FA/c///C/")
                   ("Test" . "bdgabdgabdAGdcba")
                   ))

(defun muzak--note-string-as-half-step (note)
  "Determine the step of the note in the C chromatic scale."
  (when-let ((step (-elem-index note '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B" "c"))))
    (+ 3 step))) ; C is the 3rd half step above A

(defun muzak--note-as-freq (note)
  "Convert a note to a frequency."
  (if-let ((step (muzak--note-string-as-half-step note)))
      (round (* 440 (expt 2 (/ step 12.0))))
    0))

(defun muzak--trim (notes)
  "Remove rests from the beginning and end of a note sequence."
  (cond
   ((stringp notes) (s-replace-regexp (pcre-to-elisp "^\/+|\/+$") "" notes))
   ((listp notes) (if (string= (car notes) "/") (muzak--trim (cdr notes)) notes)) ;FIXME
   ))

(defun muzak--play-note (note)
  "Play a note."
  (with-temp-buffer
    (shell-command
     (format "ffplay -autoexit -nodisp -loglevel warning -f lavfi -i \"sine=frequency=%d:duration=0.05\""
             (muzak--note-as-freq note)))))

(defun muzak--play-notes (notes &optional duration)
  "Play notes. Takes a list."
  ;; apulsator tremolo vibrato
  (call-process-shell-command
   (format "
for FREQ in %s; do
    ffmpeg -strict experimental -loglevel quiet -f lavfi -i \"sine=frequency=${FREQ}:duration=%f\" -f oga -filter tremolo -filter aphaser=in_gain=0.4:out_gain=0.74:delay=0.1:decay=0.2:speed=0.2 -filter volume=1.5 - 2>/dev/null
done | ffplay -loglevel quiet -autoexit -nodisp - &"
           (mapconcat (lambda (note)(format "%d" (muzak--note-as-freq note))) notes " ")
           (or duration 0.2))))

(defun muzak-play (song-name)
  "Play some muzak."
  (interactive "sSong Name: ")
  (message "Playing %s" song-name)
  (when-let (song-data (s-split "" (cdr (assoc song-name muzak-music-alist))))
    (muzak--play-notes (muzak--trim song-data))))

(defun muzak-play-notes (note-str)
  "Play NOTE-STR."
  (when-let (song-data (s-split "" note-str))
    (muzak--play-notes (muzak--trim song-data))))

(provide 'bezelea-muzak)
