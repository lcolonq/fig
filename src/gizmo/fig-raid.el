;;; fig-raid --- Raid Messages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun fig//respond-incoming-raid (user)
  "Generate a script to read when raided by USER."
  (fig/ask
   user
   (lambda (d) (message d))
   "You are a Twitch streamer named LCOLONQ. You do a stream about programming and you are an ASCII art VTuber. You just received a raid on Twitch from the given user. Your response should thank the user and ask them what they were doing. It should also introduce yourself."
   "GinjiVitis"
   "Is that... Is that Ginji Fucking Vitis in *MY* chat room? Thank you so much for the raid, oomfie! What were you up to today? Welcome all GinjiVitis fans. I... I always forget what to say here, I'm LCOLONQ, I'm letters and I'm really cool, yeah. We should really make a tool that generates these automatically haha. Anyway thanks again for the raid my friend."))

(provide 'fig-raid)
;;; fig-raid.el ends here
