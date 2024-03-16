;;; fig-fusion --- The Island of Dr. Clonkeau -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defun fig//fuse-usernames (n1 n2 k)
  "Fuse names N1 and N2, passing the new name to K."
  (fig/ask
   (format "%s %s" n1 n2)
   (lambda (r)
     (funcall k (s-trim r)))
   "Given two usernames, please respond with a new user name that is a combination of the two. This name should be one single word."
   "dwinkley_ pwnedary"
   "pwnley_"
  ))

(defun fig//fuse-messages (m1 m2 k)
  "Fuse messages M1 and M2, passing the new message to K."
  (fig/ask
   (format "%s\n%s" m1 m2)
   (lambda (r)
     (funcall k (s-trim r)))
   "Given two chat messages, please respond with a new message that mixes the style and content of the given messages."
   "ugly smile emoticon
I originally found you on twitch recommended but I didn't watch you until uwu shoutout
"
   "I originally found you ugly but I smiled until uwu emoticon"
  ))

(defun fig//all-chatter-messages (user)
  "Collect all the messages by USER in the current stream."
  (-map #'cdr (-filter (lambda (it) (cl-equalp (car it) user)) fig//incoming-chat-history)))

(defun fig//assess-chatter-personality (user k)
  "Determine USER's personality from their message history.
Pass the result to K."
  (if-let ((msgs (fig//all-chatter-messages user)))
      (fig/ask
       (s-join "\n" msgs)
       (lambda (r)
         (funcall k (s-trim r)))
       "Given a number of messages, infer the personality of the user who sent those messages. This description should be one sentence in the second person."
       "nut
alices18CEO
the master boot record
"
       "You are conspicuous and somewhat aloof, type mostly in lowercase, and a fan of technology, nuts, and Alice Sawyer."
       )
    (message (format "No messages from %s" user))))

(defun fig//get-chatter-color (user)
  "Retrieve USER's name color."
  (let ((color (alist-get user fig//chatter-colors "#ffffff" nil #'s-equals?)))
    (if (s-equals? "" color)
        "#ffffff"
      color)))

(defun fig//fuse-chatters (user1 user2)
  "Create a new fake chatter that represents the fusion of USER1 and USER2."
  (let ((user1-history (fig//all-chatter-messages user1))
        (user2-history (fig//all-chatter-messages user2))
        (color (fig//breed-colors (fig//get-chatter-color user1) (fig//get-chatter-color user2)))
        )
    (fig//fuse-usernames
     user1 user2
     (lambda (fuse-user)
       (message (format "fuse-user: %s" fuse-user))
       (fig//fuse-usernames
        (car user1-history) (car user2-history)
        (lambda (fuse-message)
          (message (format "fuse-message: %s" fuse-message))
          (fig//assess-chatter-personality
           user1
           (lambda (user1-personality)
             (message (format "user1-personality: %s" user1-personality))
             (fig//assess-chatter-personality
              user2
              (lambda (user2-personality)
                (message (format "user2-personality: %s" user2-personality))
                (let ((prompt
                       (format
                        "Your name is %s. You are a Twitch chatter that is a chimera-style fusion of %s and %s. Your personality can be described by the following descriptors: %s %s talks like \"%s\" and %s talks like \"%s\"."
                        fuse-user
                        user1
                        user2
                        (s-concat user1-personality " " user2-personality)
                        user1
                        (car user1-history)
                        user2
                        (car user2-history)
                        )))
                  (message prompt)
                  (push
                   (fig//make-fake-chatter
                    :profile 
                    (fig//make-fake-chatter-profile
                     :username fuse-user
                     :color color
                     :compute-likeliness (lambda (_) 1.0)
                     :send-message
                     (lambda (st)
                       (fig/ask
                        (fig//build-fake-chat-prompt st)
                        (lambda (msg)
                          (fig//fake-chatter-send st msg))
                        (fig//build-fake-chat-system-prompt st prompt)
                        "LCOLONQ: what's happening gamers? tonight we're trying to implement"
                        fuse-message))))
                   fig//fake-chatters)
                  )))))))))))

(provide 'fig-fusion)
;;; fig-fusion.el ends here
