;;; src/AI/profanity/seeds.masked.lisp
;;; Masked roots (no raw profanity in repo). Underscore = vowel slot.
(defparameter *seed-bases*
  '("f_ck" "sh_t" "a_s" "d_ck" "b_ll" "w_nk" "p_ss" "c_ck" "t_wt" "n_mb" "g_bsh" "kn_b" "b_llck"))

(defparameter *seed-joins*
  '("weasel" "bucket" "nugget" "wizard" "goblin" "carousel" "sprinkler" "muffin" "socket" "gremlin" "trolley" "sprocket"))

(defparameter *seed-boost*
  '("mega" "ultra" "hyper" "turbo" "quantum" "glitch" "void" "feral" "neon" "holo"))

(defparameter *templates*
  '("<<B>>-<<J>>"
    "<<BOOST>>-<<B>>-<<J>>"
    "<<B>>-<<J>>-<<BOOST>>"
    "<<B>>ing <<J>>"
    "respawn, <<B>>-face"
    "crawl back, <<B>>-<<J>>"))
