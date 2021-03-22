(start-process-shell-command "redshift" nil "redshift -c $HOME/.emacs.d/external/redshift/redshift.conf")
(start-process-shell-command "xkeysnail" nil "doas /usr/local/bin/xkeysnail -q $HOME/.emacs.d/external/xkeysnail/config.py")
(defun modremap ()
  "My preferred keyboard layouts."
  (start-process-shell-command "setxkbmap" nil "setxkbmap -model pc104aw-zqu -layout us,us,th -variant ,cmk_ed_dh,pat -option 'grp:ctrls_toggle'"))
