# -*- coding: utf-8 -*-

import re
from xkeysnail.transform import *

# define timeout for multipurpose_modmap
define_timeout(0.2)

# [Global modemap] Change modifier keys as in xmodmap
define_modmap({
    Key.LEFT_CTRL: Key.LEFT_META,
    Key.LEFT_ALT: Key.LEFT_CTRL,
    Key.RIGHT_ALT: Key.RIGHT_CTRL,
    Key.MENU: Key.RIGHT_ALT,
    Key.RIGHT_CTRL: Key.RIGHT_ALT
})
# [Multipurpose modmap]
define_multipurpose_modmap({
    Key.CAPSLOCK: [Key.BACKSPACE, Key.LEFT_ALT],
    Key.LEFT_CTRL: [Key.ESC, Key.LEFT_CTRL],
    Key.RIGHT_CTRL: [Key.ESC, Key.RIGHT_CTRL],
    Key.LEFT_SHIFT: [Key.KPLEFTPAREN, Key.LEFT_SHIFT],
    Key.RIGHT_SHIFT: [Key.KPRIGHTPAREN, Key.RIGHT_SHIFT]
})    
