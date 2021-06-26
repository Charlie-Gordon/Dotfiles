# -*- coding: utf-8 -*-

import re
from xkeysnail.transform import *

# define timeout for multipurpose_modmap
define_timeout(0.3)

# [Global modemap] Change modifier keys as in xmodmap
define_modmap({
    Key.LEFT_CTRL: Key.LEFT_ALT,
    Key.LEFT_ALT: Key.LEFT_CTRL,
    Key.RIGHT_ALT: Key.RIGHT_CTRL,
})

# [Multipurpose modmap] Give a key two meanings. A normal key when pressed and
# released, and a modifier key when held down with another key. See Xcape,
# Carabiner and caps2esc for ideas and concept.
define_multipurpose_modmap({
    Key.ENTER: [Key.ENTER, Key.RIGHT_ALT],
    Key.CAPSLOCK: [Key.BACKSPACE, Key.LEFT_ALT],
    Key.LEFT_CTRL: [Key.ESC, Key.LEFT_CTRL],
    Key.RIGHT_CTRL: [Key.ESC, Key.RIGHT_CTRL],
    Key.LEFT_SHIFT: [Key.KPLEFTPAREN, Key.LEFT_SHIFT],
    Key.RIGHT_SHIFT: [Key.KPRIGHTPAREN, Key.RIGHT_SHIFT],
    Key.TAB: [Key.TAB, Key.LEFT_META]
})
