# Autogenerated config.py
#
# NOTE: config.py is intended for advanced users who are comfortable
# with manually migrating the config file on qutebrowser upgrades. If
# you prefer, you can also configure qutebrowser using the
# :set/:bind/:config-* commands without having to write a config.py
# file.
#
# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

# Change the argument to True to still load settings configured via autoconfig.yml
config.load_autoconfig(True)

# ui
config.source('theme.py')
c.fonts.default_family = '"Iosevka Aile"'
c.fonts.default_size = '12pt'
c.fonts.hints = 'bold 20pt default_family'
c.fonts.completion.entry = '12pt "Iosevka Aile"'
c.fonts.debug_console = '12pt "Iosevka Aile"'
c.fonts.prompts = 'default_size Iosevka Aile'
c.fonts.statusbar = '12pt "Iosevka Aile"'
c.colors.webpage.preferred_color_scheme = "dark"
c.completion.shrink = True
c.completion.use_best_match = True
c.downloads.position = "bottom"
c.downloads.remove_finished = 10000
c.statusbar.widgets = ["progress", "keypress", "url", "history"]
c.scrolling.bar = "always"
c.tabs.position = "left"
c.tabs.title.format = "{index}: {audio}{current_title}"
c.tabs.title.format_pinned = "{index}: {audio}{current_title}"

# privacy
config.set('content.headers.user_agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36')
config.set('content.canvas_reading', False)
config.set('content.webgl', False)
config.set('content.headers.accept_language', 'en-US,en;q=0.5')
config.set('content.headers.custom', {"accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"})
config.set('content.webrtc_ip_handling_policy', 'default-public-interface-only')
config.set('content.geolocation', False)
config.set('content.cookies.accept', 'no-3rdparty')

# urls
c.url.default_page = 'about:blank'
c.url.start_pages = 'about:blank'
c.url.searchengines = {'DEFAULT': 'https://duckduckgo.com/?q={}',
                       'wk': 'https://en.wikipedia.org/wiki/{}',
                       'weba': 'https://web.archive.org/web/{}',
                       'od': 'https://odysee.com/$/search?q={}',
                       'yt': 'https://www.youtube.com/results?search_query={}',
                       'gl': 'https://gitlab.com/explore?name={}',
                       'gh': 'https://github.com/search?q={}',
                       'inv': 'https://redirect.invidious.io/search?q={}'}


c.editor.command = ['emacsclient', '+{line}:{column}', '{file}']

config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')
config.set('content.mute', True, 'https://www.youtube.com/*')

# hints
c.hints.selectors["code"] = [
    # Selects all code tags whose direct parent is not a pre tag
    ":not(pre) > code",
    "pre"
]

config.set('hints.chars', 'qwfarstxcdv')

# binding
bindings = {
    'xD': 'hint links userscript ~/.config/qutebrowser/userscripts/emacs-mpv-start.sh',
    'xd': 'hint links userscript ~/.config/qutebrowser/userscripts/emacs-mpv-start-lowres.sh',
    'xa': 'hint links userscript ~/.config/qutebrowser/userscripts/emacs-mpv-start-audio.sh',
    '[[': 'navigate decrement',
    ']]': 'navigate increment',
    '{{': 'navigate prev',
    '}}': 'navigate next',
    'cs': 'config-source',
    'ce': 'config-edit',
    ',cd': 'spawn --userscript ~/.config/qutebrowser/userscripts/org-capture-ref',
    ',cf': 'spawn --userscript ~/.config/qutebrowser/userscripts/org-capture-ref --file',
    ',cr': 'spawn --userscript ~/.config/qutebrowser/userscripts/org-capture-ref --content-reflowed',
    ',cc': 'spawn --userscript ~/.config/qutebrowser/userscripts/org-capture-ref --content-full',
    'ec': 'edit-command',
    'et': 'edit-text',
    'eu': 'edit-url',
    'w': 'tab-next',
    'q': 'tab-prev',
    'td': 'config-cycle colors.webpage.darkmode.enabled true false;; restart',
}

for key, bind in bindings.items():
    config.bind(key, bind)
    

