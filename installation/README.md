# Installation

To install the compiled executable in Mint 20.1 (MATE) :

1. Create a directory named `tasmotasbacker` somewhere in the search path such as in `~/.local/bin`.
2. Into that directory, copy 
    1. the executable file `tasmotasbacker`,
    2. the image file `images/tasmotasbacker.png`.
    
3. Copy the `tasmotasbacker.desktop` file to '~/.local/share/applications'.
4. Replace `$USER` in the `Exec=` and `Icon=` lines of the copied `tasmotatsbacker.desktop` file to the correct user name. If the executable is renamed or if it is placed in a different directory than `~/.local/bin/tasmotasbacker` then the rest of those lines will also need to be adjusted.

Since my user name is `michel` on my Mint 20.1 machine the two lines in my `tasmotasbacker.desktop` file are

<pre>
Exec=/home/michel/.local/bin/tasmotasbacker/tasmotasbacker
Icon=/home/michel/.local/bin/tasmotasbacker/tasmotasbacker.png
</pre>

If need be, see [Desktop Entry Specification](https://specifications.freedesktop.org/desktop-entry-spec/latest/index.html) and  Annexe [A. Registered Categories](https://specifications.freedesktop.org/menu-spec/menu-spec-1.0.html#category-registry) to change the category or categories under which the executable will appear in the menu.
