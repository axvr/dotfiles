# dotfiles

![Image of Vim config](https://raw.githubusercontent.com/axvr/raider.vim/d692fad234334f52a2260e3da823d4eef36754cb/raider1.png)

_The above image shows my Vim configuration.  The colour scheme is
[Raider.vim](https://github.com/axvr/raider.vim) and the font is
[Inconsolata](https://levien.com/type/myfonts/inconsolata.html) by
[Raph Levien](https://levien.com/)._

These configuration files are managed using [GNU Stow (`stow(8)`)](https://www.gnu.org/software/stow/).

```sh
# Clone this repo
git clone <url> --recurse-submodules

# Restore Vim plugins
git submodule init
git submodule update

# Add Vim plugins
git submodule add -f <url> vim/.vim/pack/core/.../...

# Update Vim plugins
git submodule update --remote --merge

# Remove Vim plugins
git submodule deinit -f vim/.vim/pack/core/.../..
git rm -f !$
rm -rf .git/modules/!$
```

## Legal

*No Rights Reserved.*

All source code, documentation and associated files packaged in this repository
are dedicated to the public domain, unless otherwise stated.  A full copy of
the CC0 (Creative Commons Zero v1.0 Universal) public domain dedication should
be provided in the `COPYING` file.
