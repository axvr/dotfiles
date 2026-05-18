# dotfiles

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
git submodule-pull

# Remove Vim plugins
git submodule-purge vim/.vim/pack/core/.../...
```

## Legal

*No Rights Reserved.*

All source code, documentation and associated files packaged in this repository
are dedicated to the public domain, unless otherwise stated.  A full copy of
the CC0 (Creative Commons Zero v1.0 Universal) public domain dedication should
be provided in the `COPYING` file.
