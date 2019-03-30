# dotfiles

My dotfiles are managed using [GNU Stow (`stow(8)`)](https://www.gnu.org/software/stow/).

---

![My Vim set-up](https://raw.githubusercontent.com/axvr/photon.vim/images/photon1.png)

_The above image shows my Vim configuration. The colour scheme is
[photon.vim](https://github.com/axvr/photon.vim) and the font is
[Inconsolata](https://levien.com/type/myfonts/inconsolata.html) by
[Raph Levien](https://levien.com/)._

---

### Clone this repo

```sh
git clone <url> --recurse-submodules
```

### Restore Vim plugins

```sh
git submodule init
git submodule update
```

### Add Vim plugins

```sh
git submodule add -f <url> vim/.vim/pack/core/.../...
```

### Update Vim plugins

```sh
git submodule update --remote --merge
```

### Remove Vim plugins

```sh
git submodule deinit -f vim/.vim/pack/core/.../..
git rm -f !$
rm -rf .git/modules/!$
```

---

These config files are all dedicated to the Public Domain, without any
conditions whatsoever (unless stated otherwise).
