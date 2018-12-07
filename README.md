# dotfiles

My dotfiles are managed using [GNU Stow (`stow(8)`)](https://www.gnu.org/software/stow/).

---

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
git submodule deinit vim/.vim/pack/core/.../...
git rm vim/.vim/pack/core/.../...
rm -rf .git/modules/vim/.vim/pack/core/.../...
```

---

These config files are all dedicated to the Public Domain, without any
conditions whatsoever (unless stated otherwise).
