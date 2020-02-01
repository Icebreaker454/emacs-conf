# Changelog for Emacs configuration by Icebreaker

## v1.0.0 - Released 01.02.2020
### Added
- Helm integration
- JEDI is now the backend for Company auto-completion
- Added a hook for automatically selecting `pyenv` version and `jedi` `"--virtual-env"` server arg
to the `projectile-after-switch-project` hook.
- PyTest integration
- GO support with `go-mode`
- Lua support with `lua-mode`
- Doom-modeline

### Changed
- Command map is now `C-c p`
- Python mode configuration rewritten as a hook for python mode


### Removed
- Removed `ag` in favour of default `helm-ag`
- Anaconda-mode support is dropped in favour of Jedi.
