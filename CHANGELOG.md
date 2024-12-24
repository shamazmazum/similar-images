# Changelog

## v0.3 -> v0.4

* Remove GTK viewer completely
* SDL viewer now has more informational and concise labels
* CLI tool now has subcommands `find` for searching and `prune` for pruning
  stale entries from a database.
* Add exhaustive search option.

## v0.2 -> v0.3

* Leaky GTK viewer is replaced by somewhat more simple SDL2 viewer by default
  (an old viewer is still accessible in `similar-images/viewer` system). Keys in
  the new viewer: `esc` for exit, `delete` for deleting a file, arrows for
  navigation.
