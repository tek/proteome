# Intro

**Proteome** is a [Neovim] plugin written in [Haskell] and powered by [Ribosome].

It provides project-specific configuration file loading and performs runtime tasks on projects.

# Install

The plugin can be loaded by specifying the repo to a package manager like any other, for example by cloning it in a
subdirectory of `'packpath'` or using one of the many plugin managers.

```vim
Plug 'tek/proteome'
```

Since the plugin is written in Haskell, its executable has to be fetched or built on the first start.

* If the [Nix package manager](https://nixos.org/learn.html) is available, the plugin will be fetched from the Nix cache
  (or built if the current commit isn't in the cache)
* Otherwise it will be downloaded from Github's releases.
* If the variable `g:proteome_fetch_bin` is set to `1`, Nix is ignored and the binary is downloaded from Github
  regardless.

# Overview

For the project configuration feature, Proteome determines the project type and name on startup based on several
configurable heuristics.
Based on that data, Neovim config files in `{runtimepath}/project` and `{runtimepath}/project` are sourced.

In addition to this, the plugin provides the following functionality:

* [File picking and creation](#files)
* [Grepping](#grep)
* [Tag file creation and navigation](#tags)
* [Buffer listing](#buffers)
* [Copying, moving and removing files](#file-paths)

Most of these tasks are performed in pop up menus with fuzzy matching that have common mappings:

* `j/k`: Navigate in normal mode
* `<c-j>/<c-k>`: Navigate in insert mode
* `<space>`: Toggle an item for batch processing
* `*`: Toggle all items
* `<esc>` and `<c-c>`: Quit without action
* `<m-f>`: Cycle the filter type – fuzzy, prefix, regex, substring

## Configuration

The most important configuration is read from the variable `g:proteome_project_config` with the schema:

```vim
{
  'baseDirs': ['directories'],
  'typeDirs': ['directories'],
  'projectTypes': { 'type': 'directories' },
  'typeMap': { 'type': ['types'] },
  'typeMarkers': { 'type': ['filenames'] },
  'langMap': { 'type': 'lang' },
  'langsMap': { 'lang': ['langs'] },
}
```

All parts are optional, but you probably want at least one of `baseDirs`,
`typeDirs` and `typeMarkers`.

# Project detection

**Proteome** needs to determine the name and type of your project.
The type can be arbitrarily defined for your own purposes, but mostly you
should want it to represent the project's main language.
The builtin tools can detect the type from the contents or the path of the
project.

## Project content

The project config variable's key `typeMarkers` is read as a mapping of project
type names to filename globs that are matched against the working directory.
If any of the globs match, the corresponding type will be used for the project.
A few defaults are built-in, so if you're lucky you won't have to configure
anything.

## Project path

Path based type detection uses the project config keys `baseDirs`, `typeDirs`
and `projectTypes`.
The most direct variant is `projectTypes`, which simply maps types to a list of
concrete paths.
If the working directory is among the paths, the corresponding type will be
chosen.

The `baseDirs` variant assumes that your projects are arranged as `type/.../name`
subdirectories of a common base directory.
If the working directory matches `basedir/type/.../name`, where `basedir` is one of
the directories in this config option, the path segments will be used for type
and name.

The `typeDirs` variable is similar to that, it assumes `basedir/name`, with
`basedir` taken from the values and the type from the keys in the config.

## Additional types and languages

The `typeMap`, `langMap` and `langsMap` settings can be used to add types and
languages to a project based on the main type.

# Project-specific config

On startup, Proteome loads the following sequence of files from the `project` subdirectory in your `runtimepath`:

* `all.vim`
* `type.vim`
* `type/name.vim`

In the first stage of initialization, the `project` directory is used.
After that, the second stage loads the same set of files from the
`project_after` directory.

# `ProSave`

This command is intended to be used when saving files and executes the tag
generation as well as writing the current list of buffers to disk.

An autocmd will execute it for you whenever you save a file, waiting a number
of seconds (`g:proteome_save_interval`) between executions to avoid running
multiple times when executing `:wa`.

# Files

The command `ProFiles` takes a list of directories and lists all files inside, recursively.
If no arguments were given, the current working directory is used.
The default is to use `rg`, with fallback on a builtin directory traversal algorithm.

Mappings:
* `<cr>` to edit a file
* `<tab>` to complete the longest common prefix of existing directories
* `<c-y>` to edit a new file with the path given on the prompt, creating necessary directories
* `<c-b>` to cycle the base dir for file creation
* `<c-d>` to insert the current buffer's directory
* `<c-s>` to cycle the matched segments – full, filename, directory

See the [overview](#overview) for common mappings.

When directories have been specified, one of them is treated as "active" and displayed in the status window.
This directory is used as the base path when creating new files, and it can be changed by pressing `<c-b>`.
When inserting the current buffer's dir or tab completing, the base dir is updated with the best match for the inserted
path.

Config for the built-in engine:

* `g:proteome_files_exclude_hidden` Boolean indicating whether to exclude hidden files
* `g:proteome_files_exclude_files` List of regexes to use as exclude filter for files
* `g:proteome_files_exclude_directories` List of regexes to use as exclude filter for directories

The regexes are matches against the entire path.

Config for `rg`:

* `g:proteome_files_rg_exclude` List of excludes in _ripgrep_ format (extended gitignore)

General config:

* `g:proteome_files_exclude_wildignore` Boolean indicating whether to honor the nvim option `'wildignore'`
* `g:proteome_files_use_rg` Set to `v:false` to prevent the file name collection from using _ripgrep_

**Note**: The target directories are passed to `rg` as absolute paths, so if your global git `excludesfile` specifies
any patterns with the `/foo` syntax to match files in the project root, `rg` will not apply those correctly.

# Grep

The command `ProGrep` will execute `grep` or another, configurable, program
using the specified pattern and display the results in a menu.

Mappings:
* `<space>` to mark an item
* `<cr>` to jump to a match
* `y` to yank the matching line
* `r` to start replace mode
* `d` to delete the matching line
* `<c-s>` to cycle the matched segments – full, content, path, file name, directory

See the [overview](#overview) for common mappings.

Deleting a line will compensate for resulting double empty lines.

`ProGrepIn(path, pattern)` allows you to specify the directory to search.

`ProGrepOpt(options, pattern)` allows you to specify additional options for the grep program.
For command invocation, whitespace must be escaped:

```vim
ProGrepOpt -r\ bar foo
```

`ProGrepOptIn(path, options, pattern)` combines the last two.

The function `ProGrepList(pattern, path, options)` returns a list of dictionaries with the results.

## Configuration

`g:proteome_grep_cmdline` is read as a template string that configures the
program and options to be executed.
The strings `{pattern}` and `{path}` will be replaced accordingly.
If those strings are missing, they will be appended.

Default:

```vim
let g:proteome_grep_cmdline = 'grep -Hnr {pattern} {path}'
```

The program should be configured to output vim-compatible results.
Example for `rg`:

```vim
let g:proteome_grep_cmdline = 'rg --vimgrep --no-heading --multiline'
```

## Replace mode

When starting replace mode, all marked lines are loaded into a scratch buffer.
When this buffer is saved, the modified lines will be written to the
corresponding files.

If a line is left empty, it will be deleted, along with the line taking its
place, if both surrounding lines are either whitespace or the buffer edge.

# Tags

Proteome can generate and navigate tags.

![tags menu](ops/img/tags-menu.png "The tags menu displaying results for traverse_")

## Navigation

Suggested mappings:

```vim
nnoremap <c-]> <cmd>ProNextTag<cr>
nnoremap g] <cmd>ProTag<cr>
```

### `ProNextTag [name]`

Jump to the tag for the given name or the name under the cursor if omitted.

If the command is issued repeatedly without moving the cursor, jump to the next tag of the same name.

### `ProTags [regex]`

Open a menu showing all tags matching the given regex.
See the [overview](#overview) for key mappings.

The menu extracts the package and module from the file path if possible (currently implemented for Haskell and Nix
paths).

If the regex is omitted, load all available tags.
Since a tag file often contains hundreds of thousands of entries, this may be extremely slow.

### `ProTag [name]`

Open a menu showing all tags that match the given name, or name under the cursor if omitted, exactly.
See the [overview](#overview) for key mappings.

### `ProGenTags`

This command triggers the execution of `ctags` or another tag generation tool.

It can be configured with these variables:

* `g:proteome_tags_command` The executable, like `ctags`.
* `g:proteome_tags_args` A template string for the arguments passed to the command.
* `g:proteome_tags_file_name` The final name for the tags file, default `.tags`.

To prevent the tag file from being gone while the tagger is running, the
process outputs first to a temporary file and renames it if it was successful.

The args template can contain several variable that will be filled in before
executing the command:

* `{langsComma}` A comma-separated list of project languages (default is the project type).
* `{tagFile}` The temporary file name that the command should write to.
* `{root}` The project root directory, useful when having multiple projects.

The default template, for `ctags`, is `-R --languages={langsComma} -f {tagFile} {root}`.

# Buffers

The command `ProBuffers` displays the currently loaded file buffers in MRU
order.

Mappings:
* `<space>` to mark an item
* `<cr>` to load a buffer
* `d` to delete a buffer
* `D` to delete a buffer, discarding changes
* `w` to wipe a buffer
* `W` to wipe a buffer, discarding changes

See the [overview](#overview) for common mappings.

Config:
* `g:proteome_buffers_current_last` Boolean indicating whether to display the
  active buffer at the end of the MRU list

# File paths

The commands `ProMove` and `ProCopy` move or copy the current buffer's file to
the specified location, considering whether the destination is a directory.

If the argument does not contain any slashes, the file is renamed, keeping its
containing directory.
If you want to move something to the top level directory, prefix the argument
with `./`.

If the old file name had an extension and the new one doesn't, the old
extension is added.

The special pattern that starts with a number of _N_ `^` characters causes the _Nth_ directory segment from the right to
be replaced by what follows.
For example, with a buffer path of `dir1/dir2/dir3/dir4/file`, the pattern `^^^new` will result in the path
`dir1/new/dir3/dir4/file`.

If the new file name starts with a `*` and is followed by a number of extensions, only the extensions will be renamed.
If the buffer path has more extensions than the new file name, only the matching extensions at the end are affected.
For example, with a buffer file name of `file.a.b.c`, the pattern `*.x.y` will result in `file.a.x.y`.

The command `ProRemove` moves the current file to a trash dir, located in
`$XDG_CACHE_DIR/proteome/trash`.

Convenient aliases could be:

```vim
command! -nargs=+ -complete=file Mv ProMove <args>
command! -nargs=+ -complete=file Cp ProCopy <args>
command! Rm ProRemove
```

[Neovim]: https://github.com/neovim/neovim
[Haskell]: https://www.haskell.org
[Ribosome]: https://github.com/tek/ribosome
[nix]: https://nixos.org
