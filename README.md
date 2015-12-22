[![Build Status](https://travis-ci.org/10sr/git-command-el.svg?branch=master)](https://travis-ci.org/10sr/git-command-el)



git-command.el
===============


Dead simple git interface.

This packgage provides a way to invoke Git from a command-line interface using
minibuffer.
While runnning git command, `$GIT_EDITOR` and `$GIT_PAGER` are set nicely so you
can use emacsclient to open files and get outputs.


Usage
-----

This package provides only one user command:

    M-x git-command

to interactively give git command and just invoke that command with
`$GIT_EDITOR` and `$GIT_PAGER` are set to use emacsclient.

Optionally, you can give prefix argument to create a new buffer for that git
invocation.


Completion
-----------

It is highly recommended to Install `pcmpl-git` with this package.
That enables completion when entering git command interactively.



License
--------

This software is unlicensed. For details, see `LICENSE`.
