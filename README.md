# emacs-edit-python

Quick hacks to improve Python editing in Emacs.

## Adding imports

`edit-python-import-from` inserts an unqualified
"`from module import name`" for the identifier at point;

`edit-python-import-qualified` inserts a qualified "`import module`"
or "`import module as alias`". Both functions look up files of the
current project to suggest module names.

