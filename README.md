# Tools for cleaning up CG grammars

### Download

`git clone https://github.com/inariksit/cgclean`

### Install

Go to the cgclean directory and type `stack build`.

You should have the following things installed:
* Stack (https://haskellstack.org/)
* BNFC (http://bnfc.digitalgrammars.com/)

ALso 3 of my other repositories, but they will be downloaded automatically by stack.yaml. However, you will need Happy, Alex, Minisat and some other stuff. I will update this documentation as soon as I have my first non-me user.

### Usage
So now you have managed to install. Then just type the following:

`stack exec cg-clean <file>.rlx < all | compact-strings | find-repetition | sort-by-target >`

The options `compact-strings`, `find-repetition` and `sort-by-target` will be explained here soon. `All` just means "do all of these 3 things."

Output will be generated in `<file>.compact.rlx`, `<file>.repetitive.txt` and `<file>.sorted.rlx`.
    
