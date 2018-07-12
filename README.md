# puzma
A language for specifying and typesetting grid-based puzzles. "Puzma" stands for **puzzle markup**.

Currently this code allows placement of string clues at locations within a rectangular grid. It generates an svg (scalable vector graphics file) from Puzma code, the syntax of which is designed to be readable and concise. For example, the code
```
RectangleGrid{ height:10, width: 10 }
at (3, 3) Clue{1}
at (5, 7) Clue{A}
```
produces the image

<img src="https://github.com/sglasman/puzma/blob/master/examples/readme-example.svg" height="432" width="432"/>

This project is under active development, so more features and richer syntax are on their way.
