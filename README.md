# puzma
A language for specifying and typesetting grid-based puzzles. "Puzma" stands for **puzzle markup**.

Puzma generates an svg (scalable vector graphics) file from code, the syntax of which is designed to be readable and concise. The following code displays many of the features of the language:
```
RectangleGrid{ height:10, width: 10 }
RowLayout (0, [_2_3__4_5_])
at (3, 3) Clue{1}
at <5, 7> Clue{A}
at (6, 9) #
ThickLine [<2,9>, <4,9>]
```
It produces the image

<img src="https://github.com/sglasman/puzma/blob/master/examples/readme-example.svg" height=864 width=864/>

This project is under active development, so more features (including, hopefully, PDF support) are on their way.

## Using puzma

There is a Windows executable in the repo, which I'll keep updated as I implement new features. Alternatively, if you have [stack](haskellstack.org) installed, you should be able to clone the repo and `stack build`.

Command-line usage is `puzma input-filename [output-filename]`. If you don't supply an output filename, a default will be generated.

## Specification

What follows is a more or less comprehensive description of the language.

A Puzma program consists of the following declarations, in this order:

* A grid declaration, specifying the grid on which the puzzle is drawn;
* Optionally, a series of clue layout declarations;
* Optionally, a series of object declarations.

Whitespace (spaces, tabs and line breaks) is meaningless in Puzma, and you can put in as much or as little as you like anywhere you like.

I'll explain the first and third bullets, and then get back around to layouts.

### The grid declaration
The grid declaration is of the form

```
GRID TYPE { GRID PROPERTIES }
```

The currently implemented grid types are `RectangleGrid`, `SudokuGrid` and `SlitherlinkGrid`. A grid property is a property name separated by a colon from a property value (for example, `height: 5`). Grid properties must be separated by commas. A `RectangleGrid` must have the grid properties `height` and `width` specified, giving the dimensions of the grid in cells. The other implemented grid property is `gridsize`, the side length of a grid cell in pixels, which defaults to 36 if not specified.

The shortest legal Puzma program is
```
SudokuGrid{}
```
which produces the image

<img src="https://github.com/sglasman/puzma/blob/master/examples/sudoku-example.svg" height=792 width=792/>

### Object declarations
Object declarations are used to place objects at specified locations in the grid. Currently implemented object declarations are of the form
```
at COORDINATE CLUE
```
and
```
ThickLine [START COORDINATE, END COORDINATE]
```
A list of currently implemented clue types is as follows:
* `Clue {CONTENT}` or simply `{CONTENT}` to place a plain string clue.
* `ShadedCell` or `#` to place a shaded cell.
* `ShadedClue {CONTENT}` or `#{CONTENT}` to place a shaded cell containing a white-on-black clue.
* `SmallClue {CONTENT}` to place a tiny clue at the top left of a cell. This is necessary in TomTom, for instance.
* `TapaClue {n1, n2..}` to place a Tapa clue with one to four elements.
* `UnshadedCircle` and `ShadedCircle`.

The `ThickLine` declaration is for drawing a thicker line on the grid, a feature used in many puzzle types (Star Battle, Heyawake).

Puzma uses a simple, flexible coordinate system. Coordinates in parentheses `(a, b)` represent the center of cells; the convention is that `(1, 1)` is the top left cell. Coordinates in angle brackets `<a, b>` represent grid interstices; the convention is that `<0, 0>` is the top left corner of the grid. Central and interstitial coordinates can even be mixed: an object at `(a, b>` will be placed halfway along a vertical grid line between two vertices. This will be useful in typesetting puzzles such as Kropki.

There is no requirement for coordinates to refer to locations inside the grid, and indeed in many puzzle types (Skyscrapers, for example), clues are expected to appear outside the grid.

### Layout declarations
In puzzles with a sufficient density of clues, it will be inconvenient to specify each clue in its own `at` declaration. It's easier to declare a gridful of clues with a single layout declaration.

This has the form
```
GridLayout [CLUES|CLUES|...|CLUES]
```
where each row of clues is separated by a pipe `|`, and the whole lot is enclosed in square brackets. Clues are read from left to right then top to bottom. Single character plain clues can be written as is; multi-character clues or other kinds of clues should be enclosed in braces `{ }`. An unadorned `#` will give a shaded cell, and an underscore `_` will give an empty cell. Clues in a row may be optionally separated by commas.

As an illustration, the code
```
RectangleGrid {height:3, width:3}
GridLayout [1{TapaClue{1 3}}2|_{UnshadedCircle}{11}|_0#]
```
generates the image

<img src="https://github.com/sglasman/puzma/blob/master/examples/layout-example.svg" height=360 width=360/>

In some cirumstances, you might want to declare a single row or column of clues rather than the whole grid. In this case, you can use the syntax
```
RowLayout/ColumnLayout (ROW INDEX, [CLUES])
```
In this case, either of the parentheses can be replaced with angle brackets to place the clues between rows, interstitially within a row, or both. For example, the code
```
RectangleGrid {height:5, width:4} 
RowLayout <1, [1234]) 
RowLayout <4, [567]> 
ColumnLayout (0, [ABCD]> 
ColumnLayout (5, [EFGHI])
```
generates the image

<img src="https://github.com/sglasman/puzma/blob/master/examples/rowlayout-example.svg" height=504 width=576/>

#### The thick line layout declaration

Similarly, in a puzzle with an elaborate arrangement of thick lines, it might be excessively cumbersome to write a separate declaration for each line segment. Instead, one can write a thick line layout, whose syntax is best illustrated first with an example: the code
```
RectangleGrid {height:4, width:4}
ThickLineLayout [_i_|
                 _--_|
                 i_i|
                 -__-|
                 i_i|
                 _--_|
                 _i_]
```
generates the image

<img src="https://github.com/sglasman/puzma/blob/master/examples/thicklinelayout-example.svg" height=432 width=432/>

In any row of a grid of width `n`, there are `n - 1` short vertical line segments that may be thickened (this excludes the grid's left and right boundary lines, which are thick by default). Between any two rows, there are `n` short horizontal line segments that may be thickened. The rows (separated by `|`s) of the ThickLineLayout declaration correspond alternately to these two situations. An `i` indicates a vertical line segment to be thickened, a dash `-` indicates a horizontal line segment to be thickened, and an underscore `_` denotes a line segment left as is. The line breaks are unnecessary to Puzma, and included strictly for readability.

### More examples

In the /examples/ folder of the repository, I've included some more examples of Puzma code, which together exhibit all of the graphical features currently implemented.
