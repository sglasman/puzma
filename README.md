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

<img src="https://github.com/sglasman/puzma/blob/master/examples/readme-example.svg"/>

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

The currently implemented grid types are `RectangleGrid` and `SudokuGrid`. A grid property is a property name separated by a colon from a property value (for example, `height: 5`). Grid properties must be separated by commas. A `RectangleGrid` must have the grid properties `height` and `width` specified, giving the dimensions of the grid in cells. The other implemented grid property is `gridsize`, the side length of a grid cell in pixels, which defaults to 36 if not specified.

The shortest legal Puzma program is
```
SudokuGrid{}
```
which produces the image
<img src="https://github.com/sglasman/puzma/blob/master/examples/sudoku-example.svg"/>

### Object declarations
Object declarations are used to place objects at specified locations in the grid. Currently implemented object declarations are of the form
```
at COORDINATE CLUE
```
and
```
ThickLine [START COORDINATE, END COORDINATE]
```
Currently implemented clue types are simple text clues of the form `Clue {TEXT}`, as well as shaded cells, specified as `ShadedCell` or simply `#`. More kinds of clues will be implemented soon. The `ThickLine` declaration is for drawing a thicker line on the grid, a feature used in many puzzle types (Star Battle, Heyawake).

Puzma uses a simple, flexible coordinate system. Coordinates in parentheses `(a, b)` represent the center of cells; the convention is that `(1, 1)` is the top left cell. Coordinates in angle brackets `<a, b>` represent grid interstices; the convention is that `<0, 0>` is the top left corner of the grid. Central and interstitial coordinates can even be mixed: an object at `(a, b>` will be placed halfway along a vertical grid line between two vertices. This will be useful in typesetting puzzles such as Kropki.

There is no requirement for coordinates to refer to locations inside the grid, and indeed in many puzzle types (Skyscrapers, for example), clues are expected to appear outside the grid.

### Layout declarations
In puzzles with a sufficient density of clues, it will be inconvenient to specify each clue in its own `at` declaration. It's easier to declare a gridful of clues with a single layout declaration.

This has the form
```
GridLayout [CLUES|CLUES|...|CLUES]
```
where each row of clues is separated by a pipe `|`, and the whole lot is enclosed in square brackets. Clues are read from left to right then top to bottom. Single character clues can be written as is; multi-character clues should be enclosed in braces `{ }`. An unadorned `#` will give a shaded cell, and an underscore `_` will give an empty cell. Clues in a row may be optionally separated by commas.

As an illustration, the code
```
RectangleGrid {height:3, width:3}
GridLayout [132|_#{11}|_0#]
```
generates the image
<img src="https://github.com/sglasman/puzma/blob/master/examples/layout-example.svg"/>

In some cirumstances, you might want to declare a single row or column of clues rather than the whole grid. In this case, you can use the syntax
```
RowLayout/ColumnLayout (ROW INDEX, [CLUES])
```
In this case, either of the parentheses can be replaced with angle brackets to place the clues between rows, interstitially within a row, or both. For example, the code
```
RectangleGrid {height:5, width:4} 
RowLayout (0, [1234]) 
RowLayout (6, [567]> 
ColumnLayout (0, [ABCD]> 
ColumnLayout (5, [EFGHI])
```
generates the image
<img src="https://github.com/sglasman/puzma/blob/master/examples/rowlayout-example.svg"/>
