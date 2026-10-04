# fitgrids

Grid components for Delphi and Lazarus: `TStringGrid` descendants that add
clipboard support, an edit-finished event, row and column editing, a data-source
binding, design-time cell colours, and numeric input validation.

**[dvmorozov.github.io/fitgrids](https://dvmorozov.github.io/fitgrids/)** — what
each component does, what state it is in, and the class diagram.

## Using it

The package is `package/FitGrids.lpk` for Lazarus and `package/FitGrids.dpk` for
Delphi. Open it in the IDE and compile — there are no dependencies beyond the LCL
or the VCL — then drop a grid on a form like any other component.

`examples/` is a demo application showing every grid in one window.

Written for [Fit](https://dvmorozov.github.io/fit/), and used by
[MotifMASTER](https://dvmorozov.github.io/motifmaster/) as well.

## License

MPL-2.0 - see [LICENSE](LICENSE). Every source file has said so since 2019.
The Mozilla Public License is file-level: a program under any licence, free or
commercial, may use this package, and changes to its own files stay under MPL-2.0
with their source available. [Fit](https://github.com/dvmorozov/fit), the
application it was written for, is licensed separately, under GPL-3.0-or-later.
