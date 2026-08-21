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

GPL-3.0-or-later - see [LICENSE](LICENSE). Same terms as
[fit](https://github.com/dvmorozov/fit), the application this package was written
for; a repository with no license file grants no rights at all, which is not what
publishing it was for.
