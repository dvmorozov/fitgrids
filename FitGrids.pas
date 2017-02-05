{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FitGrids;

interface

uses
  NumericGrid, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('NumericGrid', @NumericGrid.Register);
end;

initialization
  RegisterPackage('FitGrids', @Register);
end.
