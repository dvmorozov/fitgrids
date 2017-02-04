unit ExamplesMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, NumericGrid;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColoredGrid1: TColoredGrid;
    ColorStringGrid1: TColorStringGrid;
    DataGrid1: TDataGrid;
    GEFGrid1: TGEFGrid;
    IDAGrid1: TIDAGrid;
    NumericGrid1: TNumericGrid;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

