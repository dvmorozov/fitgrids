unit ExamplesMain;

{$IFDEF Lazarus}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, NumericGrid;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColoredGrid1: TColoredGrid;
    ColorStringGrid1: TColorStringGrid;
    DataGrid1: TDataGrid;
    GEFGrid1: TGEFGrid;
    IDAGrid1: TIDAGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
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

{ TForm1 }

end.

