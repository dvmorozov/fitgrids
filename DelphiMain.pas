unit DelphiMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NumericGrid, Vcl.Grids;

type
  TForm1 = class(TForm)
    ColorStringGrid1: TColorStringGrid;
    NumericGrid1: TNumericGrid;
    GEFGrid1: TGEFGrid;
    IDAGrid1: TIDAGrid;
    DataGrid1: TDataGrid;
    ColoredGrid1: TColoredGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
