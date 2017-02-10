unit ExamplesMain;

{$IFDEF Lazarus}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  NumericGrid;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
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
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
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

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  ColorStringGrid1.CopyToClipBoard;
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
  ColorStringGrid1.PasteFromClipBoard;
end;

end.

