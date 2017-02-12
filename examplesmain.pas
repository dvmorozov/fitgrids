unit ExamplesMain;

{$IFDEF Lazarus}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  NumericGrid
{$IFDEF Lazarus}
  , LCLType
{$ENDIF}
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
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
    Label8: TLabel;
    NumericGrid1: TNumericGrid;
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure GEFGrid1GridEditingFinished(Sender: TObject; Col, Row: LongInt);
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

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
    GEFGrid1.CopyToClipBoard;
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
    ColorStringGrid1.PasteFromClipBoard;
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
begin
    GEFGrid1.PasteFromClipBoard;
end;

procedure TForm1.GEFGrid1GridEditingFinished(Sender: TObject; Col, Row: LongInt
  );
var Msg: string;
begin
    Msg := 'Editing finished. Col = ' + IntToStr(Col) +
        ', Row = ' + IntToStr(Row);
    Application.MessageBox(PChar(Msg),
        'Event handler', MB_OK or MB_ICONINFORMATION);
end;

end.

