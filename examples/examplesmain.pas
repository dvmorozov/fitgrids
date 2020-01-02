unit ExamplesMain;

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
    NumericGrid
{$IFDEF Lazarus}
    , LCLType, Masks, Grids, Menus
{$ELSE}
    , Windows, Vcl.Menus, Grids
{$ENDIF};

type
    TDataSource = class(TInterfacedObject, IGridDataSource)
    public
        //  Convert data value obtained from data source into string
        //  representation. Can be used also to number rows or columns.
        function ValueToString(const ACol, ARow: longint): string;
        //  Convert string into value.
        procedure StringToValue(const ACol, ARow: longint; const AString: string);
        //  Set correct "default" value for cell during the cleaning operation.
        procedure SetValueByDefault(const ACol, ARow: longint);
        //  Return cell color and True if it should be set, otherwise False.
        function GetCellColor(const ACol, ARow: longint; var Color: TColor): boolean;
        //  возвращает True, если должен быть установлен цвет Color,
        //  в противном случае - False
        function GetCellEditMask(const ACol, ARow: longint): string;
        function GetCellEnabledCharSet(const ACol, ARow: longint): TCharSet;
        //  Return True if input for given cell is disabled.
        function IsCellDisabled(const ACol, ARow: longint): boolean;
        //  Check if given text is convertible into data source value without
        //  throwing an exception. However if cell coordinates are invalid
        //  exception is thrown.
        function IsDataValid(const ACol, ARow: longint;
            const AString: string): boolean;

        //  Check if action is possible.
        function MayIDoInsertRows(StartRow, RowsCount: longint): boolean;
        function MayIDoDeleteRows(StartRow, RowsCount: longint): boolean;
        function MayIDoAddRow: boolean;

        function MayIDoInsertColumns(StartCol, ColsCount: longint): boolean;
        function MayIDoDeleteColumns(StartCol, ColsCount: longint): boolean;
        function MayIDoAddColumn: boolean;

        function MayIDoDeleteAllData: boolean;
        function MayIDoClearSelectedArea: boolean;
        function MayIDoClearAllCells: boolean;

        procedure RowsDeleted(const StartPos, Count: longint);
        procedure RowsInserted(const StartPos, Count: longint);
        procedure RowAdded;

        procedure ColumnsDeleted(const StartPos, Count: longint);
        procedure ColumnsInserted(const StartPos, Count: longint);
        procedure ColumnAdded;

        procedure AllDataDeleted;

        //  Total number of columns including Fixed.
        function GetColCount: longint;
        //  Total number of rows including Fixed.
        function GetRowCount: longint;
        //  Number of fixed columns.
        function GetFixedCols: longint;
        //  Number of fixed rows.
        function GetFixedRows: longint;
        function GetColNumFixed: boolean;
        function GetRowNumFixed: boolean;

        function GetColWidth(const Col: longint): longint;
        procedure SaveColWidth(const Col, Width: longint);
        function GetRowHeight(const Row: longint): longint;
        procedure SaveRowHeight(const Row, Height: longint);
        //  Automatic adjusting cell widths and heights.
        function AutoWidths: boolean;
        function AutoHeights: boolean;

        function GetSelection: TGridRect;
        procedure SaveSelection(const Selection: TGridRect);
        //  The number of currently selected column.
        function GetCol: longint;
        procedure SaveCol(const Col: longint);
        //  The number of currently selected row.
        function GetRow: longint;
        procedure SaveRow(const Row: longint);
        function GetLeftCol: longint;
        procedure SaveLeftCol(const LeftCol: longint);
        function GetTopRow: longint;
        procedure SaveTopRow(const TopRow: longint);
    end;

    { TForm1 }

    TForm1 = class(TForm)
        BitBtnColorStringGridCopy: TBitBtn;
        BitBtnColorStringGridPaste: TBitBtn;
        ColoredGrid1: TColoredGrid;
        ColorStringGrid1: TColorStringGrid;
        DataGrid1: TDataGrid;
        GEFGrid1: TGEFGrid;
        IDAGrid1: TIDAGrid;
        Label1: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        MenuItemAddColumn: TMenuItem;
        MenuItemAddRow: TMenuItem;
        MenuItemDeleteRow: TMenuItem;
        MenuItemDeleteColumn: TMenuItem;
        MenuItemInsertColumn: TMenuItem;
        MenuItemInsertRow: TMenuItem;
        NumericGrid1: TNumericGrid;
        PopupMenuIDAGrid: TPopupMenu;
        procedure BitBtnColorStringGridCopyClick(Sender: TObject);
        procedure BitBtn3Click(Sender: TObject);
        procedure BitBtnColorStringGridPasteClick(Sender: TObject);
        procedure BitBtn5Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure GEFGrid1GridEditingFinished(Sender: TObject; Col, Row: longint);
        procedure MenuItemAddColumnClick(Sender: TObject);
        procedure MenuItemAddRowClick(Sender: TObject);
        procedure MenuItemDeleteRowClick(Sender: TObject);
        procedure MenuItemDeleteColumnClick(Sender: TObject);
        procedure MenuItemInsertColumnClick(Sender: TObject);
        procedure MenuItemInsertRowClick(Sender: TObject);
    private
        { private declarations }
        DataSource: TDataSource;

        procedure FillIDAGrid;
    public
        { public declarations }
    end;

var
    Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.BitBtnColorStringGridCopyClick(Sender: TObject);
begin
    ColorStringGrid1.CopyToClipBoard;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
    GEFGrid1.CopyToClipBoard;
end;

procedure TForm1.BitBtnColorStringGridPasteClick(Sender: TObject);
begin
    ColorStringGrid1.PasteFromClipBoard;
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
begin
    GEFGrid1.PasteFromClipBoard;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    NumericGrid1.ColOptions[1] := coReal;
    NumericGrid1.ColOptions[2] := coInteger;
    NumericGrid1.ColOptions[3] := coChars;
    NumericGrid1.ColOptions[4] := coText;
    NumericGrid1.ColOptions[5] := coDisabled;

    DataSource := TDataSource.Create;

    DataGrid1.SetGridDataSource(DataSource as IGridDataSource);
    ColoredGrid1.SetGridDataSource(DataSource as IGridDataSource);

    FillIDAGrid;
end;

procedure TForm1.GEFGrid1GridEditingFinished(Sender: TObject; Col, Row: longint);
var
    Msg: string;
begin
    Msg := 'Editing finished. Col = ' + IntToStr(Col) + ', Row = ' +
        IntToStr(Row);
    Application.MessageBox(PChar(Msg),
        'Event handler', MB_OK or MB_ICONINFORMATION);
end;

procedure TForm1.MenuItemAddColumnClick(Sender: TObject);
begin
    IDAGrid1.AddColumn;
end;

procedure TForm1.MenuItemAddRowClick(Sender: TObject);
begin
    IDAGrid1.AddRow;
end;

procedure TForm1.MenuItemDeleteRowClick(Sender: TObject);
begin
    IDAGrid1.DeleteRows(IDAGrid1.Row, 1);
end;

procedure TForm1.MenuItemDeleteColumnClick(Sender: TObject);
begin
    IDAGrid1.DeleteColumns(IDAGrid1.Col, 1);
end;

procedure TForm1.MenuItemInsertColumnClick(Sender: TObject);
begin
    IDAGrid1.InsertColumns(IDAGrid1.Col, 1, True);
end;

procedure TForm1.MenuItemInsertRowClick(Sender: TObject);
begin
    IDAGrid1.InsertRows(IDAGrid1.Row, 1, True);
end;

function TDataSource.ValueToString(const ACol, ARow: longint): string;
begin
    if (ACol = 1) and (ARow = 0) then
        Result := 'Col width'
    else
    if (ACol = ARow) and (ACol <> 0) then
        Result := 'Cell color'
    else
    if (ACol = 0) and (ARow = 1) then
        Result := 'Row height'
    else
    if (ACol <> 0) and (ARow <> 0) then
        Result := 'Cell text'
    else
        Result := '';
end;

{$hints off}
procedure TDataSource.StringToValue(const ACol, ARow: longint; const AString: string);
begin
end;

procedure TDataSource.SetValueByDefault(const ACol, ARow: longint);
begin

end;

function TDataSource.GetCellColor(const ACol, ARow: longint;
    var Color: TColor): boolean;
begin
    if ACol = ARow then
        Color := clYellow
    else
        Color := clAqua;
    Result := True;
end;

function TDataSource.GetCellEditMask(const ACol, ARow: longint): string;
begin
    Result := '';
end;

function TDataSource.GetCellEnabledCharSet(const ACol, ARow: longint): TCharSet;
begin
    Result := [];
end;

function TDataSource.IsCellDisabled(const ACol, ARow: longint): boolean;
begin
    Result := False;
end;

function TDataSource.IsDataValid(const ACol, ARow: longint;
    const AString: string): boolean;
begin
    Result := False;
end;

function TDataSource.MayIDoInsertRows(StartRow, RowsCount: longint): boolean;
begin
    Result := True;
end;

function TDataSource.MayIDoDeleteRows(StartRow, RowsCount: longint): boolean;
begin
    Result := True;
end;

function TDataSource.MayIDoInsertColumns(StartCol, ColsCount: longint): boolean;
begin
    Result := True;
end;

function TDataSource.MayIDoDeleteColumns(StartCol, ColsCount: longint): boolean;
begin
    Result := True;
end;
{$hints on}

function TDataSource.MayIDoAddRow: boolean;
begin
    Result := True;
end;

function TDataSource.MayIDoAddColumn: boolean;
begin
    Result := True;
end;

function TDataSource.MayIDoDeleteAllData: boolean;
begin
    Result := True;
end;

function TDataSource.MayIDoClearSelectedArea: boolean;
begin
    Result := True;
end;

function TDataSource.MayIDoClearAllCells: boolean;
begin
    Result := True;
end;

{$hints off}
procedure TDataSource.RowsDeleted(const StartPos, Count: longint);
begin

end;

procedure TDataSource.RowsInserted(const StartPos, Count: longint);
begin
end;

procedure TDataSource.ColumnsDeleted(const StartPos, Count: longint);
begin

end;

procedure TDataSource.ColumnsInserted(const StartPos, Count: longint);
begin

end;
{$hints on}

procedure TDataSource.RowAdded;
begin
end;

procedure TDataSource.ColumnAdded;
begin

end;

procedure TDataSource.AllDataDeleted;
begin

end;

function TDataSource.GetColCount: longint;
begin
    Result := 5;
end;

function TDataSource.GetRowCount: longint;
begin
    Result := 5;
end;

function TDataSource.GetFixedCols: longint;
begin
    Result := 1;
end;

function TDataSource.GetFixedRows: longint;
begin
    Result := 1;
end;

function TDataSource.GetColNumFixed: boolean;
begin
    Result := True;
end;

function TDataSource.GetRowNumFixed: boolean;
begin
    Result := True;
end;

{$hints off}
function TDataSource.GetColWidth(const Col: longint): longint;
begin
    Result := 150;
end;

procedure TDataSource.SaveColWidth(const Col, Width: longint);
begin

end;

function TDataSource.GetRowHeight(const Row: longint): longint;
begin
    Result := 20;
end;

procedure TDataSource.SaveRowHeight(const Row, Height: longint);
begin

end;
{$hints on}

function TDataSource.AutoWidths: boolean;
begin
    Result := False;
end;

function TDataSource.AutoHeights: boolean;
begin
    Result := False;
end;

function TDataSource.GetSelection: TGridRect;
begin
{$IFDEF Lazarus}
    Result := TGridRect.Create(0, 0, 0, 0);
{$ELSE}
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 0;
    Result.Bottom := 0;
{$ENDIF}
end;

{$hints off}
procedure TDataSource.SaveSelection(const Selection: TGridRect);
begin

end;
{$hints on}

function TDataSource.GetCol: longint;
begin
    //  Must return valid value in the given range.
    Result := 1;
end;

function TDataSource.GetRow: longint;
begin
    //  Must return valid value in the given range.
    Result := 1;
end;

{$hints off}
procedure TDataSource.SaveCol(const Col: longint);
begin

end;

procedure TDataSource.SaveRow(const Row: longint);
begin

end;

procedure TDataSource.SaveLeftCol(const LeftCol: longint);
begin

end;

procedure TDataSource.SaveTopRow(const TopRow: longint);
begin

end;
{$hints on}

function TDataSource.GetLeftCol: longint;
begin
    Result := 0;
end;

function TDataSource.GetTopRow: longint;
begin
    Result := 0;
end;

procedure TForm1.FillIDAGrid;
var
    i, j: longint;
begin
    for i := IDAGrid1.FixedRows to IDAGrid1.RowCount - 1 do
        for j := IDAGrid1.FixedCols to IDAGrid1.ColCount - 1 do
            IDAGrid1.Cells[j, i] :=
                '(' + IntToStr(j) + ', ' + IntToStr(i) + ')';
end;

end.
