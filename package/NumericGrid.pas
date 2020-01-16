{------------------------------------------------------------------------------------------------------------------------
    This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.
    Copyright (C) Dmitry Morozov: dvmorozov@hotmail.com
                        LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
                        Facebook: https://www.facebook.com/dmitry.v.morozov
------------------------------------------------------------------------------------------------------------------------}
unit NumericGrid;

interface

uses
{$IFDEF Lazarus}
    LCLIntf, PropEdits, GraphPropEdits,
{$ELSE}
    System.Types, Windows, DesignIntf, DesignEditors, VclEditors,
{$ENDIF}
    SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Grids, ClipBrd;

var
    //  Cell colors by default.
    CL_ODD_ROW: TColor = clWhite;
    CL_EVEN_ROW: TColor = clYellow;
    CL_DISABLED_ROW: TColor = clGray;
    CL_SELECTED: TColor = $2A92B0;

    GridDataSourceGUID: TGUID = '{401B6CC0-0915-11D5-968F-8FBD7448F374}';

    MIN_HEIGHT: longint = 10;
    MIN_WIDTH: longint = 40;

const
    REAL_SET: set of AnsiChar = ['0'..'9', '.', ',', '-', '+'];
    //  Positive real numbers.
    POS_REAL_SET: set of AnsiChar = ['0'..'9', '.', ',', '+'];
    INT_SET: set of AnsiChar = ['0'..'9', '-', '+'];
    //  Positive integer numbers.
    //POS_INT_SET: set of Char = ['0'..'9', '+'];
    CHAR_SET: set of AnsiChar = ['A'..'Z', 'a'..'z'];

type
    EColorStringGrid = class(Exception);
    ENumericGrid = class(Exception);
    EIDA_Grid = class(Exception);

    TCharSet = set of AnsiChar;

    IGridDataSource = interface
        ['{401B6CC0-0915-11D5-968F-8FBD7448F374}']
        //  Convert data value obtained from data source into string
        //  representation. Can be used also to number rows or columns.
        function ValueToString(const ACol, ARow: longint): string;
        //  Convert string into value.
        procedure StringToValue(const ACol, ARow: longint; const AString: string);
        //  Set correct "default" value for cell during the cleaning operation.
        procedure SetValueByDefault(const ACol, ARow: longint);
        //  Return cell color and True if it should be set, otherwise False.
        function GetCellColor(const ACol, ARow: longint; var Color: TColor): boolean;
        function GetCellEditMask(const ACol, ARow: longint): string;
        function GetCellEnabledCharSet(const ACol, ARow: longint): TCharSet;
        //  Return True if input for given cell is disabled.
        function IsCellDisabled(const ACol, ARow: longint): boolean;
        //  Check if given text is convertible into data source value without
        //  throwing an exception. However if cell coordinates are invalid
        //  exception is thrown.
        function IsDataValid(const ACol, ARow: longint; const AString: string): boolean;

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

    TGridEditingFinished = procedure(Sender: TObject;
        //  Coordinates of edited cell.
        Col, Row: longint) of object;

    TGridModified = procedure(Sender: TObject) of object;

    TClipboardGrid = class(TStringGrid)
    protected
        function CheckingTextValidity(St: string; ACol, ARow: longint): boolean;
            virtual;

        //  Methods defined as private in the parent class.
        //  Must be redefined.
        procedure SetColCount(Value: longint); virtual;
        function GetColCount: longint; virtual;
        procedure SetRowCount(Value: longint); virtual;
        function GetRowCount: longint; virtual;

    const
        DelimiterChars: set of AnsiChar = [#9, #10, #13, ' ', ',', ';'];
        //  Maximum size of pasted data.
    const
        BufCount = 10240;

        procedure ExtractGridSizes(Buffer: array of char; const Count: longint;
            var BufferCols, BufferRows: longint);
        function ExtractString(Buffer: array of char; const Count: longint;
            var Index: longint): string;
        procedure ClearFixed;

    public
        function CopyToClipBoard: boolean; virtual;
        function PasteFromClipBoard: boolean; virtual;
        procedure EnumerateRows;

    published
        property ColCount: longint read GetColCount write SetColCount;
        property RowCount: longint read GetRowCount write SetRowCount;
    end;

    //  The grid controls exit from cell editing. At the moment
    //  of exit event of type TGridEditingFinished is generated.
    TGEFGrid = class(TClipboardGrid)
    protected
        FGridEditingFinished: TGridEditingFinished;
        FGridModified: TGridModified;
        FModified: boolean;

        //  Control exit from the table.
        procedure DoExit; override;
        //  Check if input into given cell is possible.
        function CanEditAcceptKey(Key: Char): Boolean;
            {$IFNDEF Lazarus} override; {$ELSE} virtual; {$ENDIF}
        //  Set Modified state to True if CanEditAcceptKey returns True.
        procedure KeyPress(var Key: char); override;
        //  Call EditingFinished according to Modified state.
        function SelectCell(ACol, ARow: longint): boolean; override;
        //  Call OnGridEditingFinished.
        procedure EditingFinished(
            const ACol, ARow: longint   //  Coordinates of edited cell.
            ); virtual;
        //  Call OnGridModified.
        procedure SetModified(const AModified: boolean);

    published
        //  The property shows that content of current cell has been modified.
        //  In such condition EditingFinished is called.
        //  After cell changing is set to False.
        property Modified: boolean read FModified write SetModified;

        property OnGridEditingFinished: TGridEditingFinished
            read FGridEditingFinished write FGridEditingFinished;
        property OnGridModified: TGridModified
        //  The property shows that data in the grid were modified.
            read FGridModified write FGridModified;
    end;

    TGridResizedEvent = procedure(Sender: TObject) of object;

    //  The grid implements operations of inserting, adding, deleting rows and colums
    //  as well as pasting/copying text from/in the ClipBooard (IDA = Insert, Delete, Add).
    TIDAGrid = class(TGEFGrid)
    protected
        FColNumFixed: boolean;  //  The number of columns can't be changed.
        FRowNumFixed: boolean;  //  The number of rows can't be changed.
        FChangeable: boolean;   //  Text in cells can be edited.
        //  Don't affect possibility of deleting/adding rows/columns (by default = True).
        SelFlag: boolean;       //  Is used to control selection of cells by mouse.

        StartCoord: TGridCoord;
        SavedCoord: TGridCoord;

        FOnGridResized: TGridResizedEvent;

        function CanEditModify: Boolean; {$IFNDEF Lazarus} override; {$ENDIF}
        //  Add new row when key Tab is pressed at the end of row if allowed.
        procedure KeyPress(var Key: char); override;

        procedure _InsertRows(StartRow, RowsCount: longint; Clear: boolean); virtual;
        procedure _DeleteRows(StartRow, RowsCount: longint); virtual;
        procedure _AddRow; virtual;

        procedure _InsertColumns(StartCol, ColsCount: longint; Clear: boolean); virtual;
        procedure _DeleteColumns(StartCol, ColsCount: longint); virtual;
        procedure _AddColumn; virtual;
        //  Delete all data and form empty table.
        procedure _DeleteAllData; virtual;
        //  Remove text from selected cells.
        procedure _ClearSelectedArea; virtual;
        //  Remove text from all cells.
        procedure _ClearAllCells; virtual;
        //  Clear given table region and call DataChanged.
        procedure ClearArea(const Left, Top, Right, Bottom: longint);
        //  Handle data changing.
        procedure DataChanged(const Left, Top, Right, Bottom: longint); virtual;
        //  Fill given table region with data.
        procedure FillArea(const Left, Top, Right, Bottom: longint); virtual;
        //  Fill fixed colums (row headers). Can be used, for example, for rows numeration.
        procedure FillRowHeaders; virtual;
        //  Fill fixed rows (column headers).
        procedure FillColHeaders; virtual;

    public
        constructor Create(AOwner: TComponent); override;

        function PasteFromClipBoard: boolean; override;

        procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
            X, Y: integer); override;
        procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
            X, Y: integer); override;

        //  Check if actions are possible.
        function MayIDoInsertRows(StartRow, RowsCount: longint): boolean; virtual;
        function MayIDoDeleteRows(StartRow, RowsCount: longint): boolean; virtual;
        function MayIDoAddRow: boolean; virtual;

        function MayIDoInsertColumns(StartCol, ColsCount: longint): boolean; virtual;
        function MayIDoDeleteColumns(StartCol, ColsCount: longint): boolean; virtual;
        function MayIDoAddColumn: boolean; virtual;

        function MayIDoDeleteAllData: boolean; virtual;
        function MayIDoClearSelectedArea: boolean; virtual;
        function MayIDoClearAllCells: boolean; virtual;

        procedure InsertRows(StartRow, RowsCount: longint; Clear: boolean);
        procedure DeleteRows(StartRow, RowsCount: longint);
        procedure AddRow;

        procedure InsertColumns(StartCol, ColsCount: longint; Clear: boolean);
        procedure DeleteColumns(StartCol, ColsCount: longint);
        procedure AddColumn;

        procedure DeleteAllData;        //  Delete all data and form empty table.
        procedure ClearSelectedArea;    //  Clear cells of selected region.
        procedure ClearAllCells;        //  Clear all cells of the table.

        procedure DeleteSelection;      //  Delete selected region.

        procedure SelectAll;            //  Select all editable cells.
        procedure ClearSelection;       //  Clear selection.


        procedure SetAutoColWidth(ACol: longint);
        procedure AutoColWidths;
        procedure SetAutoRowHeight(ARow: longint);
        procedure AutoRowHeights;

    published
        property DoubleBuffered;

        //  Setting up these properties to True blocks the methods changing
        //  number of columns or (and) rows. In attempting change fixed
        //  parameter the table shows error dialog. These properties don't
        //  affect changing row/column number by means of ColCount, RowCount.
        property ColNumFixed: boolean read FColNumFixed write FColNumFixed;
        property RowNumFixed: boolean read FRowNumFixed write FRowNumFixed;
        property Changeable: boolean read FChangeable write FChangeable;

        property OnGridResized: TGridResizedEvent read FOnGridResized write FOnGridResized;
    end;

    //  The grid which can call methods of class - data source
    //  and exchange data with objects of that class (class must
    //  implement special interface).
    TDataGrid = class(TIDAGrid)
    protected
        FGridDataSource: IGridDataSource;

        function GetMyGridDataSource: IGridDataSource;
        //  Firstly call DataChanged then inherited method.
        procedure EditingFinished(
            const ACol, ARow: longint   //  Coordinates of edited cell.
            ); override;
        function CanEditAcceptKey(Key: Char): Boolean;
            {$IFNDEF Lazarus} override; {$ELSE} virtual; {$ENDIF}
        //  Check if cell editing is possible
        function CanEditModify: Boolean; {$IFNDEF Lazarus} override; {$ENDIF}
        procedure _InsertRows(StartRow, RowsCount: longint; Clear: boolean); override;
        procedure _DeleteRows(StartRow, RowsCount: longint); override;
        procedure _AddRow; override;

        procedure _InsertColumns(StartCol, ColsCount: longint; Clear: boolean);
            override;
        procedure _DeleteColumns(StartCol, ColsCount: longint); override;
        procedure _AddColumn; override;

        procedure _DeleteAllData; override;
        procedure _ClearSelectedArea; override;
        procedure _ClearAllCells; override;

        procedure DataChanged(const Left, Top, Right, Bottom: longint); override;
        procedure FillArea(const Left, Top, Right, Bottom: longint); override;

        //  Set up parameters of table based on data source.
        procedure GetTableParams;
        //  Set up widths and heights of cells based on data source.
        procedure GetWidthsHeights;
        //  Save table parameters in data source including
        //  widths and heights of cells.
        procedure SaveTableParams;
        //  Fill table with data from data source (only data region).
        procedure FillTable;
        //  Fill fixed colums (row headers) based on data source.
        procedure FillRowHeaders; override;
        //  Fill fixed rows (column headers) based on data source.
        procedure FillColHeaders; override;

    public
        //  Check if action is possible by means of data source.
        function MayIDoInsertRows(StartRow, RowsCount: longint): boolean; override;
        function MayIDoDeleteRows(StartRow, RowsCount: longint): boolean; override;
        function MayIDoAddRow: boolean; override;

        function MayIDoInsertColumns(StartCol, ColsCount: longint): boolean; override;
        function MayIDoDeleteColumns(StartCol, ColsCount: longint): boolean; override;
        function MayIDoAddColumn: boolean; override;

        function MayIDoDeleteAllData: boolean; override;
        function MayIDoClearSelectedArea: boolean; override;
        function MayIDoClearAllCells: boolean; override;

        procedure HideTable;
        procedure ShowTable;

        //  Connect data source to the table and initialize table with data.
        //  Pass nil to disconnect data source.
        procedure SetGridDataSource(GridDataSource: IGridDataSource);
    end;

    TColoredGrid = class(TDataGrid)
    protected
        FOddRowColor: TColor;
        FEvenRowColor: TColor;
        FSelectedRegionColor: TColor;
        FDisabledColor: TColor;

        function GetOddRowColor: TColor; virtual;
        procedure SetOddRowColor(const AOddRowColor: TColor); virtual;
        function GetEvenRowColor: TColor; virtual;
        procedure SetEvenRowColor(const AEvenRowColor: TColor); virtual;
        function GetSelectedRegionColor: TColor; virtual;
        procedure SetSelectedRegionColor(const ASelectedRegionColor: TColor); virtual;
        function GetDisabledColor: TColor; virtual;
        procedure SetDisabledColor(const ADisabledColor: TColor); virtual;

        procedure DrawCell(ACol, ARow: longint; ARect: TRect;
            AState: TGridDrawState); override;

    public
        constructor Create(AOwner: TComponent); override;

    published
        property OddRowColor: TColor read GetOddRowColor write SetOddRowColor;
        property EvenRowColor: TColor read GetEvenRowColor write SetEvenRowColor;
        property SelectedRegionColor: TColor read GetSelectedRegionColor
            write SetSelectedRegionColor;
        property DisabledColor: TColor read GetDisabledColor write SetDisabledColor;
    end;

    TColOption = longint;

    TGetCellColorEvent = procedure(Sender: TObject; ColNum, RowNum: longint;
        var CellColor: TColor) of object;

    // Grid allows setting up colors of different types of cells at design time.
    TColorStringGrid = class(TClipboardGrid)
    protected
        FColorMatrix: array of array of TColor;
        FOddRowColor: TColor;
        FEvenRowColor: TColor;
        FSelectedRegionColor: TColor;
        FOnGetCellColor: TGetCellColorEvent;
        FColNumFixed: boolean;  //  The number of columns can't be changed.
        FRowNumFixed: boolean;  //  The number of rows can't be changed.

        procedure SetOddRowColor(Color: TColor);
        procedure SetEvenRowColor(Color: TColor);
        procedure SetSelectedRegionColor(Color: TColor);
        procedure DrawCell(ACol, ARow: longint; ARect: TRect;
            AState: TGridDrawState); override;

        function GetCellColor(const ColNum, RowNum: longint): TColor;
        procedure SetColCount(Value: longint); override;
        function GetColCount: longint; override;
        procedure SetRowCount(Value: longint); override;
        function GetRowCount: longint; override;

        function GetCellsColors(ACol, ARow: longint): TColor;
        procedure SetCellsColors(ACol, ARow: longint; AColor: TColor);

        procedure InitColorMatrix;
        procedure FinalizeColorMatrix;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure SelectAll;
        procedure ResetAll;
        procedure ClearSelection;

        property InplaceEditor;
        property CellsColors[ACol, ARow: longint]: TColor
            read GetCellsColors write SetCellsColors;

    published
        property DoubleBuffered;
        property OddRowColor: TColor read FOddRowColor write SetOddRowColor;
        property EvenRowColor: TColor read FEvenRowColor write SetEvenRowColor;
        property SelectedRegionColor: TColor read FSelectedRegionColor
            write SetSelectedRegionColor;
        property ColNumFixed: boolean read FColNumFixed write FColNumFixed;
        property RowNumFixed: boolean
        //  Setting up this property to True blocks adding row by pressing
        //  the Tab key, deleting rows and changing row number by pasting
        //  from clipboard.
            read FRowNumFixed write FRowNumFixed;
        property OnGetCellColor: TGetCellColorEvent
            read FOnGetCellColor write FOnGetCellColor;
    end;

    //  The grid allows to contol input of numbers.
    TNumericGrid = class(TColorStringGrid)
    protected
        FDisabledColor: TColor;
        ColOptArray: array of TColOption;
        SelFlag: boolean;
        StartCoord: TGridCoord;
        SavedCoord: TGridCoord;
        procedure SetColOption(index: longint; Value: TColOption);
        function GetColOption(index: longint): TColOption;
        procedure SetOptCount(AColOptCount: longint);
        procedure SetDisabledColor(Color: TColor);
        procedure SetColCount(Value: longint); override;
        function CheckingTextValidity(St: string; ACol, ARow: longint): boolean;
            override;
        function CanEditAcceptKey(Key: char): boolean;
            {$IFNDEF Lazarus} override; {$ELSE} virtual; {$ENDIF}
        procedure KeyPress(var Key: char); override;

        procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
            X, Y: integer); override;
        procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
            X, Y: integer); override;

    public
        constructor Create(AOwner: TComponent); override;

        procedure InsertRows(StartPos, Count: longint; Clear: boolean); virtual;
        procedure DeleteRows(StartPos, Count: longint); virtual;
        procedure SetColWidthByDefault(ACol: longint);
        //  Calculate and set up column widths in such manner to show all the data.
        procedure ResetColWidths;
        procedure DeleteSelection;
        destructor Destroy; override;

        property ColOptions[index: longint]: TColOption
            read GetColOption write SetColOption;

    published
        property DisabledColor: TColor read FDisabledColor write SetDisabledColor;
    end;

    //  Not implemented yet!
    //  The grid allows showing icons in cells to the left of the text.
    //  With every cell can be associated pointer to list of images and
    //  index of image which is shown at the moment.
    TIconicGrid = class(TNumericGrid)
    end;

    //  Not implemented yet!
    //  Displayed icons can be animated.
    TAnimatedGrid = class(TIconicGrid)
    end;

const
    coReal = 1000;
    coInteger = 1001;
    coText = 1002;
    coChars = 1003;
    coDisabled = 1004;

    SelectOptions: set of TGridOption =
        [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
        goRangeSelect, goDrawFocusSelected, goTabs, goThumbTracking,
        goRowSizing, goColSizing];

    EditingOptions: set of TGridOption =
        [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
        goEditing, goDrawFocusSelected, goTabs, goThumbTracking,
        goAlwaysShowEditor, goRowSizing, goColSizing];

    StaticOptions: set of TGridOption =
        [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goTabs,
        goThumbTracking, goRowSizing, goColSizing];

//  Return max. text width in the column of the grid with given number ColNum.
function GetMaxTextWidth(const Grid: TStringGrid; const ColNum: longint): longint;
//  Return max. text height in the row of the grid with given number RowNum.
function GetMaxTextHeight(const Grid: TStringGrid; const RowNum: longint): longint;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('FitGrids', [TColorStringGrid]);
    RegisterComponents('FitGrids', [TNumericGrid]);

    RegisterComponents('FitGrids', [TGEFGrid]);
    RegisterComponents('FitGrids', [TIDAGrid]);
    RegisterComponents('FitGrids', [TDataGrid]);
    RegisterComponents('FitGrids', [TColoredGrid]);

    RegisterPropertyEditor(TypeInfo(TColor), TColorStringGrid,
        'OddRowColor', TColorProperty);
    RegisterPropertyEditor(TypeInfo(TColor), TColorStringGrid,
        'EvenRowColor', TColorProperty);
    RegisterPropertyEditor(TypeInfo(TColor), TColorStringGrid,
        'SelectedRegionColor', TColorProperty);
    RegisterPropertyEditor(TypeInfo(TColor), TColorStringGrid,
        'DisabledColor', TColorProperty);
    RegisterPropertyEditor(TypeInfo(boolean), TColorStringGrid,
        'RowNumFixed', TEnumProperty);
    RegisterPropertyEditor(TypeInfo(boolean), TColorStringGrid,
        'ColNumFixed', TEnumProperty);
    RegisterPropertyEditor(TypeInfo(boolean), TColorStringGrid,
        'DoubleBuffered', TEnumProperty);
    RegisterPropertyEditor(TypeInfo(TGetCellColorEvent), TColorStringGrid,
        'OnGetCellColor', TMethodProperty);

    RegisterPropertyEditor(
        TypeInfo(TColor), TColoredGrid, 'OddRowColor', TColorProperty);
    RegisterPropertyEditor(
        TypeInfo(TColor), TColoredGrid, 'EvenRowColor', TColorProperty);
    RegisterPropertyEditor(
        TypeInfo(TColor), TColoredGrid, 'SelectedRegionColor', TColorProperty);
    RegisterPropertyEditor(
        TypeInfo(TColor), TColoredGrid, 'DisabledColor', TColorProperty);

    RegisterPropertyEditor(
        TypeInfo(boolean), TIDAGrid, 'DoubleBuffered', TEnumProperty);
    RegisterPropertyEditor(
        TypeInfo(boolean), TIDAGrid, 'RowNumFixed', TEnumProperty);
    RegisterPropertyEditor(
        TypeInfo(boolean), TIDAGrid, 'ColNumFixed', TEnumProperty);
    RegisterPropertyEditor(
        TypeInfo(boolean), TIDAGrid, 'Changeable', TEnumProperty);
    RegisterPropertyEditor(
        TypeInfo(TGridResizedEvent), TIDAGrid, 'OnGridResized', TMethodProperty);

    RegisterPropertyEditor(
        TypeInfo(TGridEditingFinished), TGEFGrid,
        'OnGridEditingFinished', TMethodProperty);
    RegisterPropertyEditor(
        TypeInfo(TGridModified), TGEFGrid,
        'OnGridModified', TMethodProperty);
end;

procedure TIDAGrid.MouseUp;
begin
    SelFlag := False;
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TIDAGrid.MouseDown;
var
    Coord: TGridCoord;
    R: TGridRect;
begin
    Coord := MouseCoord(X, Y);
    if Shift = [ssLeft, ssDouble] then
    begin
        if (Coord.X > FixedCols - 1) and (Coord.Y > FixedRows - 1) then
        begin
            //  Transition into edit mode by click.
            ClearSelection;
            Col := Coord.X;
            Row := Coord.Y;
            if CanEditModify then
                Options := EditingOptions;
        end;
    end
    else
    if Shift = [ssLeft] then
    begin
        Coord := MouseCoord(X, Y);
        if (Coord.X <= FixedCols - 1) or (Coord.Y <= FixedRows - 1) then
        begin
            Options := SelectOptions;
            if (Coord.Y <= FixedRows - 1) and (Coord.X >= FixedCols) then
                //  Column selected.
            begin
                SelFlag := True;
                StartCoord := MouseCoord(X, Y);
                SavedCoord := StartCoord;
                R.Top := FixedRows;
                R.Bottom := RowCount - 1;
                R.Left := StartCoord.X;
                R.Right := StartCoord.X;
                //  Cell must be selected before updating selection.
                Col := R.Left;
                Row := R.Top;
                Selection := R;
            end;

            if (Coord.X <= FixedCols - 1) and (Coord.Y >= FixedRows) then
                //  Row selected.
            begin
                SelFlag := True;
                StartCoord := MouseCoord(X, Y);
                SavedCoord := StartCoord;
                R.Left := FixedCols;
                R.Right := ColCount - 1;
                R.Top := StartCoord.Y;
                R.Bottom := StartCoord.Y;
                //  Cell must be selected before updating selection.
                Col := R.Left;
                Row := R.Top;
                Selection := R;
            end;

            if (Coord.X <= FixedCols - 1) and (Coord.Y <= FixedRows - 1) then
                //  All table is selected.
                SelectAll;
        end
        else
        begin
            //  Resets selected region.
            ClearSelection;
            Options := StaticOptions;
        end;
    end;
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TIDAGrid.MouseMove(Shift: TShiftState; X, Y: integer);
var
    Coord: TGridCoord;
    R: TGridRect;
begin
    if SelFlag then
    begin
        Coord := MouseCoord(X, Y);
        if (Coord.X <> SavedCoord.X) or (Coord.Y <> SavedCoord.Y) then
        begin
            if (StartCoord.Y <= FixedRows - 1) and (StartCoord.X >= FixedCols) and
                (Coord.X >= FixedCols) then
            begin
                //  Columns are selected.
                R.Top := FixedRows;
                R.Bottom := RowCount - 1;
                if Coord.X < StartCoord.X then
                begin
                    R.Left := Coord.X;
                    R.Right := StartCoord.X;
                end;
                if Coord.X > StartCoord.X then
                begin
                    R.Left := StartCoord.X;
                    R.Right := Coord.X;
                end;
                if Coord.X = StartCoord.X then
                begin
                    R.Left := StartCoord.X;
                    R.Right := StartCoord.X;
                end;
                Selection := R;
                if (Coord.X - LeftCol = VisibleColCount) and
                    (Coord.X < ColCount - 1) then
                    LeftCol := LeftCol + 1;
                if (Coord.X = LeftCol) and (LeftCol > FixedCols) then
                    LeftCol := LeftCol - 1;
            end;

            if (StartCoord.X <= FixedCols - 1) and (StartCoord.Y >= FixedRows) and
                (Coord.Y >= FixedRows) then
            begin
                //  Rows are selected.
                R.Left := FixedCols;
                R.Right := ColCount - 1;
                if Coord.Y < StartCoord.Y then
                begin
                    R.Top := Coord.Y;
                    R.Bottom := StartCoord.Y;
                end;
                if Coord.Y > StartCoord.Y then
                begin
                    R.Top := StartCoord.Y;
                    R.Bottom := Coord.Y;
                end;
                if Coord.Y = StartCoord.Y then
                begin
                    R.Top := StartCoord.Y;
                    R.Bottom := StartCoord.Y;
                end;
                Selection := R;
                if (Coord.Y - TopRow = VisibleRowCount) and
                    (Coord.Y < RowCount - 1) then
                    TopRow := TopRow + 1;
                if (Coord.Y = TopRow) and (TopRow > FixedRows) then
                    TopRow := TopRow - 1;
            end;
            SavedCoord := Coord;
        end;  //  if (Coord.X <> SavedCoord.X) or (Coord.Y <> SavedCoord.Y) then...
    end;
    inherited MouseMove(Shift, X, Y);
end;

procedure TIDAGrid.InsertRows(StartRow, RowsCount: longint; Clear: boolean);
begin
    if MayIDoInsertRows(StartRow, RowsCount) then
    begin
        _InsertRows(StartRow, RowsCount, Clear);
        if Assigned(OnGridResized) then
            OnGridResized(Self);
        if Assigned(OnGridModified) then
            OnGridModified(Self);
    end
    else
        raise EIDA_Grid.Create('Rows inserting is not allowed...');
end;

procedure TIDAGrid.DeleteRows(StartRow, RowsCount: longint);
begin
    if MayIDoDeleteRows(StartRow, RowsCount) then
    begin
        _DeleteRows(StartRow, RowsCount);
        if Assigned(OnGridResized) then
            OnGridResized(Self);
        if Assigned(OnGridModified) then
            OnGridModified(Self);
    end
    else
        raise EIDA_Grid.Create('Rows deleting is not allowed...');
end;

procedure TIDAGrid.InsertColumns(StartCol, ColsCount: longint; Clear: boolean);
begin
    if MayIDoInsertColumns(StartCol, ColsCount) then
    begin
        _InsertColumns(StartCol, ColsCount, Clear);
        if Assigned(OnGridResized) then
            OnGridResized(Self);
        if Assigned(OnGridModified) then
            OnGridModified(Self);
    end
    else
        raise EIDA_Grid.Create('Columns inserting is not allowed...');
end;

procedure TIDAGrid.DeleteColumns(StartCol, ColsCount: longint);
begin
    if MayIDoDeleteColumns(StartCol, ColsCount) then
    begin
        _DeleteColumns(StartCol, ColsCount);
        if Assigned(OnGridResized) then
            OnGridResized(Self);
        if Assigned(OnGridModified) then
            OnGridModified(Self);
    end
    else
        raise EIDA_Grid.Create('Columns deleting is not allowed...');
end;

procedure TIDAGrid.DeleteAllData;
begin
    if MayIDoDeleteAllData then
    begin
        _DeleteAllData;
        if Assigned(OnGridResized) then
            OnGridResized(Self);
        if Assigned(OnGridModified) then
            OnGridModified(Self);
    end
    else
        raise EIDA_Grid.Create('Data deleting not allowed...');
end;

procedure TIDAGrid.ClearSelectedArea;
begin
    if MayIDoClearSelectedArea then
    begin
        _ClearSelectedArea;
        if Assigned(OnGridModified) then
            OnGridModified(Self);
    end
    else
        raise EIDA_Grid.Create('Area clearing is impossible...');
end;

procedure TIDAGrid.ClearAllCells;
begin
    if MayIDoClearAllCells then
    begin
        _ClearAllCells;
        if Assigned(OnGridModified) then
            OnGridModified(Self);
    end
    else
        raise EIDA_Grid.Create('Table clearing is impossible...');
end;

procedure TIDAGrid.DeleteSelection;
begin
    //  Deleting all the table.
    //  Must be the first due to conditions.
    if (Selection.Left = FixedCols) and (Selection.Right = ColCount - 1) and
        (Selection.Top = FixedRows) and (Selection.Bottom = RowCount - 1) then
    begin
        if MayIDoDeleteAllData then
        begin
            if MessageDlg('Clear table ?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) =
                mrYes then
            begin
                DeleteAllData;
                //  Clearing is necessary because after deleting
                //  all the data at least one cell is remained.
                ClearSelection;
            end;
        end
        else
            MessageDlg('Table clearing is impossible...', mtWarning, [mbOK], 0);
        Exit;
    end;

    //  Deleting all the rows.
    if (Selection.Left = FixedCols) and (Selection.Right = ColCount - 1) then
    begin
        if MayIDoDeleteRows(Selection.Top, Selection.Bottom - Selection.Top + 1) then
        begin
            if Selection.Top <> Selection.Bottom then
                //  Confirmation is requested only if the number of
                //  rows is greater than 1.
                if MessageDlg('Delete selected rows ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) <> mrYes then
                    Exit;

            DeleteRows(Selection.Top, Selection.Bottom - Selection.Top + 1);
        end
        else
        if MayIDoClearSelectedArea then
        begin
            if MessageDlg('Clear selected area ?', mtConfirmation,
                [mbYes, mbNo, mbCancel], 0) = mrYes then
            begin
                ClearSelectedArea;
                ClearSelection;
            end;
        end
        else
            MessageDlg('Rows deleting disabled...', mtWarning, [mbOK], 0);
        Exit;
    end;

    //  Deleting all the columns.
    if (Selection.Top = FixedRows) and (Selection.Bottom = RowCount - 1) then
    begin
        if MayIDoDeleteColumns(Selection.Left, Selection.Right - Selection.Left + 1) then
        begin
            if Selection.Left <> Selection.Right then
                //  Confirmation is requested only if the number of
                //  columns is greater than 1.
                if MessageDlg('Delete selected columns ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) <> mrYes then
                    Exit;

            DeleteColumns(Selection.Left, Selection.Right - Selection.Left + 1);
        end
        else
        if MayIDoClearSelectedArea then
        begin
            if MessageDlg('Clear selected area ?', mtConfirmation,
                [mbYes, mbNo, mbCancel], 0) = mrYes then
            begin
                ClearSelectedArea;
                ClearSelection;
            end;
        end
        else
            MessageDlg('Columns deleting is disabled...', mtWarning, [mbOK], 0);
        Exit;
    end;

    //  Cleaning selected region.
    if ((Selection.Top <> FixedRows) or (Selection.Bottom <> RowCount - 1)) and
        ((Selection.Left <> FixedCols) or (Selection.Right <> ColCount - 1)) then
    begin
        if MayIDoClearSelectedArea then
        begin
            if MessageDlg('Clear selected area ?', mtConfirmation,
                [mbYes, mbNo, mbCancel], 0) = mrYes then
            begin
                ClearSelectedArea;
                ClearSelection;
            end;
        end
        else
            MessageDlg('Clearing is disabled...', mtWarning, [mbOK], 0);
    end;
end;

procedure TColoredGrid.DrawCell(ACol, ARow: longint; ARect: TRect;
    AState: TGridDrawState);

    {$hints off}
    function GetColorByDefault(ACol, ARow: longint): TColor;
    begin
        if Odd(ARow) then
            Result := OddRowColor
        else
            Result := EvenRowColor;
    end;

    {$hints on}

var
    SaveColor, TempColor: TColor;
    X, Y: integer;
begin
    if Assigned(OnDrawCell) then
        OnDrawCell(Self, ACol, ARow, ARect, AState)
    else
    begin
        SaveColor := Canvas.Brush.Color;
        if not (gdFixed in AState) then
            //  Fixed cell are displayed by default.
        begin
            //  Inherited method is called to draw cell borders
            //  by the default way.
            inherited DrawCell(ACol, ARow, ARect, AState);

            if (gdSelected in AState) or (gdFocused in AState) then
                //  The cell belongs to selected region.
                Canvas.Brush.Color := SelectedRegionColor
            else
            if GetMyGridDataSource <> nil then
            begin
                TempColor := clDefault;
                if GetMyGridDataSource.GetCellColor(ACol, ARow, TempColor) then
                    //  Data source object set up a new color.
                    Canvas.Brush.Color := TempColor
                else
                //  Data source object don't propose a special color.
                //  So apply default color.
                if GetMyGridDataSource.IsCellDisabled(ACol, ARow) then
                    Canvas.Brush.Color := DisabledColor
                else
                    Canvas.Brush.Color := GetColorByDefault(ACol, ARow);
            end
            else
                Canvas.Brush.Color := GetColorByDefault(ACol, ARow);

            Inc(ARect.Left);
            Dec(ARect.Right);
            Inc(ARect.Top);
            Dec(ARect.Bottom);
            Canvas.FillRect(ARect);
            X := ARect.Right - Canvas.TextWidth(Cells[ACol, ARow]) - 2;
            Y := ARect.Bottom - Canvas.TextHeight(Cells[ACol, ARow]) - 2;
            Canvas.TextRect(ARect, X, Y, Cells[ACol, ARow]);
        end
        else
            inherited DrawCell(ACol, ARow, ARect, AState);
        Canvas.Brush.Color := SaveColor;
    end;
end;

constructor TColoredGrid.Create;
begin
    inherited Create(AOwner);
    OddRowColor := CL_ODD_ROW;
    EvenRowColor := CL_EVEN_ROW;
    SelectedRegionColor := CL_SELECTED;
end;

procedure TDataGrid.SetGridDataSource(GridDataSource: IGridDataSource);
begin
    //  If new and old data sources are the same then
    //  parameters aren't saved because the number of
    //  rows or columns can be changed.
    if (FGridDataSource <> nil) and (GridDataSource <> FGridDataSource) then
        SaveTableParams;

    FGridDataSource := GridDataSource;
    if GridDataSource <> nil then
    begin
        GetTableParams;
        ShowTable;

        FillRowHeaders;
        FillColHeaders;
        FillTable;

        //  Must be after table filling to properly
        //  calculate widths and heights of cells.
        GetWidthsHeights;

        //  By default in assigning passive data source
        //  text in cells can be edited.
        Changeable := True;
    end
    else
        HideTable;
end;

procedure TDataGrid._DeleteAllData;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            AllDataDeleted;

    //  In the case of exception in above lines the table remains unchanged.
    inherited;
end;

procedure TDataGrid._ClearSelectedArea;
begin
    inherited;
    with Selection do
        DataChanged(Left, Top, Right, Bottom);
end;

procedure TDataGrid._ClearAllCells;
begin
    inherited;
    DataChanged(FixedCols, FixedRows, ColCount - 1, RowCount - 1);
end;

procedure TDataGrid._InsertRows(StartRow, RowsCount: longint; Clear: boolean);
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            RowsInserted(StartRow, RowsCount);

    //  In the case of exception in above lines the table remains unchanged.
    inherited;
end;

procedure TDataGrid._AddRow;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            RowAdded;

    //  In the case of exception in above lines the table remains unchanged.
    inherited;
end;

procedure TDataGrid._DeleteRows(StartRow, RowsCount: longint);
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            RowsDeleted(StartRow, RowsCount);

    //  In the case of exception in above lines the table remains unchanged.
    inherited;
end;

procedure TDataGrid._InsertColumns(StartCol, ColsCount: longint; Clear: boolean);
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            ColumnsInserted(StartCol, ColsCount);

    //  In the case of exception in above lines the table remains unchanged.
    inherited;
end;

function TDataGrid.GetMyGridDataSource: IGridDataSource;
begin
    if not (csDestroying in ComponentState) then
        Result := FGridDataSource
    else
        Result := nil;
end;

procedure TDataGrid._AddColumn;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            ColumnAdded;

    //  In the case of exception in above lines the table remains unchanged.
    inherited;
end;

procedure TDataGrid._DeleteColumns(StartCol, ColsCount: longint);
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            ColumnsDeleted(StartCol, ColsCount);

    //  In the case of exception in above lines the table remains unchanged.
    inherited;
end;

function TColoredGrid.GetOddRowColor: TColor;
begin
    Result := FOddRowColor;
end;

procedure TColoredGrid.SetOddRowColor(const AOddRowColor: TColor);
begin
    FOddRowColor := AOddRowColor;
end;

function TColoredGrid.GetEvenRowColor: TColor;
begin
    Result := FEvenRowColor;
end;

procedure TColoredGrid.SetEvenRowColor(const AEvenRowColor: TColor);
begin
    FEvenRowColor := AEvenRowColor;
end;

function TColoredGrid.GetSelectedRegionColor: TColor;
begin
    Result := FSelectedRegionColor;
end;

procedure TColoredGrid.SetSelectedRegionColor(const ASelectedRegionColor: TColor);
begin
    FSelectedRegionColor := ASelectedRegionColor;
end;

function TColoredGrid.GetDisabledColor: TColor;
begin
    Result := FDisabledColor;
end;

procedure TColoredGrid.SetDisabledColor(const ADisabledColor: TColor);
begin
    FDisabledColor := ADisabledColor;
end;

function TIDAGrid.PasteFromClipBoard: boolean;
var
    Count: longint;
    Buffer: array[0..BufCount] of char;
    St: string;
    Index: longint;
    BufferColCount, BufferRowCount: longint;
    TempCol, TempRow: longint;
    i, j: longint;
    SavedSelection, InsertedArea: TGridRect;
    SelectionSize: longint;
begin
    Result := False;
    if not Clipboard.HasFormat(CF_TEXT) then
    begin
        MessageDlg('Clipboard doesn''t contain text data...', mtError, [mbOK], 0);
        Exit;
    end;
    if MessageDlg('Overwrite this data ?', mtWarning, [mbYes, mbNo, mbCancel], 0) <>
        mrYes then
        Exit;

    Count := ClipBoard.GetTextBuf(@Buffer, BufCount);

    BufferColCount := 0;
    BufferRowCount := 0;
    ExtractGridSizes(Buffer, Count, BufferColCount, BufferRowCount);

    //  Coordinates must be saved during paste operation.
    SavedSelection := Selection;

    SelectionSize := SavedSelection.Bottom - SavedSelection.Top + 1;
    if (BufferRowCount > SelectionSize) and (not RowNumFixed) then
        InsertRows(SavedSelection.Bottom, BufferRowCount - SelectionSize, True);

    SelectionSize := SavedSelection.Right - SavedSelection.Left + 1;
    if (BufferColCount > SelectionSize) and (not ColNumFixed) then
        InsertColumns(SavedSelection.Right, BufferColCount - SelectionSize, True);

    Index := 0;
    try
        for j := 0 to BufferRowCount - 1 do
            for i := 0 to BufferColCount - 1 do
            begin
                St := ExtractString(Buffer, Count, Index);
                TempCol := SavedSelection.Left + i;
                TempRow := SavedSelection.Top + j;
                if (TempCol <= ColCount - 1) and (TempRow <= RowCount - 1) then
                    Cells[TempCol, TempRow] := St;
            end;
    except
        MessageDlg('Vague number of cell, since' + 'data do not have tabular format...',
            mtError, [mbOK], 0);
        Exit;
    end;

    //  Calculation of coordinates of pasted region.
    InsertedArea.Left := SavedSelection.Left;
    InsertedArea.Top := SavedSelection.Top;
    if ColCount - 1 < SavedSelection.Left + BufferColCount - 1 then
        InsertedArea.Right := ColCount - 1
    else
        InsertedArea.Right := SavedSelection.Left + BufferColCount - 1;

    if RowCount - 1 < SavedSelection.Top + BufferRowCount - 1 then
        InsertedArea.Bottom := RowCount - 1
    else
        InsertedArea.Bottom := SavedSelection.Top + BufferRowCount - 1;

    Selection := InsertedArea;  //  Pasted data are selected.

    //  Checking for data validity.
    with Selection do
        DataChanged(Left, Top, Right, Bottom);
    if Assigned(OnGridModified) then
        OnGridModified(Self);
    Result := True;
end;

procedure TIDAGrid.SelectAll;
var
    R: TGridRect;
begin
    R.Left := FixedCols;
    R.Right := ColCount - 1;
    R.Top := FixedRows;
    R.Bottom := RowCount - 1;
    Selection := R;
end;

procedure TIDAGrid.ClearSelection;
var
    R: TGridRect;
begin
    R.Left := Col;
    R.Right := Col;
    R.Top := Row;
    R.Bottom := Row;
    Selection := R;
end;

procedure TIDAGrid.KeyPress(var Key: char);
begin
    inherited KeyPress(Key);
    (* TODO: It is necessary to design callback to permit adding new row.
    if Key = #9 then
        if (not RowNumFixed) and (Col = 1) and (Row = 1) then
        begin
            AddRow;
            Col := 1;
            Row := RowCount - 1;
        end;
    *)
end;

procedure TIDAGrid.AddRow;
begin
    if MayIDoAddRow then
    begin
        _AddRow;
        if Assigned(OnGridResized) then
            OnGridResized(Self);
        if Assigned(OnGridModified) then
            OnGridModified(Self);
    end
    else
        raise EIDA_Grid.Create('Row adding is impossible...');
end;

procedure TIDAGrid.AddColumn;
begin
    if MayIDoAddColumn then
    begin
        _AddColumn;
        if Assigned(OnGridResized) then
            OnGridResized(Self);
        if Assigned(OnGridModified) then
            OnGridModified(Self);
    end
    else
        raise EIDA_Grid.Create('Column adding is impossible...');
end;

procedure TDataGrid.ShowTable;
begin
    Enabled := True;
    Color := clWindow;
end;

procedure TDataGrid.HideTable;
begin
    RowCount := FixedRows + 1;
    ColCount := FixedCols + 1;
    Cells[1, 1] := '';
    Enabled := False;
    Color := clLtGray;
end;

function TNumericGrid.CanEditAcceptKey(Key: char): boolean;
begin
    if Key >= #20 then

        case ColOptions[Col] of
            coReal:
                if CharInSet(Key, REAL_SET) then
                    Result := True
                else
                    Result := False;

            coInteger:
                if CharInSet(Key, INT_SET) then
                    Result := True
                else
                    Result := False;

            coChars:
                if CharInSet(Key, CHAR_SET) then
                    Result := True
                else
                    Result := False;

            coText: Result := True;

            coDisabled: Result := False;

            else
                Result := True;
        end
    //  Special characters processing.
    else
        Result := True;
end;

procedure TNumericGrid.SetColOption(Index: longint; Value: TColOption);
var
    i: longint;
begin
    if Assigned(ColOptarray) then
    begin
        if (Index < 0) or (Index >= Length(ColOptarray)) then
            raise ENumericGrid.Create('Invalid option index...')
        else
            ColOptArray[Index] := Value;
        if Value = coDisabled then
        begin
{$IFNDEF Lazarus}
            TabStops[Index] := False;
{$ENDIF}
            if Assigned(FColorMatrix) then
                for i := 0 to RowCount - 1 do
                    CellsColors[Index, i] := DisabledColor;
        end;
    end;
end;

function TNumericGrid.GetColOption(Index: longint): TColOption;
begin
    if (Index < 0) or (Index >= Length(ColOptarray)) then
        raise ENumericGrid.Create('Invalid option index...')
    else
        Result := ColOptarray[Index];
end;

procedure TNumericGrid.MouseUp;
begin
    SelFlag := False;
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TNumericGrid.MouseDown;
var
    Coord: TGridCoord;
    R: TGridRect;
begin
    inherited MouseDown(Button, Shift, X, Y);

    Coord := MouseCoord(X, Y);
{$ifndef lazarus}
    //  In Lazarus this is achieved by turning on the AutoEdit flag.
    if Shift = [ssLeft, ssDouble] then
    begin
        if (Coord.X > FixedCols - 1) and (Coord.Y > FixedRows - 1) then
        begin
            R.Left := Coord.X;
            R.Right := Coord.X;
            R.Top := Coord.Y;
            R.Bottom := Coord.Y;
            Selection := R;
            Col := Coord.X;
            Row := Coord.Y;
            if goEditing in Options then
                EditorMode := True;
        end;
    end
    else
{$endif}
    //  This works if goRangeSelect is turned on.
    if Shift = [ssLeft] then
    begin
        Coord := MouseCoord(X, Y);
        if (Coord.X <= FixedCols - 1) or (Coord.Y <= FixedRows - 1) then
        begin
            EditorMode := False;

            if (Coord.Y <= FixedRows - 1) and (Coord.X >= FixedCols) then
                //  Column selected.
            begin
                SelFlag := True;
                StartCoord := MouseCoord(X, Y);
                SavedCoord := StartCoord;
                R.Top := FixedRows;
                R.Bottom := RowCount - 1;
                R.Left := StartCoord.X;
                R.Right := StartCoord.X;
                //  Cell must be selected before updating selection.
                Col := R.Left;
                Row := R.Top;
                Selection := R;
            end;
            if (Coord.X <= FixedCols - 1) and (Coord.Y >= FixedRows) then
                //  Row selected.
            begin
                SelFlag := True;
                StartCoord := MouseCoord(X, Y);
                SavedCoord := StartCoord;
                R.Left := FixedCols;
                R.Right := ColCount - 1;
                R.Top := StartCoord.Y;
                R.Bottom := StartCoord.Y;
                //  Cell must be selected before updating selection.
                Col := R.Left;
                Row := R.Top;
                Selection := R;
            end;
            if (Coord.X <= FixedCols - 1) and (Coord.Y <= FixedRows - 1) then
                //   All table selected.
            begin
                R.Left := FixedCols;
                R.Right := ColCount - 1;
                R.Top := FixedRows;
                R.Bottom := RowCount - 1;
                Selection := R;
            end;
        end
{$ifndef lazarus}
        else
            //  Reset of selected area.
        begin
            R.Left := Coord.X;
            R.Right := Coord.X;
            R.Top := Coord.Y;
            R.Bottom := Coord.Y;
            Selection := R;
            EditorMode := False;
        end;
{$endif}
    end;
end;

procedure TNumericGrid.MouseMove(Shift: TShiftState; X, Y: integer);
var
    Coord: TGridCoord;
    R: TGridRect;
begin
    if SelFlag then
    begin
        Coord := MouseCoord(X, Y);
        if (Coord.X <> SavedCoord.X) or (Coord.Y <> SavedCoord.Y) then
        begin
            if (StartCoord.Y <= FixedRows - 1) and (StartCoord.X >= FixedCols) and
                (Coord.X >= FixedCols) then    //  Columns are selected.
            begin
                R.Top := FixedRows;
                R.Bottom := RowCount - 1;
                if Coord.X < StartCoord.X then
                begin
                    R.Left := Coord.X;
                    R.Right := StartCoord.X;
                end;
                if Coord.X > StartCoord.X then
                begin
                    R.Left := StartCoord.X;
                    R.Right := Coord.X;
                end;
                if Coord.X = StartCoord.X then
                begin
                    R.Left := StartCoord.X;
                    R.Right := StartCoord.X;
                end;
                Selection := R;
                if (Coord.X - LeftCol = VisibleColCount) and
                    (Coord.X < ColCount - 1) then
                    LeftCol := LeftCol + 1;
                if (Coord.X = LeftCol) and (LeftCol > FixedCols) then
                    LeftCol := LeftCol - 1;
            end;
            if (StartCoord.X <= FixedCols - 1) and (StartCoord.Y >= FixedRows) and
                (Coord.Y >= FixedRows) then    //  Rows are selected.
            begin
                R.Left := FixedCols;
                R.Right := ColCount - 1;
                if Coord.Y < StartCoord.Y then
                begin
                    R.Top := Coord.Y;
                    R.Bottom := StartCoord.Y;
                end;
                if Coord.Y > StartCoord.Y then
                begin
                    R.Top := StartCoord.Y;
                    R.Bottom := Coord.Y;
                end;
                if Coord.Y = StartCoord.Y then
                begin
                    R.Top := StartCoord.Y;
                    R.Bottom := StartCoord.Y;
                end;
                Selection := R;
                if (Coord.Y - TopRow = VisibleRowCount) and
                    (Coord.Y < RowCount - 1) then
                    TopRow := TopRow + 1;
                if (Coord.Y = TopRow) and (TopRow > FixedRows) then
                    TopRow := TopRow - 1;
            end;
            SavedCoord := Coord;
        end;{if (Coord.X <> SavedCoord.X) or (Coord.Y <> SavedCoord.Y) then...}
    end;
    inherited MouseMove(Shift, X, Y);
end;

procedure TNumericGrid.InsertRows(StartPos, Count: longint; Clear: boolean);
var
    i, j: longint;
begin
    RowCount := RowCount + Count;
    for i := RowCount - 1 downto StartPos + Count do
        for j := 0 to ColCount - 1 do
            Cells[j, i] := Cells[j, i - Count];
    if Clear then
        for i := 0 to Count - 1 do
            for j := 0 to ColCount - 1 do
                Cells[j, StartPos + i] := '';
end;

procedure TNumericGrid.DeleteRows(StartPos, Count: longint);
var
    i, j: longint;
begin
    for i := StartPos to RowCount - 1 - Count do
        for j := 0 to ColCount - 1 do
            Cells[j, i] := Cells[j, i + Count];
    RowCount := RowCount - Count;
end;

procedure TNumericGrid.DeleteSelection;
var
    i, j: longint;
begin
    if (Selection.Left = FixedCols) and (Selection.Right = ColCount - 1) then
        //    Deleting all the rows of table.
    begin
        if not RowNumFixed then
        begin
            if Selection.Top <> Selection.Bottom then
                if MessageDlg('Delete all selected rows ?', mtWarning,
                    [mbYes, mbNo, mbCancel], 0) <> mrYes then
                    Exit;
            for i := 0 to RowCount - 2 - Selection.Bottom do
                for j := 1 to ColCount - 1 do
                    Cells[j, Selection.Top + i] := Cells[j, Selection.Bottom + i + 1];
            RowCount := RowCount - (Selection.Bottom - Selection.Top + 1);
        end
        else
            MessageDlg('Rows deleting is not allowed...', mtWarning, [mbOK], 0);
        ClearSelection;
        Exit;
    end;

    if (Selection.Top = FixedRows) and (Selection.Bottom = RowCount - 1) then
        //    Deleting all the columns of table.
    begin
        if not ColNumFixed then
        begin
            if Selection.Left <> Selection.Right then
                if MessageDlg('Delete all selected columns ?', mtWarning,
                    [mbYes, mbNo, mbCancel], 0) <> mrYes then
                    Exit;
            for i := 0 to ColCount - 2 - Selection.Right do
                for j := 1 to RowCount - 1 do
                    Cells[Selection.Left + i, j] := Cells[Selection.Right + i + 1, j];
            ColCount := ColCount - (Selection.Right - Selection.Left + 1);
        end
        else
            MessageDlg('Columns deleting is not allowed...', mtWarning, [mbOK], 0);
        ClearSelection;
        Exit;
    end;

    if ((Selection.Top <> FixedRows) or (Selection.Bottom <> RowCount - 1)) and
        ((Selection.Left <> FixedCols) or (Selection.Right <> ColCount - 1)) then
        //    Cleaning of selected region.
    begin
        if Selection.Top <> Selection.Bottom then
            if MessageDlg('Clear all selected cells ?', mtWarning,
                [mbYes, mbNo, mbCancel], 0) <> mrYes then
                Exit;
        for i := Selection.Top to Selection.Bottom do
            for j := Selection.Left to Selection.Right do
                Cells[j, i] := '';
        ClearSelection;
    end;
end;

procedure TColorStringGrid.DrawCell(ACol, ARow: longint; ARect: TRect;
    AState: TGridDrawState);
var
    SaveColor: TColor;
    X, Y: integer;
begin
    SaveColor := Canvas.Brush.Color;
    if not (gdFixed in AState) then
    begin
        //  Inherited method is called to draw cell borders
        //  by the default way.
        inherited DrawCell(ACol, ARow, ARect, AState);
        //  Redrawing content of the cell.
        if (gdSelected in AState) or (gdFocused in AState) then
            Canvas.Brush.Color := SelectedRegionColor
        else
            Canvas.Brush.Color := GetCellColor(ACol, ARow);

        if goHorzLine in Options then
        begin
            ARect.Left := ARect.Left + GridLineWidth;
            ARect.Bottom := ARect.Bottom - GridLineWidth;
        end;
        if goVertLine in Options then
        begin
            ARect.Top := ARect.Top + GridLineWidth;
            ARect.Right := ARect.Right - GridLineWidth;
        end;
        Canvas.FillRect(ARect);
        X := ARect.Right - Canvas.TextWidth(Cells[ACol, ARow]) - 2;
        Y := ARect.Bottom - Canvas.TextHeight(Cells[ACol, ARow]) - 2;
        Canvas.TextRect(ARect, X, Y, Cells[ACol, ARow]);

        if Assigned(OnDrawCell) then
            OnDrawCell(Self, ACol, ARow, ARect, AState);
    end
    else
        inherited DrawCell(ACol, ARow, ARect, AState);
    Canvas.Brush.Color := SaveColor;
end;

function TColorStringGrid.GetCellColor(const ColNum, RowNum: longint): TColor;
var
    CellColor: TColor;
begin
    if Assigned(OnGetCellColor) then
    begin
        CellColor := clDefault;
        OnGetCellColor(Self, ColNum, RowNum, CellColor);
        Result := CellColor;
    end
    else
    if Assigned(FColorMatrix) then
        Result := CellsColors[ColNum, RowNum]
    else
    if Odd(RowNum) then
        Result := OddRowColor
    else
        Result := EvenRowColor;
end;

constructor TColorStringGrid.Create;
begin
    inherited Create(AOwner);
    if csDesigning in ComponentState then
    begin
        OddRowColor := clWhite;
        EvenRowColor := clYellow;
        SelectedRegionColor := $0064CCEA - $003A3A3A;
    end;
end;

destructor TColorStringGrid.Destroy;
begin
    FinalizeColorMatrix;
    inherited;
end;

function TColorStringGrid.GetCellsColors(ACol, ARow: longint): TColor;
begin
    if (ACol < 0) or (ACol >= ColCount) then
        raise EColorStringGrid.Create('Invalid column number...');
    if (ARow < 0) or (ARow >= RowCount) then
        raise EColorStringGrid.Create('Invalid row number...');
    Result := FColorMatrix[ARow, ACol];
end;

procedure TColorStringGrid.SetCellsColors(ACol, ARow: longint; AColor: TColor);
begin
    if (ACol < 0) or (ACol >= ColCount) then
        raise EColorStringGrid.Create('Invalid column number...');
    if (ARow < 0) or (ARow >= RowCount) then
        raise EColorStringGrid.Create('Invalid row number...');
    FColorMatrix[ARow, ACol] := AColor;
end;

constructor TNumericGrid.Create;
begin
    inherited Create(AOwner);
    if csDesigning in ComponentState then
        DisabledColor := clGray;
end;

procedure TColorStringGrid.SetOddRowColor(Color: TColor);
var
    i, j: longint;
begin
    FOddRowColor := Color;
    if Assigned(FColorMatrix) then
    begin
        for i := 0 to Length(FColorMatrix) - 1 do
            for j := 0 to Length(FColorMatrix[i]) - 1 do
                if Odd(i) then
                    FColorMatrix[i, j] := Color;
    end;
end;

procedure TColorStringGrid.SetEvenRowColor(Color: TColor);
var
    i, j: longint;
begin
    FEvenRowColor := Color;
    if Assigned(FColorMatrix) then
    begin
        for i := 0 to Length(FColorMatrix) - 1 do
            for j := 0 to Length(FColorMatrix[i]) - 1 do
                if not Odd(i) then
                    FColorMatrix[i, j] := Color;
    end;
end;

procedure TColorStringGrid.SetSelectedRegionColor(Color: TColor);
begin
    FSelectedRegionColor := Color;
    Repaint;
end;

procedure TNumericGrid.SetDisabledColor(Color: TColor);
var
    i, j: longint;
begin
    FDisabledColor := Color;
    if Assigned(FColorMatrix) and Assigned(ColOptArray) then
    begin
        for i := 0 to RowCount - 1 do
            if ColCount = Length(ColOptArray) then
                for j := 0 to ColCount - 1 do
                    if ColOptArray[j] = coDisabled then
                        CellsColors[j, i] := Color;
    end;
    Repaint;
end;

procedure TClipboardGrid.SetColCount(Value: longint);
begin
    TStringGrid(Self).ColCount := Value;
end;

function TClipboardGrid.GetColCount: longint;
begin
    Result := TStringGrid(Self).ColCount;
end;

procedure TClipboardGrid.SetRowCount(Value: longint);
begin
    TStringGrid(Self).RowCount := Value;
end;

function TClipboardGrid.GetRowCount: longint;
begin
    Result := TStringGrid(Self).RowCount;
end;

procedure TClipboardGrid.EnumerateRows;
var
    i: longint;
begin
    if FixedCols <> 0 then
        for i := FixedRows to RowCount - 1 do
            Cells[0, i] := IntToStr(i);
end;

function TClipboardGrid.CopyToClipBoard: boolean;
var
    St, St2, St3: string;
    i, j: longint;
begin
    Result := False;
    with Selection do
    begin
        if (Top = Bottom) and (Left = Right) then
        begin
            MessageDlg('Choose area for copying...', mtWarning, [mbOK], 0);
            Exit;
        end;
        try
            St := '';
            for i := Top to Bottom do
            begin
                St2 := '';
                for j := Left to Right do
                begin
                    if j <> Right then
                        St3 := Cells[j, i] + #9
                    else
                        St3 := Cells[j, i];
                    St2 := St2 + St3;
                end;
                St := St + St2 + #13#10;
            end;
            ClipBoard.SetTextBuf(PChar(St));
        except
            Exit
        end;
    end;{With Selection do...}
    Result := True;
end;

{$hints off}
function TClipboardGrid.CheckingTextValidity(St: string; ACol, ARow: longint): boolean;
begin
    Result := True;
end;

{$hints on}

procedure TClipboardGrid.ExtractGridSizes(Buffer: array of char;
    const Count: longint; var BufferCols, BufferRows: longint);
var
    i: longint;
    Flag, PrevIsDelimiter: boolean;
begin
    BufferCols := 0;
    BufferRows := 0;
    Flag := True;
    PrevIsDelimiter := False;
    for i := 0 to Count - 1 do
    begin
        if CharInSet(Buffer[i], DelimiterChars) then
        begin
            if Flag and not PrevIsDelimiter then
                Inc(BufferCols);
            PrevIsDelimiter := True;
        end
        else
            PrevIsDelimiter := False;

        if Buffer[i] = #13 then
            Flag := False;
        if Buffer[i] = #10 then
        begin
            Flag := False;
            Inc(BufferRows);
        end;
    end;
end;

function TClipboardGrid.ExtractString(Buffer: array of char;
    const Count: longint; var Index: longint): string;
var
    St: string;
    i, j, k: longint;
const
    BadSymbols: set of AnsiChar = [#10, #13];
begin
    St := '';
    for i := Index to Count - 1 do
    begin
        if CharInSet(Buffer[i], DelimiterChars) then
        begin
            for j := Index to i - 1 do
            begin
                if not CharInSet(Buffer[j], BadSymbols) then
                begin
                    St := St + Buffer[j];
                end;
            end;
            j := i;
            if Buffer[j] = #9 then
                k := j + 1
            else
                for k := j to Count - 1 do
                    if not CharInSet(Buffer[k], DelimiterChars) then
                        Break;
            Index := k;
            Result := St;
            Exit;
        end;
    end;
    Result := St;
end;

procedure TClipboardGrid.ClearFixed;
var
    i, j: longint;
begin
    for i := 0 to FixedRows - 1 do
        for j := 0 to ColCount - 1 do
            Cells[j, i] := '';
    for i := 0 to FixedCols - 1 do
        for j := FixedRows to RowCount - 1 do
            Cells[i, j] := '';
end;

function TClipboardGrid.PasteFromClipBoard: boolean;
var
    Count: longint;
    Buffer: array[0..BufCount] of char;
    St: string;
    Index: longint;
    BufferColCount, BufferRowCount: longint;
    TempCol, TempRow: longint;
    i, j: longint;
begin
    Result := False;
    BufferColCount := 0;
    BufferRowCount := 0;
    if not Clipboard.HasFormat(CF_TEXT) then
    begin
        MessageDlg('Clipboard contains no text data...', mtError, [mbOK], 0);
        Exit;
    end;
    if MessageDlg('Overwrite this data ?', mtWarning, [mbYes, mbNo, mbCancel], 0) <>
        mrYes then
        Exit;

    Count := ClipBoard.GetTextBuf(@Buffer, BufCount);
    ExtractGridSizes(Buffer, Count, BufferColCount, BufferRowCount);
    if Row < FixedRows then
        Row := FixedRows;
    RowCount := BufferRowCount + Row;
    ColCount := BufferColCount + FixedCols;
    Col := FixedCols;
    Index := 0;
    try
        for j := 0 to BufferRowCount - 1 do
            for i := 0 to BufferColCount - 1 do
            begin
                St := ExtractString(Buffer, Count, Index);
                TempCol := FixedCols + i;
                TempRow := Row + j;
                if (TempCol <= ColCount - 1) and (TempRow <= RowCount - 1) then
                begin
                    if not CheckingTextValidity(St, TempCol, TempRow) then
                        Cells[TempCol, TempRow] := ''
                    else
                        Cells[TempCol, TempRow] := St;
                end;
            end;
    except
        MessageDlg('Vague number of cell, since data do not have tabular format...',
            mtError, [mbOK], 0);
        Result := False;
        Exit;
    end;
    ClearFixed;
    EnumerateRows;
    Result := True;
end;

procedure TColorStringGrid.SelectAll;
var
    R: TGridRect;
begin
    EditorMode := False;
    R.Left := FixedCols;
    R.Right := ColCount - 1;
    R.Top := FixedRows;
    R.Bottom := RowCount - 1;
    Selection := R;
end;

procedure TColorStringGrid.ClearSelection;
var
    R: TGridRect;
begin
    R.Left := Col;
    R.Right := Col;
    R.Top := Row;
    R.Bottom := Row;
    Selection := R;
end;

{$hints off}
function TNumericGrid.CheckingTextValidity(St: string; ACol, ARow: longint): boolean;
begin
    if St = '' then
    begin
        Result := True;
        Exit;
    end;
    if ColOptions[ACol] = coInteger then
    begin
        try
            StrToInt(St);
        except
            Result := False;
            Exit
        end;
        Result := True;
        Exit;
    end;
    if ColOptions[ACol] = coReal then
    begin
        try
            StrToFloat(St);
        except
            Result := False;
            Exit
        end;
        Result := True;
        Exit;
    end;
    Result := True;
end;

{$hints on}

procedure TColorStringGrid.ResetAll;
var
    i, j: longint;
begin
    ColCount := 10;
    RowCount := 2;
    FixedCols := 1;
    FixedRows := 1;
    Col := 1;
    Row := 1;
    ClearSelection;
    for i := 0 to ColCount - 1 do
        ColWidths[i] := DefaultColWidth;
    for i := 0 to RowCount - 1 do
        RowHeights[i] := DefaultRowHeight;
    for i := 0 to ColCount - 1 do
        for j := 0 to RowCount - 1 do
            Cells[i, j] := '';
end;

destructor TNumericGrid.Destroy;
begin
    Finalize(ColOptarray);
    inherited Destroy;
end;

procedure TNumericGrid.SetOptCount;
begin
    SetLength(ColOptarray, AColOptCount);
end;

procedure TNumericGrid.SetColCount(Value: longint);
begin
    SetOptCount(Value);
    inherited;
end;

procedure TNumericGrid.KeyPress(var Key: char);
var
    i: longint;
    St: AnsiString;

begin
    if goEditing in Options then
    begin
        if CanEditAcceptKey(Key) then
            inherited KeyPress(Key)
        //  Block further processing.
        else
            Key := #0;
    end
    else
        inherited KeyPress(Key);

    case Key of
        #9:
        begin
            if not RowNumFixed then
                if (Col = 1) and (Row = 1) then
                begin
                    RowCount := RowCount + 1;
                    Str(RowCount - 1, St);
                    Cells[0, RowCount - 1] := string(St);
                    Col := 1;
                    Row := RowCount - 1;
                    for i := 1 to ColCount - 1 do
                        Cells[i, Row] := '';
                end;
        end;

        ',':
            //  Substitute decimal separator.
            if ColOptions[Col] = coReal then
                Key := '.';
    end;
end;

procedure TNumericGrid.ResetColWidths;
var
    i: longint;
begin
    for i := 0 to ColCount - 1 do
        SetColWidthByDefault(i);
end;

procedure TIDAGrid.AutoColWidths;
var
    i: longint;
begin
    for i := 0 to ColCount - 1 do
        SetAutoColWidth(i);
end;

procedure TNumericGrid.SetColWidthByDefault(ACol: longint);
var
    Width: longint;
begin
    Width := GetMaxTextWidth(Self, ACol);
    if Width = 0 then
        Width := 40
    else
        Width := Width + 10;
    ColWidths[ACol] := Width;
end;

procedure TIDAGrid.SetAutoColWidth(ACol: longint);
var
    Width: longint;
begin
    Width := GetMaxTextWidth(Self, ACol);
    if Width = 0 then
        Width := MIN_WIDTH;
    Width := Width + 10;
    ColWidths[ACol] := Width;
end;

procedure TColorStringGrid.SetColCount(Value: longint);
var
    i, j: longint;
    SavedLength: longint;
begin
    if not (csDesigning in ComponentState) then
    begin
        //    At the design-time this doesn't work.
        if not Assigned(FColorMatrix) then
            InitColorMatrix;
        for i := 0 to RowCount - 1 do
            if Length(FColorMatrix[i]) <> Value then
            begin
                SavedLength := Length(FColorMatrix[i]);
                SetLength(FColorMatrix[i], Value);
                for j := SavedLength to Length(FColorMatrix[i]) - 1 do
                    if Odd(i) then
                        FColorMatrix[i, j] := OddRowColor
                    else
                        FColorMatrix[i, j] := EvenRowColor;
            end;
    end;
    TStringGrid(Self).ColCount := Value;
end;

procedure TColorStringGrid.InitColorMatrix;
var
    i: longint;
begin
    SetLength(FColorMatrix, RowCount);
    for i := 0 to RowCount - 1 do
        SetLength(FColorMatrix[i], ColCount);
end;

procedure TColorStringGrid.FinalizeColorMatrix;
var
    i: longint;
begin
    if Assigned(FColorMatrix) then
    begin
        for i := 0 to RowCount - 1 do
            Finalize(FColorMatrix[i]);
        Finalize(FColorMatrix);
    end;
end;

function TColorStringGrid.GetColCount: longint;
begin
    Result := TStringGrid(Self).ColCount;
end;

procedure TColorStringGrid.SetRowCount(Value: longint);
var
    i, j: longint;
    SavedLength: longint;
begin
    if not (csDesigning in ComponentState) then
    begin
        //    At the design-time this doesn't work.
        if not Assigned(FColorMatrix) then
            InitColorMatrix;
        if Value < RowCount then
        begin
            for i := Value to RowCount - 1 do
                Finalize(FColorMatrix[i]);
            SetLength(FColorMatrix, Value);
        end;
        if Value > RowCount then
        begin
            SavedLength := Length(FColorMatrix);
            SetLength(FColorMatrix, Value);
            for i := SavedLength to Length(FColorMatrix) - 1 do
            begin
                SetLength(FColorMatrix[i], ColCount);
                for j := 0 to ColCount - 1 do
                    if Odd(i) then
                        FColorMatrix[i, j] := OddRowColor
                    else
                        FColorMatrix[i, j] := EvenRowColor;
            end;
        end;
    end;
    TStringGrid(Self).RowCount := Value;
end;

function TColorStringGrid.GetRowCount: longint;
begin
    Result := TStringGrid(Self).RowCount;
end;

function GetMaxTextWidth(const Grid: TStringGrid; const ColNum: longint): longint;
var
    i: longint;
begin
    Result := 0;
    with Grid do
        for i := 0 to RowCount - 1 do
            if Canvas.TextWidth(Cells[ColNum, i]) > Result then
                Result := Canvas.TextWidth(Cells[ColNum, i]);
end;

function GetMaxTextHeight(const Grid: TStringGrid; const RowNum: longint): longint;
var
    i: longint;
begin
    Result := 0;
    with Grid do
        for i := 0 to ColCount - 1 do
            if Canvas.TextHeight(Cells[i, RowNum]) > Result then
                Result := Canvas.TextHeight(Cells[i, RowNum]);
end;

procedure TIDAGrid._AddColumn;
begin
    ColCount := ColCount + 1;
    FillArea(ColCount - 1, FixedRows, ColCount - 1, RowCount - 1);
    FillColHeaders;
end;

procedure TIDAGrid._AddRow;
begin
    RowCount := RowCount + 1;
    FillArea(FixedCols, RowCount - 1, ColCount - 1, RowCount - 1);
    FillRowHeaders;
end;

procedure TIDAGrid._DeleteAllData;
begin
    if not ColNumFixed then
        ColCount := FixedCols + 1;
    if not RowNumFixed then
        RowCount := FixedRows + 1;
    FillArea(FixedCols, FixedRows, ColCount - 1, RowCount - 1);
    FillRowHeaders;
    FillColHeaders;
end;

procedure TIDAGrid._DeleteColumns(StartCol, ColsCount: integer);
var
    i, j: longint;
begin
    for i := StartCol to ColCount - 1 - ColsCount do
        for j := 0 to RowCount - 1 do
            Cells[i, j] := Cells[i + ColsCount, j];
    ColCount := ColCount - ColsCount;
    FillColHeaders;
end;

procedure TIDAGrid._DeleteRows(StartRow, RowsCount: integer);
var
    i, j: longint;
begin
    for i := StartRow to RowCount - 1 - RowsCount do
        for j := 0 to ColCount - 1 do
            Cells[j, i] := Cells[j, i + RowsCount];
    RowCount := RowCount - RowsCount;
    FillRowHeaders;
end;

procedure TIDAGrid._InsertColumns(StartCol, ColsCount: integer; Clear: boolean);
var
    i, j: longint;
begin
    ColCount := ColCount + ColsCount;
    for i := ColCount - 1 downto StartCol + ColsCount do
        for j := 0 to RowCount - 1 do
            Cells[i, j] := Cells[i - ColsCount, j];
    if Clear then
        ClearArea(StartCol, FixedRows, StartCol + ColsCount - 1, RowCount - 1)
    else
        FillArea(StartCol, FixedRows, StartCol + ColsCount - 1, RowCount - 1);
    FillColHeaders;
end;

procedure TIDAGrid._InsertRows(StartRow, RowsCount: integer; Clear: boolean);
var
    i, j: longint;
begin
    RowCount := RowCount + RowsCount;
    for i := RowCount - 1 downto StartRow + RowsCount do
        for j := 0 to ColCount - 1 do
            Cells[j, i] := Cells[j, i - RowsCount];
    if Clear then
        ClearArea(FixedCols, StartRow, ColCount - 1, StartRow + RowsCount - 1)
    else
        FillArea(FixedCols, StartRow, ColCount - 1, StartRow + RowsCount - 1);
    FillRowHeaders;
end;

procedure TIDAGrid._ClearAllCells;
begin
    ClearArea(FixedCols, FixedRows, ColCount - 1, RowCount - 1);
end;

procedure TIDAGrid._ClearSelectedArea;
begin
    with Selection do
        ClearArea(Left, Top, Right, Bottom);
end;

function TIDAGrid.MayIDoAddColumn: boolean;
begin
    Result := not ColNumFixed;
end;

function TIDAGrid.MayIDoAddRow: boolean;
begin
    Result := not RowNumFixed;
end;

function TIDAGrid.MayIDoClearAllCells: boolean;
begin
    Result := True;
end;

function TIDAGrid.MayIDoClearSelectedArea: boolean;
begin
    Result := True;
end;

function TIDAGrid.MayIDoDeleteAllData: boolean;
begin
    Result := not (ColNumFixed and RowNumFixed);
end;

{$hints off}
function TIDAGrid.MayIDoDeleteColumns(StartCol, ColsCount: integer): boolean;
begin
    Result := not ColNumFixed;
end;

function TIDAGrid.MayIDoDeleteRows(StartRow, RowsCount: integer): boolean;
begin
    Result := not RowNumFixed;
end;

function TIDAGrid.MayIDoInsertColumns(StartCol, ColsCount: integer): boolean;
begin
    Result := not ColNumFixed;
end;

function TIDAGrid.MayIDoInsertRows(StartRow, RowsCount: integer): boolean;
begin
    Result := not RowNumFixed;
end;

{$hints on}

function TDataGrid.MayIDoAddColumn: boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoAddColumn and GetMyGridDataSource.MayIDoAddColumn
    else
        Result := inherited MayIDoAddColumn;
end;

function TDataGrid.MayIDoAddRow: boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoAddRow and GetMyGridDataSource.MayIDoAddRow
    else
        Result := inherited MayIDoAddRow;
end;

function TDataGrid.MayIDoClearAllCells: boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoClearAllCells and
            GetMyGridDataSource.MayIDoClearAllCells
    else
        Result := inherited MayIDoClearAllCells;
end;

function TDataGrid.MayIDoClearSelectedArea: boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoClearSelectedArea and
            GetMyGridDataSource.MayIDoClearSelectedArea
    else
        Result := inherited MayIDoClearSelectedArea;
end;

function TDataGrid.MayIDoDeleteAllData: boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoDeleteAllData and
            GetMyGridDataSource.MayIDoDeleteAllData
    else
        Result := inherited MayIDoDeleteAllData;
end;

function TDataGrid.MayIDoDeleteColumns(StartCol, ColsCount: integer): boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoDeleteColumns(StartCol, ColsCount) and
            GetMyGridDataSource.MayIDoDeleteColumns(StartCol, ColsCount)
    else
        Result := inherited MayIDoDeleteColumns(StartCol, ColsCount);
end;

function TDataGrid.MayIDoDeleteRows(StartRow, RowsCount: integer): boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoDeleteRows(StartRow, RowsCount) and
            GetMyGridDataSource.MayIDoDeleteRows(StartRow, RowsCount)
    else
        Result := inherited MayIDoDeleteRows(StartRow, RowsCount);
end;

function TDataGrid.MayIDoInsertColumns(StartCol, ColsCount: integer): boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoInsertColumns(StartCol, ColsCount) and
            GetMyGridDataSource.MayIDoInsertColumns(StartCol, ColsCount)
    else
        Result := inherited MayIDoInsertColumns(StartCol, ColsCount);
end;

function TDataGrid.MayIDoInsertRows(StartRow, RowsCount: integer): boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoInsertRows(StartRow, RowsCount) and
            GetMyGridDataSource.MayIDoInsertRows(StartRow, RowsCount)
    else
        Result := inherited MayIDoInsertRows(StartRow, RowsCount);
end;

{ TGEFGrid }
function TGEFGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
    Result := {$IFNDEF Lazarus} inherited CanEditAcceptKey(Key) and
        CanEditModify; {$ELSE} True; {$ENDIF}
end;

procedure TGEFGrid.DoExit;
begin
    if Modified then
    begin
        EditingFinished(Col, Row);
        Modified := False;
    end;
    inherited;
end;

procedure TGEFGrid.EditingFinished(const ACol, ARow: integer);
begin
    if Assigned(OnGridEditingFinished) then
        OnGridEditingFinished(Self, ACol, ARow);
end;

procedure TGEFGrid.KeyPress(var Key: char);
begin
    if (goEditing in Options) and CanEditAcceptKey(Key) then
        Modified := True;
    inherited;
end;

function TGEFGrid.SelectCell(ACol, ARow: integer): boolean;
var
    MyResult: boolean;
begin
    //  Selected cell is not always current cell.
    MyResult := True;
    if Modified then
    begin
        try
            EditingFinished(ACol, ARow);
            Modified := False;
        except
            MyResult := False;
            //  Method must finish without exception.
            MessageDlg('Invalid input...', mtError, [mbOK], 0);
        end;
    end;
    Result := MyResult and inherited SelectCell(ACol, ARow);
end;

procedure TDataGrid.EditingFinished(const ACol, ARow: integer);
begin
    DataChanged(Col, Row, Col, Row);
    inherited;
end;

procedure TDataGrid.DataChanged(const Left, Top, Right, Bottom: integer);
var
    i, j: longint;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
        begin
            for i := Left to Right do
                for j := Top to Bottom do
                    if Cells[i, j] = '' then
                    begin
                        SetValueByDefault(i, j);
                        Cells[i, j] := ValueToString(i, j);
                    end
                    else
                    begin
                        if not IsDataValid(i, j, Cells[i, j]) then
                        begin
                            SetValueByDefault(i, j);
                            Cells[i, j] := ValueToString(i, j);
                        end
                        else
                            StringToValue(i, j, Cells[i, j]);
                    end;
        end;
end;

procedure TDataGrid.FillRowHeaders;
begin
    FillArea(0, FixedRows, FixedCols - 1, RowCount - 1);
end;

procedure TDataGrid.FillColHeaders;
begin
    FillArea(0, 0, ColCount - 1, FixedRows - 1);
end;

procedure TDataGrid.FillTable;
begin
    FillArea(FixedCols, FixedRows, ColCount - 1, RowCount - 1);
end;

procedure TDataGrid.GetTableParams;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
        begin
            ColCount := GetColCount;
            RowCount := GetRowCount;
            FixedCols := GetFixedCols;
            FixedRows := GetFixedRows;
            ColNumFixed := GetColNumFixed;
            RowNumFixed := GetRowNumFixed;

            Col := GetCol;
            Row := GetRow;

            //  LeftCol, TopRow must be set up after Col, Row.
            LeftCol := GetLeftCol;
            TopRow := GetTopRow;

            Selection := GetSelection;

            EditorMode := False;
        end;
end;

procedure TDataGrid.GetWidthsHeights;
var
    i: longint;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
        begin
            if AutoWidths then
                AutoColWidths
            else
                for i := 0 to ColCount - 1 do
                    ColWidths[i] := GetColWidth(i);
            if AutoHeights then
                AutoRowHeights
            else
                for i := 0 to RowCount - 1 do
                    RowHeights[i] := GetRowHeight(i);
        end;
end;

procedure TIDAGrid.ClearArea(const Left, Top, Right, Bottom: integer);
var
    i, j: longint;
begin
    for i := Top to Bottom do
        for j := Left to Right do
            Cells[j, i] := '';
    DataChanged(Left, Top, Right, Bottom);
end;

procedure TIDAGrid.FillRowHeaders;
begin

end;

procedure TIDAGrid.FillColHeaders;
begin

end;

procedure TDataGrid.SaveTableParams;
var
    i: longint;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
        begin
            for i := 0 to ColCount - 1 do
                SaveColWidth(i, ColWidths[i]);
            for i := 0 to RowCount - 1 do
                SaveRowHeight(i, RowHeights[i]);

            SaveLeftCol(LeftCol);
            SaveTopRow(TopRow);
            SaveCol(Col);
            SaveRow(Row);

            SaveSelection(Selection);
        end;
end;

procedure TGEFGrid.SetModified(const AModified: boolean);
begin
    FModified := AModified;
    if AModified and Assigned(OnGridModified) then
        OnGridModified(Self);
end;

procedure TIDAGrid.AutoRowHeights;
var
    i: longint;
begin
    for i := 0 to RowCount - 1 do
        SetAutoRowHeight(i);
end;

procedure TIDAGrid.SetAutoRowHeight(ARow: integer);
var
    Height: longint;
begin
    Height := GetMaxTextHeight(Self, ARow);
    if Height = 0 then
        Height := MIN_HEIGHT;
    Height := Height + 2;
    RowHeights[ARow] := Height;
end;

function TIDAGrid.CanEditModify: Boolean;
begin
    Result := Changeable;
end;

constructor TIDAGrid.Create(AOwner: TComponent);
begin
    inherited;
    Changeable := True;
end;

procedure TIDAGrid.FillArea(const Left, Top, Right, Bottom: integer);
var
    i, j: longint;
begin
    for i := Left to Right do
        for j := Top to Bottom do
            Cells[i, j] := '';
end;

procedure TIDAGrid.DataChanged(const Left, Top, Right, Bottom: longint);
begin
    FillArea(Left, Top, Right, Bottom);
end;

function TDataGrid.CanEditModify: Boolean;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            Result := {$IFNDEF Lazarus} inherited CanEditModify and {$ENDIF}
                (not IsCellDisabled(Col, Row))
    else Result := {$IFNDEF Lazarus} inherited CanEditModify; {$ELSE} True; {$ENDIF}
end;

function TDataGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            Result := {$IFNDEF Lazarus} inherited CanEditAcceptKey(Key) and {$ENDIF}
                (Key in GetCellEnabledCharSet(Col, Row))
    else Result := {$IFNDEF Lazarus} inherited CanEditAcceptKey(Key); {$ELSE} True; {$ENDIF}
end;

procedure TDataGrid.FillArea(const Left, Top, Right, Bottom: integer);
var
    i, j: longint;
begin
    if GetMyGridDataSource <> nil then
        for i := Left to Right do
            for j := Top to Bottom do
                Cells[i, j] := GetMyGridDataSource.ValueToString(i, j);
end;

initialization
    RegisterClass(TGEFGrid);
    RegisterClass(TIDAGrid);
    RegisterClass(TColoredGrid);
    RegisterClass(TDataGrid);
    RegisterClass(TNumericGrid);
    RegisterClass(TColorStringGrid);
    RegisterClass(TIconicGrid);
    RegisterClass(TAnimatedGrid);
end.
