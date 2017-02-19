//      двойной косой чертой комментируются замечания, сохраняемые во
//      всех версиях исходника; фигурными скобками комментируются замечания,
//      сохраняемые только в версии исходника для бесплатного распространения
{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit NumericGrid;

{$IFDEF Lazarus}
{$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF Lazarus}
    LCLIntf, PropEdits, GraphPropEdits,
{$ELSE}
    System.Types, Windows,
{$ENDIF}
    Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Grids, ClipBrd;

var
    //  настройки цветов по умолчанию
    CL_ODD_ROW: TColor = clWhite;
    CL_EVEN_ROW: TColor = clYellow;
    CL_DISABLED_ROW: TColor = clGray;
    CL_SELECTED: TColor = $2A92B0;

    GridDataSourceGUID: TGUID = '{401B6CC0-0915-11D5-968F-8FBD7448F374}';

    MIN_HEIGHT: LongInt = 10;
    MIN_WIDTH: LongInt  = 40;

type
    EColorStringGrid = class(Exception);
    ENumericGrid = class(Exception);
    EIDA_Grid = class(Exception);

    TCharSet = set of Char;

    IGridDataSource = interface
    ['{401B6CC0-0915-11D5-968F-8FBD7448F374}']
        function ValueToString(const ACol, ARow: LongInt): string;
            //  преобразует значение в источнике данных в строковое
            //  представление, используется также и для нумерации
            //  (или другого способа обозначения) Fixed колонок/строк
        procedure StringToValue(const ACol, ARow: LongInt;
            //  преобразует строку в значение; если преобразовать
            //  невозможно - вызывается исключение
            const AString: string);
        procedure SetValueByDefault(const ACol, ARow: LongInt);
            //  устанавливает правильное значение "по умолчанию"
            //  для данной ячейки; используется при обработке операции
            //  очистки ячейки
        function GetCellColor(const ACol, ARow: LongInt; var Color: TColor): Boolean;
            //  возвращает True, если должен быть установлен цвет Color,
            //  в противном случае - False
        function GetCellEditMask(const ACol, ARow: LongInt): string;
        function GetCellEnabledCharSet(const ACol, ARow: LongInt): TCharSet;
        function IsCellDisabled(const ACol, ARow: LongInt): Boolean;
            //  возвращает признак запрещения ввода в ячейки -
            //  True - ввод запрещен
        function IsDataValid(const ACol, ARow: LongInt;
            //  выполняет "мягкую" проверку данных без возбуждения
            //  исключения; однако, если координаты строки/колонки
            //  имеют недорустимые значения исключение вызывается
            const AString: string): Boolean;

        //  функции проверки возможности выполнения действия
        function MayIDoInsertRows(StartRow, RowsCount: LongInt): Boolean;
        function MayIDoDeleteRows(StartRow, RowsCount: LongInt): Boolean;
        function MayIDoAddRow: Boolean;

        function MayIDoInsertColumns(StartCol, ColsCount: LongInt): Boolean;
        function MayIDoDeleteColumns(StartCol, ColsCount: LongInt): Boolean;
        function MayIDoAddColumn: Boolean;

        function MayIDoDeleteAllData: Boolean;
        function MayIDoClearSelectedArea: Boolean;
        function MayIDoClearAllCells: Boolean;

        procedure RowsDeleted(const StartPos, Count: LongInt);
        procedure RowsInserted(const StartPos, Count: LongInt);
        procedure RowAdded;

        procedure ColumnsDeleted(const StartPos, Count: LongInt);
        procedure ColumnsInserted(const StartPos, Count: LongInt);
        procedure ColumnAdded;

        procedure AllDataDeleted;

        function GetColCount: LongInt;  //  полное число колонок, включая Fixed
        function GetRowCount: LongInt;  //  полное число строк, включая Fixed
        function GetFixedCols: LongInt; //  число фиксированных колонок
        function GetFixedRows: LongInt; //  число фиксированных строк
        function GetColNumFixed: Boolean;
        function GetRowNumFixed: Boolean;

        function GetColWidth(const Col: LongInt): LongInt;
        procedure SaveColWidth(const Col, Width: LongInt);
        function GetRowHeight(const Row: LongInt): LongInt;
        procedure SaveRowHeight(const Row, Height: LongInt);
        function AutoWidths: Boolean;
        function AutoHeights: Boolean;
            //  определяют необходимость использования
            //  автоматической настройки ширины и высоты ячеек

        function GetSelection: TGridRect;
        procedure SaveSelection(const Selection: TGridRect);
        function GetCol: LongInt;       //  номер текущей выбранной колонки
        procedure SaveCol(const Col: LongInt);
        function GetRow: LongInt;       //  номер текущей выбранной строки
        procedure SaveRow(const Row: LongInt);
        function GetLeftCol: LongInt;
        procedure SaveLeftCol(const LeftCol: LongInt);
        function GetTopRow: LongInt;
        procedure SaveTopRow(const TopRow: LongInt);
    end;

    TGridEditingFinished = procedure(Sender: TObject;
        Col, Row: LongInt   //  координаты редактируемой ячейки
        ) of object;

    TGridModified = procedure(Sender: TObject) of object;

    TClipboardGrid = class(TStringGrid)
    protected
        procedure EnumerateRows;
        function CheckingTextValidity(St: string;
            ACol, ARow: LongInt): Boolean; virtual;

        //  методы объявлены как private -
        //  нельзя перекрыть - нужно переопределить
        procedure SetColCount(Value: Longint); virtual;
        function GetColCount: LongInt; virtual;
        procedure SetRowCount(Value: Longint); virtual;
        function GetRowCount: LongInt; virtual;

        const DelimiterChars: set of Char = [#9, #10, #13, ' ', ',', ';'];
        //  Maximum size of pasted data.
        const BufCount = 10240;

        procedure ExtractGridSizes(Buffer: array of Char;
            const Count: LongInt; var BufferCols, BufferRows: LongInt);
        function ExtractString(Buffer: array of Char;
            const Count: LongInt; var Index: LongInt): string;
        procedure ClearFixed;

    public
        function CopyToClipBoard: Boolean; virtual;
        function PasteFromClipBoard: Boolean; virtual;

    published
        property ColCount: LongInt
            read GetColCount            write SetColCount;
        property RowCount: LongInt
            read GetRowCount            write SetRowCount;
    end;

    TGEFGrid = class(TClipboardGrid)
        //  сетка, контролирующая выход из режима редактирования ячейки
        //  в момент выхода из редактируемой ячейки вызывается событие
        //  типа TGridEditingFinished; выход из ячейки редактирования
        //  обнаруживается путем контроля событий выхода из таблицы,
        //  нажатия клавиш и кнопок мыши
    protected
        FGridEditingFinished: TGridEditingFinished;
        FGridModified: TGridModified;
        FModified: Boolean;

        procedure DoExit; override;
            //  контролирует выход из таблицы
        (*???function CanEditAcceptKey(Key: Char): Boolean; override;*)
            //  проверяет, возможен ли ввод в данную ячейку
        procedure KeyPress(var Key: Char); override;
            //  устанавливает состояние Modified в True,
            //  если CanEditAcceptKey возвращает True
        function SelectCell(ACol, ARow: Longint): Boolean; override;
            //  вызывает EditingFinished в соответствии с
            //  состоянием Modified

        procedure EditingFinished(
            //  не делает ничего кроме вызова события OnGridEditingFinished;
            //  нужно перекрывать в потомках для реализации специальных действий
            const ACol, ARow: LongInt   //  координаты редактируемой ячейки
            ); virtual;

        procedure SetModified(const AModified: Boolean);
            //  вызывает событие OnGridModified

    published
        property Modified: Boolean
            //  свойство показывает, что содержимое текущей ячейки
            //  было изменено; при этом условии вызывается EditingFinished;
            //  после изменения ячейки ввода сбрасывается
            read FModified              write SetModified;

        property OnGridEditingFinished: TGridEditingFinished
            read FGridEditingFinished   write FGridEditingFinished;
        property OnGridModified: TGridModified
            //  событие только отражает факт изменения данных в таблице
            read FGridModified          write FGridModified;
    end;

    TGridResizedEvent = procedure(Sender: TObject) of object;

    TIDAGrid = class(TGEFGrid)
        //  сетка, реализующая операции вставки, удаления добавления строк
        //  и столбцов, а также вставку/копирование текста в/из ClipBoard
        //  (IDA = Insert, Delete, Add)
    protected
        FColNumFixed: Boolean;  //  число колонок не может изменяться
        FRowNumFixed: Boolean;  //  число столбцов не может изменяться
        FChangeable: Boolean;   //  текст в ячейках может редактироваться;
                                //  не влияет на возможность удаления/
                                //  добавления строк/столбцов (по умолч. = True)
        //  ??? надо бы также управлять возможностью удаления/добавления
        //  строк/столбцов

        SelFlag: Boolean;       //  используется для организации изменения
                                //  размера выделенной области по движению мыши
        StartCoord: TGridCoord;
        SavedCoord: TGridCoord;

        FOnGridResized: TGridResizedEvent;

        (*???function CanEditModify: Boolean; override;*)
        procedure KeyPress(var Key: Char); override;
            //  если изменение числа строк разрешено, то добавляет новую
            //  строку по нажатию клавиши Tab при достижении конца таблицы

            //  виртуальные методы, которые выполняют всю работу
        procedure _InsertRows(StartRow, RowsCount: LongInt; Clear: Boolean
            ); virtual;
        procedure _DeleteRows(StartRow, RowsCount: LongInt
            ); virtual;
        procedure _AddRow; virtual;

        procedure _InsertColumns(StartCol, ColsCount: LongInt; Clear: Boolean
            ); virtual;
        procedure _DeleteColumns(StartCol, ColsCount: LongInt
            ); virtual;
        procedure _AddColumn; virtual;

        procedure _DeleteAllData; virtual;      //  удаляет все данные,
                                                //  формируя пустую таблицу
        procedure _ClearSelectedArea; virtual;
            //  очищает ячейки выделенной области
        procedure _ClearAllCells; virtual;
            //  очищает все ячейки таблицы

        procedure ClearArea(
            //  очищает заданную область таблицы и вызывает DataChanged
            const Left, Top, Right, Bottom: LongInt);
        procedure DataChanged(
            //  выполняет некоторые действия при изменении
            //  данных в области таблицы (здесь ничего не делает)
            const Left, Top, Right, Bottom: LongInt); virtual;
        procedure FillArea(
            //  вызывается, когда нужно заполнить некоторую
            //  область таблицы (здесь ничего не делает)
            const Left, Top, Right, Bottom: LongInt); virtual;
        procedure FillRowHeaders; virtual;
            //  заполняет фиксированные колонки начиная с
            //  первой строки, следующей за фиксированными строками
            //  здесь ничего не делает - ??? можно сделать перенумеровку строк
        procedure FillColHeaders; virtual;
            //  заполняет фиксированные строки (заголовки колонок таблицы)
            //  здесь ничего не делает

    public
        constructor Create(AOwner: TComponent); override;

        function PasteFromClipBoard: Boolean; override;

        procedure MouseUp(Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer
            ); override;
        procedure MouseMove(Shift: TShiftState; X, Y: Integer
            ); override;
        procedure MouseDown(Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer
            ); override;

        //  функции проверки возможности выполнения действия
        function MayIDoInsertRows(StartRow, RowsCount: LongInt): Boolean; virtual;
        function MayIDoDeleteRows(StartRow, RowsCount: LongInt): Boolean; virtual;
        function MayIDoAddRow: Boolean; virtual;

        function MayIDoInsertColumns(StartCol, ColsCount: LongInt): Boolean; virtual;
        function MayIDoDeleteColumns(StartCol, ColsCount: LongInt): Boolean; virtual;
        function MayIDoAddColumn: Boolean; virtual;

        function MayIDoDeleteAllData: Boolean; virtual;
        function MayIDoClearSelectedArea: Boolean; virtual;
        function MayIDoClearAllCells: Boolean; virtual;

        //  выполнение действия - перед выполнением
        //  проверяется возможность выполнения
        procedure InsertRows(StartRow, RowsCount: LongInt; Clear: Boolean);
        procedure DeleteRows(StartRow, RowsCount: LongInt);
        procedure AddRow;

        procedure InsertColumns(StartCol, ColsCount: LongInt; Clear: Boolean);
        procedure DeleteColumns(StartCol, ColsCount: LongInt);
        procedure AddColumn;

        procedure DeleteAllData;        //  удаляет все данные, формируя пустую таблицу
        procedure ClearSelectedArea;    //  очищает клетки выделенной области
        procedure ClearAllCells;        //  очищает все ячейки таблицы

        procedure DeleteSelection;      //  алгоритм удаления выделенной области

        procedure SelectAll;            //  выбирает все нефиксированные ячейки
        procedure ClearSelection;       //  сбрасывает область выделения


        procedure SetAutoColWidth(ACol: LongInt);
        procedure AutoColWidths;
        procedure SetAutoRowHeight(ARow: LongInt);
        procedure AutoRowHeights;

    published
        property DoubleBuffered;

            //  установка этих свойств в True запрещает работу методов,
            //  изменяющих число столбцов или (и) строк таблицы; при
            //  попытке изменить фиксированный параметр таблица выдает
            //  сообщение об ошибке; эти свойства на влияют на изменение
            //  числа строк/столбцов свойствами ColCount, RowCount;
        property ColNumFixed: Boolean   read FColNumFixed   write FColNumFixed;
        property RowNumFixed: Boolean   read FRowNumFixed   write FRowNumFixed;
        property Changeable: Boolean    read FChangeable    write FChangeable;

        property OnGridResized: TGridResizedEvent
            read FOnGridResized write FOnGridResized;
    end;

    TDataGrid = class(TIDAGrid)
        //  таблица, которая может выполнять действия над классом -
        //  источником данных и обмениваться с ним данными;
        //  !!! при работе с методами, изменяющими число строк или
        //  столбцов таблицы предпочтение отдается свойствам
        //  ColNumFixed, RowNumFixed предка, не зависимо от того,
        //  поддерживаются ли такие действия классом - источником данных !!!
        //  в случае, если объект - источник не присоединен, таблица
        //  просто работает так же как и предок
    protected
        FGridDataSource: IGridDataSource;

        function GetMyGridDataSource: IGridDataSource;
        procedure EditingFinished(
            //  в первую очередь вызывает DataChanged, затем inherited
            const ACol, ARow: LongInt   //  координаты редактируемой ячейки
            ); override;
        (*???function CanEditAcceptKey(Key: Char): Boolean; override;*)
            //  проверяет разрешает ли источник данных ввод символа
        (*???function CanEditModify: Boolean; override;*)
            //  дополнительно проверяет разрешает ли источник
            //  данных редактировать текущую ячейку
        procedure _InsertRows(StartRow, RowsCount: LongInt; Clear: Boolean
            ); override;
        procedure _DeleteRows(StartRow, RowsCount: LongInt
            ); override;
        procedure _AddRow; override;

        procedure _InsertColumns(StartCol, ColsCount: LongInt; Clear: Boolean
            ); override;
        procedure _DeleteColumns(StartCol, ColsCount: LongInt
            ); override;
        procedure _AddColumn; override;

        procedure _DeleteAllData; override;     //  удаляет все данные,
                                                //  формируя пустую таблицу
        procedure _ClearSelectedArea; override;
        procedure _ClearAllCells; override;

        procedure DataChanged(
            //  вызывает обновление данных в объекте - источнике данных;
            //  если в какой - либо ячейке области находится пустая строка,
            //  то для этой ячейки устанавливается значение "по умолчанию"
            const Left, Top, Right, Bottom: LongInt); override;
        procedure FillArea(
            //  заполняет область строками, получаемыми от источника данных
            const Left, Top, Right, Bottom: LongInt); override;

        //  !!! при вызове этих функций предполагается,
        //  что источник данных не равен nil !!!
        procedure GetTableParams;
            //  устанавливает параметры таблицы, пользуясь
            //  функциями объекта - источника данных
        procedure GetWidthsHeights;
            //  устанавливает ширины и высоты ячеек, пользуясь
            //  функциями объекта - источника данных
        procedure SaveTableParams;
            //  соханяет параметры таблицы в источнике данных,
            //  а также ширины и высоты ячеек
        procedure FillTable;
            //  выполняет заполнение таблицы данными
            //  (заполняет только область данных)
        procedure FillRowHeaders; override;
            //  заполняет фиксированные колонки начиная с
            //  первой строки, следующей за фиксированными строками
        procedure FillColHeaders; override;
            //  заполняет фиксированные строки (заголовки колонок таблицы)

    public
        //  функции проверки возможности выполнения действия;
        //  дополнительно вызывают проверку возможно выполнения
        //  действия в объекте - источнике данных
        function MayIDoInsertRows(StartRow, RowsCount: LongInt): Boolean; override;
        function MayIDoDeleteRows(StartRow, RowsCount: LongInt): Boolean; override;
        function MayIDoAddRow: Boolean; override;

        function MayIDoInsertColumns(StartCol, ColsCount: LongInt): Boolean; override;
        function MayIDoDeleteColumns(StartCol, ColsCount: LongInt): Boolean; override;
        function MayIDoAddColumn: Boolean; override;

        function MayIDoDeleteAllData: Boolean; override;
        function MayIDoClearSelectedArea: Boolean; override;
        function MayIDoClearAllCells: Boolean; override;

        procedure HideTable;
        procedure ShowTable;

        procedure SetGridDataSource(
            //  присоединяет объект - источник данных, выполняет
            //  первоначальное заполнение таблицы данными; для
            //  отсоединения объекта нужно передать методу nil
            GridDataSource: IGridDataSource);
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

        procedure DrawCell(ACol, ARow: Longint;
            ARect: TRect; AState: TGridDrawState); override;

    public
        constructor Create(AOwner: TComponent); override;

    published
        property OddRowColor: TColor read GetOddRowColor write SetOddRowColor;
        property EvenRowColor: TColor read GetEvenRowColor write SetEvenRowColor;
        property SelectedRegionColor: TColor
            read GetSelectedRegionColor write SetSelectedRegionColor;
        property DisabledColor: TColor
            read GetDisabledColor write SetDisabledColor;
    end;

    TColOption = LongInt;

    TGetCellColorEvent = procedure(Sender: TObject;
    ColNum, RowNum: LongInt; var CellColor: TColor) of object;

    // Grid allows setting up colors of different types of cells at design time.
    TColorStringGrid = class(TClipboardGrid)
    protected
        FColorMatrix: array of array of TColor;
        FOddRowColor: TColor;
        FEvenRowColor: TColor;
        FSelectedRegionColor: TColor;
        FOnGetCellColor: TGetCellColorEvent;
        FColNumFixed: Boolean;  //  число колонок не может изменяться
        FRowNumFixed: Boolean;  //  число столбцов не может изменяться

        procedure SetOddRowColor(Color: TColor);
        procedure SetEvenRowColor(Color: TColor);
        procedure SetSelectedRegionColor(Color: TColor);
        procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
                  AState: TGridDrawState); override;

        function GetCellColor(const ColNum, RowNum: LongInt): TColor;
        (*function CreateEditor: TInplaceEdit; override;*)

            //  методы объявлены как private -
            //  нельзя перекрыть - нужно переопределить
        procedure SetColCount(Value: Longint); override;
        function GetColCount: LongInt; override;
        procedure SetRowCount(Value: Longint); override;
        function GetRowCount: LongInt; override;

        function GetCellsColors(ACol, ARow: LongInt): TColor;
        procedure SetCellsColors(ACol, ARow: LongInt; AColor: TColor);

        procedure InitColorMatrix;
        procedure FinalizeColorMatrix;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure SelectAll;
        procedure ResetAll;
        procedure ClearSelection;

        property InplaceEditor;
        property CellsColors[ACol, ARow: LongInt]: TColor
            read GetCellsColors write SetCellsColors;

    published
        property DoubleBuffered;
        property OddRowColor: TColor
            read FOddRowColor           write SetOddRowColor;
        property EvenRowColor: TColor
            //   https://www.evernote.com/shard/s132/nl/14501366/28b37bf8-5cce-4a99-a6f5-8249dd0bad1c
            read FEvenRowColor          write SetEvenRowColor;
        property SelectedRegionColor: TColor
            read FSelectedRegionColor   write SetSelectedRegionColor;
        property ColNumFixed: Boolean
            read FColNumFixed           write FColNumFixed;
        property RowNumFixed: Boolean
            //  установка этого свойства в True запрещает добавление строки в
            //  таблицу по клавише Tab, удаление строк и изменение количества строк
            //  вставкой из буфера обмена
            read FRowNumFixed           write FRowNumFixed;
        property OnGetCellColor: TGetCellColorEvent
            read FOnGetCellColor        write FOnGetCellColor;
    end;

    TNumericGrid = class(TColorStringGrid)
        //  сетка, кот. может контролировать ввод чисел
    protected
        FDisabledColor: TColor;
        ColOptArray: array of TColOption;
        SelFlag: Boolean;
        StartCoord: TGridCoord;
        SavedCoord: TGridCoord;
        procedure SetColOption(index: LongInt; Value: TColOption);
        function GetColOption(index: LongInt): TColOption;
        procedure SetOptCount(AColOptCount: LongInt);
        procedure SetDisabledColor(Color: TColor);
        procedure SetColCount(Value: Longint); override;
        function CheckingTextValidity(St: string; ACol,
            ARow: LongInt): Boolean; override;

        function CanEditAcceptKey(Key: Char): Boolean; virtual;
        procedure KeyPress(var Key: Char); override;

        procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer); override;
        procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer); override;

    public
        constructor Create(AOwner: TComponent); override;

        procedure InsertRows(StartPos, Count: LongInt; Clear: Boolean); virtual;
        procedure DeleteRows(StartPos, Count: LongInt); virtual;
        procedure SetColWidthByDefault(ACol: LongInt);
        procedure ResetColWidths;
            //  вычисляет и устанавливает ширину колонок так,
            //  чтобы все данные были видны
        procedure DeleteSelection;
        destructor Destroy; override;

        property ColOptions[index: LongInt]: TColOption
            read GetColOption       write SetColOption;

    published
        property DisabledColor: TColor
            read FDisabledColor     write SetDisabledColor;
    end;

    TIconicGrid = class(TNumericGrid)
        //  с каждой клеткой может быть связан указатель на список TImageList
        //  и индекс картинки, которая будет выведена в клетке левее текста
    end;

    TAnimatedGrid = class(TIconicGrid)
        //  выводимые картинки могут анимироваться (сменой индекса картинки
        //  по таймеру)
    end;

const
    coReal = 1000;
    coInteger = 1001;
    coText = 1002;
    coChars = 1003;
    coDisabled = 1004;

    SelectOptions: set of TGridOption = [goFixedVertLine,
        goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect,
        goDrawFocusSelected, goTabs, goThumbTracking,
        goRowSizing, goColSizing];

    EditingOptions: set of TGridOption = [goFixedVertLine,
        goFixedHorzLine, goVertLine, goHorzLine, goEditing,
        goDrawFocusSelected, goTabs, goThumbTracking,
        goAlwaysShowEditor, goRowSizing, goColSizing];

    StaticOptions: set of TGridOption = [goFixedVertLine,
        goFixedHorzLine, goVertLine, goHorzLine, goTabs,
        goThumbTracking, goRowSizing, goColSizing];

function GetMaxTextWidth(
    //  возвращает максимальную ширину текста в колонке ColNum данного Grid'а
    const Grid: TStringGrid; const ColNum: LongInt): LongInt;
function GetMaxTextHeight(
    //  возвращает максимальную высоту текста в строке RowNum данного Grid'а
    const Grid: TStringGrid; const RowNum: LongInt): LongInt;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('Fit', [TColorStringGrid]);
    RegisterComponents('Fit', [TNumericGrid]);

    RegisterComponents('Fit', [TGEFGrid]);
    RegisterComponents('Fit', [TIDAGrid]);
    RegisterComponents('Fit', [TDataGrid]);
    RegisterComponents('Fit', [TColoredGrid]);

    RegisterPropertyEditor(TypeInfo(TColor), TColorStringGrid,
        'OddRowColor', TColorProperty);
    RegisterPropertyEditor(TypeInfo(TColor), TColorStringGrid,
        'EvenRowColor', TColorProperty);
    RegisterPropertyEditor(TypeInfo(TColor), TColorStringGrid,
        'SelectedRegionColor', TColorProperty);
    RegisterPropertyEditor(TypeInfo(TColor), TColorStringGrid,
        'DisabledColor', TColorProperty);
    RegisterPropertyEditor(TypeInfo(Boolean), TColorStringGrid,
        'RowNumFixed', TEnumProperty);
    RegisterPropertyEditor(TypeInfo(Boolean), TColorStringGrid,
        'ColNumFixed', TEnumProperty);
    RegisterPropertyEditor(TypeInfo(Boolean), TColorStringGrid,
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
        TypeInfo(Boolean), TIDAGrid, 'DoubleBuffered', TEnumProperty);
    RegisterPropertyEditor(
        TypeInfo(Boolean), TIDAGrid, 'RowNumFixed', TEnumProperty);
    RegisterPropertyEditor(
        TypeInfo(Boolean), TIDAGrid, 'ColNumFixed', TEnumProperty);
    RegisterPropertyEditor(
        TypeInfo(Boolean), TIDAGrid, 'Changeable', TEnumProperty);
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
var Coord: TGridCoord;
    R: TGridRect;
begin
    Coord := MouseCoord(X, Y);
    if Shift = [ssLeft, ssDouble] then
    begin
        if (Coord.X > FixedCols - 1) and (Coord.Y > FixedRows - 1) then
        begin
            //  переход в режим редактирования по клику
            ClearSelection;
            Col := Coord.X;
            Row := Coord.Y;
            (*???if CanEditModify then*)
               Options := EditingOptions;
        end
    end else
        if Shift = [ssLeft] then
        begin
            Coord := MouseCoord(X, Y);
            if (Coord.X <= FixedCols - 1) or (Coord.Y <= FixedRows - 1) then
            begin
                Options := SelectOptions;
                if (Coord.Y <= FixedRows - 1) and (Coord.X >= FixedCols) then
                //  выбран столбец
                begin
                    SelFlag := True;
                    StartCoord := MouseCoord(X, Y);
                    SavedCoord := StartCoord;
                    R.Top := FixedRows; R.Bottom := RowCount - 1;
                    R.Left := StartCoord.X; R.Right := StartCoord.X;
                    Selection := R;
                end;

                if (Coord.X <= FixedCols - 1) and (Coord.Y >= FixedRows) then
                //  выбрана строка
                begin
                    SelFlag := True;
                    StartCoord := MouseCoord(X, Y);
                    SavedCoord := StartCoord;
                    R.Left := FixedCols; R.Right := ColCount - 1;
                    R.Top := StartCoord.Y; R.Bottom := StartCoord.Y;
                    Selection := R;
                end;

                if (Coord.X <= FixedCols - 1) and (Coord.Y <= FixedRows - 1) then
                    //  выбрана вся таблица
                    SelectAll;
            end else begin
                //  сброс выделенной области
                ClearSelection;
                Options := StaticOptions;
        end;
    end;
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TIDAGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var Coord: TGridCoord;
    R: TGridRect;
begin
    if SelFlag then
    begin
        Coord := MouseCoord(X, Y);
        if (Coord.X <> SavedCoord.X) or (Coord.Y <> SavedCoord.Y) then
        begin
            if (StartCoord.Y <= FixedRows - 1) and
               (StartCoord.X >= FixedCols) and (Coord.X >= FixedCols) then
            begin
                //  выбираются столбцы
                R.Top := FixedRows; R.Bottom := RowCount - 1;
                if Coord.X < StartCoord.X then
                begin R.Left := Coord.X; R.Right := StartCoord.X end;
                if Coord.X > StartCoord.X then
                begin R.Left := StartCoord.X; R.Right := Coord.X end;
                if Coord.X = StartCoord.X then
                begin R.Left := StartCoord.X; R.Right := StartCoord.X end;
                Selection := R;
                if (Coord.X - LeftCol = VisibleColCount) and
                   (Coord.X < ColCount - 1) then LeftCol := LeftCol + 1;
                if (Coord.X = LeftCol) and
                   (LeftCol > FixedCols) then LeftCol := LeftCol - 1;
            end;

            if (StartCoord.X <= FixedCols - 1) and
               (StartCoord.Y >= FixedRows) and (Coord.Y >= FixedRows) then
            begin
                //  выбираются строки
                R.Left := FixedCols; R.Right := ColCount - 1;
                if Coord.Y < StartCoord.Y then
                begin R.Top := Coord.Y; R.Bottom := StartCoord.Y end;
                if Coord.Y > StartCoord.Y then
                begin R.Top := StartCoord.Y; R.Bottom := Coord.Y end;
                if Coord.Y = StartCoord.Y then
                begin R.Top := StartCoord.Y; R.Bottom := StartCoord.Y end;
                Selection := R;
                if (Coord.Y - TopRow = VisibleRowCount) and
                   (Coord.Y < RowCount - 1) then TopRow := TopRow + 1;
                if (Coord.Y = TopRow) and
                   (TopRow > FixedRows) then TopRow := TopRow - 1;
            end;
            SavedCoord := Coord;
        end;    //  if (Coord.X <> SavedCoord.X) or (Coord.Y <> SavedCoord.Y) then...
    end;
    inherited MouseMove(Shift, X, Y);
end;

procedure TIDAGrid.InsertRows(StartRow, RowsCount: LongInt; Clear: Boolean);
begin
    if MayIDoInsertRows(StartRow, RowsCount) then
    begin
        _InsertRows(StartRow, RowsCount, Clear);
        if Assigned(OnGridResized) then OnGridResized(Self);
        if Assigned(OnGridModified) then OnGridModified(Self);                        
    end else raise EIDA_Grid.Create('Rows inserting is not allowed...');
end;

procedure TIDAGrid.DeleteRows(StartRow, RowsCount: LongInt);
begin
    if MayIDoDeleteRows(StartRow, RowsCount) then
    begin
        _DeleteRows(StartRow, RowsCount);
        if Assigned(OnGridResized) then OnGridResized(Self);
        if Assigned(OnGridModified) then OnGridModified(Self);        
    end else raise EIDA_Grid.Create('Rows deleting is not allowed...');
end;

procedure TIDAGrid.InsertColumns(StartCol, ColsCount: LongInt; Clear: Boolean);
begin
    if MayIDoInsertColumns(StartCol, ColsCount) then
    begin
        _InsertColumns(StartCol, ColsCount, Clear);
        if Assigned(OnGridResized) then OnGridResized(Self);
        if Assigned(OnGridModified) then OnGridModified(Self);        
    end else raise EIDA_Grid.Create('Columns inserting is not allowed...');
end;

procedure TIDAGrid.DeleteColumns(StartCol, ColsCount: LongInt);
begin
    if MayIDoDeleteColumns(StartCol, ColsCount) then
    begin
        _DeleteColumns(StartCol, ColsCount);
        if Assigned(OnGridResized) then OnGridResized(Self);
        if Assigned(OnGridModified) then OnGridModified(Self);                
    end else raise EIDA_Grid.Create('Columns deleting is not allowed...');
end;

procedure TIDAGrid.DeleteAllData;
begin
    if MayIDoDeleteAllData then
    begin
        _DeleteAllData;
        if Assigned(OnGridResized) then OnGridResized(Self);
        if Assigned(OnGridModified) then OnGridModified(Self);        
    end else raise EIDA_Grid.Create('Data deleting not allowed...');    
end;

procedure TIDAGrid.ClearSelectedArea;
begin
    if MayIDoClearSelectedArea then
    begin
        _ClearSelectedArea;
        if Assigned(OnGridModified) then OnGridModified(Self);        
    end
    else raise EIDA_Grid.Create('Area clearing is impossible...');
end;

procedure TIDAGrid.ClearAllCells;
begin
    if MayIDoClearAllCells then
    begin
        _ClearAllCells;
        if Assigned(OnGridModified) then OnGridModified(Self);
    end
    else raise EIDA_Grid.Create('Table clearing is impossible...');
end;

procedure TIDAGrid.DeleteSelection;
begin
    //  очистка всей таблицы - должно идти первым, потому что требует
    //  одновременного выполнения последующих условий, касающихся удаления
    //  строк и столбцов
    if (Selection.Left = FixedCols) and (Selection.Right = ColCount - 1) and
       (Selection.Top = FixedRows) and (Selection.Bottom = RowCount - 1) then
    begin
        if MayIDoDeleteAllData then
        begin
            if MessageDlg('Clear table ?', mtConfirmation,
                [mbYes, mbNo, mbCancel], 0) = mrYes then
            begin
                DeleteAllData;
                ClearSelection; //  нужна здесь, так как после удаления всех
                                //  данный остается как минимум одна пустая ячейка
            end;
        end
        else MessageDlg('Table clearing is impossible...', mtWarning, [mbOk], 0);
        Exit;
    end;

    //  удаление строк таблицы
    if (Selection.Left = FixedCols) and (Selection.Right = ColCount - 1) then
    begin
        if MayIDoDeleteRows(Selection.Top, Selection.Bottom - Selection.Top + 1) then
        begin
            if Selection.Top <> Selection.Bottom then
                //  вопрос задается только если число строк превышает 1
                if MessageDlg('Delete selected rows ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) <> mrYes then Exit;

            DeleteRows(Selection.Top, Selection.Bottom - Selection.Top + 1)
        end else
            if MayIDoClearSelectedArea then
            begin
                if MessageDlg('Clear selected area ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) = mrYes then
                begin
                    ClearSelectedArea;
                    ClearSelection;
                end;
            end else    //  ничего нельзя сделать
                MessageDlg('Rows deleting is impossible...', mtWarning, [mbOk], 0);
        Exit;
    end;

    //  удаление столбцов таблицы
    if (Selection.Top = FixedRows) and (Selection.Bottom = RowCount - 1) then
    begin
        if MayIDoDeleteColumns(Selection.Left, Selection.Right - Selection.Left + 1) then
        begin
            if Selection.Left <> Selection.Right then
                //  вопрос задается только если число столбцов превышает 1
                if MessageDlg('Delete selected columns ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) <> mrYes then Exit;

            DeleteColumns(Selection.Left, Selection.Right - Selection.Left + 1);
        end else
            if MayIDoClearSelectedArea then
            begin
                if MessageDlg('Clear selected area ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) = mrYes then
                begin
                    ClearSelectedArea;
                    ClearSelection;
                end;
            end else    //  ничего нельзя сделать
                MessageDlg('Columns deleting is impossible...', mtWarning, [mbOk], 0);
        Exit;
    end;

    //  очистка выделенной области таблицы
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
        end else    //  нельзя очистить таблицу
            MessageDlg('Clearing is impossible...', mtWarning, [mbOk], 0);
    end;
end;

procedure TColoredGrid.DrawCell(ACol, ARow: Longint;
    ARect: TRect; AState: TGridDrawState);

    function GetColorByDefault(ACol, ARow: LongInt): TColor;
    begin
        if Odd(ARow) then Result := OddRowColor
        else Result := EvenRowColor;
    end;

var SaveColor, TempColor: TColor;
    X, Y: Integer;
begin
    if Assigned(OnDrawCell) then OnDrawCell(Self, ACol, ARow, ARect, AState)
    else begin
        SaveColor := Canvas.Brush.Color;
        if not (gdFixed in AState) then
        (*фиксированные клетки рисуются по старому*)
        begin
            //  метод предка вызывается для того, чтобы
            //  нарисовать границы ячейки способом по-умолчанию
            inherited DrawCell(ACol, ARow, ARect, AState);

            if (gdSelected in AState) or (gdFocused in AState) then
                (*клетка принадлежит выделенной области - рисовать цветом
                выделенной области*)
                Canvas.Brush.Color := SelectedRegionColor
            else
                if GetMyGridDataSource <> nil then
                begin
                    if GetMyGridDataSource.GetCellColor(ACol, ARow, TempColor) then
                        (*объект - сервер установил новый цвет*)
                        Canvas.Brush.Color := TempColor
                    else (*объект - сервер предлагает рисовать цветом по умолчанию*)
                        if GetMyGridDataSource.IsCellDisabled(ACol, ARow) then
                            Canvas.Brush.Color := DisabledColor
                        else Canvas.Brush.Color := GetColorByDefault(ACol, ARow);
                end else Canvas.Brush.Color := GetColorByDefault(ACol, ARow);

            Inc(ARect.Left); Dec(ARect.Right);
            Inc(ARect.Top); Dec(ARect.Bottom);
            Canvas.FillRect(ARect);
            X := ARect.Right - Canvas.TextWidth(Cells[ACol, ARow]) - 2;
            Y := ARect.Bottom - Canvas.TextHeight(Cells[ACol, ARow]) - 2;
            Canvas.TextRect(ARect, X, Y, Cells[ACol, ARow]);
        end else inherited DrawCell(ACol, ARow, ARect, AState);
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

procedure TDataGrid.SetGridDataSource(
    GridDataSource: IGridDataSource);
begin
    if (FGridDataSource <> nil) and
       (GridDataSource <> FGridDataSource) then SaveTableParams;
       //   если предыдущий и новый источник данных совпадают
       //   это означает попытку обновления данных источника -
       //   сохранять параметры нельзя - число строк/столбцов
       //   могло измениться...

    FGridDataSource := GridDataSource;
    if GridDataSource <> nil then
    begin
        GetTableParams;
        ShowTable;

        FillRowHeaders;
        FillColHeaders;
        FillTable;

        GetWidthsHeights;   //  д.б. после заполнения таблицы
                            //  для правильной автоматической
                            //  установки ширин и высот ячеек

        Changeable := True; //  по умолчанию, при установке
                            //  пассивного источника, текст
                            //  в ячейках может редактироваться
    end else HideTable;
end;

procedure TDataGrid._DeleteAllData;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do AllDataDeleted;
    inherited;  //  здесь, чтобы обеспечить возможность объекту
                //  сгенерировать исключение в случае ошибки, и
                //  не дать возможности изменить таблицу
end;

procedure TDataGrid._ClearSelectedArea;
begin
    inherited;
    with Selection do DataChanged(Left, Top, Right, Bottom);
end;

procedure TDataGrid._ClearAllCells;
begin
    inherited;
    DataChanged(FixedCols, FixedRows, ColCount - 1, RowCount - 1);
end;

procedure TDataGrid._InsertRows(StartRow, RowsCount: LongInt; Clear: Boolean);
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do RowsInserted(StartRow, RowsCount);
    inherited;  //  здесь, чтобы обеспечить возможность объекту
                //  сгенерировать исключение в случае ошибки, и
                //  не дать возможности изменить таблицу
end;

procedure TDataGrid._AddRow;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do RowAdded;
    inherited;  //  здесь, чтобы обеспечить возможность объекту
                //  сгенерировать исключение в случае ошибки, и
                //  не дать возможности изменить таблицу
end;

procedure TDataGrid._DeleteRows(StartRow, RowsCount: LongInt);
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do RowsDeleted(StartRow, RowsCount);
    inherited;  //  здесь, чтобы обеспечить возможность объекту
                //  сгенерировать исключение в случае ошибки, и
                //  не дать возможности изменить таблицу
end;

procedure TDataGrid._InsertColumns(
    StartCol, ColsCount: LongInt; Clear: Boolean);
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do ColumnsInserted(StartCol, ColsCount);
    inherited;  //  здесь, чтобы обеспечить возможность объекту
                //  сгенерировать исключение в случае ошибки, и
                //  не дать возможности изменить таблицу
end;

function TDataGrid.GetMyGridDataSource: IGridDataSource;
begin
    if not (csDestroying in ComponentState) then Result := FGridDataSource
    else Result := nil;
end;

procedure TDataGrid._AddColumn;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do ColumnAdded;
    inherited;  //  здесь, чтобы обеспечить возможность объекту
                //  сгенерировать исключение в случае ошибки, и
                //  не дать возможности изменить таблицу
end;

procedure TDataGrid._DeleteColumns(StartCol, ColsCount: LongInt);
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do ColumnsDeleted(StartCol, ColsCount);
    inherited;  //  здесь, чтобы обеспечить возможность объекту
                //  сгенерировать исключение в случае ошибки, и
                //  не дать возможности изменить таблицу
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

function TIDAGrid.PasteFromClipBoard: Boolean;
var Count: Longint;
    Buffer: array[0..BufCount] of Char;
    St: string;
    Index: LongInt;
    BufferColCount, BufferRowCount: LongInt;
    TempCol, TempRow: LongInt;
    i, j: LongInt;
    SavedSelection, InsertedArea: TGridRect;
    SelectionSize: LongInt;
begin
    if not Clipboard.HasFormat(CF_TEXT) then
    begin
        MessageDlg('Clipboard doesn''t contain text data...', mtError, [mbOk], 0);
        Exit;
    end;
    if MessageDlg('Overwrite this data ?', mtWarning,
    [mbYes, mbNo, mbCancel], 0) <> mrYes then Exit;

    //  ??? если нажата кнопка No, то нужно вставить
    //  строки, добавляя новые строки ячеек

    Count := ClipBoard.GetTextBuf(@Buffer, BufCount);
    ExtractGridSizes(Buffer, Count, BufferColCount, BufferRowCount);

    SavedSelection := Selection;
        //  координаты выделенной области нужно
        //  сохранить, поскольку после вставки строк/столбцов
        //  Selection сбрасывается кем-то из предков

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
        MessageDlg('Vague number of cell, since' +
            'data do not have tabular format...',
            mtError, [mbOk], 0);
        Exit;
    end;

    //  вычисление координат области которая была вставлена
    InsertedArea.Left := SavedSelection.Left;
    InsertedArea.Top := SavedSelection.Top;
    if ColCount - 1 < SavedSelection.Left + BufferColCount - 1 then
        InsertedArea.Right := ColCount - 1
    else InsertedArea.Right := SavedSelection.Left + BufferColCount - 1;

    if RowCount - 1 < SavedSelection.Top + BufferRowCount - 1 then
        InsertedArea.Bottom := RowCount - 1
    else InsertedArea.Bottom := SavedSelection.Top + BufferRowCount - 1;

    Selection := InsertedArea;  //  вставленные данные выделяются

    //  проверка правильности вставленного текста
    with Selection do DataChanged(Left, Top, Right, Bottom);
    if Assigned(OnGridModified) then OnGridModified(Self);    
end;

procedure TIDAGrid.SelectAll;
var R: TGridRect;
begin
    R.Left := FixedCols; R.Right := ColCount - 1;
    R.Top := FixedRows; R.Bottom := RowCount - 1;
    Selection := R;
end;

procedure TIDAGrid.ClearSelection;
var R: TGridRect;
begin
    R.Left := Col; R.Right := Col;
    R.Top := Row; R.Bottom := Row;
    Selection := R;
end;

procedure TIDAGrid.KeyPress(var Key: Char);
begin
    inherited KeyPress(Key);
    (* ??? добавление строки д.б. запрещено до тех пор пока не
    будет механизма обновления данных в таблице по инициативе источника
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
        if Assigned(OnGridResized) then OnGridResized(Self);
        if Assigned(OnGridModified) then OnGridModified(Self);                
    end else raise EIDA_Grid.Create('Row adding is impossible...');
end;

procedure TIDAGrid.AddColumn;
begin
    if MayIDoAddColumn then
    begin
        _AddColumn;
        if Assigned(OnGridResized) then OnGridResized(Self);
        if Assigned(OnGridModified) then OnGridModified(Self);
    end else raise EIDA_Grid.Create('Column adding is impossible...');
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

function TNumericGrid.CanEditAcceptKey(Key: Char): Boolean;
const
    REAL_SET: set of Char = ['0'..'9', '.', ',', '-', '+'];
    //  Positive real numbers.
    //POS_REAL_SET: set of Char = ['0'..'9', '.', ',', '+'];
    INT_SET: set of Char = ['0'..'9', '-', '+'];
    //  Positive integer numbers.
    //POS_INT_SET: set of Char = ['0'..'9', '+'];
    CHAR_SET: set of Char = ['A'..'Z', 'a'..'z'];

begin
    if Key >= #20 then

        case ColOptions[Col] of
            coReal :
                if Key in REAL_SET then Result := True
                else Result := False;

            coInteger :
                if Key in INT_SET then Result := True
                else Result := False;

            coChars :
                if Key in CHAR_SET then Result := True
                else Result := False;

            coText : Result := True;

            coDisabled : Result := False;

            else Result := True;
        end
    //  Special characters processing.
    else Result := True;
end;

procedure TNumericGrid.SetColOption(Index: LongInt; Value: TColOption);
var i: LongInt;
begin
     if Assigned(ColOptarray) then
     begin
          if (Index < 0) or (Index >= Length(ColOptarray)) then
             raise ENumericGrid.Create('Invalid option index...')
          else ColOptArray[Index] := Value;
          if Value = coDisabled then
          begin
               (*???TabStops[Index] := False;*)
               if Assigned(FColorMatrix) then
                  for i := 0 to RowCount - 1 do
                      CellsColors[Index, i] := DisabledColor;
          end;
     end;
end;

function TNumericGrid.GetColOption(Index: LongInt): TColOption;
begin
     if (Index < 0) or (Index >= Length(ColOptarray)) then
        raise ENumericGrid.Create('Invalid option index...')
     else Result := ColOptarray[Index];
end;

procedure TNumericGrid.MouseUp;
begin
    SelFlag := False;
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TNumericGrid.MouseDown;
var Coord: TGridCoord;
    R: TGridRect;
begin
    inherited MouseDown(Button, Shift, X, Y);
    
    Coord := MouseCoord(X, Y);
{$ifndef lazarus}
    //  в библ. Lazarus это уже реализовано включением
    //  флага AutoEdit
    if Shift = [ssLeft, ssDouble] then
    begin
        if (Coord.X > FixedCols - 1) and
           (Coord.Y > FixedRows - 1) then
        begin
            R.Left := Coord.X; R.Right := Coord.X;
            R.Top := Coord.Y; R.Bottom := Coord.Y;
            Selection := R;
            Col := Coord.X;
            Row := Coord.Y;
            if goEditing in Options then
                EditorMode := True;
        end;
    end
    else
{$endif}
    //  это работает, если включено goRangeSelect
    if Shift = [ssLeft] then
    begin
        Coord := MouseCoord(X, Y);
        if (Coord.X <= FixedCols - 1) or
           (Coord.Y <= FixedRows - 1) then
        begin
            EditorMode := False;
            
            if (Coord.Y <= FixedRows - 1) and
               (Coord.X >= FixedCols) then(*выбран столбец*)
            begin
                SelFlag := True;
                StartCoord := MouseCoord(X, Y);
                SavedCoord := StartCoord;
                R.Top := FixedRows; R.Bottom := RowCount - 1;
                R.Left := StartCoord.X; R.Right := StartCoord.X;
                Selection := R;
            end;
            if (Coord.X <= FixedCols - 1) and
               (Coord.Y >= FixedRows) then(*выбрана строка*)
            begin
                SelFlag := True;
                StartCoord := MouseCoord(X, Y);
                SavedCoord := StartCoord;
                R.Left := FixedCols; R.Right := ColCount - 1;
                R.Top := StartCoord.Y; R.Bottom := StartCoord.Y;
                Selection := R;
            end;
            if (Coord.X <= FixedCols - 1) and
               (Coord.Y <= FixedRows - 1) then(*выбрана вся таблица*)
            begin
                R.Left := FixedCols; R.Right := ColCount - 1;
                R.Top := FixedRows; R.Bottom := RowCount - 1;
                Selection := R;
            end;
        end
{$ifndef lazarus}
        else
        (*сброс выделенной области*)
        begin
            R.Left := Coord.X; R.Right := Coord.X;
            R.Top := Coord.Y; R.Bottom := Coord.Y;
            Selection := R;
            EditorMode := False;
        end;
{$endif}
    end;
end;

procedure TNumericGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var Coord: TGridCoord;
    R: TGridRect;
begin
  if SelFlag then
  begin
    Coord := MouseCoord(X, Y);
    if (Coord.X <> SavedCoord.X) or (Coord.Y <> SavedCoord.Y) then
    begin
      if (StartCoord.Y <= FixedRows - 1) and
         (StartCoord.X >= FixedCols) and
         (Coord.X >= FixedCols) then(*выбираются столбцы*)
      begin
        R.Top := FixedRows; R.Bottom := RowCount - 1;
        if Coord.X < StartCoord.X then
        begin R.Left := Coord.X; R.Right := StartCoord.X end;
        if Coord.X > StartCoord.X then
        begin R.Left := StartCoord.X; R.Right := Coord.X end;
        if Coord.X = StartCoord.X then
        begin R.Left := StartCoord.X; R.Right := StartCoord.X end;
        Selection := R;
        if (Coord.X - LeftCol = VisibleColCount) and
           (Coord.X < ColCount - 1) then LeftCol := LeftCol + 1;
        if (Coord.X = LeftCol) and
           (LeftCol > FixedCols) then LeftCol := LeftCol - 1;
      end;
      if (StartCoord.X <= FixedCols - 1) and
         (StartCoord.Y >= FixedRows) and
         (Coord.Y >= FixedRows) then(*выбираются строки*)
      begin
        R.Left := FixedCols; R.Right := ColCount - 1;
        if Coord.Y < StartCoord.Y then
        begin R.Top := Coord.Y; R.Bottom := StartCoord.Y end;
        if Coord.Y > StartCoord.Y then
        begin R.Top := StartCoord.Y; R.Bottom := Coord.Y end;
        if Coord.Y = StartCoord.Y then
        begin R.Top := StartCoord.Y; R.Bottom := StartCoord.Y end;
        Selection := R;
        if (Coord.Y - TopRow = VisibleRowCount) and
           (Coord.Y < RowCount - 1) then TopRow := TopRow + 1;
        if (Coord.Y = TopRow) and
           (TopRow > FixedRows) then TopRow := TopRow - 1;
      end;
      SavedCoord := Coord;
    end;{if (Coord.X <> SavedCoord.X) or (Coord.Y <> SavedCoord.Y) then...}
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TNumericGrid.InsertRows(StartPos, Count: LongInt; Clear: Boolean);
var i, j: LongInt;
begin
  RowCount := RowCount + Count;
  for i := RowCount - 1 downto StartPos + Count do
    for j := 0 to ColCount - 1 do Cells[j, i] := Cells[j, i - Count];
  if Clear then
    for i := 0 to Count - 1 do
      for j := 0 to ColCount - 1 do Cells[j, StartPos + i] := '';
end;

procedure TNumericGrid.DeleteRows(StartPos, Count: LongInt);
var i, j: LongInt;
begin
  for i := StartPos to RowCount - 1 - Count do
    for j := 0 to ColCount - 1 do Cells[j, i] := Cells[j, i + Count];
  RowCount := RowCount - Count;
end;

procedure TNumericGrid.DeleteSelection;
var i, j: LongInt;
begin
  if (Selection.Left = FixedCols) and (Selection.Right = ColCount - 1) then
  (*удаление строк таблицы*)
  begin
    if not RowNumFixed then
    begin
      if Selection.Top <> Selection.Bottom then
      if MessageDlg('Delete all selected rows ?', mtWarning,
      [mbYes, mbNo, mbCancel], 0) <> mrYes then Exit;
      for i := 0 to RowCount - 2 - Selection.Bottom do
        for j := 1 to ColCount - 1 do
          Cells[j, Selection.Top + i] := Cells[j, Selection.Bottom + i + 1];
      RowCount := RowCount - (Selection.Bottom - Selection.Top + 1);
    end
    else MessageDlg('Rows deleting is not allowed...', mtWarning, [mbOk], 0);
    ClearSelection;
    Exit;
  end;

  if (Selection.Top = FixedRows) and (Selection.Bottom = RowCount - 1) then
  (*удаление столбцов таблицы*)
  begin
    if not ColNumFixed then
    begin
      if Selection.Left <> Selection.Right then
      if MessageDlg('Delete all selected columns ?', mtWarning,
      [mbYes, mbNo, mbCancel], 0) <> mrYes then Exit;
      for i := 0 to ColCount - 2 - Selection.Right do
        for j := 1 to RowCount - 1 do
          Cells[Selection.Left + i, j] := Cells[Selection.Right + i + 1, j];
      ColCount := ColCount - (Selection.Right - Selection.Left + 1);
    end
    else MessageDlg('Columns deleting is not allowed...', mtWarning, [mbOk], 0);
    ClearSelection;
    Exit;
  end;

  if ((Selection.Top <> FixedRows) or (Selection.Bottom <> RowCount - 1)) and
     ((Selection.Left <> FixedCols) or (Selection.Right <> ColCount - 1)) then
  (*очистка выделенной области таблицы*)
  begin
    if Selection.Top <> Selection.Bottom then
      if MessageDlg('Clear all selected cells ?', mtWarning,
      [mbYes, mbNo, mbCancel], 0) <> mrYes then Exit;
    for i := Selection.Top to Selection.Bottom do
      for j := Selection.Left to Selection.Right do Cells[j, i] := '';
    ClearSelection;
  end;
end;

procedure TColorStringGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
    AState: TGridDrawState);
var SaveColor: TColor;
    X, Y: Integer;
begin
     SaveColor := Canvas.Brush.Color;
     if not (gdFixed in AState) then
     begin
        //  метод предка вызывается для того, чтобы
        //  нарисовать границы ячейки способом по-умолчанию
        inherited DrawCell(ACol, ARow, ARect, AState);
        //  перерисовываем содержимое ячейки
        if (gdSelected in AState) or (gdFocused in AState) then
            Canvas.Brush.Color := SelectedRegionColor
        else Canvas.Brush.Color := GetCellColor(ACol, ARow);

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
     else inherited DrawCell(ACol, ARow, ARect, AState);
     Canvas.Brush.Color := SaveColor;
end;

function TColorStringGrid.GetCellColor(const ColNum, RowNum: LongInt): TColor;
var CellColor: TColor;
begin
     if Assigned(OnGetCellColor) then
     begin
          OnGetCellColor(Self, ColNum, RowNum, CellColor);
          Result := CellColor;
     end
     else
         if Assigned(FColorMatrix) then
            Result := CellsColors[ColNum, RowNum]
         else
             if Odd(RowNum) then Result := OddRowColor
             else Result := EvenRowColor;
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

function TColorStringGrid.GetCellsColors(ACol, ARow: LongInt): TColor;
begin
     if (ACol < 0) or (ACol >= ColCount) then
        raise EColorStringGrid.Create('Invalid column number...');
     if (ARow < 0) or (ARow >= RowCount) then
        raise EColorStringGrid.Create('Invalid row number...');
     Result := FColorMatrix[ARow, ACol];
end;

procedure TColorStringGrid.SetCellsColors(ACol, ARow: LongInt; AColor: TColor);
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
  if csDesigning in ComponentState then DisabledColor := clGray;
end;

procedure TColorStringGrid.SetOddRowColor(Color: TColor);
var i, j: LongInt;
begin
  FOddRowColor := Color;
  if Assigned(FColorMatrix) then
  begin
       for i := 0 to Length(FColorMatrix) - 1 do
           for j := 0 to Length(FColorMatrix[i]) - 1 do
               if Odd(i) then FColorMatrix[i, j] := Color;
  end;
  //Repaint;
end;

procedure TColorStringGrid.SetEvenRowColor(Color: TColor);
var i, j: LongInt;
begin
  FEvenRowColor := Color;
  if Assigned(FColorMatrix) then
  begin
       for i := 0 to Length(FColorMatrix) - 1 do
           for j := 0 to Length(FColorMatrix[i]) - 1 do
               if not Odd(i) then FColorMatrix[i, j] := Color;
  end;
  //Repaint;
end;

procedure TColorStringGrid.SetSelectedRegionColor(Color: TColor);
begin
  FSelectedRegionColor := Color;
  Repaint;
end;

procedure TNumericGrid.SetDisabledColor(Color: TColor);
var i, j: LongInt;
begin
    FDisabledColor := Color;
    if Assigned(FColorMatrix) and Assigned(ColOptArray) then
    begin
      for i := 0 to RowCount - 1 do
         if ColCount = Length(ColOptArray) then
            for j := 0 to ColCount - 1 do
                if ColOptArray[j] = coDisabled then CellsColors[j, i] := Color;
    end;
    Repaint;
end;

procedure TClipboardGrid.SetColCount(Value: Longint);
begin
    TStringGrid(Self).ColCount := Value;
end;

function TClipboardGrid.GetColCount: LongInt;
begin
    Result := TStringGrid(Self).ColCount;
end;

procedure TClipboardGrid.SetRowCount(Value: Longint);
begin
    TStringGrid(Self).RowCount := Value;
end;

function TClipboardGrid.GetRowCount: LongInt;
begin
    Result := TStringGrid(Self).RowCount;
end;

procedure TClipboardGrid.EnumerateRows;
var i: LongInt;
begin
    if FixedCols <> 0 then
        for i := FixedRows to RowCount - 1 do Cells[0, i] := IntToStr(i);
end;

function TClipboardGrid.CopyToClipBoard: Boolean;
var St, St2, St3: string;
    i, j: LongInt;
begin
    Result := False;
    with Selection do
    begin
        if (Top = Bottom) and (Left = Right) then
        begin
            MessageDlg('Choose area for copying...', mtWarning, [mbOk], 0);
            Exit;
        end;
        try
            St := '';
            for i := Top to Bottom do
            begin
                St2 := '';
                for j := Left to Right do
                begin
                    if j <> Right then St3 := Cells[j, i] + #9
                        else St3 := Cells[j, i];
                    St2 := St2 + St3;
                end;
                St := St + St2 + #13#10;
            end;
            ClipBoard.SetTextBuf(PChar(St));
        except Exit end;
    end;{With Selection do...}
    Result := True;
end;

function TClipboardGrid.CheckingTextValidity(St: string;
    ACol, ARow: LongInt): Boolean;
begin
    Result := True;
end;

procedure TClipboardGrid.ExtractGridSizes(Buffer: array of Char;
    const Count: LongInt; var BufferCols, BufferRows: LongInt);
var i: LongInt;
    Flag, PrevIsDelimiter: Boolean;
begin
    BufferCols := 0; BufferRows := 0;
    Flag := True; PrevIsDelimiter := False;
    for i := 0 to Count - 1 do
    begin
        if (Buffer[i] in DelimiterChars) then
        begin
            if Flag and not PrevIsDelimiter then Inc(BufferCols);
            PrevIsDelimiter := True;
        end
        else
            PrevIsDelimiter := False;

        if Buffer[i] = #13 then Flag := False;
        if Buffer[i] = #10 then
        begin
            Flag := False;
            Inc(BufferRows);
        end;
    end;
end;

function TClipboardGrid.ExtractString(Buffer: array of Char;
    const Count: LongInt; var Index: LongInt): string;
var St: string;
    i, j, k: LongInt;
const BadSymbols: set of Char = [#10, #13];
begin
    St := '';
    for i := Index to Count - 1 do
    begin
        if Buffer[i] in DelimiterChars then
        begin
            for j := Index to i - 1 do
            begin
                if not (Buffer[j] in BadSymbols) then
                begin
                    St := St + Buffer[j];
                end;
            end;
            j := i;
            if Buffer[j] = #9 then k := j + 1
            else
                for k := j to Count - 1 do
                    if not (Buffer[k] in DelimiterChars) then Break;
            Index := k;
            Result := St;
            Exit;
        end;
    end;
    Result := St;
end;

procedure TClipboardGrid.ClearFixed;
var i, j: LongInt;
begin
    for i := 0 to FixedRows - 1 do
        for j := 0 to ColCount - 1 do Cells[j, i] := '';
    for i := 0 to FixedCols - 1 do
        for j := FixedRows to RowCount - 1 do Cells[i, j] := '';
end;

function TClipboardGrid.PasteFromClipBoard: Boolean;
var Count: Longint;
    Buffer: array[0..BufCount] of Char;
    St: string;
    Index: LongInt;
    BufferColCount, BufferRowCount: LongInt;
    TempCol, TempRow: LongInt;
    i, j: LongInt;
begin
    Result := False;
    BufferColCount := 0; BufferRowCount := 0;
    if not Clipboard.HasFormat(CF_TEXT) then
    begin
        MessageDlg('Clipboard contains no text data...', mtError, [mbOk], 0);
        Exit;
    end;
    if MessageDlg('Overwrite this data ?', mtWarning,
        [mbYes, mbNo, mbCancel], 0) <> mrYes then Exit;

    (*если нажата кнопка No, то нужно вставить строки, добавляя новые строки ячеек*)

    Count := ClipBoard.GetTextBuf(@Buffer, BufCount);
    ExtractGridSizes(Buffer, Count, BufferColCount, BufferRowCount);
    if Row < FixedRows then Row := FixedRows;
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
                    else Cells[TempCol, TempRow] := St;
                end;
            end;
    except
        MessageDlg('Vague number of cell, since data do not have tabular format...',
            mtError, [mbOk], 0);
        Result := False;
        Exit;
    end;
    ClearFixed;
    EnumerateRows;
    Result := True;
end;

procedure TColorStringGrid.SelectAll;
var R: TGridRect;
begin
    EditorMode := False;
    R.Left := FixedCols; R.Right := ColCount - 1;
    R.Top := FixedRows; R.Bottom := RowCount - 1;
    Selection := R;
end;

procedure TColorStringGrid.ClearSelection;
var R: TGridRect;
begin
    R.Left := Col; R.Right := Col;
    R.Top := Row; R.Bottom := Row;
    Selection := R;
end;

(*???
function TColorStringGrid.CreateEditor: TInplaceEdit;
begin
//  Result := TModifiedEditor.Create(Self);
  Result := inherited CreateEditor;
end;
*)
function  TNumericGrid.CheckingTextValidity(St: string;
ACol, ARow: LongInt): Boolean;
begin
  if St = '' then
  begin Result := True; Exit end;
  if ColOptions[ACol] = coInteger then
  begin
    try StrToInt(St); except Result := False; Exit end;
    Result := True;
    Exit;
  end;
  if ColOptions[ACol] = coReal then
  begin
    try StrToFloat(St); except Result := False; Exit end;
    Result := True;
    Exit;
  end;
  Result := True;
end;

procedure TColorStringGrid.ResetAll;
var i, j: LongInt;
begin
  ColCount := 10;
  RowCount := 2;
  FixedCols := 1;
  FixedRows := 1;
  Col := 1; Row := 1;
  ClearSelection;
  for i := 0 to ColCount - 1 do ColWidths[i] := DefaultColWidth;
  for i := 0 to RowCount - 1 do RowHeights[i] := DefaultRowHeight;
  for i := 0 to ColCount - 1 do
    for j := 0 to RowCount - 1 do  Cells[i, j] := '';
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

procedure TNumericGrid.SetColCount(Value: Longint);
begin
     SetOptCount(Value);
     inherited;
end;

procedure TNumericGrid.KeyPress(var Key: Char);
var i: LongInt;
    St: string;

begin
    if goEditing in Options then
    begin
        if CanEditAcceptKey(Key) then
            inherited KeyPress(Key)
        //  Block further processing.
        else Key := #0;
    end
    else
        inherited KeyPress(Key);

    case Key of
        #9 : begin
                 if not RowNumFixed then
                     if (Col = 1) and (Row = 1) then
                     begin
                         RowCount := RowCount + 1;
                         Str(RowCount - 1, St);
                         Cells[0, RowCount - 1] := St;
                         Col := 1;
                         Row := RowCount - 1;
                         for i := 1 to ColCount - 1 do Cells[i, Row] := '';
                     end
             end;

        ',' :
            //  Substitute decimal separator.
            if ColOptions[Col] = coReal then Key := '.';
    end;
end;

procedure TNumericGrid.ResetColWidths;
var i: LongInt;
begin
    for i := 0 to ColCount - 1 do SetColWidthByDefault(i);
end;

procedure TIDAGrid.AutoColWidths;
var i: LongInt;
begin
    for i := 0 to ColCount - 1 do SetAutoColWidth(i);
end;

procedure TNumericGrid.SetColWidthByDefault(ACol: LongInt);
var Width: LongInt;
begin
    Width := GetMaxTextWidth(Self, ACol);
    if Width = 0 then Width := 40 else Width := Width + 10;
    ColWidths[ACol] := Width;
end;

procedure TIDAGrid.SetAutoColWidth(ACol: LongInt);
var Width: LongInt;
begin
    Width := GetMaxTextWidth(Self, ACol);
    if Width = 0 then Width := MIN_WIDTH;
    Width := Width + 10;
    ColWidths[ACol] := Width;
end;

procedure TColorStringGrid.SetColCount(Value: Longint);
var i, j: LongInt;
    SavedLength: LongInt;
begin
     if not (csDesigning in ComponentState) then
     begin
          (*!!! в design-time это не работает !!!*)
          if not Assigned(FColorMatrix) then InitColorMatrix;
          for i := 0 to RowCount - 1 do
              if Length(FColorMatrix[i]) <> Value then
              begin
                   SavedLength := Length(FColorMatrix[i]);
                   SetLength(FColorMatrix[i], Value);
                   for j := SavedLength to Length(FColorMatrix[i]) - 1 do
                       if Odd(i) then FColorMatrix[i, j] := OddRowColor
                   else FColorMatrix[i, j] := EvenRowColor;
              end;
     end;
     TStringGrid(Self).ColCount := Value;
end;

procedure TColorStringGrid.InitColorMatrix;
var i: LongInt;
begin
     SetLength(FColorMatrix, RowCount);
     for i := 0 to RowCount - 1 do SetLength(FColorMatrix[i], ColCount);
end;

procedure TColorStringGrid.FinalizeColorMatrix;
var i: LongInt;
begin
     if Assigned(FColorMatrix) then
     begin
          for i := 0 to RowCount - 1 do Finalize(FColorMatrix[i]);
          Finalize(FColorMatrix);
     end;
end;

function TColorStringGrid.GetColCount: LongInt;
begin
     Result := TStringGrid(Self).ColCount;
end;

procedure TColorStringGrid.SetRowCount(Value: Longint);
var i, j: LongInt;
    SavedLength: LongInt;
begin
     if not (csDesigning in ComponentState) then
     begin
          (*!!! в design-time это не работает !!!*)     
          if not Assigned(FColorMatrix) then InitColorMatrix;
          if Value < RowCount then
          begin
               for i := Value to RowCount - 1 do Finalize(FColorMatrix[i]);
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
                        if Odd(i) then FColorMatrix[i, j] := OddRowColor
                        else FColorMatrix[i, j] := EvenRowColor;
               end;
          end;
     end;
     //???  inherited; можно ли заменить на это
     TStringGrid(Self).RowCount := Value;
end;

function TColorStringGrid.GetRowCount: LongInt;
begin
     Result := TStringGrid(Self).RowCount;
end;

function GetMaxTextWidth(
    const Grid: TStringGrid; const ColNum: LongInt): LongInt;
var i: LongInt;
begin
    Result := 0;
    with Grid do
        for i := 0 to RowCount - 1 do
            if Canvas.TextWidth(Cells[ColNum, i]) > Result then
                Result := Canvas.TextWidth(Cells[ColNum, i]);
end;

function GetMaxTextHeight(
    const Grid: TStringGrid; const RowNum: LongInt): LongInt;
var i: LongInt;
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
    if not ColNumFixed then ColCount := FixedCols + 1;
    if not RowNumFixed then RowCount := FixedRows + 1;
    //???    ClearAllCells; можно ввестиновый параметр функции Clear
    //  и руководствоваться им
    FillArea(FixedCols, FixedRows, ColCount - 1, RowCount - 1);
    FillRowHeaders;
    FillColHeaders;
end;

procedure TIDAGrid._DeleteColumns(StartCol, ColsCount: Integer);
var i, j: LongInt;
begin
    for i := StartCol to ColCount - 1 - ColsCount do
        for j := 0 to RowCount - 1 do Cells[i, j] := Cells[i + ColsCount, j];
    ColCount := ColCount - ColsCount;
    FillColHeaders;
end;

procedure TIDAGrid._DeleteRows(StartRow, RowsCount: Integer);
var i, j: LongInt;
begin
    for i := StartRow to RowCount - 1 - RowsCount do
        for j := 0 to ColCount - 1 do Cells[j, i] := Cells[j, i + RowsCount];
    RowCount := RowCount - RowsCount;
    FillRowHeaders;
end;

procedure TIDAGrid._InsertColumns(StartCol, ColsCount: Integer;
    Clear: Boolean);
var i, j: LongInt;
begin
    ColCount := ColCount + ColsCount;
    for i := ColCount - 1 downto StartCol + ColsCount do
        for j := 0 to RowCount - 1 do Cells[i, j] := Cells[i - ColsCount, j];
    if Clear then
        ClearArea(StartCol, FixedRows, StartCol + ColsCount - 1, RowCount - 1)
    else FillArea(StartCol, FixedRows, StartCol + ColsCount - 1, RowCount - 1);
    FillColHeaders;
end;

procedure TIDAGrid._InsertRows(StartRow, RowsCount: Integer;
    Clear: Boolean);
var i, j: LongInt;
begin
    RowCount := RowCount + RowsCount;
    for i := RowCount - 1 downto StartRow + RowsCount do
        for j := 0 to ColCount - 1 do Cells[j, i] := Cells[j, i - RowsCount];
    if Clear then
        ClearArea(FixedCols, StartRow, ColCount - 1, StartRow + RowsCount - 1)
    else FillArea(FixedCols, StartRow, ColCount - 1, StartRow + RowsCount - 1);
    FillRowHeaders;
end;

procedure TIDAGrid._ClearAllCells;
begin
    ClearArea(FixedCols, FixedRows, ColCount - 1, RowCount - 1);
end;

procedure TIDAGrid._ClearSelectedArea;
begin
    with Selection do ClearArea(Left, Top, Right, Bottom);
end;

function TIDAGrid.MayIDoAddColumn: Boolean;
begin
    Result := not ColNumFixed;
end;

function TIDAGrid.MayIDoAddRow: Boolean;
begin
    Result := not RowNumFixed;
end;

function TIDAGrid.MayIDoClearAllCells: Boolean;
begin
    Result := True;
end;

function TIDAGrid.MayIDoClearSelectedArea: Boolean;
begin
    Result := True;
end;

function TIDAGrid.MayIDoDeleteAllData: Boolean;
begin
    Result := not (ColNumFixed and RowNumFixed);
end;

function TIDAGrid.MayIDoDeleteColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    Result := not ColNumFixed;
end;

function TIDAGrid.MayIDoDeleteRows(StartRow, RowsCount: Integer): Boolean;
begin
    Result := not RowNumFixed;
end;

function TIDAGrid.MayIDoInsertColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    Result := not ColNumFixed;
end;

function TIDAGrid.MayIDoInsertRows(StartRow, RowsCount: Integer): Boolean;
begin
    Result := not RowNumFixed;
end;

function TDataGrid.MayIDoAddColumn: Boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoAddColumn and
            GetMyGridDataSource.MayIDoAddColumn
    else Result := inherited MayIDoAddColumn;
end;

function TDataGrid.MayIDoAddRow: Boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoAddRow and
            GetMyGridDataSource.MayIDoAddRow
    else Result := inherited MayIDoAddRow;
end;

function TDataGrid.MayIDoClearAllCells: Boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoClearAllCells and
            GetMyGridDataSource.MayIDoClearAllCells
    else Result := inherited MayIDoClearAllCells;
end;

function TDataGrid.MayIDoClearSelectedArea: Boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoClearSelectedArea and
            GetMyGridDataSource.MayIDoClearSelectedArea
    else Result := inherited MayIDoClearSelectedArea;
end;

function TDataGrid.MayIDoDeleteAllData: Boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoDeleteAllData and
            GetMyGridDataSource.MayIDoDeleteAllData
    else Result := inherited MayIDoDeleteAllData;
end;

function TDataGrid.MayIDoDeleteColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoDeleteColumns(StartCol, ColsCount) and
            GetMyGridDataSource.MayIDoDeleteColumns(StartCol, ColsCount)
    else Result := inherited MayIDoDeleteColumns(StartCol, ColsCount);
end;

function TDataGrid.MayIDoDeleteRows(StartRow,
    RowsCount: Integer): Boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoDeleteRows(StartRow, RowsCount) and
            GetMyGridDataSource.MayIDoDeleteRows(StartRow, RowsCount)
    else Result := inherited MayIDoDeleteRows(StartRow, RowsCount);
end;

function TDataGrid.MayIDoInsertColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoInsertColumns(StartCol, ColsCount) and
            GetMyGridDataSource.MayIDoInsertColumns(StartCol, ColsCount)
    else Result := inherited MayIDoInsertColumns(StartCol, ColsCount);
end;

function TDataGrid.MayIDoInsertRows(StartRow,
    RowsCount: Integer): Boolean;
begin
    if GetMyGridDataSource <> nil then
        Result := inherited MayIDoInsertRows(StartRow, RowsCount) and
            GetMyGridDataSource.MayIDoInsertRows(StartRow, RowsCount)
    else Result := inherited MayIDoInsertRows(StartRow, RowsCount);
end;

{ TGEFGrid }
(*???
function TGEFGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
    Result := inherited CanEditAcceptKey(Key) and CanEditModify;
end;
*)
procedure TGEFGrid.DoExit;
begin
    if Modified then
    begin
        EditingFinished(Col, Row);
        Modified := False;
    end;
    inherited;
end;

procedure TGEFGrid.EditingFinished(const ACol, ARow: Integer);
begin
    if Assigned(OnGridEditingFinished) then
        OnGridEditingFinished(Self, Col, Row);
end;

procedure TGEFGrid.KeyPress(var Key: Char);
begin
    if (goEditing in Options)
       (*???and CanEditAcceptKey(Key)*)
       then Modified := True;
    //  на EditorMode полагаться нельзя - переключается по Enter даже
    //  когда редактирование таблицы запрещено
    inherited;
end;

function TGEFGrid.SelectCell(ACol, ARow: Integer): Boolean;
var MyResult: Boolean;
begin
    //  выбираемая ячейка всегда не совпадает с текущей
    MyResult := True;
    if Modified then
    begin
        try
            EditingFinished(Col, Row);
            Modified := False;
        except
            MyResult := False;
            //  нельзя здесь перевозбуждать исключение -
            //  функция должна завершиться нормально...
            MessageDlg('Invalid input...', mtError, [mbOk], 0);
        end;
    end;
    Result := MyResult and inherited SelectCell(ACol, ARow);
end;

procedure TDataGrid.EditingFinished(const ACol, ARow: Integer);
begin
    DataChanged(Col, Row, Col, Row);
    inherited;  //  событие может быть вызвано только
                //  после выполнения основных действий
end;

procedure TDataGrid.DataChanged(const Left, Top, Right,
    Bottom: Integer);
var i, j: LongInt;
begin
    if GetMyGridDataSource <> nil then with GetMyGridDataSource do
    begin
        for i := Left to Right do
            for j := Top to Bottom do
                if Cells[i, j] = '' then
                begin
                    //  устанавливается значение "по умолчанию"
                    SetValueByDefault(i, j);
                    Cells[i, j] := ValueToString(i, j);
                end else
                begin
                    //  устанавливается введенное значение
                    if not IsDataValid(i, j, Cells[i, j]) then
                        // "мягкая" проверка нужна, поскольку
                        //  функция должна выполниться до конца
                        //  и обновить все данные, которые возможно
                    begin
                        //  ??? здесь можно сделать "жескую" проверку
                        //  в блоке try except с вызовом специального
                        //  интерфейса для отображения всех сообщений об
                        //  ошибках конечно нужно сделать специальное окно
                        SetValueByDefault(i, j);
                        Cells[i, j] := ValueToString(i, j);
                    end else StringToValue(i, j, Cells[i, j]);
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

            //  LeftCol, TopRow должны устанавливаться после Col, Row,
            //  в противном случае устанавливается неправильно
            LeftCol := GetLeftCol;
            TopRow := GetTopRow;

            Selection := GetSelection;

            EditorMode := False;
        end;
end;

procedure TDataGrid.GetWidthsHeights;
var i: LongInt;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
        begin
            if AutoWidths then AutoColWidths else
                for i := 0 to ColCount - 1 do ColWidths[i] := GetColWidth(i);
            if AutoHeights then AutoRowHeights else
                for i := 0 to RowCount - 1 do RowHeights[i] := GetRowHeight(i);
        end;
end;

procedure TIDAGrid.DataChanged(const Left, Top, Right, Bottom: Integer);
begin

end;

procedure TIDAGrid.ClearArea(const Left, Top, Right, Bottom: Integer);
var i, j: LongInt;
begin
    for i := Top to Bottom do
        for j := Left to Right do Cells[j, i] := '';
    DataChanged(Left, Top, Right, Bottom);
end;

procedure TIDAGrid.FillRowHeaders;
begin

end;

procedure TIDAGrid.FillColHeaders;
begin

end;

procedure TDataGrid.SaveTableParams;
var i: LongInt;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
        begin
            for i := 0 to ColCount - 1 do SaveColWidth(i, ColWidths[i]);
            for i := 0 to RowCount - 1 do SaveRowHeight(i, RowHeights[i]);

            SaveLeftCol(LeftCol);
            SaveTopRow(TopRow);
            SaveCol(Col);
            SaveRow(Row);

            SaveSelection(Selection);
        end;
end;

procedure TGEFGrid.SetModified(const AModified: Boolean);
begin
    FModified := AModified;
    if  AModified and Assigned(OnGridModified) then OnGridModified(Self);
end;

procedure TIDAGrid.AutoRowHeights;
var i: LongInt;
begin
    for i := 0 to RowCount - 1 do SetAutoRowHeight(i);
end;

procedure TIDAGrid.SetAutoRowHeight(ARow: Integer);
var Height: LongInt;
begin
    Height := GetMaxTextHeight(Self, ARow);
    if Height = 0 then Height := MIN_HEIGHT;
    Height := Height + 2;
    RowHeights[ARow] := Height;
end;
(*???
function TIDAGrid.CanEditModify: Boolean;
begin
    Result := Changeable;
end;
*)
constructor TIDAGrid.Create(AOwner: TComponent);
begin
    inherited;
    Changeable := True; //  по умолчанию текст в ячейках
                        //  может редактироваться
end;
(*???
function TDataGrid.CanEditModify: Boolean;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            Result := inherited CanEditModify and (not IsCellDisabled(Col, Row))
    else Result := inherited CanEditModify;
end;
*)
(*???
function TDataGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
    if GetMyGridDataSource <> nil then
        with GetMyGridDataSource do
            Result := inherited CanEditAcceptKey(Key) and
                (Key in GetCellEnabledCharSet(Col, Row))
    else Result := inherited CanEditAcceptKey(Key);
end;
*)
procedure TIDAGrid.FillArea(const Left, Top, Right, Bottom: Integer);
begin

end;

procedure TDataGrid.FillArea(const Left, Top, Right, Bottom: Integer);
var i, j: LongInt;
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

