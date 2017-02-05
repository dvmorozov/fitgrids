//      двойной косой чертой комментируются замечания, сохраняемые во
//      всех версиях исходника; фигурными скобками комментируются замечания,
//      сохраняемые только в версии исходника для бесплатного распространения
{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit TableComp;

{$IFDEF Lazarus}
{$MODE Delphi}
{$ENDIF}

interface

uses
    Classes, Grids, Controls, Graphics, Tools,
    SysUtils, NumericGrid, MyExceptions, SelfCopied;

type
    ETableCompList = class(Exception);

    TTableCompList = class(TSelfCopiedCompList, IGridDataSource)
        //  абстрактный список компонентов, который умеет отображать
        //  свойства компонентов в таблице; класс реализует функции
        //  сохранения/чтения свойств таблицы, но никак не связывает
        //  расположение данных в таблице с элементами списка

        //  !!! при изменении количества колонок, возвращаемое GetColCount
        //  нужно обязательно провести соответсвующие изменения в SetCaption,
        //  SetColOptions, SetColFunc, SetRowContents, GetRowContents !!!
    protected
        FCaption: string;

        SavedColWidths: TLongArray;
        SavedRowHeights: TLongArray;
            //  массивы удаляются в деструкторе, поэтому удаление
            //  элементов массива в момент удаления всего обекта
            //  должно быть запрещено
        AreColWidthsReady: Boolean;     //  признак того, что массив уже
        AreRowHeightsReady: Boolean;    //  инициализирован

            //  сохраненные свойства таблицы
        FSavedCol, FSavedRow, FSavedLeftCol, FSavedTopRow: LongInt;
        FSavedSelection: TGridRect;

        SettingsSaved: Boolean;
            //  признак того, что параметры таблицы были
            //  сохранены - устанавливается в GridRelease
        HeightsSaved, WidthsSaved: Boolean;

        Destroying: Boolean;
            //  объект находится в состоянии удаления

        //  проверяют допустимость значений индекса колонки (строки),
        //  в случае недопустимости возбуждается исключение
        procedure CheckColIndex(const Index: LongInt);
        procedure CheckRowIndex(const Index: LongInt);

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        ////////////////////////////////////////////////////////////////////////
        //  методы старого интерфейса сохранить для совместимости с прежними
        //  приложениями, а также для того, чтобы можно было работать с
        //  неактивными сетками или когда активность таблицы не требуется
        //
        //  методы данного интерфейса подразумевают, что источник данных
        //  является активным, а сетка пассивной (это отражается в названиях
        //  методов)
        ////////////////////////////////////////////////////////////////////////

        procedure GridAssign(Grid: TStringGrid); virtual;
            //  установка параметров таблицы; когда данные устанавливаются
            //  в сетку по инициативе источника данных ввод в таблицу запрещен
        procedure GridRelease(Grid: TStringGrid); virtual;
            //  сохранение изменений параметров таблицы
        procedure SetDataToGrid(Grid: TStringGrid); virtual; abstract;
        function GetDataFromGrid(Grid: TStringGrid): Boolean; virtual; abstract;

        procedure SetCaption(Grid: TStringGrid); virtual; abstract;
        procedure SetColOptions(Grid: TStringGrid); virtual; abstract;
            //  устанавливает опции колонок в Grid'е
        procedure SetColFunc(Grid: TStringGrid); virtual;
            //  устанавливает функции для колонок

        procedure SetColWidths(Grid: TStringGrid);
            //  устанавливает ширину колонок таблицы
        procedure GetColWidths(Grid: TStringGrid);
            //  сохраняет во внутренних полях
            //  значения ширины колонок таблицы
        procedure SetRowHeights(Grid: TStringGrid);
            //  устанавливает высоту строк таблицы
        procedure GetRowHeights(Grid: TStringGrid);
            //  сохраняет во внутренних полях
            //  значения высоты строк таблицы

        procedure SetRowContents(
            //  заполняет строку таблицы с номером RowNum
            Grid: TStringGrid; RowNum: LongInt); virtual; abstract;
        function GetRowContents(
            //  сохраняет содержимое строки с номером RowNum
            Grid: TStringGrid; RowNum: LongInt): Boolean; virtual; abstract;
        procedure SetColContents(
            //  заполняет столбец таблицы с номером ColNum
            Grid: TStringGrid; ColNum: LongInt); virtual; abstract;
        function GetColContents(
            //  сохраняет содержимое столбца с номером ColNum
            Grid: TStringGrid; ColNum: LongInt): Boolean; virtual; abstract;

        procedure InitColWidths;    //  первоначальная инициализация массива
        procedure InitRowHeights;

        ////////////////////////////////////////////////////////////////////////
        //  методы нового интерфейса
        //
        //  методы данного интерфейса подразумевают, что активной
        //  является сетка, а источник данных пассивный (это также
        //  отражается в названиях методов)
        ////////////////////////////////////////////////////////////////////////

        function IsDataSourceEmpty: Boolean; virtual; abstract;

        function ValueToString(const ACol, ARow: LongInt
            ): string; virtual; abstract;
        procedure BeforeStringToValue(const ACol, ARow: LongInt;
            const AString: string); virtual; abstract;
        procedure StringToValue(const ACol, ARow: LongInt;
            const AString: string
            ); virtual; abstract;
        procedure SetValueByDefault(const ACol, ARow: LongInt); virtual; abstract;
            //  устанавливает правильное значение "по умолчанию"
            //  для данной ячейки; используется при обработке операции
            //  очистки ячейки
        function GetCellColor(
            //  возвращает True, если должен быть установлен
            //  цвет Color, в противном случае - False
            //  (таблица установит цвет "по умолчанию")
            const ACol, ARow: LongInt;
            var Color: TColor): Boolean; virtual;
        function GetCellEditMask(
            const ACol, ARow: LongInt): string; virtual; abstract;
        function GetCellEnabledCharSet(
            const ACol, ARow: LongInt): TCharSet; virtual; abstract;
        function IsCellDisabled(
            //  возвращает признак запрещения ввода
            //  в ячейки - True - ввод запрещен
            const ACol, ARow: LongInt): Boolean; virtual;

        function IsDataValid(const ACol, ARow: LongInt;
            //  выполняет "мягкую" проверку данных без возбуждения
            //  исключения; однако, если координаты строки/колонки
            //  имеют недорустимые значения исключение вызывается;
            //  всегда возвращает True - перекрыть для реализации
            //  нужных проверок
            const AString: string): Boolean; virtual; abstract;

        //  все функции проверки возможности выполнения действия
        //  возвращают False; классы - потомки должны перекрыть их
        function MayIDoInsertRows(StartRow, RowsCount: LongInt): Boolean; virtual;
        function MayIDoDeleteRows(StartRow, RowsCount: LongInt): Boolean; virtual;
        function MayIDoAddRow: Boolean; virtual;

        function MayIDoInsertColumns(StartCol, ColsCount: LongInt): Boolean; virtual;
        function MayIDoDeleteColumns(StartCol, ColsCount: LongInt): Boolean; virtual;
        function MayIDoAddColumn: Boolean; virtual;

        function MayIDoDeleteAllData: Boolean; virtual;
        function MayIDoClearSelectedArea: Boolean; virtual;
        function MayIDoClearAllCells: Boolean; virtual;

        //  функции управления массивами сохраненных значений
        //  ширины (высоты) колонок (столбцов)
        procedure DeleteAllColWidthItems;
        procedure DeleteColWidthItem(const Index: LongInt);
        procedure InsertColWidthItem(const Index: LongInt);
        procedure AddColWidthItem;

        procedure DeleteAllRowHeightItems;        
        procedure DeleteRowHeightItem(const Index: LongInt);
        procedure InsertRowHeightItem(const Index: LongInt);
        procedure AddRowHeightItem;

        function GetColWidthByDefault(
            const Index: LongInt): LongInt; virtual;
        function GetRowHeightByDefault(
            const Index: LongInt): LongInt; virtual;

        //  эти методы удаления, вставки, очистки вызывают
        //  исключение с сообщением о невозможности выполнить
        //  данное действие; классы - потомки должны перекрыть
        //  эти методы для реализации требуемых действий
        procedure RowsDeleted(
            const StartRow, RowsCount: LongInt); virtual;
        procedure RowsInserted(
            const StartRow, RowsCount: LongInt); virtual;
        procedure RowAdded; virtual;

        procedure ColumnsDeleted(
            const StartCol, ColsCount: LongInt); virtual;
        procedure ColumnsInserted(
            const StartCol, ColsCount: LongInt); virtual;
        procedure ColumnAdded; virtual;

        procedure AllDataDeleted; virtual;

        function GetColCount: LongInt; virtual;
            //  полное число колонок, включая Fixed
        function GetRowCount: LongInt; virtual;
            //  полное число строк, включая Fixed
        function GetInfoCols: LongInt; virtual; abstract;
            //  число колонок информационной части таблицы
        function GetInfoRows: LongInt; virtual; abstract;
            //  число строк информационной части таблицы
        function GetFixedCols: LongInt; virtual;
            //  число фиксированных колонок (по умолчанию = 1)
        function GetFixedRows: LongInt; virtual;
            //  число фиксированных строк (по умолчанию = 1)
        function GetColNumFixed: Boolean; virtual;  //  (по умолчанию = False)
        function GetRowNumFixed: Boolean; virtual;  //  (по умолчанию = False)

        function GetColWidth(const Col: LongInt): LongInt;
        procedure SaveColWidth(const Col, Width: LongInt);
        function GetRowHeight(const Row: LongInt): LongInt;
        procedure SaveRowHeight(const Row, Height: LongInt);
        function AutoWidths: Boolean;
        function AutoHeights: Boolean;

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

        property SavedCol: LongInt
            read GetCol                 write SaveCol;
        property SavedRow: LongInt
            read GetRow                 write SaveRow;
        property SavedLeftCol: LongInt
            read GetLeftCol             write SaveLeftCol;
        property SavedTopRow: LongInt
            read GetTopRow              write SaveTopRow;
        property SavedSelection: TGridRect
            read GetSelection           write SaveSelection;

        property Caption: string
            read FCaption               write FCaption;
    end;

    ERowCompList = class(Exception);
    EColCompList = class(Exception);

    TRowCompList = class(TTableCompList)
        //  список компонентов, каждый из которых
        //  представляет собой строку таблицы; число
        //  столбцов предполагается фиксированным, поэтому
        //  для реализации переменного числа столбцов
        //  нужно сделать соответствующие изменения
    protected
        function CreateNewObject: TComponent; virtual; abstract;
            //  создает новый компонент, который будет
            //  представлять строку таблицы

    public
        //  для правильной работы функций необходимо, чтобы число
        //  строк в информационной части таблицы было равно числу
        //  элементов в списке
        function GetDataFromGrid(Grid: TStringGrid): Boolean; override;
        procedure SetDataToGrid(Grid: TStringGrid); override;

        function Add(Item: TComponent): Integer; override;
        procedure Delete(Index: Integer); override;
        procedure Insert(Index: Integer; Item: TComponent); override;

        procedure BeforeStringToValue(const ACol, ARow: Integer;
            //  проверяет правильность индексов и в случае ошибки
            //  вызывает исключение; если список пустой добавляет
            //  объект - строку; !!! нужно обязательно вызывать
            //  в классах - наследниках !!!
            const AString: string); override;

        function MayIDoInsertRows(StartRow, RowsCount: LongInt): Boolean; override;
        function MayIDoDeleteRows(StartRow, RowsCount: LongInt): Boolean; override;
        function MayIDoAddRow: Boolean; override;

        function MayIDoDeleteAllData: Boolean; override;
        function MayIDoClearAllCells: Boolean; override;
        function MayIDoClearSelectedArea: Boolean; override;

        procedure RowsDeleted(
            const StartRow, RowsCount: LongInt); override;
        procedure RowsInserted(
            const StartRow, RowsCount: LongInt); override;
        procedure RowAdded; override;

        procedure AllDataDeleted; override;
        function IsDataSourceEmpty: Boolean; override;

        function GetInfoRows: LongInt; override;
            //  минимально возможное число строк = Fixed + 1
            //  для обеспечения возможности ввода
        function GetColNumFixed: Boolean; override;
            //  число колонок фиксировано
    end;

    TColCompList = class(TTableCompList)
        //  список компонентов, каждый из которых
        //  представляет собой столбец таблицы; число
        //  строк предполагается фиксированным, поэтому
        //  для реализации переменного числа строк
        //  нужно сделать соответствующие изменения
    protected
        function CreateNewObject: TComponent; virtual; abstract;
            //  создает новый компонент, который будет
            //  представлять столбец таблицы

    public
        //  для правильной работы функций необходимо, чтобы число
        //  колонок в информационной части таблицы было равно числу
        //  элементов в списке
        function GetDataFromGrid(Grid: TStringGrid): Boolean; override;
        procedure SetDataToGrid(Grid: TStringGrid); override;

        function Add(Item: TComponent): Integer; override;
        procedure Delete(Index: Integer); override;
        procedure Insert(Index: Integer; Item: TComponent); override;

        procedure BeforeStringToValue(const ACol, ARow: LongInt;
            //  проверяет правильность индексов и в случае ошибки
            //  вызывает исключение; если список пустой добавляет
            //  объект - столбец; !!! нужно обязательно вызывать
            //  в классах - наследниках !!!
            const AString: string); override;

        function MayIDoInsertColumns(StartCol, ColsCount: LongInt): Boolean; override;
        function MayIDoDeleteColumns(StartCol, ColsCount: LongInt): Boolean; override;
        function MayIDoAddColumn: Boolean; override;

        function MayIDoDeleteAllData: Boolean; override;
        function MayIDoClearSelectedArea: Boolean; override;
        function MayIDoClearAllCells: Boolean; override;

        procedure ColumnsDeleted(
            const StartCol, ColsCount: LongInt); override;
        procedure ColumnsInserted(
            const StartCol, ColsCount: LongInt); override;
        procedure ColumnAdded; override;

        procedure AllDataDeleted; override;
        function IsDataSourceEmpty: Boolean; override;

        function GetInfoCols: LongInt; override;
            //  минимально возможное число колонок = Fixed + 1
            //  для обеспечения возможности ввода
        function GetRowNumFixed: Boolean; override;
            //  число строк всегда фиксировано
    end;

    TIconicCompList = class(TTableCompList)
    protected
        FImageList: TImageList;
    public
    end;

implementation

constructor TTableCompList.Create;
begin
    inherited Create(AOwner);

    FSavedCol := GetFixedCols;
    FSavedRow := GetFixedRows;
    FSavedLeftCol := FSavedCol;
    FSavedTopRow := FSavedRow;
    with FSavedSelection do
    begin
        Left := FSavedCol;
        Top := FSavedRow;
        Right := FSavedCol;
        Bottom := FSavedRow;        
    end;
end;

destructor TTableCompList.Destroy;
begin
    Destroying := True;
    Finalize(SavedColWidths);
    Finalize(SavedrowHeights);
    inherited Destroy;
end;

procedure TTableCompList.GridAssign(Grid: TStringGrid);
(*var i, j: LongInt;*)
begin
    with Grid do
    begin
        if Grid is TColorStringGrid then
            with Grid as TColorStringGrid do
            begin
                //  это нужно, чтобы действительно вызвать
                //  переопределенное свойство
                RowCount := GetRowCount;
                ColCount := GetColCount;
            end
        else
        begin
            RowCount := GetRowCount;
            ColCount := GetColCount;
        end;

        //  если здесь очищать, то работа изрядно замедляется
        //  for j := 0 to RowCount - 1 do
        //      for i := 0 to ColCount - 1 do Cells[i, j] := '';

        FixedCols := GetFixedCols;
        FixedRows := GetFixedRows;

        LeftCol := GetLeftCol;
        TopRow := GetTopRow;
        Col := GetCol;
        Row := GetRow;

        Selection := GetSelection;
        EditorMode := False;

        Options := StaticOptions;
            //  "по умолчанию", когда данные устанавливаются в сетку
            //  по активности источника ввод в таблицу запрещен
    end;    //  with Grid do...

    //  все операции с ячейками должны быть
    //  до установки ширины/высоты ячеек
    SetCaption(Grid);
    SetColOptions(Grid);
    SetColFunc(Grid);

    //  перенумеровка здесь нужна потому, что источник данных
    //  сам не нумерует строки в SetRowContents; при таком
    //  подключении источника данных таблица вынуждена сама
    //  нумеровать строки, если над ней производятся операции,
    //  поэтому лучше использовать способ таблицы для нумерации
    if Grid is TColorStringGrid then
        with Grid as TColorStringGrid do EnumerateRows;

    if Grid is TIDAGrid then
        with Grid as TIDAGrid do Changeable := False;
    //  по умолчанию ввод текста в ячейки запрещен

    if Grid is TDataGrid then
        with Grid as TDataGrid do ShowTable;

    SetDataToGrid(Grid);    //  первый раз должна выполняться до
                            //  установки высоты и ширины ячеек

    SetColWidths(Grid);
    SetRowHeights(Grid);
end;

procedure TTableCompList.SetColWidths(Grid: TStringGrid);
var i: LongInt;
begin
    if (Grid is TIDAGrid) and (not WidthsSaved) then
        with Grid as TIDAGrid do AutoColWidths
    else
        with Grid do
            for i := 0 to ColCount - 1 do ColWidths[i] := GetColWidth(i);
end;

procedure TTableCompList.GetColWidths(Grid: TStringGrid);
var i: LongInt;
begin
    with Grid do
        for i := 0 to ColCount - 1 do SaveColWidth(i, ColWidths[i]);
end;

procedure TTableCompList.GridRelease(Grid: TStringGrid);
begin
    GetRowHeights(Grid);
    GetColWidths(Grid);

    with Grid do
    begin
        SaveLeftCol(LeftCol);
        SaveTopRow(TopRow);
        SaveCol(Col);
        SaveRow(Row);

        SaveSelection(Selection);
    end;

    SettingsSaved := True;
end;

{ TRowCompList }

procedure TRowCompList.AllDataDeleted;
begin
    if not (Count = 0) then Clear;
end;

function TRowCompList.GetColNumFixed: Boolean;
begin
    Result := True;
end;

function TRowCompList.GetDataFromGrid(Grid: TStringGrid): Boolean;
var i: LongInt;
begin
    //  !!! не должно быть очистки списка, поскольку
    //  GetRowContents не создает новые объекты !!!
    Result := True;
    with Grid do
        for i := FixedRows to RowCount - 1 do
            if not GetRowContents(Grid, i) then Result := False;
end;

function TRowCompList.GetInfoRows: LongInt;
begin
    if Count <> 0 then Result := Count
    else Result := 1;
end;

function TRowCompList.IsDataSourceEmpty: Boolean;
begin
    Result := Count = 0;
end;

function TRowCompList.MayIDoAddRow: Boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoClearAllCells: Boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoClearSelectedArea: Boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoDeleteAllData: Boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoDeleteRows(StartRow,
    RowsCount: Integer): Boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoInsertRows(StartRow,
    RowsCount: Integer): Boolean;
begin
    Result := True;
end;

procedure TRowCompList.RowAdded;
begin
    if Count = 0 then Add(CreateNewObject);
        //  в пустой объект добавляется строка
    Add(CreateNewObject);
end;

procedure TRowCompList.RowsDeleted(const StartRow, RowsCount: Integer);
var i: LongInt;
    First, Last: LongInt;
begin
    //  удаление элементов при пустом списке не должно
    //  вызывать исключения, так как пользователь, в
    //  принципе, может попытаться удалить пустой столбец
    //  для которого реально нет элемента списка -
    //  просто ничего не нужно делать
    if not (Count = 0) then
    begin
        Last := StartRow - GetFixedRows + RowsCount - 1;
        First := StartRow - GetFixedRows;
        if (First < 0) or (Last > Count - 1) then
            raise ERowCompList.Create('Invalid deleting parameters...');
        i := 0;
        while i < RowsCount do
        begin
            Delete(First);
            Inc(i);
        end;
    end;
end;

procedure TRowCompList.RowsInserted(const StartRow, RowsCount: Integer);
var i: LongInt;
    First: LongInt;
begin
    First := StartRow - GetFixedRows;
    if Count = 0 then Add(CreateNewObject);
        //  в пустой объект добавляется строка - сначала нужно
        //  добавить, а уже потом проверять

    if (First < 0) or (First > Count - 1) then
        raise ERowCompList.Create('Invalid insertion parameters...');

    for i := 1 to RowsCount do Insert(First, CreateNewObject);
end;

procedure TRowCompList.BeforeStringToValue(const ACol, ARow: Integer;
    const AString: string);
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);
    
    if Count = 0 then Add(CreateNewObject);
        //  в пустой объект добавляется строка
end;

function TRowCompList.Add(Item: TComponent): Integer;
var Flag: Boolean;
begin
    Flag := Count = 0;
    Result := inherited Add(Item);  //  список уже не пуст !!!
    if AreRowHeightsReady and not Flag then AddRowHeightItem;
        //  должна вызываться последней, чтобы проверка
        //  индексов дала правильные результаты; для
        //  пустого объекта Add вызывается дважды,
        //  поэтому первый раз нужно пропустить
end;

procedure TRowCompList.Delete(Index: Integer);
begin
    if (not Destroying) and AreRowHeightsReady then
        DeleteRowHeightItem(Index);
        //  должна вызываться первой, чтобы проверка
        //  индексов дала правильные результаты
    inherited;  //  число элементов в списке изменилось
    if (not Destroying) and
        (Count = 0) and AreRowHeightsReady then AddRowHeightItem;
        //  если удалены все данные нужно добавить один
        //  элемент на пустую строку
end;

procedure TRowCompList.Insert(Index: Integer; Item: TComponent);
var Flag: Boolean;
begin
    Flag := Count = 0;
    inherited;
    if AreRowHeightsReady and not Flag then InsertRowHeightItem(Index);
        //  должна вызываться последней, чтобы проверка
        //  индексов дала правильные результаты
end;

procedure TRowCompList.SetDataToGrid(Grid: TStringGrid);
var i: LongInt;
begin
    with Grid do
        for i := FixedRows to RowCount - 1 do SetRowContents(Grid, i);
end;

{ TColCompList }

function TColCompList.Add(Item: TComponent): Integer;
var Flag: Boolean;
begin
    Flag := Count = 0;
    Result := inherited Add(Item);  //  список уже не пуст !!!
    if AreColWidthsReady and not Flag then AddColWidthItem;
        //  должна вызываться последней, чтобы проверка
        //  индексов дала правильные результаты; для
        //  пустого объекта Add вызывается дважды,
        //  поэтому первый раз нужно пропустить
end;

procedure TColCompList.AllDataDeleted;
begin
    if not (Count = 0) then Clear;
end;

procedure TColCompList.ColumnAdded;
begin
    if Count = 0 then Add(CreateNewObject);
        //  в пустой объект добавляется столбец
    Add(CreateNewObject);
end;

procedure TColCompList.ColumnsDeleted(const StartCol, ColsCount: Integer);
var i: LongInt;
    First, Last: LongInt;
begin
    //  удаление элементов при пустом списке не должно
    //  вызывать исключения, так как пользователь, в
    //  принципе, может попытаться удалить пустую строку
    //  для которой реально нет элемента списка -
    //  просто ничего не нужно делать
    if not (Count = 0) then
    begin
        Last := StartCol - GetFixedCols + ColsCount - 1;
        First := StartCol - GetFixedCols;
        if (First < 0) or (Last > Self.Count - 1) then
            raise EColCompList.Create('Invalid deleting parameters...');
        i := 0;
        while i < ColsCount do
        begin
            Delete(First);
            Inc(i);
        end;
    end;
end;

procedure TColCompList.ColumnsInserted(const StartCol, ColsCount: Integer);
var i: LongInt;
    First: LongInt;
begin
    First := StartCol - GetFixedCols;
    if Count = 0 then Add(CreateNewObject);
        //  в пустой объект добавляется столбец - сначала нужно
        //  добавить, а уже потом проверять

    if (First < 0) or (First > Self.Count - 1) then
        raise EColCompList.Create('Invalid insertion parameters...');

    for i := 1 to ColsCount do Insert(First, CreateNewObject);
end;

procedure TColCompList.Delete(Index: Integer);
begin
    if (not Destroying) and AreColWidthsReady then
        DeleteColWidthItem(Index);
            //  должна вызываться первой, чтобы проверка
            //  индексов дала правильные результаты
    inherited;  //  число элементов в списке изменилось
    if (not Destroying) and
        (Count = 0) and AreColWidthsReady then AddColWidthItem;
        //  если удалены все данные нужно добавить один
        //  элемент на пустой столбец
end;

function TColCompList.GetDataFromGrid(Grid: TStringGrid): Boolean;
var i: LongInt;
begin
    Result := True;
    with Grid do
        for i := FixedCols to ColCount - 1 do
            if not GetColContents(Grid, i) then Result := False;
end;

function TColCompList.GetInfoCols: LongInt;
begin
    if Count <> 0 then Result := Count
    else Result := 1;
end;

function TColCompList.GetRowNumFixed: Boolean;
begin
    Result := True;
end;

procedure TColCompList.Insert(Index: Integer; Item: TComponent);
var Flag: Boolean;
begin
    Flag := Count = 0;
    inherited;
    if AreColWidthsReady and not Flag then InsertColWidthItem(Index);
        //  должна вызываться последней, чтобы проверка
        //  индексов дала правильные результаты
end;

function TColCompList.IsDataSourceEmpty: Boolean;
begin
    Result := Count = 0;
end;

function TColCompList.MayIDoAddColumn: Boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoClearAllCells: Boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoClearSelectedArea: Boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoDeleteAllData: Boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoDeleteColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoInsertColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    Result := True;
end;

procedure TColCompList.SetDataToGrid(Grid: TStringGrid);
var i: LongInt;
begin
    with Grid do
        for i := FixedCols to ColCount - 1 do SetColContents(Grid, i);
end;

procedure TColCompList.BeforeStringToValue(const ACol, ARow: Integer;
    const AString: string);
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);
    
    if Count = 0 then Add(CreateNewObject);
        //  в пустой объект добавляется столбец
end;

function TTableCompList.GetCellColor(const ACol, ARow: Integer;
    var Color: TColor): Boolean;
begin
    Result := False;
end;

function TTableCompList.IsCellDisabled(const ACol, ARow: Integer): Boolean;
begin
    Result := False;
end;

procedure TTableCompList.AllDataDeleted;
begin
    raise ETableCompList.Create('All data deleting is impossible...');
end;

procedure TTableCompList.ColumnAdded;
begin
    raise ETableCompList.Create('Columns adding is impossible...');
end;

procedure TTableCompList.ColumnsDeleted(const StartCol,
    ColsCount: Integer);
begin
    raise ETableCompList.Create('Columns deleting is impossible...');
end;

procedure TTableCompList.ColumnsInserted(const StartCol,
    ColsCount: Integer);
begin
    raise ETableCompList.Create('Columns insertion is impossible...');
end;

procedure TTableCompList.RowAdded;
begin
    raise ETableCompList.Create('Row adding is impossible...');
end;

procedure TTableCompList.RowsDeleted(const StartRow, RowsCount: Integer);
begin
    raise ETableCompList.Create('Row deleting is impossible...');
end;

procedure TTableCompList.RowsInserted(const StartRow, RowsCount: Integer);
begin
    raise ETableCompList.Create('Row insertion is impossible...');
end;

function TTableCompList.MayIDoAddColumn: Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoAddRow: Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoClearAllCells: Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoClearSelectedArea: Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoDeleteAllData: Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoDeleteColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoDeleteRows(StartRow,
    RowsCount: Integer): Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoInsertColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoInsertRows(StartRow,
    RowsCount: Integer): Boolean;
begin
    Result := False;
end;

function TTableCompList.GetColNumFixed: Boolean;
begin
    Result := False;
end;

function TTableCompList.GetFixedCols: LongInt;
begin
    Result := 1;
end;

function TTableCompList.GetFixedRows: LongInt;
begin
    Result := 1;
end;

function TTableCompList.GetRowNumFixed: Boolean;
begin
    Result := False;
end;

function TTableCompList.GetColCount: LongInt;
begin
    Result := GetInfoCols + GetFixedCols;
end;

function TTableCompList.GetRowCount: LongInt;
begin
    Result := GetInfoRows + GetFixedRows;
end;

function TTableCompList.GetCol: LongInt;
begin
    Result := FSavedCol;
end;

function TTableCompList.GetColWidth(const Col: Integer): LongInt;
begin
    CheckColIndex(Col);
    InitColWidths;
    //  первоначальная инициализация массива
    //  сделана здесь потому, что не всегда
    //  удобно делать инициализацию в конструкторе,
    //  например, когда число колонок неизвестно
    //  на этапе создания объекта
    Result := SavedColWidths[Col];
end;

function TTableCompList.GetLeftCol: LongInt;
begin
    Result := FSavedLeftCol;
end;

function TTableCompList.GetRow: LongInt;
begin
    Result := FSavedRow;
end;

function TTableCompList.GetRowHeight(const Row: Integer): LongInt;
begin
    CheckRowIndex(Row);
    InitRowHeights;
    //  первоначальная инициализация массива
    //  сделана здесь потому, что не всегда
    //  удобно делать инициализацию в конструкторе,
    //  например, когда число строк неизвестно
    //  на этапе создания объекта
    Result := SavedRowHeights[Row];
end;

function TTableCompList.GetSelection: TGridRect;
begin
    Result := FSavedSelection;
end;

function TTableCompList.GetTopRow: LongInt;
begin
    Result := FSavedTopRow;
end;

procedure TTableCompList.SaveCol(const Col: Integer);
begin
    FSavedCol := Col;
end;

procedure TTableCompList.SaveColWidth(const Col, Width: Integer);
begin
    CheckColIndex(Col);
    InitColWidths;
    //  первоначальная инициализация массива
    //  сделана здесь потому, что не всегда
    //  удобно делать инициализацию в конструкторе,
    //  например, когда число колонок неизвестно
    //  на этапе создания объекта
    SavedColWidths[Col] := Width;
    WidthsSaved := True;
end;

procedure TTableCompList.SaveLeftCol(const LeftCol: Integer);
begin
    FSavedLeftCol := LeftCol;
end;

procedure TTableCompList.SaveRow(const Row: Integer);
begin
    FSavedRow := Row;
end;

procedure TTableCompList.SaveRowHeight(const Row, Height: Integer);
begin
    CheckRowIndex(Row);
    InitRowHeights;
    //  первоначальная инициализация массива
    //  сделана здесь потому, что не всегда
    //  удобно делать инициализацию в конструкторе,
    //  например, когда число строк неизвестно
    //  на этапе создания объекта
    SavedRowHeights[Row] := Height;
    HeightsSaved := True;
end;

procedure TTableCompList.SaveSelection(const Selection: TGridRect);
begin
    FSavedSelection := Selection;
end;

procedure TTableCompList.SaveTopRow(const TopRow: Integer);
begin
    FSavedTopRow := TopRow;
end;

procedure TTableCompList.AddColWidthItem;
begin
    CheckColIndex(Length(SavedColWidths)(* - 1 + 1*));
    AddItemLongArr(SavedColWidths,
        GetColWidthByDefault(Length(SavedColWidths)(* - 1 + 1*)));
        //  последний элемент имеет индекс Length - 1,
        //  а проверять нужно индекс на 1 больше
end;

procedure TTableCompList.AddRowHeightItem;
begin
    CheckRowIndex(Length(SavedRowHeights)(* - 1 + 1*));
    AddItemLongArr(SavedRowHeights,
        GetRowHeightByDefault(Length(SavedRowHeights)(* - 1 + 1*)));
        //  последний элемент имеет индекс Length - 1,
        //  а проверять нужно индекс на 1 больше
end;

procedure TTableCompList.DeleteColWidthItem(const Index: Integer);
begin
    CheckColIndex(Index);
    DeleteItemLongArr(SavedColWidths, Index);
end;

procedure TTableCompList.DeleteRowHeightItem(const Index: Integer);
begin
    CheckRowIndex(Index);
    DeleteItemLongArr(SavedRowHeights, Index);
end;

procedure TTableCompList.InsertColWidthItem(const Index: Integer);
begin
    CheckColIndex(Index);
    InsertItemLongArr(SavedColWidths, Index, GetColWidthByDefault(Index));
end;

procedure TTableCompList.InsertRowHeightItem(const Index: Integer);
begin
    CheckRowIndex(Index);
    InsertItemLongArr(SavedRowHeights, Index, GetRowHeightByDefault(Index));
end;

procedure TTableCompList.CheckColIndex(const Index: Integer);
begin
    if (Index < 0) or (Index >= GetColCount) then
        raise ETableCompList.Create('Invalid column index...');
end;

procedure TTableCompList.CheckRowIndex(const Index: Integer);
begin
    if (Index < 0) or (Index >= GetRowCount) then
        raise ETableCompList.Create('Invalid row index...');
end;

procedure TTableCompList.DeleteAllColWidthItems;
begin
    Finalize(SavedColWidths);
end;

procedure TTableCompList.DeleteAllRowHeightItems;
begin
    Finalize(SavedRowHeights);
end;

function TTableCompList.GetColWidthByDefault(
    const Index: Integer): LongInt;
begin
    CheckColIndex(Index);
    Result := 64;
end;

function TTableCompList.GetRowHeightByDefault(
    const Index: Integer): LongInt;
begin
    CheckRowIndex(Index);
    Result := 20;
end;

procedure TTableCompList.GetRowHeights(Grid: TStringGrid);
var i: LongInt;
begin
    with Grid do
        for i := 0 to RowCount - 1 do SaveRowHeight(i, RowHeights[i]);
end;

procedure TTableCompList.SetRowHeights(Grid: TStringGrid);
var i: LongInt;
begin
    if (Grid is TIDAGrid) and (not HeightsSaved) then
        with Grid as TIDAGrid do AutoRowHeights
    else
        with Grid do
            for i := 0 to RowCount - 1 do RowHeights[i] := GetRowHeight(i);
end;

procedure TTableCompList.InitColWidths;
var i: LongInt;
begin
    if not AreColWidthsReady then
    begin
        DeleteAllColWidthItems;
        for i := 1 to GetColCount do AddColWidthItem;
        AreColWidthsReady := True;
    end;
end;

procedure TTableCompList.InitRowHeights;
var i: LongInt;
begin
    if not AreRowHeightsReady then
    begin
        DeleteAllRowHeightItems;
        for i := 1 to GetRowCount do AddRowHeightItem;
        AreRowHeightsReady := True;
    end;
end;

function TTableCompList.AutoHeights: Boolean;
begin
    Result := not HeightsSaved;
end;

function TTableCompList.AutoWidths: Boolean;
begin
    Result := not WidthsSaved;
end;

procedure TTableCompList.SetColFunc(Grid: TStringGrid);
var i: LongInt;
begin
    //  указатели на ф-и располагаются в Fixed строках
    //  "шапки" таблицы
    with Grid do
        if FixedRows <> 0 then
            for i := 0 to ColCount - 1 do Objects[i, 0] := nil;
end;

initialization
    RegisterClass(TRowCompList);
    RegisterClass(TColCompList);
    RegisterClass(TTableCompList);
end.
