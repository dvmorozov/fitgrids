//      двойной косой чертой комментируются замечания, сохраняемые во
//      всех версиях исходника; фигурными скобками комментируются замечания,
//      сохраняемые только в версии исходника для бесплатного распространения
{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit Tools;

{$MODE Delphi}

interface

uses SysUtils, Classes, SimpMath, CBRCComponent, MyExceptions;

type
    FParamRequest = function(Param: string): Double of object;
        //  возвращает значение для параметра Param
    TCharSet = set of Char;
    TVector3Array = array of TDoubleVector3;

    ETools = class(Exception);

const
    //  константы ошибок для процедуры расчета строки
    CALC_NO_ERRORS          : LongInt = 0;
    CALC_INVALID_PARAMETER  : LongInt = 1;
    CALC_INVALID_EXPRESSION : LongInt = 2;

procedure AddVectorToArray(
    //  добавляет вектор к концу массива векторов
    var Arr: TVector3Array;
    const Vector: TDoubleVector3
    );
procedure InsertVectorIntoArray(
    //  вставляет вектор в массив в позицию Index
    var Arr: TVector3Array;
    const Index: LongInt;
    const Vector: TDoubleVector3
    );
procedure DeleteVectorFromArray(
    var Arr: TVector3Array;
    const Index: LongInt
    );

type
    TLongArray = array of LongInt;

procedure DeleteItemLongArr(
    var Arr: TLongArray;
    const Index: LongInt
    );
procedure InsertItemLongArr(
    var Arr: TLongArray;
    const Index: LongInt;
    const Item: LongInt     //  вставляемое значение
    );
procedure AddItemLongArr(
    var Arr: TLongArray;
    const Item: LongInt
    );

procedure CheckArrItemIndex(const MinIndex, MaxIndex, Index: LongInt);
    //  проверяет допустимость индекса некоторого элемента,
    //  в случае недопустимости вызывается исключение

//  функции преобразования вектора в строку и обратно
const DoubleVector3EditMask = '!\(0\.0999\,\ 0\.0999\,\ 0\.0999\);1;';
//  !!! маска должна соответствовать формату Str в StringAsDoubleVector3 !!!

function DoubleVector3AsString(const Vect: TDoubleVector3;
    FixedMode: Boolean; //  True - фиксированное число знаков после запятой
    Precision, Digits: LongInt): string;
        //  !!! возврат должен соответствовать маске !!!
function StringAsDoubleVector3(const Str: string): TDoubleVector3;

function StrToFloatDef(St: string; DefVal: Extended): Extended;
function StrToVector3(const St: string): TDoubleVector3;
    //  преобразует строку в формате (*.*,*.*,*.*) в вектор
function Vector3ToStr(const Vector: TDoubleVector3): string;
    //  преобразует вектор в строку в формате (*.*,*.*,*.*)

function WithGivenAccuracy(
    Value: Double;      //  число для преобразования
    Decimals: LongInt   //  требуемое число знаков после запятой
    ): Double;
function GetCmdLineParameters: string;
    //  возвращает подстроку параметров из командной строки программы
    //  обрамленную символами "", если командная строка не содержит других
    //  параметров кроме пути к программе, то возвращается пустая строка 
function GetRandomWithSign: Double;

function CalculateSimpExpr(var Expression: string; var ErrorCode: LongInt;
    const ParamRequest: FParamRequest): Double;
function CalculateExpr(var Expression: string; var ErrorCode: LongInt;
    //  вычисляет значение выражения переданного в строке Expression
    const ParamRequest: FParamRequest): Double;


function GetCharPosition(St: string; Ch: Char;
    //  возарщает -1 если символ не обнаружен
    Direction: ShortInt; StartIndex: LongInt): LongInt;
function GetCharSetPosition(St: string; ChSet: TCharSet;
    Direction: ShortInt;    //  Direction = 1 - движение по строке вправо
                            //  Direction = -1 - движение по строке влево;
                            //  возвращает -1 - ошибка
    StartIndex: LongInt; var Ch: Char): LongInt;

procedure GetPosInArrays(
    //  возвращает номер массива и индекс элемента в этом массиве по
    //  сквозному индексу элемента среди всех массивов
    const ArraysLengths: array of LongInt;  //  массив длин массивов
    const Index: LongInt;                   //  "сквозной" индекс элемента
    var ArrayNumber: LongInt;               //  номер массива, содержащего
                                            //  нужный элемент
    var ArrayIndex: LongInt                 //  индекс элемента в этом массиве
    );

function ReadComponentByReader(const Reader: TReader): TComponent;
procedure UtilizeObject(PtrToObject: TObject);
    //  функция уничтожения объектов - выполняет дополнительные
    //  действия, связанные с уничтожением объектов; пока только
    //  проверка на тип TCBRCComponent и вызов его метода Free
    //  с помощью этой функции нужно уничтожать все объекты программы

implementation

function StrToFloatDef(St: string; DefVal: Extended): Extended;
var Temp: Extended;
begin
    try Temp := StrToFloat(St);
    except Temp := DefVal; end;
    Result := Temp;
end;

function StrToVector3(const St: string): TDoubleVector3;
var i: LongInt;
    St2: string;
    Index, PrevIndex: LongInt;
    TempChar: Char;
begin
    PrevIndex := 2;     //  символы нумеруются, начиная с 1 ?
    for i := 1 to 3 do
    begin
        Index := GetCharSetPosition(St, [',', ')'], 1, PrevIndex, TempChar);
        St2 := Copy(St, PrevIndex, Index - PrevIndex);
        Result[i] := StrToFloat(St2);
        PrevIndex := Index + 1;
    end;
end;

function Vector3ToStr(const Vector: TDoubleVector3): string;
begin
    Result := '(' + FloatToStrF(Vector[1], ffGeneral, 6, 4) + ',' +
                    FloatToStrF(Vector[2], ffGeneral, 6, 4) + ',' +
                    FloatToStrF(Vector[3], ffGeneral, 6, 4) + ')';
end;

function DoubleVector3AsString(const Vect: TDoubleVector3;
    FixedMode: Boolean; Precision, Digits: LongInt): string;
var St: string;
    SavedDecimalSeparator: Char;
begin
    SavedDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    St := '(';
    if FixedMode then begin
        St := St + FloatToStrF(Vect[1], ffFixed, Precision, Digits) + ', ';
        St := St + FloatToStrF(Vect[2], ffFixed, Precision, Digits) + ', ';
        St := St + FloatToStrF(Vect[3], ffFixed, Precision, Digits);
    end else begin
        St := St + FloatToStr(Vect[1]) + ', ';
        St := St + FloatToStr(Vect[2]) + ', ';
        St := St + FloatToStr(Vect[3]);
    end;
    St := St + ')';
    DecimalSeparator := SavedDecimalSeparator;
    Result := St;
end;

function StringAsDoubleVector3(const Str: string): TDoubleVector3;
var i, BegIndex, VectIndex: LongInt;
    Str2: string;
    PrevIsDelimiter: Boolean;
        //  предыдуший символ был среди разделителей ?
begin
    BegIndex := -1; VectIndex := 1; PrevIsDelimiter := False;
    for i := 1 to Length(Str) do
        if IsDelimiter('() ,_', Str, i) then
        begin
            if BegIndex <> -1 then
            begin
                Str2 := Copy(Str, BegIndex, i - BegIndex);
                Result[VectIndex] := StrToFloat(Str2);
                Inc(VectIndex);
                BegIndex := -1;
            end;
            PrevIsDelimiter := True;
        end else
        begin
            if PrevIsDelimiter then BegIndex := i;
            PrevIsDelimiter := False;
        end;
end;

function WithGivenAccuracy(Value: Double; Decimals: LongInt): Double;
var PowerOf10: Double;
    TempLong: LongInt;
//    St: string;
begin
    PowerOf10 := GetPowerOf10(Decimals);
    Result := Value * PowerOf10;
    TempLong := Round(Result);
    Result := TempLong / PowerOf10;
//    St := FloatToStrF(Value, ffFixed, 8, Decimals);
//    Result := StrToFloat(St);
end;

function GetCmdLineParameters: string;
var St: string;
    Index: LongInt;
    Index1, Index2: LongInt;
begin
    St := '';//???GetCommandLine;
    Index1 := -1; Index2 := -1;

    for Index := Length(St) downto 1 do
        if IsDelimiter('"', St, Index) then begin Index2 := Index; Break end;
    for Index := Index2 - 1 downto 1 do
        if IsDelimiter('"', St, Index) then begin Index1 := Index; Break end;

    if (Index1 = -1) or (Index2 = -1) then begin Result := ''; Exit end;
    St := Copy(St, Index1 + 1, Index2 - Index1 - 1);
    if UpperCase(ST) = UpperCase(ParamStr(0)) then St := '';
    Result := St;
end;

function GetCharPosition(St: string; Ch: Char;
    Direction: ShortInt; StartIndex: LongInt): LongInt;
var i: LongInt;
begin
    if StartIndex > Length(St) then begin Result := -1; Exit end;
    case Direction of
        1 : begin
            for i := StartIndex to Length(St) do
                if St[i] = Ch then begin Result := i; Exit end;
            Result := -1;
        end;
        -1 : begin
            for i := StartIndex downto 1 do
                if St[i] = Ch then begin Result := i; Exit end;
            Result := -1;
        end;
        else Result := -1;
    end;
end;

function GetCharSetPosition(St: string; ChSet: TCharSet;
    Direction: ShortInt; StartIndex: LongInt; var Ch: Char): LongInt;
var i: LongInt;
begin
    if StartIndex > Length(St) then begin Result := -1; Exit end;
    case Direction of
        1 : begin
            for i := StartIndex to Length(St) do
                if St[i] in ChSet then begin Result := i; Ch := St[i]; Exit end;
            Result := -1;
        end;
        -1 : begin
            for i := StartIndex downto 1 do
                if St[i] in ChSet then begin Result := i; Ch := St[i]; Exit end;
            Result := -1;
        end;
        else Result := -1;
    end;
end;

function CalculateSimpExpr(var Expression: string; var ErrorCode: LongInt;
const ParamRequest: FParamRequest): Double;

    procedure MakeAllOper(var Expression: string; OperSet: TCharSet;
        var ErrorCode: LongInt; const ParamRequest: FParamRequest);
    var Index, IndexL, IndexR: LongInt;
        ArgStrL, ArgStrR: string;
        ArgL, ArgR: Double;
        Value: Double;
        St: string;
        TempIndex: LongInt;
        Oper, TempCh: Char;
    begin
        repeat
            Index := GetCharSetPosition(Expression, OperSet, 1, 1, Oper);
            if Index = -1 then Exit;
            IndexL := GetCharSetPosition(Expression,
                ['*', '/', '+', '-'], -1, Index - 1, TempCh);
            if IndexL = -1 then IndexL := 0;
            IndexR := GetCharSetPosition(Expression,
                ['*', '/', '+', '-'], 1, Index + 1, TempCh);
            if IndexR = -1 then IndexR := Length(Expression) + 1;
            if (Expression[1] = '-') then
            begin
                if (Index = 1) then
                begin
                    //  ситуация, когда первое число отрицательное}
                    //  здесь проходит когда проверяется наличие операций ['+', '-']
                    TempIndex := Index;
                    Index := GetCharSetPosition(Expression, OperSet, 1, TempIndex + 1, Oper);
                    if Index = -1 then Exit;
                    IndexR := GetCharSetPosition(Expression,
                        ['*', '/', '+', '-'], 1, Index + 1, TempCh);
                    if IndexR = -1 then IndexR := Length(Expression) + 1;
                    IndexL := 0;
                end else IndexL := 0    //  ['*', '/']
            end;

            if (IndexR = Index + 1) and (Expression[Index + 1] = '-') then
            begin
                //  ситуация, когда следующее за операцией число отрицательное
                IndexR := GetCharSetPosition(Expression,
                    ['*', '/', '+', '-'], 1, IndexR + 1, TempCh);
                if IndexR = -1 then IndexR := Length(Expression) + 1;
            end;

            if (IndexL = Index - 1) and (Expression[Index] = '-') then
            begin
                //  ситуация, когда предыдущее операции число отрицательное
                IndexL := GetCharSetPosition(Expression,
                    ['*', '/', '+', '-'], 1, IndexL - 1, TempCh);
                if IndexR = -1 then IndexL := 0;
            end;

            ArgStrL := Copy(Expression, IndexL + 1, Index - IndexL - 1);
            ArgStrR := Copy(Expression, Index + 1, IndexR - Index - 1);
            Delete(Expression, IndexL + 1, IndexR - IndexL - 1);
            if ArgStrL <> '' then
            begin
                try ArgL := StrToFloat(ArgStrL);
                except
                    if Assigned(ParamRequest) then
                    begin
                        if ArgStrL[1] in ['-', '+'] then
                            case ArgStrL[1] of
                                '-': ArgL := (-1) * ParamRequest(Copy(
                                    ArgStrL, 2, Length(ArgStrL) - 1));
                                '+': ArgL := ParamRequest(Copy(ArgStrL, 2,
                                    Length(ArgStrL) - 1));
                            end
                        else ArgL := ParamRequest(ArgStrL)
                    end{if Assigned(ParamRequest) then...}
                    else begin ErrorCode := CALC_INVALID_PARAMETER; Exit end;
                end;{except...}
            end{if ArgStrL <> '' then...}
            else ArgL := 0;

            try ArgR := StrToFloat(ArgStrR);
            except
                if Assigned(ParamRequest) then
                begin
                    if ArgStrR[1] in ['-', '+'] then
                        case ArgStrR[1] of
                            '-': ArgR := (-1) * ParamRequest(Copy(
                                ArgStrR, 2, Length(ArgStrR) - 1));
                            '+': ArgR := ParamRequest(Copy(ArgStrR, 2,
                                Length(ArgStrR) - 1));
                        end
                    else ArgR := ParamRequest(ArgStrR)
                end
                else begin ErrorCode := CALC_INVALID_PARAMETER; Exit end;
            end;

            case Oper of
                '*' : Value := ArgL * ArgR;
                '/' : Value := ArgL / ArgR;
                '+' : Value := ArgL + ArgR;
                '-' : Value := ArgL - ArgR;
            end;

            St := FloatToStrF(Value, ffGeneral, 6, 4);
            Insert(St, Expression, IndexL + 1);
        until Index = -1;
    end;{MakeAllOper}

var SaveDecimalSeparator: Char;
begin
    ErrorCode := CALC_NO_ERRORS;
    SaveDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    MakeAllOper(Expression, ['*', '/'], ErrorCode, ParamRequest);
    MakeAllOper(Expression, ['+', '-'], ErrorCode, ParamRequest);
    try Result := StrToFloat(Expression);
    except
        if Assigned(ParamRequest) then
        begin
            if Expression[1] in ['-', '+'] then
                case Expression[1] of
                    '-': Result := (-1) * ParamRequest(Copy(
                        Expression, 2, Length(Expression) - 1));
                    '+': Result := ParamRequest(Copy(
                        Expression, 2, Length(Expression) - 1));
                end
            else Result := ParamRequest(Expression)
        end
        else begin Result := 0; ErrorCode := CALC_INVALID_PARAMETER; Exit end;
    end;
    DecimalSeparator := SaveDecimalSeparator;
end;

function CalculateExpr(var Expression: string;
    var ErrorCode: LongInt; const ParamRequest: FParamRequest): Double;
var Index, Index2: LongInt;
    SExpr: string;
    Value: Double;
    St: string;
    SaveDecimalSeparator: Char;
begin
    ErrorCode := CALC_NO_ERRORS;
    SaveDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    repeat
        Index := GetCharPosition(Expression, ')', 1, 1);
        if Index <> -1 then
        begin
            Index2 := GetCharPosition(Expression, '(', -1, Index);
            SExpr := Copy(Expression, Index2 + 1, Index - Index2 - 1);
            Delete(Expression, Index2, Index - Index2 + 1);
            Value := CalculateSimpExpr(SExpr, ErrorCode, ParamRequest);
            if ErrorCode <> 0 then begin Result := 0; Exit end;
            St := FloatToStrF(Value, ffGeneral, 6, 4);
            Insert(St, Expression, Index2);
        end;
    until Index = -1;
    Result := CalculateSimpExpr(Expression, ErrorCode, ParamRequest);
    DecimalSeparator := SaveDecimalSeparator;
end;

function GetRandomWithSign: Double;
begin
    case Round(Random) of
        0: Result := (-1) * Random;
        1: Result := Random;
    end;
end;

procedure GetPosInArrays(
    const ArraysLengths: array of LongInt; const Index: LongInt;
    var ArrayNumber: LongInt; var ArrayIndex: LongInt);
var TotalNumber: LongInt;   //  полное число элементов
    LengthsSum: LongInt;    //  сумма длин массивов
    i: LongInt;
begin
    if Length(ArraysLengths) = 0 then
        raise ETools.Create('ArraysLengths must be assigned...');
    TotalNumber := 0;
    for i := 0 to Length(ArraysLengths) - 1 do
        TotalNumber := TotalNumber + ArraysLengths[i];
    if (Index < 0) or (Index >= TotalNumber) then
        raise ETools.Create('Invalid item index...');

    LengthsSum := 0;
    for i := 0 to Length(ArraysLengths) - 1 do
    begin
        if (Index >= LengthsSum) and (Index < LengthsSum + ArraysLengths[i]) then
        begin
            ArrayNumber := i;
            ArrayIndex := Index - LengthsSum;
            Exit;
        end else LengthsSum := LengthsSum + ArraysLengths[i];
    end;
end;

procedure AddVectorToArray(
    //  добавляет вектор к концу массива векторов
    var Arr: TVector3Array;
    const Vector: TDoubleVector3
    );
begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[Length(Arr) - 1] := Vector;
end;

procedure InsertVectorIntoArray(
    //  вставляет вектор в массив в позицию Index
    var Arr: TVector3Array;
    const Index: LongInt;
    const Vector: TDoubleVector3
    );
var i: LongInt;
begin
    if (Index < 0) or (Index >= Length(Arr)) then
        raise ETools.Create('Invalid array index...');
    SetLength(Arr, Length(Arr) + 1);
    for i := Length(Arr) - 2 downto Index do Arr[i + 1] := Arr[i];
    Arr[Index] := Vector;
end;

procedure DeleteVectorFromArray(
    var Arr: TVector3Array;
    const Index: LongInt
    );
var i: LongInt;
begin
    if (Index < 0) or (Index >= Length(Arr)) then
        raise ETools.Create('Invalid array index...');
    for i := Index + 1 to Length(Arr) - 1 do Arr[i - 1] := Arr[i];
    SetLength(Arr, Length(Arr) - 1);
end;

function ReadComponentByReader(const Reader: TReader): TComponent;
var SaveReaderOwner: TComponent;
begin
    SaveReaderOwner := Reader.Owner;
    Reader.Owner := nil;
    Result := Reader.ReadComponent(nil);
    Reader.Owner := SaveReaderOwner;
end;

procedure UtilizeObject(PtrToObject: TObject);
begin
    if PtrToObject is TCBRCComponent then TCBRCComponent(PtrToObject).Free
    else PtrToObject.Free;
end;

procedure CheckArrItemIndex(const MinIndex, MaxIndex, Index: LongInt);
begin
    if (Index < MinIndex) or (Index > MaxIndex) then
        raise ETools.Create('Invalid item index (' + IntToStr(Index) + ')...');
end;

procedure DeleteItemLongArr(
    var Arr: TLongArray;
    const Index: LongInt
    );
var i: LongInt;
begin
    CheckArrItemIndex(0, Length(Arr) - 1, Index);
    for i := Index + 1 to Length(Arr) - 1 do Arr[i - 1] := Arr[i];
    SetLength(Arr, Length(Arr) - 1);
end;

procedure InsertItemLongArr(
    var Arr: TLongArray;
    const Index: LongInt;
    const Item: LongInt     //  вставляемое значение
    );
var i: LongInt;
begin
    CheckArrItemIndex(0, Length(Arr) - 1, Index);
    SetLength(Arr, Length(Arr) + 1);
    for i := Length(Arr) - 2 downto Index do Arr[i + 1] := Arr[i];
    Arr[Index] := Item;
end;

procedure AddItemLongArr(
    var Arr: TLongArray;
    const Item: LongInt
    );
begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[Length(Arr) - 1] := Item;
end;


end.
