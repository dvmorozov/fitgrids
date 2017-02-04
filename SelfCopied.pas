//      двойной косой чертой комментируютс€ замечани€, сохран€емые во
//      всех верси€х исходника; фигурными скобками комментируютс€ замечани€,
//      сохран€емые только в версии исходника дл€ бесплатного распространени€
{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit SelfCopied;

{$MODE Delphi}

interface

uses
    Classes, ComponentList, SysUtils, CBRCComponent, MyExceptions;

type
    ISelfCopied = interface
        ['{DF1ABB41-F255-11D4-968F-C7AD39AA7469}']
        function GetCopy: TObject;
            //  возвращает только собственно копию объекта,
            //  без копий ассоциированных объектов
        procedure CopyParameters(const Dest: TObject);

            //  задание специальных режимов копировани€; введение
            //  спец. режимов копировани€ вызываетс€ необходимостью
            //  вложенности ф-й копировани€
        procedure SetSelfCopyingMode(const AMode: LongInt);
        function GetSelfCopyingMode: LongInt;
    end;

const SelfCopiedGUID: TGUID = '{DF1ABB41-F255-11D4-968F-C7AD39AA7469}';

type
    TSelfCopiedComponent = class(TCBRCComponent, ISelfCopied)
    public
        function GetCopy: TObject; virtual;
        procedure CopyParameters(const Dest: TObject); virtual;

        procedure SetSelfCopyingMode(const AMode: LongInt); virtual; abstract;
        function GetSelfCopyingMode: LongInt; virtual; abstract;
    end;

    ESelfCopiedCompList = class(Exception);

    TSelfCopiedCompList = class(TComponentList, ISelfCopied)
        //  !!! по умолчанию список всегда активен, поэтому копи€ списка
        //  всегда находтс€ в активном состо€нии; при необходимости объект,
        //  которому нужна копи€ должен сам сделать список неактивным !!!
    public
        function GetCopy: TObject; virtual;
        function GetSharedCopy: TObject; virtual;
            //  возвращает копию списка, элементы которого
            //  €вл€ютс€ собственными элементами
        procedure CopyParameters(const Dest: TObject); virtual;

        procedure SetSelfCopyingMode(const AMode: LongInt); virtual; abstract;
        function GetSelfCopyingMode: LongInt; virtual; abstract;

        procedure Insert(Index: Integer; Item: TComponent); override;
        function Add(Item: TComponent): Integer; override;
    end;

implementation

function TSelfCopiedCompList.GetCopy: TObject;
begin
    Result := NewInstance;
    TSelfCopiedCompList(Result).Create(nil);
    CopyParameters(Result);
end;

function TSelfCopiedCompList.GetSharedCopy: TObject;
var i: LongInt;
begin
    Result := NewInstance;
    TSelfCopiedCompList(Result).Create(nil);
    for i := 0 to Count - 1 do
        TSelfCopiedCompList(Result).Add(TComponent(Items[i]));
end;

procedure TSelfCopiedCompList.CopyParameters(const Dest: TObject);
var i: LongInt;
    ISC: ISelfCopied;
begin
    if Dest.ClassType <> Self.ClassType then
        raise ESelfCopiedCompList.Create('Invalid destination type...');

    if Count <> 0 then
        if Count <> TSelfCopiedCompList(Dest).Count then
        begin
            TSelfCopiedCompList(Dest).Clear;
            for i := 0 to Count - 1 do
            begin
                if Items[i].GetInterface(SelfCopiedGUID, ISC) then
                    TSelfCopiedCompList(Dest).Add(TComponent(ISC.GetCopy))
                else raise ESelfCopiedCompList.Create('Invalid item type...');
            end;
        end else
        begin
            for i := 0 to Count - 1 do
            begin
                if Items[i].GetInterface(SelfCopiedGUID, ISC) then
                    ISC.CopyParameters(TSelfCopiedCompList(Dest).Items[i])
                else raise ESelfCopiedCompList.Create('Invalid item type...');
            end;
        end;
end;

procedure TSelfCopiedCompList.Insert(Index: Integer; Item: TComponent);
var ISC: ISelfCopied;
begin
    if Item.GetInterface(SelfCopiedGUID, ISC) then inherited
    else raise ESelfCopiedCompList.Create('Invalid item type...');
end;

function TSelfCopiedCompList.Add(Item: TComponent): Integer;
var ISC: ISelfCopied;
begin
    if Item.GetInterface(SelfCopiedGUID, ISC) then Result := inherited Add(Item)
    else raise ESelfCopiedCompList.Create('Invalid item type...');
end;

function TSelfCopiedComponent.GetCopy: TObject;
begin
    Result := NewInstance;
    try
        TSelfCopiedComponent(Result).Create(nil);
        CopyParameters(Result);
    except
        Result.Free;
        raise;
    end;
end;

procedure TSelfCopiedComponent.CopyParameters(const Dest: TObject);
begin
    if Dest.ClassType <> Self.ClassType then
        raise ESelfCopiedCompList.Create('Invalid destination type...');
end;

initialization
end.
