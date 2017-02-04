//      двойной косой чертой комментируются замечания, сохраняемые во
//      всех версиях исходника; фигурными скобками комментируются замечания,
//      сохраняемые только в версии исходника для бесплатного распространения
{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit CBRCComponent;

{$MODE Delphi}

interface

uses Classes;

type
    TCBRCComponent = class(TComponent)
        {  the component Controlled By References Counter (CBRC) }
        //  компонент, уничтожение которого управляется числом ссылок на него
        //  через интерфейсы; при вызове деструктора компонент уничтожается
        //  только в том случае, если число ссылок на его интерфейсы равно нулю,
        //  если же число ссылок стало равно нулю, то компонент не уничтожается
        //  до тех пор пока не будет вызван деструктор (??? нало ли сделать
        //  классовый метод, который бы создавал объекты, управляемые только
        //  числом ссылок и возвращал бы ссылку на IUnknown объекта ???)
        //  !!! объект не должен давать указатель на свой интерфейс
        //  никакому из дочерних объектов, в противном случае такой
        //  объект вообще не сможет уничтожиться !!!
    protected
        FRefCount: LongInt;         //  число ссылок на объект
        IntControlled: Boolean;     //  признак того, что уничтожение
                                    //  объекта управляется числом ссылок
                                    //  устанавливается в методе Free

        function _AddRef: Integer; virtual; stdcall; 
        function _Release: Integer; virtual; stdcall;
    public
        procedure Free;
            //  поскольку метод Free не виртуальный, а просто переопределен
            //  то нельзя присваивать указатель на объект переменной типа -
            //  предка TCBRCComponent, либо нужно делать явное преобразование
            //  типа перед вызовом Free

        property RefCount: LongInt read FRefCount;
    end;

implementation

procedure TCBRCComponent.Free;
begin
    //  Free - метод класса, поэтому может вызываться даже когда
    //  объект не был распределен в памяти, поэтому перед доступом
    //  требуется проверка
    if Assigned(Self) then
    begin
        if RefCount = 0 then Destroy
        else IntControlled := True;
    end;
end;

function TCBRCComponent._AddRef: Integer;
begin
    Inc(FRefCount);
    Result := RefCount;
end;

function TCBRCComponent._Release: Integer;
begin
    Dec(FRefCount);
    Result := RefCount;
    if IntControlled and (Result = 0) then Free;
end;

initialization
    RegisterClass(TCBRCComponent);
end.

