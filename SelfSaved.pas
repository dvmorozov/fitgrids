//      двойной косой чертой комментируются замечания, сохраняемые во
//      всех версиях исходника; фигурными скобками комментируются замечания,
//      сохраняемые только в версии исходника для бесплатного распространения
{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit SelfSaved;

{$MODE Delphi}

interface

uses Classes, ClassInheritIDs;

type
    TPropHeaderRec = record
        //  запись, характеризующая свойства класса в файле
        ClassInheritID: Byte;           //  уникальный идентификатор класса
                                        //  в цепи наследования
        PropVersionNum: Byte            //  номер версии свойств класса
    end;

    TSelfSavedComponent = class(TComponent)
        //  компонент, который умеет сохраняться в потоке, позволяя
        //  при этом реализовать добавление новых параметров и
        //  изменение порядка наследования без потери способности
        //  читать существующие файлы; запись (чтение) свойств класса
        //  начинаются со свойств класса последнего в цепи наследования
        //  функции записи/чтения сделаны классовыми для того, чтобы
        //  иметь возможность перемещаться по цепи наследования
    private
        procedure ReadData(Reader: TReader);
            //  вызывает ф-ю чтения свойств цепи наследования, читая
            //  маркеры начала и конца данных
        class procedure ReadInheritChain(
            //  последовательно читает заголовки свойств из файла и
            //  ищет в цепи наследования класс, который отвечает за
            //  чтение этих данных
            const Reader: TReader;
            const AnObject: TSelfSavedComponent
                //  объект, свойства которого нужно записать
            );
        class procedure ReadPropHeader(const Reader: TReader;
            //  читает параметры, характеризующие сохраненные свойства класса
            out PropHeaderRec: TPropHeaderRec
            );

        procedure WriteData(Writer: TWriter);
            //  вызывает запись свойств класса, записывая
            //  маркеры начала и конца данных
        class procedure WriteInheritChain(
            //  записывает свойства всей цепи наследования до класса
            //  TSelfSavedComponent, последовательно перемещаясь по цепи
            const Writer: TWriter;
            const AnObject: TSelfSavedComponent
                //  объект, свойства которого нужно записать
            ); virtual;
        class procedure WritePropHeader(const Writer: TWriter); virtual;
            //  записывет параметры, характеризующие сохраненные свойства класса

    protected
        class function IsClassInheritIDValid(
            //  проверяет, поддерживает ли класс данный ID
            const ClassInheritID: Byte      //  уникальный идентификатор класса
                                            //  в цепи наследования
            ): Boolean; virtual;
        class function GetPropHeaderRec: TPropHeaderRec; virtual;
            //  возвращает запись заголовка свойств класса
        class procedure ReadProperties(
            //  выполняет чтение свойств класса из файла (здесь ничего не делает)
            //  если номер версии свойств в файле меньше, чем текущий,
            //  то выполняется алгоритм правильной инициализации свойств
            //  класса, значения которых нельзя прочитать из файла
            const Reader: TReader;
            const PropHeaderRec: TPropHeaderRec;
                //  запись заголовка свойств, прочитанная из файла
                //  запись передается полностью (а не только номер версии свойств)
                //  на тот случай, если класс поддерживает несколько ID
            const AnObject: TSelfSavedComponent
                //  объект, свойства которого нужно прочитать
            ); virtual;

        class procedure WriteProperties(
            //  выполняет запись свойств класса в файл (здесь ничего не делает)
            const Writer: TWriter;
            const AnObject: TSelfSavedComponent
                //  объект, свойства которого нужно записать
            ); virtual;

    public
        procedure DefineProperties(Filer: TFiler); override;
    end;

    TSelfSavedComponents = class of TSelfSavedComponent;

implementation

{ TSelfSavedComponent }

class function TSelfSavedComponent.IsClassInheritIDValid(
    const ClassInheritID: Byte): Boolean;
begin
    if ClassInheritID = GetPropHeaderRec.ClassInheritID then
        Result := True else Result := False;
end;

procedure TSelfSavedComponent.DefineProperties(Filer: TFiler);
begin
    Filer.DefineProperty(' ', ReadData, WriteData, True)
end;

class function TSelfSavedComponent.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SelfSavedInheritID;
    Result.PropVersionNum := SelfSavedCurVerNum;
end;

procedure TSelfSavedComponent.ReadData(Reader: TReader);
begin
    with Reader do
    begin
        ReadListBegin;
        ReadInheritChain(Reader, Self);
        ReadListEnd;
    end;
end;

class procedure TSelfSavedComponent.ReadProperties(
    const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent
    );
begin
end;

class procedure TSelfSavedComponent.ReadPropHeader(const Reader: TReader;
    out PropHeaderRec: TPropHeaderRec);
begin
    with Reader, PropHeaderRec do
    begin
        ClassInheritID := ReadInteger;
        PropVersionNum := ReadInteger;
    end;
end;

procedure TSelfSavedComponent.WriteData(Writer: TWriter);
begin
    with Writer do
    begin
        WriteListBegin;
        WriteInheritChain(Writer, Self);
        WriteListEnd;
    end;
end;

class procedure TSelfSavedComponent.WriteInheritChain(
    const Writer: TWriter;
    const AnObject: TSelfSavedComponent
    );
var CurClassType: TSelfSavedComponents;
begin
    CurClassType := TSelfSavedComponents(AnObject.ClassType);
    repeat
        (*with CurClassType do
        begin*)
            CurClassType.WritePropHeader(Writer);
            CurClassType.WriteProperties(Writer, AnObject);
        (*end;*)
        CurClassType := TSelfSavedComponents(CurClassType.ClassParent);
    until CurClassType = TSelfSavedComponent.ClassParent;
end;

class procedure TSelfSavedComponent.WriteProperties(
    const Writer: TWriter;
    const AnObject: TSelfSavedComponent
    );
begin
end;

class procedure TSelfSavedComponent.WritePropHeader(const Writer: TWriter);
var PropHeaderRec: TPropHeaderRec;
begin
    PropHeaderRec := GetPropHeaderRec;
    with Writer, PropHeaderRec do
    begin
        WriteInteger(ClassInheritID);
        WriteInteger(PropVersionNum);
    end;
end;

class procedure TSelfSavedComponent.ReadInheritChain(
    const Reader: TReader;
    const AnObject: TSelfSavedComponent
    );
var PropHeaderRec: TPropHeaderRec;
    CurClassType: TSelfSavedComponents;
begin
    while not Reader.EndOfList do
    begin
        ReadPropHeader(Reader, PropHeaderRec);

        CurClassType := TSelfSavedComponents(AnObject.ClassType);
        repeat
            (*with CurClassType do*)
                if CurClassType.IsClassInheritIDValid(PropHeaderRec.ClassInheritID) then
                begin
                    CurClassType.ReadProperties(Reader, PropHeaderRec, AnObject);
                    Break;
                end;
            CurClassType := TSelfSavedComponents(CurClassType.ClassParent);
        until CurClassType = TSelfSavedComponent.ClassParent;
    end;
end;

end.
