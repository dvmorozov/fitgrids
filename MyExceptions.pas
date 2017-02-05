{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit MyExceptions;

{$IFDEF Lazarus}
{$MODE Delphi}
{$ENDIF}

interface

uses
    Classes, SysUtils;

type
    //  критическая ошибка, связанная с невозможностью
    //  выполнения действия, требуемого пользователем
    EUserException = class(Exception);

implementation

initialization
end.

