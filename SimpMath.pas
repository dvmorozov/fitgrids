//      двойной косой чертой комментируются замечания, сохраняемые во
//      всех версиях исходника; фигурными скобками комментируются замечания,
//      сохраняемые только в версии исходника для бесплатного распространения
{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit SimpMath;

//{$mode objfpc}{$H+}
{$IFDEF Lazarus}
{$MODE Delphi}
{$ENDIF}

interface

uses
    Math, Classes, CBRCComponent, SysUtils;
    
const TINY = 1e-6;

type
    EPointsArrayIsNotAssigned = class(Exception);
    TwoDimArray = array of array[1..2] of Double;

type
    TDoubleVector3 = array[1..3] of Double;

    IVector = interface;

    ISpace = interface
        //  пространство векторов
        function GetScalarMul(const Vect1, Vect2: IVector): Double;
            //  скалярное произведение векторов
    end;

    IVector = interface
        //  вещественный вектор
        function GetSpace: ISpace;
        procedure SetSpace(const ASpace: ISpace);
        function GetNorma: Double;
        procedure SetNorma(const ANorma: Double);
        function GetCompsNumber: LongInt;
        function GetComp(index: LongInt): Double;
        procedure SetComp(index: LongInt; AComp: Double);
        function GetNormComp(index: LongInt): Double;

        property Space: ISpace
                read GetSpace           write SetSpace;
        property Norma: Double
                read GetNorma           write SetNorma;
        property CompsNumber: LongInt
                read GetCompsNumber;
        property Comps[index: LongInt]: Double
                read GetComp            write SetComp;
        property NormComps[index: LongInt]: Double
                read GetNormComp;
    end;

    IComplexVector = interface(IVector)
        //  комплексный вектор
        function GetImComp(index: LongInt): Double;
        procedure SetImComp(index: LongInt; AImComp: Double);
        function GetNormImComp(index: LongInt): Double;

        property ImComps[index: LongInt]: Double
            //  мнимые части компонент вектора
                read GetImComp          write SetImComp;
        property NormImComps[index: LongInt]: Double
            //  мнимые части нормированного комплексного вектора
                read GetNormImComp;
    end;

    E3DVector = class(Exception);    //??? убрать

    T3DVector = class(TCBRCComponent, IVector)  // ??? убрать
        //  класс - вектор; реализует набор необходимых
        //  ф - й для работы с 3 - мерным вектором
        //  !!! не закончен !!!
    protected
        FSpace: ISpace;
        FVector: TDoubleVector3;
        FNormalizedVector: TDoubleVector3;
        FNorma: Double;

        function GetSpace: ISpace;
        procedure SetSpace(const ASpace: ISpace);
        function GetNorma: Double;
        procedure SetNorma(const ANorma: Double);
        function GetCompsNumber: LongInt;
        function GetComp(index: LongInt): Double;
        procedure SetComp(index: LongInt; AComp: Double);
        function GetNormComp(index: LongInt): Double;

    public
        property Space: ISpace
                read GetSpace           write SetSpace;
        property Norma: Double
                read GetNorma           write SetNorma;
        property CompsNumber: LongInt
                read GetCompsNumber;
        property Comps[index: LongInt]: Double
                read GetComp            write SetComp;
        property NormComps[index: LongInt]: Double
                read GetNormComp;
    end;

    T3DComplexVector = class(T3DVector, IComplexVector)  //??? убрать
    protected
        FImVector: TDoubleVector3;  //  мнимая часть

        function GetImComp(index: LongInt): Double;
        procedure SetImComp(index: LongInt; AImComp: Double);
        function GetNormImComp(index: LongInt): Double;
    public
        property ImComps[index: LongInt]: Double
                read GetImComp          write SetImComp;
        property NormImComps[index: LongInt]: Double
                read GetNormImComp;
    end;

procedure ConvertSphericalToDekart(Theta, Phi, R: Double; var x, y, z: Double);
    //  Theta находится в интервале от 0 до pi; Phi - в интервале от -pi до pi
procedure ConvertDekartToSpherical(x, y, z: Double; var Theta, Phi, R: Double);
procedure ConvertDekartToAphine(
    //  преобразование вектора из декартовых координат в афинные;
    //  Alpha не используется (см. условия ниже)
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vector: TDoubleVector3);
procedure ConvertAphineToDekart(
    //  преобразование вектора из афинных координат в ортонормированную СК;
    //  Alpha не используется (см. условия ниже)
    //  преобразования выполняются при следующих предположениях:
    //  1. Alpha, Beta, Gamma - углы между осями в афинной СК
    //  (выраженные в радианах), причем Gamma = e1^e2; Beta = e3^e1;
    //  Alpha = e2^e3;
    //  2. ось e1 афинной СК совпадает с осью e1 декартовой СК
    //  3. ось e2 афинной СК лежит в плоскости e1e2 декартовой СК
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vector: TDoubleVector3);

procedure DecPhi(Dec: Double; var Phi: Double);
procedure DecTheta(Dec: Double; var Theta: Double);
procedure IncPhi(Inc: Double; var Phi: Double);
procedure IncTheta(Inc: Double; var Theta: Double);

procedure PutValueIntoInterval(
    //  помещает переданную величину в заданный числовой интервал
    const MinLimit, MaxLimit: Double;
    var Value: Double);
function IsValueIntoInterval(const MinLimit, MaxLimit, Value: Double): Boolean;
    //  возвращает True, если Value находится в заданном интервале,
    //  False - в противном случае

function GetScalarMul(const Vect1, Vect2: TDoubleVector3): Double;
    //  скалярное произведение в ортонормированной СК
function GetScalarMulA(
    //  скалярное произведение не в ортонормированной СК;
    //  углы между векторами базиса, в котором заданы вектора
    //  (в радианах): Gamma = e1^e2; Beta = e3^e1; Alpha = e2^e3
    const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
function GetScalarMulAN(
    //  скалярное произведение не в ортонормированной СК находится
    //  между единичными векторами, направленными вдоль исходных векторов;
    //  углы между векторами базиса, в котором заданы вектора
    //  (в радианах): Gamma = e1^e2; Beta = e3^e1; Alpha = e2^e3
    const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
function GetAngle(
    //  возвращает угол между векторами
    const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
function GetVectorMulA(
    //  векторное произведение не в ортонормированной СК;
    //  углы между векторами базиса, в котором заданы вектора
    //  (в радианах): Gamma = e1^e2; Beta = e3^e1; Alpha = e2^e3
    const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): TDoubleVector3;

function GetVectModule(
    //  возвращает модуль вектора, заданного в ортонормированной СК
    const Vect: TDoubleVector3): Double;

function GetVectModuleA(
    //  возвращает модуль вектора, заданного не в ортонормированной СК
    const Vect: TDoubleVector3;
    const A, B, C, Alpha, Beta, Gamma: Double): Double;


procedure GetUnitVect(
    //  вычисляет единичный вектор для вектора, заданного в декартовой СК
    const Vect: TDoubleVector3;
    var UnitVect: TDoubleVector3);

procedure GetUnitVectA(
    //  вычисляет единичный вектор для вектора, заданного не в ортонормированной СК
    const Vect: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double; var UnitVect: TDoubleVector3);

procedure GetMutualVectors(
    //  возвращает три вектора, которые представляют собой взаимные вектора
    //  к векторам, образующим базис с заданными параметрами, причем
    //  Vect1 = (1/V)[e2 x e3], |Vect1| = (1/V)|e2||e3|Sin(Alpha);
    //  Vect2 = (1/V)[e3 x e1], |Vect2| = (1/V)|e1||e3|Sin(Beta);
    //  Vect3 = (1/V)[e1 x e2], |Vect3| = (1/V)|e1||e2|Sin(Gamma);
    //  |e1| = A, |e2| = B; |e3| = C;
    //  Gamma = e1^e2; Beta = e3^e1; Alpha = e2^e3(углы в радианах);
    //  V - объем ячейки, построенной на векторах базиса
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vect1, Vect2, Vect3: TDoubleVector3);

procedure GetMutualVectorsInNewBasis(
    const A, B, C, Alpha, Beta, Gamma: Double;
        //  параметры исходного базиса, в котором заданы все вектора
    NewBasisVect1, NewBasisVect2, NewBasisVect3: TDoubleVector3;
        //  вектора нового базиса (определены в старом)
    var Vect1, Vect2, Vect3: TDoubleVector3
        //  взаимные вектора к векторам нового базиса (определены в старом)
    );

function GetVolume(const A, B, C, Alpha, Beta, Gamma: Double): Double;
    //  возвращает объем ячейки, построенной на векторах базиса

function GetVectInNewBasis(
    //  возвращает координаты вектора относительно нового базиса
    const A, B, C, Alpha, Beta, Gamma: Double;
        //  параметры исходного базиса, в котором заданы все вектора
    NewBasisVect1, NewBasisVect2, NewBasisVect3: TDoubleVector3;
        //  вектора нового базиса (определены в старом)
    InitialVect: TDoubleVector3
        //  вектор в "старом" базисе
    ): TDoubleVector3;

function MulVectByValue(const Vect: TDoubleVector3;
    Value: Double): TDoubleVector3;
procedure SetVectModule(var Vect: TDoubleVector3;
    const A, B, C, Alpha, Beta, Gamma, Module: Double);

function GetSubVect(Vect1, Vect2: TDoubleVector3): TDoubleVector3;
function ArcSin(x: Double): Double;
function ArcCos(x: Double): Double;
function GetNumberDegree(Number: Double): LongInt;
    //  возвращает порядок данного числа
function GetPowerOf10(Power: LongInt): Double;
function Sign(Number: Double): LongInt;

function Lagrange(
    PointsArray: TwoDimArray;   //  1 - й - X, 2 - й - Y
    const X: Double): Double;

function GaussPoint(
    const A,    //  значение интеграла от этой ф-и по всей области определения
    Sigma, x0, x: Double): Double;
function LorentzPoint(
    const A,    //  значение интеграла от этой ф-и по всей области определения
    Sigma, x0, x: Double): Double;
function PseudoVoigtPoint(
    const A, Sigma, Eta, x0, x: Double
    ): Double;
function AsymPseudoVoigtPoint(
    const A, Sigma, Eta, x0, x, DeltaSigma: Double
    ): Double;
function TwoBranchesPseudoVoigtPoint(
    const A, Sigma, Eta, SigmaRight, EtaRight, x0, x: Double
    ): Double;
procedure Gauss(PointsArray: TwoDimArray; const A, Sigma, x0: Double);
procedure Lorentz(PointsArray: TwoDimArray; const A, Sigma, x0: Double);
procedure PseudoVoigt(PointsArray: TwoDimArray; const A, Sigma, Eta, x0: Double);
procedure AsymPseudoVoigt(
    PointsArray: TwoDimArray; const A, Sigma, Eta, x0, DeltaSigma: Double);
procedure TwoBranchesPseudoVoigt(PointsArray: TwoDimArray;
    const A, Sigma, Eta, SigmaRight, EtaRight, x0: Double);
function CalcPolinom2(const A, B, C, x0, x: Double): Double;

implementation

//  вспомогательные функции для работы с векторами
function GetD(const Alpha, Beta, Gamma: Double): Double; forward;
function GetPAlpha(const Alpha, Beta, Gamma: Double): Double; forward;
function GetPBeta(const Alpha, Beta, Gamma: Double): Double; forward;
function GetPGamma(const Alpha, Beta, Gamma: Double): Double; forward;

function ArcSin(x: Double): Double;
var TempDouble: Double;
begin
    //  особенности расчета: если x = 1, то 1 - Sqr(x) видимо
    //  получается отрицательной, вследствии ошибки расчета Sqr(x),
    //  в результате при извлечении корня возникает исключение
    TempDouble := 1 - Sqr(x);
    if Abs(TempDouble) < TINY then TempDouble := 0;
        TempDouble := Sqrt(TempDouble);
    if TempDouble <> 0 then Result := ArcTan2(x, TempDouble)
    else Result := pi / 2;
end;

function ArcCos(x: Double): Double;
var TempDouble: Double;
begin
    //  особенности расчета: если x = 1, то 1 - Sqr(x) видимо
    //  получается отрицательной, вследствии ошибки расчета Sqr(x),
    //  в результате при извлечении корня возникает исключение
    TempDouble := 1 - Sqr(x);
    if Abs(TempDouble) < TINY then TempDouble := 0;
    if x <> 0 then Result := ArcTan2(Sqrt(TempDouble), x)
    else Result := pi / 2;
end;

function GetScalarMul(const Vect1, Vect2: TDoubleVector3): Double;
begin
    Result := Vect1[1] * Vect2[1] + Vect1[2] * Vect2[2] + Vect1[3] * Vect2[3];
end;

function GetScalarMulA(const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
begin
    Result := Vect1[1] * Vect2[1] * Sqr(A) +
    Vect1[2] * Vect2[2] * Sqr(B) + Vect1[3] * Vect2[3] * Sqr(C) +
    (Vect1[2] * Vect2[1] + Vect1[1] * Vect2[2]) * A * B * Cos(Gamma) +
    (Vect1[1] * Vect2[3] + Vect1[3] * Vect2[1]) * C * A * Cos(Beta) +
    (Vect1[3] * Vect2[2] + Vect1[2] * Vect2[3]) * B * C * Cos(Alpha);
end;

function GetScalarMulAN(const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
var V1, V2: TDoubleVector3;
begin
    GetUnitVectA(Vect1, A, B, C, Alpha, Beta, Gamma, V1);
    GetUnitVectA(Vect2, A, B, C, Alpha, Beta, Gamma, V2);

    Result := GetScalarMulA(V1, V2, A, B, C, Alpha, Beta, Gamma);
end;

function GetAngle(const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
begin
    Result := ArcCos(GetScalarMulAN(Vect1, Vect2, A, B, C, Alpha, Beta, Gamma));
end;

function GetVectModule(const Vect: TDoubleVector3): Double;
begin
    Result := Sqrt(GetScalarMul(Vect, Vect));
end;

function GetVectModuleA(
    const Vect: TDoubleVector3;
    const A, B, C, Alpha, Beta, Gamma: Double): Double;
begin
    Result := Sqrt(GetScalarMulA(Vect, Vect, A, B, C, Alpha, Beta, Gamma));
end;

function MulVectByValue(const Vect: TDoubleVector3;
    Value: Double): TDoubleVector3;
begin
    Result[1] := Vect[1] * Value;
    Result[2] := Vect[2] * Value;
    Result[3] := Vect[3] * Value;
end;

procedure SetVectModule(var Vect: TDoubleVector3;
    const A, B, C, Alpha, Beta, Gamma, Module: Double);
var TempModule: Double;
begin
    TempModule := GetVectModuleA(Vect, A, B, C, Alpha, Beta, Gamma);
    if TempModule <> 0 then Vect := MulVectByValue(Vect, Module / TempModule)
end;

function GetSubVect(Vect1, Vect2: TDoubleVector3): TDoubleVector3;
begin
    Result[1] := Vect1[1] - Vect2[1];
    Result[2] := Vect1[2] - Vect2[2];
    Result[3] := Vect1[3] - Vect2[3];
end;

procedure ConvertSphericalToDekart(Theta, Phi, R: Double; var x, y, z: Double);
    //  преобразует координаты сферической с.к. в вектор декартовой с.к.
begin
    x := R * Sin(Theta) * Cos(Phi);
    y := R * Sin(Theta) * Sin(Phi);
    z := R * Cos(Theta);
end;

procedure ConvertDekartToSpherical(x, y, z: Double; var Theta, Phi, R: Double);
begin
    R := Sqrt(Sqr(x) + Sqr(y) + Sqr(z));
    if z <> 0 then Theta := ArcTan2(Sqrt(Sqr(x) + Sqr(y)), z)
    else Theta := pi / 2;
    if x <> 0 then Phi := ArcTan2(y, x)
    else if y >= 0 then Phi := pi / 2 else Phi := -pi / 2;
end;

procedure DecPhi(Dec: Double; var Phi: Double);
begin
    Phi := Phi - Dec;
    if Phi > pi then Phi := -pi + (Phi - pi)
    else if Phi <= -pi then Phi := pi - (-pi - Phi);
end;

procedure DecTheta(Dec: Double; var Theta: Double);
begin
    Theta := Theta - Dec;
    if Theta < 0 then Theta := 0;
end;

procedure IncPhi(Inc: Double; var Phi: Double);
begin
    Phi := Phi + Inc;
    if Phi > pi then Phi := -pi + (Phi - pi)
    else if Phi <= -pi then Phi := pi - (-pi - Phi);
end;

procedure IncTheta(Inc: Double; var Theta: Double);
begin
   Theta := Theta + Inc;
   if Theta > pi then Theta := pi;
end;

function GetNumberDegree(Number: Double): LongInt;
var i: LongInt;
    TempDouble: Double;
begin
    TempDouble := Number;
    if Number = 0 then
    begin Result := 1;  Exit end;
    if Number >= 1 then
    begin
        i := -1;
        while Int(TempDouble) <> 0 do
        begin
            Inc(i);
            TempDouble := TempDouble / 10;
        end
    end
    else
    begin
        i := 0;
        while Int(TempDouble) = 0 do
        begin
            Dec(i);
            TempDouble := TempDouble * 10;
        end;
    end;
    Result := i;
end;

function GetPowerOf10(Power: LongInt): Double;
var i: LongInt;
    TempDouble: Double;
begin
    TempDouble := 1;
    if Power >= 0 then
        for i := 1 to Power do TempDouble := TempDouble * 10
    else
        for i := -1 downto Power do TempDouble := TempDouble * 0.1;
    Result := TempDouble;
end;

function Sign(Number: Double): LongInt;
begin
    if Number >= 0 then Result := 1
    else Result := -1;
end;

function Lagrange(PointsArray: TwoDimArray;(*1 - й - X, 2 - й - Y*)
    const X: Double): Double;
var Lagr: Double;
    p1, p2: Double;
    i, j1: LongInt;
begin
    if not Assigned(PointsArray) then
    begin
        raise EPointsArrayIsNotAssigned.Create('Points array is not assigned...');
        Exit;
    end;
    Lagr := 0;
    for i := 0 to Length(PointsArray) - 1 do
    begin
        p1 := 1; p2 := 1;
        for j1 := 0 to Length(PointsArray) - 1 do
        begin
            if i <> j1 then
            begin
                p1 := p1 * (PointsArray[i][1] - PointsArray[j1][1]);
                p2 := p2 * (X - PointsArray[j1][1]);
            end;
        end;
        if p1 <> 0 then Lagr := Lagr + PointsArray[i][2] * p2 / p1;
    end;
    Result := Lagr;
end;

//  FWHM = 2 * Sqrt(2 * ln(2)) * Sigma
function GaussPoint(
    const A,    //  значение интеграла от этой ф-и по всей области определения
    Sigma, x0, x: Double): Double;
begin
    Assert(A >= 0);
    Assert(Sigma >= 0);
    Result := (A / (Sigma * Sqrt(2 * pi))) *
        exp(-1 * Sqr(x0 - x) / (2 * Sqr(Sigma)));
end;

//  FWHM = Sigma
function LorentzPoint(
    const A,    //  значение интеграла от этой ф-и по всей области определения
    Sigma, x0, x: Double): Double;
begin
    Assert(A >= 0);
    Assert(Sigma >= 0);
    (*
    Result := (A / (Sigma * Sqrt(2 * pi))) *
        exp(-1 * Abs(x0 - x) / (2 * Sqr(Sigma)));
    *)
    Result := A * (1 / (pi * Sigma / 2)) *
        (1 / (1 + Sqr((x - x0) / (Sigma / 2))));
end;

function PseudoVoigtPoint(
    const A, Sigma, Eta, x0, x: Double): Double;
begin
    Assert(A >= 0);
    Assert(Sigma >= 0);
    Assert((Eta >= 0) and (Eta <= 1));

    Result := A * ((1 - Eta) * (
                2 * Sqrt(Ln(2)) / (Sigma * Sqrt(pi)) *
                exp(-4 * Ln(2) * Sqr(x0 - x) / Sqr(Sigma))
            )  +
            
            Eta * (
                (2 / (pi * Sigma)) *
                (1 / (1 + Sqr(2 * (x - x0) / Sigma)))
            )
        );
end;

function AsymPseudoVoigtPoint(
    const A, Sigma, Eta, x0, x, DeltaSigma: Double): Double;
begin
    Assert(A >= 0);
    Assert(Sigma >= 0);
    Assert((Eta >= 0) and (Eta <= 1));

    if(x >= x0) then
    begin
        Result := A * ((1 - Eta) * (
                    exp(-4 * Ln(2) * Sqr(x0 - x) / Sqr((Sigma + DeltaSigma)))
                )  +

                Eta * (
                    (1 / (1 + Sqr(2 * (x - x0) / (Sigma + DeltaSigma))))
                )
            );
    end
    else
    begin
        Result := A * ((1 - Eta) * (
                    exp(-4 * Ln(2) * Sqr(x0 - x) / Sqr((Sigma - DeltaSigma)))
                )  +

                Eta * (
                    (1 / (1 + Sqr(2 * (x - x0) / (Sigma - DeltaSigma))))
                )
            );
    end;
end;

function TwoBranchesPseudoVoigtPoint(
    const A, Sigma, Eta, SigmaRight, EtaRight, x0, x: Double): Double;
begin
    Assert(A >= 0);
    Assert(Sigma >= 0);
    Assert((Eta >= 0) and (Eta <= 1));
    Assert(SigmaRight >= 0);
    Assert((EtaRight >= 0) and (EtaRight <= 1));

    if(x >= x0) then
    begin
        Result := A * ((1 - EtaRight) * (
                    exp(-4 * Ln(2) * Sqr(x0 - x) / Sqr(SigmaRight))
                )  +

                EtaRight * (
                    (1 / (1 + Sqr(2 * (x - x0) / SigmaRight)))
                )
            );
    end
    else
    begin
        Result := A * ((1 - Eta) * (
                    exp(-4 * Ln(2) * Sqr(x0 - x) / Sqr(Sigma))
                )  +

                Eta * (
                    (1 / (1 + Sqr(2 * (x - x0) / Sigma)))
                )
            );
    end;
end;

procedure Gauss(PointsArray: TwoDimArray; const A, Sigma, x0: Double);
var i: LongInt;
begin
    if not Assigned(PointsArray) then
        raise EPointsArrayIsNotAssigned.Create('Points array is not assigned...');

    for i := 0 to Length(PointsArray) - 1 do
        PointsArray[i][2] := GaussPoint(A, Sigma, x0, PointsArray[i][1]);
end;

procedure Lorentz(PointsArray: TwoDimArray; const A, Sigma, x0: Double);
var i: LongInt;
begin
    if not Assigned(PointsArray) then
        raise EPointsArrayIsNotAssigned.Create('Points array is not assigned...');

    for i := 0 to Length(PointsArray) - 1 do
        PointsArray[i][2] := LorentzPoint(A, Sigma, x0, PointsArray[i][1]);
end;

procedure PseudoVoigt(PointsArray: TwoDimArray; const A, Sigma, Eta, x0: Double);
var i: LongInt;
begin
    if not Assigned(PointsArray) then
        raise EPointsArrayIsNotAssigned.Create('Points array is not assigned...');

    for i := 0 to Length(PointsArray) - 1 do
        PointsArray[i][2] := PseudoVoigtPoint(A, Sigma, Eta, x0, PointsArray[i][1]);
end;

procedure AsymPseudoVoigt(
    PointsArray: TwoDimArray; const A, Sigma, Eta, x0, DeltaSigma: Double);
var i: LongInt;
begin
    if not Assigned(PointsArray) then
        raise EPointsArrayIsNotAssigned.Create('Points array is not assigned...');

    for i := 0 to Length(PointsArray) - 1 do
        PointsArray[i][2] := AsymPseudoVoigtPoint(
            A, Sigma, Eta, x0, PointsArray[i][1], DeltaSigma);
end;

procedure TwoBranchesPseudoVoigt(PointsArray: TwoDimArray;
    const A, Sigma, Eta, SigmaRight, EtaRight, x0: Double);
var i: LongInt;
begin
    if not Assigned(PointsArray) then
        raise EPointsArrayIsNotAssigned.Create('Points array is not assigned...');

    for i := 0 to Length(PointsArray) - 1 do
        PointsArray[i][2] := TwoBranchesPseudoVoigtPoint(
            A, Sigma, Eta, SigmaRight, EtaRight, x0, PointsArray[i][1]);
end;

procedure PutValueIntoInterval(const MinLimit, MaxLimit: Double;
var Value: Double);
begin
    if Value > MaxLimit then
        Value := MinLimit + Frac((Value - MaxLimit) /
        (MaxLimit - MinLimit)) * (MaxLimit - MinLimit);
    if Value < MinLimit then
        Value := MaxLimit - Frac((MinLimit - Value) /
        (MaxLimit - MinLimit)) * (MaxLimit - MinLimit);
end;

function IsValueIntoInterval(const MinLimit, MaxLimit, Value: Double): Boolean;
begin
    if (Value >= MinLimit) and (Value <= MaxLimit) then Result := True
    else Result := False;
end;

procedure ConvertDekartToAphine(
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vector: TDoubleVector3);
var V1, V2, V3, Result: TDoubleVector3;
begin
    //  вычисляются вектора ортонормированной СК, выраженные
    //  в базисе исходных (афинных) координат
    V1[1] := 1; V1[2] := 0; V1[3] := 0;
    V2[1] := 0; V2[2] := 1; V2[3] := 0;
    V3 := GetVectorMulA(V1, V2, A, B, C, Alpha, Beta, Gamma);
    GetUnitVectA(V1, A, B, C, Alpha, Beta, Gamma, V1);
    GetUnitVectA(V3, A, B, C, Alpha, Beta, Gamma, V3);
    V2 := GetVectorMulA(V3, V1, A, B, C, Alpha, Beta, Gamma);
    GetUnitVectA(V2, A, B, C, Alpha, Beta, Gamma, V2);
    //  V1, V2, V3 - ортонормированный базис, построенный в исходном базисе
    Result[1] := Vector[1] * V1[1] + Vector[2] * V2[1] + Vector[3] * V3[1];
    Result[2] := Vector[1] * V1[2] + Vector[2] * V2[2] + Vector[3] * V3[2];
    Result[3] := Vector[1] * V1[3] + Vector[2] * V2[3] + Vector[3] * V3[3];
    Vector := Result;
end;

procedure ConvertAphineToDekart(
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vector: TDoubleVector3);
var V1, V2, V3, Result: TDoubleVector3;
begin
    //  взаимные вектора к векторам ортонормированного базиса совпадают
    //  с самими векторами ортонормированного базиса; координаты вектора
    //  в новом базисе равны скалярным произведениям вектора на векторы,
    //  взаимные по отношению к векторам нового базиса
    V1[1] := 1; V1[2] := 0; V1[3] := 0;
    V2[1] := 0; V2[2] := 1; V2[3] := 0;
    V3 := GetVectorMulA(V1, V2, A, B, C, Alpha, Beta, Gamma);
    GetUnitVectA(V1, A, B, C, Alpha, Beta, Gamma, V1);
    GetUnitVectA(V3, A, B, C, Alpha, Beta, Gamma, V3);
    V2 := GetVectorMulA(V3, V1, A, B, C, Alpha, Beta, Gamma);
    GetUnitVectA(V2, A, B, C, Alpha, Beta, Gamma, V2);
    //  V1, V2, V3 - ортонормированный базис, построенный в исходном базисе
    Result[1] := GetScalarMulA(Vector, V1, A, B, C, Alpha, Beta, Gamma);
    Result[2] := GetScalarMulA(Vector, V2, A, B, C, Alpha, Beta, Gamma);
    Result[3] := GetScalarMulA(Vector, V3, A, B, C, Alpha, Beta, Gamma);
    Vector := Result;
end;

procedure GetUnitVect(
    //  вычисляет единичный вектор для вектора, заданного в декартовой СК
    const Vect: TDoubleVector3;
    var UnitVect: TDoubleVector3);
var Module: Double;
begin
    Module := GetVectModule(Vect);
    if Module <> 0 then begin
        UnitVect[1] := Vect[1] / Module;
        UnitVect[2] := Vect[2] / Module;
        UnitVect[3] := Vect[3] / Module;
    end else begin
        UnitVect[1] := 0;
        UnitVect[2] := 0;
        UnitVect[3] := 0;
    end;
end;

procedure GetUnitVectA(
    //  вычисляет единичный вектор для вектора, заданного в афинных координатах
    const Vect: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double; var UnitVect: TDoubleVector3);
var Module: Double;
begin
    Module := GetVectModuleA(Vect, A, B, C, Alpha, Beta, Gamma);
    if Module <> 0 then begin
        UnitVect[1] := Vect[1] / Module;
        UnitVect[2] := Vect[2] / Module;
        UnitVect[3] := Vect[3] / Module;
    end else begin
        UnitVect[1] := 0;
        UnitVect[2] := 0;
        UnitVect[3] := 0;
    end;
end;

function GetD(const Alpha, Beta, Gamma: Double): Double;
begin
    Result := 1 - Sqr(Cos(Alpha)) - Sqr(Cos(Beta)) - Sqr(Cos(Gamma))
        + 2 * Cos(Alpha) * Cos(Beta) * Cos(Gamma);
end;

function GetPAlpha(const Alpha, Beta, Gamma: Double): Double;
begin
    Result := Cos(Beta) * Cos(Gamma) - Cos(Alpha);
end;

function GetPBeta(const Alpha, Beta, Gamma: Double): Double;
begin
    Result := Cos(Alpha) * Cos(Gamma) - Cos(Beta);
end;

function GetPGamma(const Alpha, Beta, Gamma: Double): Double;
begin
    Result := Cos(Beta) * Cos(Alpha) - Cos(Gamma);
end;

function GetVolume(const A, B, C, Alpha, Beta, Gamma: Double): Double;
begin
    Result := A * B * C * Sqrt(GetD(Alpha, Beta, Gamma));
end;

procedure GetMutualVectors(
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vect1, Vect2, Vect3: TDoubleVector3);
var SqrtD: Double;
    PAlpha, PBeta, PGamma: Double;
    V: Double;
    Angle: Double;
    TempVect: TDoubleVector3;
begin
    SqrtD := Sqrt(GetD(Alpha, Beta, Gamma));
    PAlpha := GetPAlpha(Alpha, Beta, Gamma);
    PBeta := GetPBeta(Alpha, Beta, Gamma);
    PGamma := GetPGamma(Alpha, Beta, Gamma);
    V := GetVolume(A, B, C, Alpha, Beta, Gamma);

    Vect1[1] := C * B * Sqr(Sin(Alpha)) / (A * SqrtD);
    Vect1[2] := C * PGamma / SqrtD;
    Vect1[3] := B * PBeta / SqrtD;
    Vect1 := MulVectByValue(Vect1, 1 / V);
    TempVect[1] := 1; TempVect[2] := 0; TempVect[3] := 0;
    Angle := GetAngle(Vect1, TempVect, A, B, C, Alpha, Beta, Gamma);
    if Angle > pi / 2 then Vect1 := MulVectByValue(Vect1, -1);

    Vect2[1] := C * PGamma / SqrtD;
    Vect2[2] := A * C * Sqr(Sin(Beta)) / (B * SqrtD);
    Vect2[3] := A * PAlpha / SqrtD;
    Vect2 := MulVectByValue(Vect2, 1 / V);
    TempVect[1] := 0; TempVect[2] := 1; TempVect[3] := 0;
    Angle := GetAngle(Vect2, TempVect, A, B, C, Alpha, Beta, Gamma);
    if Angle > pi / 2 then Vect2 := MulVectByValue(Vect2, -1);

    Vect3[1] := B * PBeta / SqrtD;
    Vect3[2] := A * PAlpha / SqrtD;
    Vect3[3] := A * B * Sqr(Sin(Gamma)) / (C * SqrtD);
    Vect3 := MulVectByValue(Vect3, 1 / V);
    TempVect[1] := 0; TempVect[2] := 0; TempVect[3] := 1;
    Angle := GetAngle(Vect3, TempVect, A, B, C, Alpha, Beta, Gamma);
    if Angle > pi / 2 then Vect3 := MulVectByValue(Vect3, -1);
end;

procedure GetMutualVectorsInNewBasis(
    const A, B, C, Alpha, Beta, Gamma: Double;
        //  параметры исходного базиса, в котором заданы все вектора
    NewBasisVect1, NewBasisVect2, NewBasisVect3: TDoubleVector3;
        //  вектора нового базиса (определены в старом)
    var Vect1, Vect2, Vect3: TDoubleVector3
        //  взаимные вектора к векторам нового базиса (определены в старом)}
    );
var NewA, NewB, NewC, NewAlpha, NewBeta, NewGamma: Double;
        //  параметры нового базиса
    NewV: Double;
        //  объем параллелепипеда, построенного на векторах нового базиса
begin
    NewA := GetVectModuleA(NewBasisVect1, A, B, C, Alpha, Beta, Gamma);
    NewB := GetVectModuleA(NewBasisVect2, A, B, C, Alpha, Beta, Gamma);
    NewC := GetVectModuleA(NewBasisVect3, A, B, C, Alpha, Beta, Gamma);
    NewAlpha := GetAngle(NewBasisVect2, NewBasisVect3, A, B, C, Alpha, Beta, Gamma);
    NewBeta := GetAngle(NewBasisVect1, NewBasisVect3, A, B, C, Alpha, Beta, Gamma);
    NewGamma := GetAngle(NewBasisVect1, NewBasisVect2, A, B, C, Alpha, Beta, Gamma);
    NewV := GetVolume(NewA, NewB, NewC, NewAlpha, NewBeta, NewGamma);

    Vect1 := GetVectorMulA(NewBasisVect2, NewBasisVect3, A, B, C, Alpha, Beta, Gamma);
    Vect1 := MulVectByValue(Vect1, 1 / NewV);
    Vect2 := GetVectorMulA(NewBasisVect3, NewBasisVect1, A, B, C, Alpha, Beta, Gamma);
    Vect2 := MulVectByValue(Vect2, 1 / NewV);
    Vect3 := GetVectorMulA(NewBasisVect1, NewBasisVect2, A, B, C, Alpha, Beta, Gamma);
    Vect3 := MulVectByValue(Vect3, 1 / NewV);
end;

function GetVectInNewBasis(
    //  возвращает координаты вектора относительно нового базиса
    const A, B, C, Alpha, Beta, Gamma: Double;
        //  параметры исходного базиса, в котором заданы все вектора
    NewBasisVect1, NewBasisVect2, NewBasisVect3: TDoubleVector3;
        //  вектора нового базиса (определены в старом)
    InitialVect: TDoubleVector3
        //  вектор в "старом" базисе
    ): TDoubleVector3;
var MutVect1, MutVect2, MutVect3: TDoubleVector3;
begin
    GetMutualVectorsInNewBasis(A, B, C, Alpha, Beta, Gamma,
    NewBasisVect1, NewBasisVect2, NewBasisVect3,
    MutVect1, MutVect2, MutVect3);
    Result[1] := GetScalarMulA(InitialVect, MutVect1, A, B, C, Alpha, Beta, Gamma);
    Result[2] := GetScalarMulA(InitialVect, MutVect2, A, B, C, Alpha, Beta, Gamma);
    Result[3] := GetScalarMulA(InitialVect, MutVect3, A, B, C, Alpha, Beta, Gamma);
end;

function GetVectorMulA(
    const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): TDoubleVector3;
var V1, V2, V3: TDoubleVector3;
begin
    GetMutualVectors(A, B, C, Alpha, Beta, Gamma, V1, V2, V3);
    V1 := MulVectByValue(V1, Vect1[2] * Vect2[3] - Vect1[3] * Vect2[2]);
    V2 := MulVectByValue(V2, Vect1[3] * Vect2[1] - Vect1[1] * Vect2[3]);
    V3 := MulVectByValue(V3, Vect1[1] * Vect2[2] - Vect1[2] * Vect2[1]);
    Result[1] := V1[1] + V2[1] + V3[1];
    Result[2] := V1[2] + V2[2] + V3[2];
    Result[3] := V1[3] + V2[3] + V3[3];
    Result := MulVectByValue(Result, GetVolume(A, B, C, Alpha, Beta, Gamma));
end;

function T3DVector.GetSpace: ISpace;
begin
    if Assigned(FSpace) then Result := FSpace
    else raise E3DVector.Create('Space is not assigned...');
end;

procedure T3DVector.SetSpace(const ASpace: ISpace);
begin
    FSpace := ASpace;
end;

function T3DVector.GetNorma: Double;
begin
    Result := FNorma;
end;

procedure T3DVector.SetNorma(const ANorma: Double);
begin
  //    пересчет компонент вектора и нормированного вектора
  //    в соответствии с заданным значением
end;

function T3DVector.GetCompsNumber: LongInt;
begin
    Result := 3;
end;

function T3DVector.GetComp(index: LongInt): Double;
begin
    if (Index < 0) or (index > CompsNumber) then
        raise E3DVector.Create('Invalid index...')
    else Result := FVector[index + 1];
end;

procedure T3DVector.SetComp(index: LongInt; AComp: Double);
begin
    if (Index < 0) or (index > CompsNumber) then
        raise E3DVector.Create('Invalid index...')
    else FVector[index + 1] := AComp;
    //  алгоритм вычисления нормы вектора и нормированного вектора
end;

function T3DVector.GetNormComp(index: LongInt): Double;
begin
    if (Index < 0) or (index > CompsNumber) then
        raise E3DVector.Create('Invalid index...')
    else Result := FNormalizedVector[index + 1];
end;

function T3DComplexVector.GetImComp(index: LongInt): Double;
begin
end;

procedure T3DComplexVector.SetImComp(index: LongInt; AImComp: Double);
begin
end;

function T3DComplexVector.GetNormImComp(index: LongInt): Double;
begin
end;

function CalcPolinom2(const A, B, C, x0, x: Double): Double;
begin
    Result := A * Sqr(x0 - x) + B * (x0 - x) + C;
end;

initialization
end.
