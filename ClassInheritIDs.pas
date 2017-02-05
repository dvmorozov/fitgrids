//      двойной косой чертой комментируются замечания, сохраняемые во
//      всех версиях исходника; фигурными скобками комментируются замечания,
//      сохраняемые только в версии исходника для бесплатного распространения
{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit ClassInheritIDs;

{$IFDEF Lazarus}
{$MODE Delphi}
{$ENDIF}

//  модуль содержит определения констант - идентификаторов
//  в цепи наследования всех классов проекта (поскольку
//  константы задаются раз и навсегда); здесь же хранятся
//  константы версий параметров классов

interface

const
    SelfSavedInheritID: Byte = 0;   //  TSelfSavedComponent
    SelfSavedCurVerNum: Byte = 0;

        RSDClassInheritID: Byte = 1;    //  TReverseSourceData
        RSDCurVerNum: Byte = 0;

            RSDAlClassInheritID: Byte = 2;  //  TRSD
            RSDAlCurVerNum: Byte = 0;

        RCDClassInheritID: Byte = 1;    //  TReverseCalcData
        RCDCurVerNum: Byte = 0;

            RCDAlClassInheritID: Byte = 2;  //  TRCD
            RCDAlCurVerNum: Byte = 0;

        RUClassInheritID: Byte = 1;     //  TReverseUnit
        RUCurVerNum: Byte = 0;

            RUAlClassInheritID: Byte = 2;   //  TRU
            RUAlCurVerNum: Byte = 0;

        DSDClassInheritID: Byte = 1;    //  TDirectSourceData
        DSDCurVerNum: Byte = 0;

            DSDAlClassInheritID: Byte = 2;  //  TDSD
            DSDAlCurVerNum: Byte = 0;

        DCDClassInheritID: Byte = 1;    //  TDirectCalcData
        DCDCurVerNum: Byte = 0;

            DCDAlClassInheritID: Byte = 2;  //  TDCD
            DCDAlCurVerNum: Byte = 0;

        DUClassInheritID: Byte = 1;     //  TDirectUnit
        DUCurVerNum: Byte = 0;

            DUAlClassInheritID: Byte = 2;   //  TDU
            DUAlCurVerNum: Byte = 0;

        CCClassInheritID: Byte = 1;     //  TCommentsClass
        CCClassCurVerNum: Byte = 0;

            CCAlClassInheritID: Byte = 2;   //  TCC
            CCAlCurVerNum: Byte = 0;

        CBRCClassInheritID: Byte = 1;   //  TCBRCComponent
        CBRCCurVerNum: Byte = 0;

            CLClassInheritID: Byte = 2;     //  TComponentList
            CLCurVerNum: Byte = 0;

                SFClassInheritID: Byte = 3;     //  TSourceFile
                SFCurVerNum: Byte = 0;

                    SFAlClassInheritID: Byte = 4;   //  TSF
                    SFAlCurVerNum: Byte = 0;

                HKLLClassInheritID: Byte = 3;   //  THKLList
                HKLLCurVerNum: Byte = 0;

                SSClassInheritID: Byte = 3;     //  TScattSphere
                SSCurVerNum: Byte = 0;

                SCCLClassInheritID: Byte = 3;   //  TSelfCopiedCompList
                SCCLCurVerNum: Byte = 0;

                    SCCLAlClassInheritID: Byte = 4; //  TSCCL
                    SCCLAlCurVerNum: Byte = 0;

                    OSSLClassInheritID: Byte = 4;   //  TObjSavingStringList
                    OSSLCurVerNum: Byte = 0;

                        SLClassInheritID: Byte = 5;     //  TSiteList
                        SLCurVerNum: Byte = 0;

                            SLAlClassInheritID: Byte = 6;   //  TSL
                            SLAlCurVerNum: Byte = 0;

                        WVLClassInheritID: Byte = 5;    //  TWaveVectorList
                        WVLCurVerNum: Byte = 0;

                            WVLAlClassInheritID: Byte = 6;  //  TWVL
                            WVLAlCurVerNum: Byte = 0;

                        RLClassInheritID: Byte = 5;     //  TRepresentationList
                        RLCurVerNum: Byte = 0;

                            RLAlClassInheritID: Byte = 6;   //  TRL
                            RLAlCurVerNum: Byte = 0;

                    TCLClassInheritID: Byte = 4;    //  TTableCompList
                    TCLCurVerNum: Byte = 0;

                        RCLClassInheritID: Byte = 7;    //  TRowCompList
                        RCLCurVerNum: Byte = 0;

                            ALClassInheritID: Byte = 5;     //  TAtomList
                            ALCurVerNum: Byte = 0;

                                ALAlClassInheritID: Byte = 6;   //  TAL
                                ALAlCurVerNum: Byte = 0;

                            NCLClassInheritID: Byte = 5;    //  TSpecimenList
                            NCLCurVerNum: Byte = 0;

                                NCLAlClassInheritID: Byte = 6;  //  TNCL
                                NCLAlCurVerNum: Byte = 0;

                                NCLMClassInheritID: Byte = 6;   //  TNeutronCompListMono
                                NCLMCurVerNum: Byte = 0;

                                    NCLMAlClassInheritID: Byte = 7; //  TNCLM
                                    NCLMAlCurVerNum: Byte = 0;

                        STCLClassInheritID: Byte = 5;   //  TSinTCompList
                        STCLCurVerNum: Byte = 0;

                            STCLAlClassInheritID: Byte = 6; //  TSTCL
                            STCLAlCurVerNum: Byte = 0;

                        CCLClassInheritID: Byte = 7;    //  TColCompList
                        CCLCurVerNum: Byte = 0;

                            RClassInheritID: Byte = 5;      //  TRepresentation
                            RCurVerNum: Byte = 2;

                                RAlClassInheritID: Byte = 6;    //  TR
                                RAlCurVerNum: Byte = 0;

            DRPClassInheritID: Byte = 2;    //  TDownhillRealParameters
            DRPCurVerNum: Byte = 0;

                SClassInheritID: Byte = 3;      //  TSite
                SCurVerNum: Byte = 1;

                    SAlClassInheritID: Byte = 4;    //  TS
                    SAlCurVerNum: Byte = 0;

                WVClassInheritID: Byte = 3;     //  TWaveVector
                WVCurVerNum: Byte = 0;

                    WVAlClassInheritID: Byte = 4;   //  TWV
                    WVAlCurVerNum: Byte = 0;

            SCCClassInheritID: Byte = 2;    //  TSelfCopiedComponent
            SCCCurVerNum: Byte = 0;

                AClassInheritID: Byte = 3;      //  TAtom
                ACurVerNum: Byte = 0;

                    AAlClassInheritID: Byte = 4;    //  TA
                    AAlCurVerNum: Byte = 0;

                    NCAlClassInheritID: Byte = 4;   //  TNC
                    NCAlCurVerNum: Byte = 0;

                NCMClassInheritID: Byte = 3;    //  TNeutronClassMono
                NCMCurVerNum: Byte = 0;

                    NCMAlClassInheritID: Byte = 4;  //  TNCM
                    NCMAlCurVerNum: Byte = 0;

                PPCClassInheritID: Byte = 3;    //  TPatternParametersContainer
                PPCCurVerNum: Byte = 0;

                    PPCAlClassInheritID: Byte = 4;  //  TPPC
                    PPCAlCurVerNum: Byte = 0;

                HKLClassInheritID: Byte = 3;    //  THKLClass
                HKLCurVerNum: Byte = 0;

                    HKLAlClassInheritID: Byte = 4;  //  THKLC
                    HKLAlCurVerNum: Byte = 0;

                STClassInheritID: Byte = 3;     //  TSinTClass
                STCurVerNum: Byte = 0;

                    STAlClassInheritID: Byte = 4;   //  TSTC
                    STAlCurVerNum: Byte = 0;

                GClassInheritID: Byte = 3;      //  TGeneralClass
                GCurVerNum: Byte = 0;

                    GAlClassInheritID: Byte = 4;    //  TGC
                    GAlCurVerNum: Byte = 0;

                CRClassInheritID: Byte = 3;     //  TCalcResults
                CRCurVerNum: Byte = 0;

                    CRAlClassInheritID: Byte = 4;   //  TCR
                    CRAlCurVerNum: Byte = 0;

                BFClassInheritID: Byte = 3;     //  TBasisFunctions
                BFCurVerNum: Byte = 0;

                    BFAlClassInheritID: Byte = 4;   //  TBF
                    BFAlCurVerNum: Byte = 0;

                SPPClassInheritID: Byte = 3;    //  TSitePlotParams
                SPPCurVerNum: Byte = 0;

                    SPPAlClassInheritID: Byte = 4;  //  TSPP
                    SPPAlCurVerNum: Byte = 0;

                SLPPClassInheritID: Byte = 3;   //  TSiteListPlotParams
                SLPPCurVerNum: Byte = 0;

                    SLPPAlClassInheritID: Byte = 4; //  TSLPP
                    SLPPAlCurVerNum: Byte = 0;

            V3DCClassInheritID: Byte = 2;   //  T3DVector
            V3DCurVerNum: Byte = 0;

                CV3DCClassInheritID: Byte = 3;  //  T3DComplexVector
                CV3DCurVerNum: Byte = 0;

implementation
end.
