program Examples;

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms,
    ExamplesMain,
    FitGrids;

{$R *.res}

begin
    RequireDerivedFormResource := True;
  Application.Title:='demo';
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
    Application.Run;
end.

