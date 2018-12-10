program ApiDemo;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormMain},
  WkHtmlToX.Bindings in '..\..\Source\WkHtmlToX.Bindings.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
