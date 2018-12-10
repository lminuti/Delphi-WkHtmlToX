program StreamDemo;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormMain},
  WkHtmlToX.Bindings in '..\..\Source\WkHtmlToX.Bindings.pas',
  WkHtmlToX.Core in '..\..\Source\WkHtmlToX.Core.pas',
  WkHtmlToX.Classes in '..\..\Source\WkHtmlToX.Classes.pas',
  WkHtmlToX.Consts in '..\..\Source\WkHtmlToX.Consts.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
