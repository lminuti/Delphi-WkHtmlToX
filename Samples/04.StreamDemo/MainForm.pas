unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, WinApi.ShellApi, System.IOUtils,
  WkHtmlToX.Core;

type
  TFormMain = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    LabelPhase: TLabel;
    MemoLog: TMemo;
    MemoHtml: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure Html2PDF(const InputHtml: string; var OutputPDF: TBytes);
    procedure Log(const AMessage: string);
    procedure ErrorHandler(Converter: IWkConverter; const AMessage: string);
    procedure FinishedHandler(Converter: IWkConverter; Success: Boolean);
    procedure PhaseChangeHandler(Converter: IWkConverter; Phase: Integer);
    procedure ProgressChangeHandler(Converter: IWkConverter; Progress: Integer);
    procedure WarningHandler(Converter: IWkConverter; const AMessage: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.ErrorHandler(Converter: IWkConverter; const AMessage: string);
begin
  Log('Error: ' + AMessage);
end;

procedure TFormMain.FinishedHandler(Converter: IWkConverter; Success: Boolean);
begin
  if Success then
    ShowMessage('Success')
  else
    ShowMessage('Failure');
end;

procedure TFormMain.PhaseChangeHandler(Converter: IWkConverter; Phase: Integer);
var
  PhaseDescription: string;
begin
  PhaseDescription := Converter.GetPhaseDescription(Phase);
  LabelPhase.Caption := PhaseDescription;
  Application.ProcessMessages;
end;

procedure TFormMain.ProgressChangeHandler(Converter: IWkConverter; Progress: Integer);
begin
  ProgressBar1.Position := Progress;
end;

procedure TFormMain.WarningHandler(Converter: IWkConverter; const AMessage: string);
begin
  Log('Warning: ' + AMessage);
end;

procedure TFormMain.Button1Click(Sender: TObject);
const
  OutputPath = 'test.pdf';
var
  OutputPDF: TBytes;
begin
  MemoLog.Clear;
  Html2PDF(MemoHtml.Text, OutputPDF);
  TFile.WriteAllBytes(OutputPath, OutputPDF);
  ShellExecute(Handle, 'open', PChar(OutputPath), '', '', SW_NORMAL);
end;

procedure TFormMain.Html2PDF(const InputHtml: string; var OutputPDF: TBytes);
var
  ObjectSettings: IWkObjectSettings;
  GlobalSettings: IWkGlobalSettings;
  Converter: IWkConverter;
begin
  Log('Version: ' + WkHtmlToPdf.Version);

  GlobalSettings := WkHtmlToPdf.CreateGlobalSettings;
  GlobalSettings['load.cookieJar'] := 'myjar.jar';
  GlobalSettings['web.defaultEncoding'] := 'utf-8';

  Log('out=' + GlobalSettings['out']);

  ObjectSettings := WkHtmlToPdf.CreateObjectSettings;
  ObjectSettings['page'] := '';
  Log('page=' + ObjectSettings['page']);

  Converter := WkHtmlToPdf.CreateConverter(GlobalSettings);
  Converter.OnProgressChanged := ProgressChangeHandler;
  Converter.OnPhaseChanged := PhaseChangeHandler;
  Converter.OnError := ErrorHandler;
  Converter.OnWarning := WarningHandler;
  Converter.OnFinished := FinishedHandler;

  Converter.AddObject(ObjectSettings, InputHtml);

  Converter.Convert;

  OutputPDF := Converter.GetOutput;
end;

procedure TFormMain.Log(const AMessage: string);
begin
  MemoLog.Lines.Add(AMessage);
end;

end.
