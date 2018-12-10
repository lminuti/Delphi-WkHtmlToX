unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, WinApi.ShellApi,
  PDFConverter, System.Actions, Vcl.ActnList;

type
  TFormMain = class(TForm)
    Button1: TButton;
    MemoLog: TMemo;
    Button2: TButton;
    Button3: TButton;
    ActionList1: TActionList;
    actStart: TAction;
    actStop: TAction;
    actHtml2Pdf: TAction;
    MemoUrls: TMemo;
    Label1: TLabel;
    procedure actHtml2PdfExecute(Sender: TObject);
    procedure actHtml2PdfUpdate(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actStartUpdate(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actStopUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FConverter: TPDFConverter;
    FListIndex: Integer;
    procedure Log(const AMessage: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.actHtml2PdfExecute(Sender: TObject);
var
  OutputPath: string;
  Guid: TGUID;
  InputUrl: string;
begin
  if CreateGuid(Guid) <> S_OK then
    raise Exception.Create('GUID error');

  if FListIndex > MemoUrls.Lines.Count - 1 then
    FListIndex := 0;

  InputUrl := MemoUrls.Lines[FListIndex];
  Inc(FListIndex);

  OutputPath := GUIDToString(Guid) + '.pdf';

  Log(Format('start: %s "%s"', [DateTimeToStr(Now), InputUrl]));
  FConverter.AddJobs(InputUrl, OutputPath,
  procedure (Success: Boolean; AMessage: string)
  begin
    Log(Format('end: %s "%s": %s', [DateTimeToStr(Now), InputUrl, AMessage]));
    if Success then
      ShellExecute(Handle, 'open', PChar(OutputPath), '', '', SW_NORMAL);
  end);
end;

procedure TFormMain.actHtml2PdfUpdate(Sender: TObject);
begin
  actHtml2Pdf.Enabled := FConverter.Active;
end;

procedure TFormMain.actStartExecute(Sender: TObject);
begin
  FConverter.Start;
end;

procedure TFormMain.actStartUpdate(Sender: TObject);
begin
  actStart.Enabled := not FConverter.Active;
end;

procedure TFormMain.actStopExecute(Sender: TObject);
begin
  FConverter.Stop;
end;

procedure TFormMain.actStopUpdate(Sender: TObject);
begin
  actStop.Enabled := FConverter.Active;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FConverter.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FConverter := TPDFConverter.Create;
end;

procedure TFormMain.Log(const AMessage: string);
begin
  MemoLog.Lines.Add(AMessage);
end;

end.
