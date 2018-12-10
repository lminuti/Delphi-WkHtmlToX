unit PDFConverter;

interface

uses
  System.SysUtils, System.Classes, System.Types, Generics.Collections;

type
  TConvertCallback = TProc<Boolean,string>;

  TThreadParams = record
    InputFileName: string;
    OutputFileName: string;
    Callback: TConvertCallback;
    Thread: TThread;
  end;

  TPDFConverter = class(TObject)
  private
    type
      TConverterThread = class(TThread)
      private
        FParamQueue: TThreadedQueue<TThreadParams>;
      protected
        procedure Execute; override;
      public
        constructor Create(ParamQueue: TThreadedQueue<TThreadParams>);
      end;
    class var FInstance: TPDFConverter;
  private
    FConverterThread: TConverterThread;
    FParamQueue: TThreadedQueue<TThreadParams>;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  public
    procedure Start;
    procedure Stop;
    procedure AddJobs(const InputFileName, OutputFileName: string; Callback: TConvertCallback);
    property Active: Boolean read GetActive write SetActive;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  WkHtmlToX.Core;

const
  AQueueDepth = 10;
  PushTimeout = 10000;
  PopTimeout = 1000;

procedure Html2PDF(const Url, OutputPath: string);
var
  ObjectSettings: IWkObjectSettings;
  GlobalSettings: IWkGlobalSettings;
  Converter: IWkConverter;
begin
  GlobalSettings := WkHtmlToPdf.CreateGlobalSettings;
  GlobalSettings['out'] := OutputPath;
//  GlobalSettings['load.cookieJar'] := 'myjar.jar';

  ObjectSettings := WkHtmlToPdf.CreateObjectSettings;
  ObjectSettings['page'] := Url;

  Converter := WkHtmlToPdf.CreateConverter(GlobalSettings);
  {
  Converter.OnProgressChanged := ProgressChangeHandler;
  Converter.OnPhaseChanged := PhaseChangeHandler;
  Converter.OnError := ErrorHandler;
  Converter.OnWarning := WarningHandler;
  Converter.OnFinished := FinishedHandler;
  }

  Converter.AddObject(ObjectSettings);

  Converter.Convert;
end;

{ TPDFConverter }

procedure TPDFConverter.AddJobs(const InputFileName, OutputFileName: string;
  Callback: TConvertCallback);
var
  Params: TThreadParams;
begin
  Params.InputFileName := InputFileName;
  Params.OutputFileName := OutputFileName;
  Params.Callback := Callback;
  Params.Thread := TThread.CurrentThread;
  FParamQueue.PushItem(Params);
end;

constructor TPDFConverter.Create;
begin
  inherited;
  if Assigned(FInstance) then
    raise Exception.Create('No more than one instance!');
  FInstance := Self;
  FParamQueue := TThreadedQueue<TThreadParams>.Create(AQueueDepth, PushTimeout, PopTimeout);
end;

destructor TPDFConverter.Destroy;
begin
  Stop;
  FInstance := nil;
  FParamQueue.Free;
  inherited;
end;

function TPDFConverter.GetActive: Boolean;
begin
  Result := Assigned(FConverterThread);
end;

procedure TPDFConverter.SetActive(const Value: Boolean);
begin
  if Value then
    Start
  else
    Stop;
end;

procedure TPDFConverter.Start;
begin
  if not Assigned(FConverterThread) then
  begin
    FConverterThread := TConverterThread.Create(FParamQueue);
    FConverterThread.Start;
  end;
end;

procedure TPDFConverter.Stop;
begin
  if Assigned(FConverterThread) then
  begin
    FConverterThread.Terminate;
    FConverterThread.WaitFor;
    FreeAndNil(FConverterThread);
  end;
end;

{ TPDFConverter.TConverterThread }

constructor TPDFConverter.TConverterThread.Create(
  ParamQueue: TThreadedQueue<TThreadParams>);
begin
  inherited Create(True);
  FParamQueue := ParamQueue;
end;

procedure TPDFConverter.TConverterThread.Execute;
var
  Params: TThreadParams;
  Success: Boolean;
  AMessage: string;
begin
  inherited;
  // Init and DeInit must be called in the same thread
  WkHtmlToPdf.Init;
  while not Terminated do
  begin
    if FParamQueue.PopItem(Params) = wrSignaled then
    begin
      Success := True;
      AMessage := 'OK';
      try
        Html2PDF(Params.InputFileName, Params.OutputFileName);
      except
        on E: Exception do
        begin
          Success := False;
          AMessage := E.Message;
        end;
      end;
      TThread.Queue(Params.Thread,
      procedure
      begin
        Params.Callback(Success, AMessage);
      end);
    end;
    Sleep(2000);
  end;
  WkHtmlToPdf.DeInit;
end;

end.
