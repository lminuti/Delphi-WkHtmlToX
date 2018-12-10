{******************************************************************************}
{                                                                              }
{  Delphi WkHtmlToX Library                                                    }
{  Copyright (c) 2018 Luca Minuti                                              }
{  https://bitbucket.org/lminuti/delphiwkhtmltox                               }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit WkHtmlToX.Classes;

interface

uses
  System.Classes, System.SysUtils, Generics.Collections,
  WkHtmlToX.Core, WkHtmlToX.Bindings;

type
  TWkGlobalSettings = class(TInterfacedObject, IWkGlobalSettings)
  private
    gs: pwkhtmltopdf_global_Settings;
  public
    procedure SetValue(const Name, Value: string);
    function GetValue(const Name: string): string;
    constructor Create;
    destructor Destroy; override;
  end;

  TWkObjectSettings = class(TInterfacedObject, IWkObjectSettings)
  private
    os: pwkhtmltopdf_object_Settings;
  public
    procedure SetValue(const Name, Value: string);
    function GetValue(const Name: string): string;
    constructor Create;
    destructor Destroy; override;
  end;

  TWkConverter = class(TInterfacedObject, IWkConverter)
  private
    FGlobalSettings: TWkGlobalSettings;
    c: pwkhtmltopdf_converter;
    FOnProgressChanged: TProgressChangedEvent;
    FOnError: TErrorEvent;
    FOnWarning: TWarningEvent;
    FOnFinished: TFinishedEvent;
    FOnPhaseChanged: TPhaseChangedEvent;
    procedure FireProgressChangedEvent(Progress: Integer);
    procedure FireErrorEvent(const AMessage: string);
    procedure FireWarningEvent(const AMessage: string);
    procedure FireFinishedEvent(Success: Boolean);
    procedure FirePhaseChangedEvent(Phase: Integer);
    function GetCurrentPhase: Integer;
    function GetOnError: TErrorEvent;
    function GetOnFinished: TFinishedEvent;
    function GetOnPhaseChanged: TPhaseChangedEvent;
    function GetOnProgressChanged: TProgressChangedEvent;
    function GetOnWarning: TWarningEvent;
    procedure SetOnError(const Value: TErrorEvent);
    procedure SetOnFinished(const Value: TFinishedEvent);
    procedure SetOnPhaseChanged(const Value: TPhaseChangedEvent);
    procedure SetOnProgressChanged(const Value: TProgressChangedEvent);
    procedure SetOnWarning(const Value: TWarningEvent);
  public
    procedure Convert;
    procedure AddObject(ObjectSettings: IWkObjectSettings); overload;
    procedure AddObject(ObjectSettings: IWkObjectSettings; const Html: string); overload;
    procedure AddObject(ObjectSettings: IWkObjectSettings; const Html: TBytes); overload;
    procedure AddObject(ObjectSettings: IWkObjectSettings; HtmlStream: TStream); overload;
    function GetPhaseDescription(Phase: Integer): string;
    property CurrentPhase: Integer read GetCurrentPhase;
    function GetOutput: TBytes; overload;
    procedure GetOutput(AStream: TStream); overload;
    constructor Create(GlobalSettings: IWkGlobalSettings);
    destructor Destroy; override;
  end;

  TWkHtmlToXFactory = class(TInterfacedObject, IWkHtmlToPdf)
  private
    procedure LoadLibrary;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure DeInit;
    function CreateGlobalSettings: IWkGlobalSettings;
    function CreateConverter(GlobalSettings: IWkGlobalSettings): IWkConverter;
    function CreateObjectSettings: IWkObjectSettings;
    function GetVersion: string;
  end;

implementation

uses
  WkHtmlToX.Consts;

var
  ConverterList: TThreadList<TWkConverter>;
  Initialized: Boolean = False;
  InitThread: Cardinal;

function GetConverter(c: pwkhtmltopdf_converter): TWkConverter;
var
  Converter: TWkConverter;
begin
  try
    for Converter in ConverterList.LockList do
      if Converter.c = c then
        Exit(Converter);
  finally
    ConverterList.UnlockList;
  end;
  raise EWkHtmlToXError.Create(sConverterNotFound);
end;

{ Callbacks }

procedure progress_changed(c: pwkhtmltopdf_converter; const p: Integer); cdecl;
var
  Converter: TWkConverter;
begin
  Converter := GetConverter(c);
  Converter.FireProgressChangedEvent(p);
end;

procedure phase_changed(c: pwkhtmltopdf_converter); cdecl;
var
  Converter: TWkConverter;
  phase: Integer;
begin
  Converter := GetConverter(c);
  phase := wkhtmltopdf_current_phase(c);
  Converter.FirePhaseChangedEvent(phase);
end;

procedure finished(c: pwkhtmltopdf_converter; const val: Integer); cdecl;
var
  Converter: TWkConverter;
begin
  Converter := GetConverter(c);
  Converter.FireFinishedEvent(val = 1);
end;

procedure error(c: pwkhtmltopdf_converter; const msg: PAnsiChar); cdecl;
var
  Converter: TWkConverter;
  AMessage: string;
begin
  Converter := GetConverter(c);
  AMessage := string(AnsiString(msg));
  Converter.FireErrorEvent(AMessage);
end;

procedure warning(c: pwkhtmltopdf_converter; const msg: PAnsiChar); cdecl;
var
  Converter: TWkConverter;
  AMessage: string;
begin
  Converter := GetConverter(c);
  AMessage := string(AnsiString(msg));
  Converter.FireWarningEvent(AMessage);
end;

{ TWkGlobalSettings }

constructor TWkGlobalSettings.Create;
begin
  inherited;
	gs := wkhtmltopdf_create_global_Settings();
end;

destructor TWkGlobalSettings.Destroy;
begin
  // TODO: test if needed
  //wkhtmltopdf_destroy_global_Settingss(gs);
  inherited;
end;

function TWkGlobalSettings.GetValue(const Name: string): string;
const
  BuffSize = 1024;
var
  Buffer: AnsiString;
begin
  SetLength(Buffer, BuffSize);
  wkhtmltopdf_get_global_Setting(gs, PAnsiChar(AnsiString(Name)), PAnsiChar(Buffer), BuffSize);
  Result := string(PAnsiChar(Buffer));
end;

procedure TWkGlobalSettings.SetValue(const Name, Value: string);
begin
  wkhtmltopdf_set_global_Setting(gs, PAnsiChar(AnsiString(Name)), PAnsiChar(AnsiString(Value)));
end;

{ TWkObjectSettings }

constructor TWkObjectSettings.Create;
begin
  inherited;
	os := wkhtmltopdf_create_object_Settings();
end;

destructor TWkObjectSettings.Destroy;
begin
  // TODO: test if needed
  //wkhtmltopdf_destroy_object_Settingss(os);
  inherited;
end;

function TWkObjectSettings.GetValue(const Name: string): string;
const
  BuffSize = 1024;
var
  Buffer: AnsiString;
begin
  SetLength(Buffer, BuffSize);
  wkhtmltopdf_get_object_Setting(os, PAnsiChar(AnsiString(Name)), PAnsiChar(Buffer), BuffSize);
  Result := string(PAnsiChar(Buffer));
end;

procedure TWkObjectSettings.SetValue(const Name, Value: string);
begin
  wkhtmltopdf_set_object_Setting(os, PAnsiChar(AnsiString(Name)), PAnsiChar(AnsiString(Value)));
end;

{ TWkConverter }

procedure TWkConverter.AddObject(ObjectSettings: IWkObjectSettings;
  const Html: string);
var
  Buffer: TBytes;
begin
  Buffer := TEncoding.UTF8.GetBytes(Html);
  AddObject(ObjectSettings, Buffer);
end;

procedure TWkConverter.AddObject(ObjectSettings: IWkObjectSettings);
begin
  if not (ObjectSettings is TWkObjectSettings) then
    raise Exception.Create(sInvalidOSImpl);
  wkhtmltopdf_add_object(c, TWkObjectSettings(ObjectSettings).os, nil);
end;

procedure TWkConverter.AddObject(ObjectSettings: IWkObjectSettings;
  HtmlStream: TStream);
var
  Buffer: TBytes;
begin
  HtmlStream.Position := 0;
  SetLength(Buffer, HtmlStream.Size);
  HtmlStream.ReadBuffer(Buffer, HtmlStream.Size);
  AddObject(ObjectSettings, Buffer);
end;

procedure TWkConverter.AddObject(ObjectSettings: IWkObjectSettings;
  const Html: TBytes);
begin
  if not (ObjectSettings is TWkObjectSettings) then
    raise Exception.Create(sInvalidOSImpl);

  wkhtmltopdf_add_object(c, TWkObjectSettings(ObjectSettings).os, @Html[0]);
end;

procedure TWkConverter.Convert;
begin
	wkhtmltopdf_set_progress_changed_callback(c, progress_changed);
	wkhtmltopdf_set_warning_callback(c, warning);
	wkhtmltopdf_set_error_callback(c, error);
	wkhtmltopdf_set_finished_callback(c, finished);
	wkhtmltopdf_set_phase_changed_callback(c, phase_changed);

  if wkhtmltopdf_convert(c) = 0 then
		raise EConvertError.Create(sConversionFailed);
end;

constructor TWkConverter.Create(GlobalSettings: IWkGlobalSettings);
begin
  inherited Create;
  if not (GlobalSettings is TWkGlobalSettings) then
    raise Exception.Create(sInvalidGSImpl);
  FGlobalSettings := TWkGlobalSettings(GlobalSettings);

	c := wkhtmltopdf_create_converter(FGlobalSettings.gs);
  ConverterList.Add(Self);
end;

destructor TWkConverter.Destroy;
begin
  ConverterList.Remove(Self);
	wkhtmltopdf_destroy_converter(c);
  inherited;
end;

procedure TWkConverter.FireErrorEvent(const AMessage: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage);
end;

procedure TWkConverter.FireFinishedEvent(Success: Boolean);
begin
  if Assigned(FOnFinished) then
    FOnFinished(Self, Success);
end;

procedure TWkConverter.FirePhaseChangedEvent(Phase: Integer);
begin
  if Assigned(FOnPhaseChanged) then
    FOnPhaseChanged(Self, Phase);
end;

procedure TWkConverter.FireProgressChangedEvent(Progress: Integer);
begin
  if Assigned(FOnProgressChanged) then
    FOnProgressChanged(Self, Progress);
end;

procedure TWkConverter.FireWarningEvent(const AMessage: string);
begin
  if Assigned(FOnWarning) then
    FOnWarning(Self, AMessage);
end;

function TWkConverter.GetCurrentPhase: Integer;
begin
  Result := wkhtmltopdf_current_phase(c);
end;

function TWkConverter.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TWkConverter.GetOnFinished: TFinishedEvent;
begin
  Result := FOnFinished;
end;

function TWkConverter.GetOnPhaseChanged: TPhaseChangedEvent;
begin
  Result := FOnPhaseChanged;
end;

function TWkConverter.GetOnProgressChanged: TProgressChangedEvent;
begin
  Result := FOnProgressChanged;
end;

function TWkConverter.GetOnWarning: TWarningEvent;
begin
  Result := FOnWarning;
end;

procedure TWkConverter.GetOutput(AStream: TStream);
var
  OutputPDF: TBytes;
begin
  OutputPDF := GetOutput;
  AStream.Write(OutputPDF, Length(OutputPDF));
end;

function TWkConverter.GetOutput: TBytes;
var
  Len: LongInt;
begin
  Len := wkhtmltopdf_get_output(c, @Result);
  if Len <= 0 then
    raise EWkHtmlConvertError.Create(sConversionFailed);
  SetLength(Result, Len);
end;

function TWkConverter.GetPhaseDescription(Phase: Integer): string;
var
  msg: PAnsiChar;
begin
	msg := wkhtmltopdf_phase_description(c, phase);
  Result := string(AnsiString(msg));
end;

procedure TWkConverter.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;
end;

procedure TWkConverter.SetOnFinished(const Value: TFinishedEvent);
begin
  FOnFinished := Value;
end;

procedure TWkConverter.SetOnPhaseChanged(const Value: TPhaseChangedEvent);
begin
  FOnPhaseChanged := Value;
end;

procedure TWkConverter.SetOnProgressChanged(const Value: TProgressChangedEvent);
begin
  FOnProgressChanged := Value;
end;

procedure TWkConverter.SetOnWarning(const Value: TWarningEvent);
begin
  FOnWarning := Value;
end;

{ TWkHtmlToXFactory }

constructor TWkHtmlToXFactory.Create;
begin
  inherited;
  LoadLibrary;
  Init;
end;

function TWkHtmlToXFactory.CreateConverter(
  GlobalSettings: IWkGlobalSettings): IWkConverter;
begin
  Result := TWkConverter.Create(GlobalSettings);
end;

function TWkHtmlToXFactory.CreateGlobalSettings: IWkGlobalSettings;
begin
  Result := TWkGlobalSettings.Create;
end;

function TWkHtmlToXFactory.CreateObjectSettings: IWkObjectSettings;
begin
  Result := TWkObjectSettings.Create;
end;

procedure TWkHtmlToXFactory.DeInit;
begin
  if Initialized then
  begin
    if InitThread <> TThread.CurrentThread.ThreadID then
      raise EWkHtmlToXError.Create(sDeInitTHreadErrror);
    wkhtmltopdf_deinit;
    Initialized := False;
  end;
end;

destructor TWkHtmlToXFactory.Destroy;
begin
  DeInit;
  inherited;
end;

procedure TWkHtmlToXFactory.LoadLibrary;
begin
  if not LoadWkHtmlToX then
    raise EWkHtmlToXError.Create(sLoadLibraryError);
end;

function TWkHtmlToXFactory.GetVersion: string;
var
  Version: PAnsiChar;
begin
  Version := wkhtmltopdf_version;
  Result := string(Version);
end;

procedure TWkHtmlToXFactory.Init;
begin
  if not Initialized then
  begin
    wkhtmltopdf_init(False);
    Initialized := True;
    InitThread := TThread.CurrentThread.ThreadID;
  end;
end;

initialization

  ConverterList := TThreadList<TWkConverter>.Create;

finalization

  ConverterList.Free;

end.
