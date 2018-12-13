{******************************************************************************}
{                                                                              }
{  Delphi WkHtmlToX Library                                                    }
{  Copyright (c) 2018 Luca Minuti                                              }
{  https://github.com/lminuti/Delphi-WkHtmlToX                                 }
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
unit WkHtmlToX.Core;

interface

uses
  System.Classes, System.SysUtils;

type
  EWkHtmlToXError = class(Exception);

  EWkHtmlConvertError = class(EWkHtmlToXError);

  IWkConverter = interface;

  TProgressChangedEvent = procedure (Converter: IWkConverter; Progress: Integer) of object;
  TFinishedEvent = procedure (Converter: IWkConverter; Success: Boolean) of object;
  TWarningEvent = procedure (Converter: IWkConverter; const AMessage: string) of object;
  TErrorEvent = procedure (Converter: IWkConverter; const AMessage: string) of object;
  TPhaseChangedEvent = procedure (Converter: IWkConverter; Phase: Integer) of object;

  IWkGlobalSettings = interface
  ['{EA868CF3-9AF6-4419-93D5-3EC75B673283}']
    procedure SetValue(const Name, Value: string);
    function GetValue(const Name: string): string;
    property Value[const Name: string]: string read GetValue write SetValue; default;
  end;

  IWkObjectSettings = interface
  ['{11C9873F-A8A8-4C7F-B1FF-00A12B5506F9}']
    procedure SetValue(const Name, Value: string);
    function GetValue(const Name: string): string;
    property Value[const Name: string]: string read GetValue write SetValue; default;
  end;

  IWkConverter = interface
  ['{C36D6D34-5FCB-4907-8A0A-3421ACC5523F}']
    procedure Convert;
    procedure AddObject(ObjectSettings: IWkObjectSettings); overload;
    procedure AddObject(ObjectSettings: IWkObjectSettings; const Html: string); overload;
    procedure AddObject(ObjectSettings: IWkObjectSettings; const Html: TBytes); overload;
    procedure AddObject(ObjectSettings: IWkObjectSettings; HtmlStream: TStream); overload;
    function GetOutput: TBytes; overload;
    procedure GetOutput(AStream: TStream); overload;
    function GetCurrentPhase: Integer;
    function GetPhaseDescription(Phase: Integer): string;

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

    property CurrentPhase: Integer read GetCurrentPhase;
    property OnPhaseChanged: TPhaseChangedEvent read GetOnPhaseChanged write SetOnPhaseChanged;
    property OnProgressChanged: TProgressChangedEvent read GetOnProgressChanged write SetOnProgressChanged;
    property OnError: TErrorEvent read GetOnError write SetOnError;
    property OnWarning: TWarningEvent read GetOnWarning write SetOnWarning;
    property OnFinished: TFinishedEvent read GetOnFinished write SetOnFinished;
  end;

  IWkHtmlToPdf = interface
  ['{3448D79F-48FC-49EA-88BB-F920FC62F403}']
    procedure Init;
    procedure DeInit;
    function CreateGlobalSettings: IWkGlobalSettings;
    function CreateConverter(GlobalSettings: IWkGlobalSettings): IWkConverter;
    function CreateObjectSettings: IWkObjectSettings;
    function GetVersion: string;
    property Version: string read GetVersion;
  end;

function WkHtmlToPdf :IWkHtmlToPdf;

implementation

uses
  WkHtmlToX.Classes;

var
  WkHtmlToXInstance :IWkHtmlToPdf;

function WkHtmlToPdf :IWkHtmlToPdf;
begin
  if not Assigned(WkHtmlToXInstance) then
    WkHtmlToXInstance := TWkHtmlToXFactory.Create;
  Result := WkHtmlToXInstance;
end;

end.
