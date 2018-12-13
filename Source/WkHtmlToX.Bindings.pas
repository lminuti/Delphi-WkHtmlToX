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

// Translation of: https://github.com/wkhtmltopdf/wkhtmltopdf/blob/master/src/lib/pdf.h

unit WkHtmlToX.Bindings;

interface

const
  wkhtmltox_dll = 'wkhtmltox.dll';

type
  PPByte = ^PByte;

  wkhtmltopdf_global_settings = record
  end;
  pwkhtmltopdf_global_settings = ^wkhtmltopdf_global_settings;

  wkhtmltopdf_object_settings = record
  end;
  pwkhtmltopdf_object_settings = ^wkhtmltopdf_object_settings;

  wkhtmltopdf_converter = record
  end;
  pwkhtmltopdf_converter = ^wkhtmltopdf_converter;

  wkhtmltopdf_str_callback = procedure (converter: pwkhtmltopdf_converter; const str: PAnsiChar); cdecl;
  wkhtmltopdf_int_callback = procedure (converter: pwkhtmltopdf_converter; const val: Integer); cdecl;
  wkhtmltopdf_void_callback = procedure (converter: pwkhtmltopdf_converter); cdecl;

var
  wkhtmltopdf_init: function (use_graphics: LongBool): Integer; stdcall;
  wkhtmltopdf_deinit: function (): Integer; stdcall;
  wkhtmltopdf_extended_qt: function (): Integer; stdcall;
  wkhtmltopdf_version: function (): PAnsiChar; stdcall;

  wkhtmltopdf_create_global_settings: function (): pwkhtmltopdf_global_settings; stdcall;
  wkhtmltopdf_destroy_global_settings: procedure (settings: pwkhtmltopdf_global_settings); stdcall;

  wkhtmltopdf_create_object_settings: function (): pwkhtmltopdf_object_settings; stdcall;
  wkhtmltopdf_destroy_object_settings: procedure (settings: pwkhtmltopdf_object_settings); stdcall;

  wkhtmltopdf_set_global_setting: function (settings: pwkhtmltopdf_global_settings; const name, value: PAnsiChar): Integer; stdcall;
  wkhtmltopdf_get_global_setting: function (settings: pwkhtmltopdf_global_settings; const name, value: PAnsiChar; vs: Integer): Integer; stdcall;
  wkhtmltopdf_set_object_setting: function (settings: pwkhtmltopdf_object_settings; const name, value: PAnsiChar): Integer; stdcall;
  wkhtmltopdf_get_object_setting: function (settings: pwkhtmltopdf_object_settings; const name, value: PAnsiChar; vs: Integer): Integer; stdcall;

  wkhtmltopdf_create_converter: function (settings: pwkhtmltopdf_global_settings): pwkhtmltopdf_converter; stdcall;
  wkhtmltopdf_destroy_converter: procedure (converter: pwkhtmltopdf_converter); stdcall;

  wkhtmltopdf_set_warning_callback: procedure (converter: pwkhtmltopdf_converter; cb: wkhtmltopdf_str_callback); stdcall;
  wkhtmltopdf_set_error_callback: procedure (converter: pwkhtmltopdf_converter; cb: wkhtmltopdf_str_callback); stdcall;
  wkhtmltopdf_set_phase_changed_callback: procedure (converter: pwkhtmltopdf_converter; cb: wkhtmltopdf_void_callback); stdcall;
  wkhtmltopdf_set_progress_changed_callback: procedure (converter: pwkhtmltopdf_converter; cb: wkhtmltopdf_int_callback); stdcall;
  wkhtmltopdf_set_finished_callback: procedure (converter: pwkhtmltopdf_converter; cb: wkhtmltopdf_int_callback); stdcall;

{
/* CAPI(void) wkhtmltopdf_begin_conversion(wkhtmltopdf_converter * converter); */
/* CAPI(void) wkhtmltopdf_cancel(wkhtmltopdf_converter * converter); */
}

  wkhtmltopdf_convert: function (converter: pwkhtmltopdf_converter): Integer; stdcall;
  wkhtmltopdf_add_object: procedure (converter: pwkhtmltopdf_converter; setting: pwkhtmltopdf_object_settings; const data: PByte); stdcall;

  wkhtmltopdf_current_phase: function (converter: pwkhtmltopdf_converter): Integer; stdcall;
  wkhtmltopdf_phase_count: function (converter: pwkhtmltopdf_converter): Integer; stdcall;

  wkhtmltopdf_phase_description: function (converter: pwkhtmltopdf_converter; phase: Integer): PAnsiChar; stdcall;
  wkhtmltopdf_progress_string: function (converter: pwkhtmltopdf_converter): PAnsiChar; stdcall;

  wkhtmltopdf_http_error_code: function (converter: pwkhtmltopdf_converter): Integer; stdcall;

  wkhtmltopdf_get_output: function (converter: pwkhtmltopdf_converter; data: PPByte): LongInt; stdcall;

function LoadWkHtmlToX :Boolean;
procedure UnLoadWkHtmlToX;

implementation

uses
  Winapi.Windows;

var
  hWkHtmlToX :HMODULE;

procedure LoadFuncPointers;
begin
  wkhtmltopdf_init := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_init');
  wkhtmltopdf_deinit := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_deinit');
  wkhtmltopdf_extended_qt := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_extended_qt');
  wkhtmltopdf_version := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_version');

  wkhtmltopdf_create_global_settings := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_create_global_settings');
  wkhtmltopdf_destroy_global_settings := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_destroy_global_settings');

  wkhtmltopdf_create_object_settings := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_create_object_settings');
  wkhtmltopdf_destroy_object_settings := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_destroy_object_settings');

  wkhtmltopdf_set_global_setting := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_set_global_setting');
  wkhtmltopdf_get_global_setting := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_get_global_setting');
  wkhtmltopdf_set_object_setting := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_set_object_setting');
  wkhtmltopdf_get_object_setting := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_get_object_setting');

  wkhtmltopdf_create_converter := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_create_converter');
  wkhtmltopdf_destroy_converter := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_destroy_converter');

  wkhtmltopdf_set_warning_callback := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_set_warning_callback');
  wkhtmltopdf_set_error_callback := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_set_error_callback');
  wkhtmltopdf_set_phase_changed_callback := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_set_phase_changed_callback');
  wkhtmltopdf_set_progress_changed_callback := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_set_progress_changed_callback');
  wkhtmltopdf_set_finished_callback := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_set_finished_callback');

  wkhtmltopdf_convert := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_convert');
  wkhtmltopdf_add_object := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_add_object');

  wkhtmltopdf_current_phase := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_current_phase');
  wkhtmltopdf_phase_count := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_phase_count');

  wkhtmltopdf_phase_description := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_phase_description');
  wkhtmltopdf_progress_string := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_progress_string');

  wkhtmltopdf_http_error_code := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_http_error_code');
  wkhtmltopdf_get_output := GetProcAddress(hWkHtmlToX, 'wkhtmltopdf_get_output');
end;

procedure ResetFuncPointers;
begin
  wkhtmltopdf_init := nil;
  wkhtmltopdf_deinit := nil;
  wkhtmltopdf_extended_qt := nil;
  wkhtmltopdf_version := nil;

  wkhtmltopdf_create_global_settings := nil;
  wkhtmltopdf_destroy_global_settings := nil;

  wkhtmltopdf_create_object_settings := nil;
  wkhtmltopdf_destroy_object_settings := nil;

  wkhtmltopdf_set_global_setting := nil;
  wkhtmltopdf_get_global_setting := nil;
  wkhtmltopdf_set_object_setting := nil;
  wkhtmltopdf_get_object_setting := nil;

  wkhtmltopdf_create_converter := nil;
  wkhtmltopdf_destroy_converter := nil;

  wkhtmltopdf_set_warning_callback := nil;
  wkhtmltopdf_set_error_callback := nil;
  wkhtmltopdf_set_phase_changed_callback := nil;
  wkhtmltopdf_set_progress_changed_callback := nil;
  wkhtmltopdf_set_finished_callback := nil;

  wkhtmltopdf_convert := nil;
  wkhtmltopdf_add_object := nil;

  wkhtmltopdf_current_phase := nil;
  wkhtmltopdf_phase_count := nil;

  wkhtmltopdf_phase_description := nil;
  wkhtmltopdf_progress_string := nil;

  wkhtmltopdf_http_error_code := nil;
  wkhtmltopdf_get_output := nil;
end;

function LoadWkHtmlToX :Boolean;
begin
  if hWkHtmlToX <> 0 then
    Exit(True);

  hWkHtmlToX := LoadLibrary(wkhtmltox_dll);
  if hWkHtmlToX = 0 then
    Exit(False);

  LoadFuncPointers;
  Result := True;
end;

procedure UnLoadWkHtmlToX;
begin
  if hWkHtmlToX <> 0 then
  begin
    FreeLibrary(hWkHtmlToX);
    hWkHtmlToX := 0;
    ResetFuncPointers;
  end;
end;

initialization

  ResetFuncPointers;

finalization
  UnLoadWkHtmlToX;

end.
