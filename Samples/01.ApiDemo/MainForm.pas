unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, WinApi.ShellApi;

type
  TFormMain = class(TForm)
    Button1: TButton;
    EditUrl: TEdit;
    ProgressBar1: TProgressBar;
    LabelPhase: TLabel;
    MemoLog: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Html2PDF(const Url, OutputPath: string);
    procedure Log(const AMessage: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  WkHtmlToX.Bindings;

procedure TFormMain.Button1Click(Sender: TObject);
const
  OutputPath = 'test.pdf';
begin
  MemoLog.Clear;
  Html2PDF(EditUrl.Text, OutputPath);
  ShellExecute(Handle, 'open', PChar(OutputPath), '', '', SW_NORMAL);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  (* We will no longer be needing wkhtmltopdf funcionality *)
  wkhtmltopdf_deinit;
  UnLoadWkHtmlToX;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  if not LoadWkHtmlToX then
    raise Exception.Create('Library not found');

  (* Init wkhtmltopdf in graphics less mode (only once) *)
  wkhtmltopdf_init(false);
end;

// Print out loading progress information
procedure progress_changed(c: pwkhtmltopdf_converter; const p: Integer); cdecl;
begin
  FormMain.ProgressBar1.Position := p;
end;

// Print loading phase information
procedure phase_changed(c: pwkhtmltopdf_converter); cdecl;
var
  phase: Integer;
  msg: PAnsiChar;
begin
  phase := wkhtmltopdf_current_phase(c);
	msg := wkhtmltopdf_phase_description(c, phase);
  FormMain.LabelPhase.Caption := string(AnsiString(msg));
  Application.ProcessMessages;
end;

// Print a message to stderr when an error occurs
procedure error(c: pwkhtmltopdf_converter; const msg: PAnsiChar); cdecl;
begin
  FormMain.Log('Error:' + string(AnsiString(msg)));
//	fprintf(stderr, "Error: %s\n", msg);
end;

// Print a message to stderr when a warning is issued
procedure warning(c: pwkhtmltopdf_converter; const msg: PAnsiChar); cdecl;
begin
  FormMain.Log('Warning: ' + string(AnsiString(msg)));
// fprintf(stderr, "Warning: %s\n", msg);
end;

// https://github.com/wkhtmltopdf/wkhtmltopdf/blob/master/examples/pdf_c_api.c
procedure TFormMain.Html2PDF(const Url, OutputPath: string);
var
  gs: pwkhtmltopdf_global_settings;
	os: pwkhtmltopdf_object_settings;
	c: pwkhtmltopdf_converter;
begin
  (*
   * Init wkhtmltopdf in graphics less mode
   * You must call this function only one time
   * Look the "FormCreate" event handler
  *)
	// wkhtmltopdf_init(false);

	(*
	 * Create a global settings object used to store options that are not
	 * related to input objects, note that control of this object is parsed to
	 * the converter later, which is then responsible for freeing it
	 *)
	gs := wkhtmltopdf_create_global_settings();
	(* We want the result to be storred in the "OutputPath" *)
  wkhtmltopdf_set_global_setting(gs, 'out', PAnsiChar(AnsiString(OutputPath)));

	wkhtmltopdf_set_global_setting(gs, 'load.cookieJar', 'myjar.jar');
	(*
	 * Create a input object settings object that is used to store settings
	 * related to a input object, note again that control of this object is parsed to
	 * the converter later, which is then responsible for freeing it
	 *)
	os := wkhtmltopdf_create_object_settings();
	(* We want to convert to convert a url *)
	wkhtmltopdf_set_object_setting(os, 'page', PAnsiChar(AnsiString(Url)));

	(* Create the actual converter object used to convert the pages *)
	c := wkhtmltopdf_create_converter(gs);

	(* Call the progress_changed function when progress changes *)
	wkhtmltopdf_set_progress_changed_callback(c, progress_changed);

	(* Call the phase _changed function when the phase changes *)
	wkhtmltopdf_set_phase_changed_callback(c, phase_changed);

	(* Call the error function when an error occurs *)
	wkhtmltopdf_set_error_callback(c, error);

	(* Call the warning function when a warning is issued *)
	wkhtmltopdf_set_warning_callback(c, warning);

	(*
	 * Add the the settings object describing the qstring documentation page
	 * to the list of pages to convert. Objects are converted in the order in which
	 * they are added
	 *)
	wkhtmltopdf_add_object(c, os, nil);

	(* Perform the actual conversion *)
	if wkhtmltopdf_convert(c) = 0 then
		Log('Conversion failed!');

	(* Output possible http error code encountered *)
	Log(Format('httpErrorCode: %d\n', [wkhtmltopdf_http_error_code(c)]));

	(* Destroy the converter object since we are done with it *)
	wkhtmltopdf_destroy_converter(c);

  (*
	 * We will no longer be needing wkhtmltopdf funcionality *
   * Look the "FormDestroy" event handler
  *)
	//wkhtmltopdf_deinit();
end;

procedure TFormMain.Log(const AMessage: string);
begin
  MemoLog.Lines.Add(AMessage);
end;

end.
