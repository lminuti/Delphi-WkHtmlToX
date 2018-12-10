object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Wrapper Demo'
  ClientHeight = 314
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPhase: TLabel
    Left = 32
    Top = 82
    Width = 54
    Height = 13
    Caption = 'LabelPhase'
  end
  object Button1: TButton
    Left = 32
    Top = 51
    Width = 289
    Height = 25
    Caption = 'HTML2PDF'
    TabOrder = 0
    OnClick = Button1Click
  end
  object EditUrl: TEdit
    Left = 32
    Top = 24
    Width = 289
    Height = 21
    TabOrder = 1
    Text = 'https://wkhtmltopdf.org/'
  end
  object ProgressBar1: TProgressBar
    Left = 32
    Top = 104
    Width = 289
    Height = 17
    TabOrder = 2
  end
  object MemoLog: TMemo
    Left = 32
    Top = 127
    Width = 289
    Height = 154
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
end
