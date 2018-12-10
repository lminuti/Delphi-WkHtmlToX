object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Stream - Demo'
  ClientHeight = 437
  ClientWidth = 470
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    470
    437)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPhase: TLabel
    Left = 8
    Top = 202
    Width = 54
    Height = 13
    Caption = 'LabelPhase'
  end
  object Button1: TButton
    Left = 8
    Top = 221
    Width = 454
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'HTML2PDF'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 252
    Width = 454
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object MemoLog: TMemo
    Left = 8
    Top = 275
    Width = 454
    Height = 154
    Anchors = [akLeft, akTop, akRight]
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
  object MemoHtml: TMemo
    Left = 8
    Top = 8
    Width = 454
    Height = 188
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '<html>'
      '<head>'
      '  <meta charset="UTF-8">'
      '</head>'
      '<body>'
      '  <h1>wkhtmltopdf</h1>'
      '  <p>Na'#239've non ASCII HTML to test</p>'
      '</body>'
      '</html>')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
end
