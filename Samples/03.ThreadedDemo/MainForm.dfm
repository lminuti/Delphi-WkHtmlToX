object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Threaded Demo'
  ClientHeight = 381
  ClientWidth = 307
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    307
    381)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 133
    Width = 188
    Height = 13
    Caption = 'You can press the button multiple times'
  end
  object Button1: TButton
    Left = 10
    Top = 153
    Width = 289
    Height = 25
    Action = actHtml2Pdf
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object MemoLog: TMemo
    Left = 8
    Top = 184
    Width = 289
    Height = 189
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Action = actStart
    TabOrder = 2
  end
  object Button3: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Action = actStop
    TabOrder = 3
  end
  object MemoUrls: TMemo
    Left = 8
    Top = 36
    Width = 289
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'https://wkhtmltopdf.org/'
      'https://www.wikipedia.org/'
      'https://github.com/lminuti')
    TabOrder = 4
  end
  object ActionList1: TActionList
    Left = 232
    Top = 184
    object actStart: TAction
      Caption = 'Start'
      OnExecute = actStartExecute
      OnUpdate = actStartUpdate
    end
    object actStop: TAction
      Caption = 'Stop'
      OnExecute = actStopExecute
      OnUpdate = actStopUpdate
    end
    object actHtml2Pdf: TAction
      Caption = 'HTML2PDF'
      OnExecute = actHtml2PdfExecute
      OnUpdate = actHtml2PdfUpdate
    end
  end
end
