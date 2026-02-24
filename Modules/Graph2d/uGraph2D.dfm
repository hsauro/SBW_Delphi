object frmMain: TfrmMain
  Left = 290
  Top = 240
  Width = 490
  Height = 400
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Graphing Client'
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 260
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 482
    Height = 41
    Align = alTop
    TabOrder = 0
    object BtnHist: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Histogram'
      TabOrder = 0
      OnClick = BtnHistClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 313
    Width = 482
    Height = 41
    Align = alBottom
    TabOrder = 1
    object BtnDrawSquares: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Draw Squares'
      TabOrder = 0
      OnClick = BtnDrawSquaresClick
    end
    object BtnClearData: TButton
      Left = 158
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Clear Data'
      TabOrder = 1
      OnClick = BtnClearDataClick
    end
    object BtnLines: TButton
      Left = 83
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Draw Lines'
      TabOrder = 2
      OnClick = BtnLinesClick
    end
    object BtnClearImage: TButton
      Left = 233
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Clear Image'
      TabOrder = 3
      OnClick = BtnClearImageClick
    end
    object BtnAxes: TButton
      Left = 308
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Create gvp'
      TabOrder = 4
      OnClick = BtnAxesClick
    end
    object BtnDrawGraph: TButton
      Left = 384
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Draw Graph'
      TabOrder = 5
      OnClick = BtnDrawGraphClick
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 354
    Width = 482
    Height = 19
    Panels = <
      item
        Text = 'Connected'
        Width = 160
      end>
    SimplePanel = False
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 41
    Width = 73
    Height = 272
    Align = alLeft
    TabOrder = 3
  end
  object pnlRight: TPanel
    Left = 403
    Top = 41
    Width = 79
    Height = 272
    Align = alRight
    TabOrder = 4
  end
  object pnlCentre: TPanel
    Left = 73
    Top = 41
    Width = 330
    Height = 272
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 5
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 330
      Height = 272
      ActivePage = tsGraph
      Align = alClient
      TabOrder = 0
      object tsGraph: TTabSheet
        Caption = 'Graph'
        object xyGraph: TSBWXYGraph
          Left = 0
          Top = 0
          Width = 322
          Height = 244
          Align = alClient
        end
      end
    end
  end
  object sbw: TSBW
    ModuleName = 'Graph'
    DisplayName = 'Display Text'
    AppVisible = True
    AutoConnect = True
    ManagedType = mtUnique
    OnRegister = sbwRegister
    OnFailedToConnect = sbwFailedToConnect
    Left = 384
    Top = 8
  end
end
