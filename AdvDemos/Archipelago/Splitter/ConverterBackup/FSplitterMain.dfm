object Form1: TForm1
  Left = 192
  Top = 105
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 193
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 276
    Height = 32
    Caption = 
      'This utility will generate 16 1024x1024 BMP '#13#10'textures from the ' +
      #39'TextureMap.jpg'#39' files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LAAction: TLabel
    Left = 96
    Top = 144
    Width = 43
    Height = 13
    Caption = 'LAAction'
    Visible = False
  end
  object EDFile: TEdit
    Left = 16
    Top = 8
    Width = 265
    Height = 21
    Enabled = False
    TabOrder = 0
    Text = 'TextureMap.jpg'
    Visible = False
  end
  object Button1: TButton
    Left = 16
    Top = 160
    Width = 65
    Height = 25
    Caption = 'Split'
    TabOrder = 1
    OnClick = Button1Click
  end
  object EDTileSize: TEdit
    Left = 16
    Top = 24
    Width = 57
    Height = 21
    Enabled = False
    TabOrder = 2
    Text = '1024'
    Visible = False
  end
  object EDMask: TEdit
    Left = 16
    Top = 32
    Width = 265
    Height = 21
    Enabled = False
    TabOrder = 3
    Text = 'Tex_%d_%d.bmp'
    Visible = False
  end
  object ProgressBar: TProgressBar
    Left = 96
    Top = 160
    Width = 185
    Height = 25
    Min = 0
    Max = 16
    Smooth = True
    TabOrder = 4
  end
  object RBFull: TRadioButton
    Left = 48
    Top = 64
    Width = 225
    Height = 17
    Caption = 'Full Resolution (64 MB graphics memory)'
    Checked = True
    TabOrder = 5
    TabStop = True
  end
  object RBHalf: TRadioButton
    Left = 48
    Top = 88
    Width = 225
    Height = 17
    Caption = 'Medium Resolution (16 MB)'
    TabOrder = 6
  end
  object RBLow: TRadioButton
    Left = 48
    Top = 112
    Width = 225
    Height = 17
    Caption = 'Low Resolution (4 MB)'
    TabOrder = 7
  end
end
