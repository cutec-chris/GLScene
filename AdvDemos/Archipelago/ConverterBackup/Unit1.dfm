object Form1: TForm1
  Left = 155
  Top = 126
  Align = alClient
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 400
  ClientWidth = 603
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 603
    Height = 400
    Camera = GLCamera
    BeforeRender = GLSceneViewerBeforeRender
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 500.000000000000000000
    Buffer.FogEnvironment.FogEnd = 1000.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clGray
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.FogEnable = True
    FieldOfView = 138.887908935546900000
    Align = alClient
    TabOrder = 0
  end
  object PAProgress: TPanel
    Left = 200
    Top = 168
    Width = 185
    Height = 49
    BorderWidth = 6
    TabOrder = 1
    Visible = False
    object Label1: TLabel
      Left = 7
      Top = 7
      Width = 171
      Height = 20
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Loading && compressing textures'
    end
    object ProgressBar: TProgressBar
      Left = 7
      Top = 27
      Width = 171
      Height = 15
      Align = alClient
      Max = 16
      TabOrder = 0
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 56
    Top = 16
    object SkyDome: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 25.000000000000000000
          Slices = 9
        end
        item
          StartAngle = 25.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Slices = 9
          Stacks = 4
        end>
      Stars = <>
      Options = [sdoTwinkle]
    end
    object DCCamera: TGLDummyCube
      Position.Coordinates = {0000000000000041000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 75.000000000000000000
        TargetObject = DCCamera
        Position.Coordinates = {000000000000803F000040400000803F}
        Left = 264
        Top = 160
      end
    end
    object TerrainRenderer: TGLTerrainRenderer
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {00002040000020400000003F00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      HeightDataSource = GLCustomHDS1
      TileSize = 32
      TilesPerTexture = 1.000000000000000000
      MaterialLibrary = MaterialLibrary
      CLODPrecision = 5
      OnHeightDataPostRender = TerrainRendererHeightDataPostRender
    end
    object DOWake: TGLDirectOpenGL
      OnProgress = DOWakeProgress
      UseBuildList = False
      OnRender = DOWakeRender
      Blend = False
    end
    object FFSailBoat: TGLFreeForm
      Scale.Coordinates = {9A99193E9A99193E9A99193E00000000}
      MaterialLibrary = MLSailBoat
    end
    object LSSun: TGLLightSource
      Ambient.Color = {9A99993E9A99993E9A99993E0000803F}
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
      LightStyle = lsParallel
      Specular.Color = {00000000000000000000000000000000}
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {0000803F0000803F0000003F00000000}
    end
    object HTFPS: TGLHUDText
      Position.Coordinates = {000096420000C841000000000000803F}
      BitmapFont = BFSmall
      ModulateColor.Color = {0000000000000000000000000000803F}
    end
    object HTHelp: TGLHUDText
      BitmapFont = BFLarge
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 56
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencerProgress
    Left = 16
    Top = 16
  end
  object MaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'detail'
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'detail.jpg'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureFormat = tfLuminance
        Material.Texture.Compression = tcStandard
        Material.Texture.Disabled = False
        Tag = 0
        TextureScale.Coordinates = {00008042000080420000804200000000}
      end
      item
        Name = 'water'
        Material.BlendingMode = bmTransparency
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = '035eau.jpg'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.MappingSCoordinates.Coordinates = {CDCC4C3D000000000000000000000000}
        Material.Texture.MappingTCoordinates.Coordinates = {00000000CDCC4C3D0000000000000000}
        Material.Texture.Disabled = False
        Tag = 0
        Texture2Name = 'waterenv'
      end
      item
        Name = 'wake'
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'wake.bmp'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Tag = 0
      end>
    Left = 56
    Top = 56
  end
  object GLHeightTileFileHDS1: TGLHeightTileFileHDS
    HTFFileName = 'Islands.htf'
    InfiniteWrap = False
    MaxPoolSize = 0
    Left = 160
    Top = 16
  end
  object BFSmall: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 120
    Top = 56
  end
  object GLCustomHDS1: TGLCustomHDS
    MaxPoolSize = 0
    OnStartPreparingData = GLCustomHDS1StartPreparingData
    OnMarkDirtyEvent = GLCustomHDS1MarkDirtyEvent
    Left = 120
    Top = 16
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Buffer.BackgroundColor = clBlack
    Buffer.Lighting = False
    Left = 312
    Top = 24
  end
  object MLSailBoat: TGLMaterialLibrary
    TexturePaths = 'sailboat'
    Left = 56
    Top = 96
  end
  object BFLarge: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = []
    Left = 120
    Top = 96
  end
end
