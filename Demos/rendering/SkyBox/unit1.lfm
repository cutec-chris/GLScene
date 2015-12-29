object Form1: TForm1
  Left = 176
  Top = 73
  Width = 651
  Height = 565
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 643
    Height = 538
    Camera = GLCamera1
    Buffer.BackgroundColor = 7168
    Buffer.AmbientColor.Color = {0000803F0000803F0000803F0000803F}
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 24
    object GLSkyBox1: TGLSkyBox
      Direction.Coordinates = {9598A23144F7DFB20000803F00000000}
      Up.Coordinates = {1DB356B30000803FB3FA87B300000000}
      MaterialLibrary = GLMaterialLibrary1
      MatNameTop = 'Top'
      MatNameBottom = 'Bottom'
      MatNameLeft = 'Left'
      MatNameRight = 'Right'
      MatNameFront = 'Front'
      MatNameBack = 'Back'
      MatNameClouds = 'Clouds'
      CloudsPlaneOffset = 0.119999997317791
      CloudsPlaneSize = 2
      object GLSphere1: TGLSphere
        Direction.Coordinates = {18EBC2BE6823FE3E49BA47BF00000000}
        PitchAngle = 86.5
        Position.Coordinates = {000000C000008040000080BF0000803F}
        TurnAngle = 10.5
        Up.Coordinates = {CE00D4BE9A8D58BF0519ACBE00000000}
        Material.MaterialLibrary = GLMaterialLibrary1
        Radius = 1
        object GLSphere2: TGLSphere
          Direction.Coordinates = {000000000000803F0000000000000000}
          PitchAngle = -8
          Position.Coordinates = {0000000000000000000000400000803F}
          TurnAngle = 91.5
          Up.Coordinates = {0000000000000000000080BF00000000}
          Material.MaterialLibrary = GLMaterialLibrary1
          Radius = 0.300000011920929
        end
      end
    end
    object GLSkyBox2: TGLSkyBox
      Direction.Coordinates = {00000000000000800000803F00000000}
      MaterialLibrary = GLMaterialLibrary1
      MatNameClouds = 'Clouds'
      CloudsPlaneOffset = 0.100000001490116
      CloudsPlaneSize = 4
    end
    object Castle: TGLDummyCube
      CubeSize = 1
      object GLCube1: TGLCube
        Position.Coordinates = {0BD43940FFFF7FC018230B400000803F}
        Material.MaterialLibrary = GLMaterialLibrary1
        CubeSize = {0000803F000000420000803F}
        object GLCube2: TGLCube
          Position.Coordinates = {000040C0000000BFCDCCCC3D0000803F}
          Material.MaterialLibrary = GLMaterialLibrary1
          CubeSize = {0000C0400000803F0000803F}
        end
      end
      object GLCube11: TGLCube
        Direction.Coordinates = {000080BF000000001AE1F7B300000000}
        Position.Coordinates = {F42B46C0FFFF7FC018230B400000803F}
        TurnAngle = -90
        Material.MaterialLibrary = GLMaterialLibrary1
        CubeSize = {0000803F000000420000803F}
        object GLCube21: TGLCube
          Position.Coordinates = {000040C0000000BFCDCCCC3D0000803F}
          Material.MaterialLibrary = GLMaterialLibrary1
          CubeSize = {0000C0400000803F0000803F}
        end
      end
      object GLCube111: TGLCube
        Direction.Coordinates = {0000803F00000000B28FF03200000000}
        Position.Coordinates = {0CD43940FFFF7FC0E8DC74C00000803F}
        TurnAngle = 90
        Material.MaterialLibrary = GLMaterialLibrary1
        CubeSize = {0000803F000000420000803F}
        object GLCube211: TGLCube
          Position.Coordinates = {000040C0000000BFCDCCCC3D0000803F}
          Material.MaterialLibrary = GLMaterialLibrary1
          CubeSize = {0000C0400000803F0000803F}
        end
      end
      object GLCube112: TGLCube
        Direction.Coordinates = {647F2B3300000000000080BF00000000}
        Position.Coordinates = {F42B46C0FFFF7FC0E8DC74C00000803F}
        TurnAngle = -180
        Material.MaterialLibrary = GLMaterialLibrary1
        CubeSize = {0000803F000000420000803F}
        object GLCube212: TGLCube
          Position.Coordinates = {000040C0000000BFCDCCCC3D0000803F}
          Material.MaterialLibrary = GLMaterialLibrary1
          CubeSize = {0000C0400000803F0000803F}
        end
      end
    end
    object GLLightSource1: TGLLightSource
      Ambient.Color = {BEC0403FBEC0403FBEC0403F0000803F}
      ConstAttenuation = 1
      QuadraticAttenuation = 9.99999974737875E-5
      Position.Coordinates = {000088C100003042C0C6B5420000803F}
      LightStyle = lsOmni
      SpotCutOff = 180
      object GLLensFlare1: TGLLensFlare
        Seed = 1465
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 300
      FocalLength = 40
      NearPlaneBias = 0.200000002980232
      Position.Coordinates = {61EB8D3FD98541C02CCF94C00000803F}
      Direction.Coordinates = {E2CB26BF5D9A063F40FD0B3F00000000}
      Up.Coordinates = {A633CE3EC0C1593F8711ADBE00000000}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 64
    Top = 24
  end
  object GLNavigator1: TGLNavigator
    VirtualUp.Coordinates = {000000000000803F000000000000803F}
    MovingObject = GLCamera1
    UseVirtualUp = True
    AutoUpdateObject = True
    MaxAngle = 90
    MinAngle = -90
    AngleLock = False
    Left = 104
    Top = 24
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 64
  end
  object GLUserInterface1: TGLUserInterface
    InvertMouse = False
    MouseSpeed = 16
    GLNavigator = GLNavigator1
    Left = 144
    Top = 24
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 248
    Top = 24
  end
end
