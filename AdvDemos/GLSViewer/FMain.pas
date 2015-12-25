{: GLSViewer main form.<p>

   Requires RxLib to compile
   (go to http://sourceforge.net/projects/rxlib for Delphi6 version)
   and Mike Lischke's GraphicEx
   (http://www.delphi-gems.com/)
}
unit FMain;

interface

uses
  LCLIntf,LCLProc, LMessages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, Menus, ImgList, ToolWin, ComCtrls, GLMaterial,
  GLScene, GLLCLViewer, GLVectorFileObjects, GLObjects, GLVectorGeometry,
  GLTexture, OpenGL1x, GLContext, ExtDlgs, GLVectorLists, GLCadencer,
  ExtCtrls, GLCoordinates, GLCrossPlatform, GLBaseClasses, Types;

type

  { TMain }

  TMain = class(TForm)
    MainMenu: TMainMenu;
    ActionList: TActionList;
    ImageList: TImageList;
    ToolBar: TToolBar;
    MIFile: TMenuItem;
    MIAbout: TMenuItem;
    ACOpen: TAction;
    ACExit: TAction;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    ToolButton1: TToolButton;
    StatusBar: TStatusBar;
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    MIOptions: TMenuItem;
    MIAntiAlias: TMenuItem;
    MIAADefault: TMenuItem;
    MSAA2X: TMenuItem;
    MSAA4X: TMenuItem;
    ACSaveAs: TAction;
    ACZoomIn: TAction;
    ACZoomOut: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    MIView: TMenuItem;
    ZoomIn1: TMenuItem;
    ZoomOut1: TMenuItem;
    FreeForm: TGLFreeForm;
    OpenDialog: TOpenDialog;
    GLLightSource: TGLLightSource;
    GLMaterialLibrary: TGLMaterialLibrary;
    CubeExtents: TGLCube;
    ACResetView: TAction;
    Resetview1: TMenuItem;
    ToolButton5: TToolButton;
    ACShadeSmooth: TAction;
    ACFlatShading: TAction;
    ACWireframe: TAction;
    ACHiddenLines: TAction;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    N2: TMenuItem;
    Smoothshading1: TMenuItem;
    Flatshading1: TMenuItem;
    Hiddenlines1: TMenuItem;
    Wireframe1: TMenuItem;
    ToolButton10: TToolButton;
    ACCullFace: TAction;
    Faceculling1: TMenuItem;
    N3: TMenuItem;
    MIBgColor: TMenuItem;
    ColorDialog: TColorDialog;
    MITexturing: TMenuItem;
    ACTexturing: TAction;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    OpenPictureDialog: TOpenPictureDialog;
    MIPickTexture: TMenuItem;
    DCTarget: TGLDummyCube;
    GLCamera: TGLCamera;
    DCAxis: TGLDummyCube;
    ACFlatLined: TAction;
    ToolButton13: TToolButton;
    FlatShadingwithlines1: TMenuItem;
    ACInvertNormals: TAction;
    MIActions: TMenuItem;
    InvertNormals1: TMenuItem;
    N4: TMenuItem;
    Saveas1: TMenuItem;
    SaveDialog: TSaveDialog;
    ACReverseRenderingOrder: TAction;
    ReverseRenderingOrder1: TMenuItem;
    ACConvertToIndexedTriangles: TAction;
    ConverttoIndexedTriangles1: TMenuItem;
    ACFPS: TAction;
    FramesPerSecond1: TMenuItem;
    GLCadencer: TGLCadencer;
    Timer: TTimer;
    GLLightmapLibrary: TGLMaterialLibrary;
    ACSaveTextures: TAction;
    SDTextures: TSaveDialog;
    Savetextures1: TMenuItem;
    MIOpenTexLib: TMenuItem;
    ODTextures: TOpenDialog;
    Optimize1: TMenuItem;
    N5: TMenuItem;
    ACOptimize: TAction;
    Stripify1: TMenuItem;
    ACStripify: TAction;
    N6: TMenuItem;
    ACLighting: TAction;
    Lighting1: TMenuItem;
    TBLighting: TToolButton;
    MSAA8X: TMenuItem;
    MSAA16X: TMenuItem;
    CSAA8X: TMenuItem;
    CSAA16X: TMenuItem;
    procedure GLSceneViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MIAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ACOpenExecute(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ACZoomInExecute(Sender: TObject);
    procedure ACZoomOutExecute(Sender: TObject);
    procedure ACExitExecute(Sender: TObject);
    procedure ACShadeSmoothExecute(Sender: TObject);
    procedure GLSceneViewerBeforeRender(Sender: TObject);
    procedure MIAADefaultClick(Sender: TObject);
    procedure GLSceneViewerAfterRender(Sender: TObject);
    procedure ACResetViewExecute(Sender: TObject);
    procedure ACCullFaceExecute(Sender: TObject);
    procedure MIBgColorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GLMaterialLibraryTextureNeeded(Sender: TObject;
      var textureFileName: String);
    procedure ACTexturingExecute(Sender: TObject);
    procedure MIPickTextureClick(Sender: TObject);
    procedure MIFileClick(Sender: TObject);
    procedure ACInvertNormalsExecute(Sender: TObject);
    procedure ACSaveAsExecute(Sender: TObject);
    procedure ACSaveAsUpdate(Sender: TObject);
    procedure ACReverseRenderingOrderExecute(Sender: TObject);
    procedure ACConvertToIndexedTrianglesExecute(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure ACFPSExecute(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ACSaveTexturesExecute(Sender: TObject);
    procedure MIOpenTexLibClick(Sender: TObject);
    procedure ACOptimizeExecute(Sender: TObject);
    procedure ACStripifyExecute(Sender: TObject);
    procedure ACLightingExecute(Sender: TObject);
  private
    { Private declarations }
    procedure DoResetCamera;
    procedure SetupFreeFormShading;
    procedure ApplyShadeModeToMaterial(aMaterial : TGLMaterial);
    procedure ApplyShadeMode;
    procedure ApplyFSAA;
    procedure ApplyFaceCull;
    procedure ApplyBgColor;
    procedure ApplyTexturing;
    procedure ApplyFPS;

    procedure DoOpen(const fileName : String);

  public
    { Public declarations }
    md, nthShow : Boolean;
    mx, my : Integer;
    hlShader : TGLShader;
    lastFileName : String;
    lastLoadWithTextures : Boolean;
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

uses GLColor, GLKeyBoard, GLGraphics, Registry, GLPersistentClasses, GLMeshUtils,
   GLFileOBJ, GLFileSTL, GLFileLWO,  GLFileMS3D,
   GLFileNMF, GLFileMD3, GLFile3DS, GLFileMD2, GLFileSMD, GLFileTIN,
   GLFilePLY, GLFileGTS, GLFileVRML, GLFileMD5, GLMeshOptimizer, GLState,
   GLRenderContextInfo, GLTextureFormat,GL,math;

type

   // Hidden line shader (specific implem for the viewer, *not* generic)
   THiddenLineShader = class (TGLShader)
      private
         LinesColor : TColorVector;
         BackgroundColor : TColorVector;
         PassCount : Integer;
      public
         procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
   end;

procedure THiddenLineShader.DoApply(var rci : TRenderContextInfo; Sender : TObject);
begin
   PassCount:=1;
   with rci.GLStates do
   begin
     PolygonMode := pmFill;
     //GL.Color3fv(@BackgroundColor);
     //ActiveTextureEnabled[ttTexture2D] := False;
     Enable(stPolygonOffsetFill);
     PolygonOffsetFactor := 1;
     PolygonOffsetUnits := 2;
   end;
end;

function THiddenLineShader.DoUnApply(var rci : TRenderContextInfo) : Boolean;
begin
   case PassCount of
      1 : with rci.GLStates do
       begin
         PassCount:=2;
         PolygonMode := pmLines;
         //GL.Color3fv(@LinesColor);
         Disable(stLighting);
         Result:=True;
      end;
      2 : begin
         rci.GLStates.Disable(stPolygonOffsetFill);
         Result:=False;
      end;
   else
      // doesn't hurt to be cautious
      Assert(False);
      Result:=False;
   end;
end;

procedure TMain.FormCreate(Sender: TObject);
var
   reg : TRegistry;
   shellCmd : String;
   keyOkay : Boolean;
const
   cKeyName : String = 'Applications\GLSViewer.exe\shell\open\command';
   cFriendlyKeyName : String = 'Applications\GLSViewer.exe';
begin
   // instantiate our specific hidden-lines shader
   hlShader:=THiddenLineShader.Create(Self);

   FreeForm.IgnoreMissingTextures:=True;

   // register as an application that handles arbitrary file classes
   try
      reg:=TRegistry.Create;
      try
         shellCmd:='"'+Application.ExeName+'" "%1"';
         reg.RootKey:=HKEY_CLASSES_ROOT;
         keyOkay:=False;
         if reg.OpenKeyReadOnly(cKeyName) then
            keyOkay:=(reg.ReadString('')=shellCmd);
         if not keyOkay then begin
            reg.CloseKey;
            if reg.OpenKey(cKeyName, True) then
               reg.WriteString('', shellCmd);
            reg.CloseKey;
            if reg.OpenKey(cFriendlyKeyName, True) then
               reg.WriteString('FriendlyAppName', 'GLSViewer, OpenGL 3D Files Viewer');
         end;
      finally
         reg.Free;
      end;
   except
      // ignore all registry issues (not critical)
   end;
end;

procedure TMain.FormShow(Sender: TObject);
var
   i : Integer;
begin
   if not nthShow then begin

      OpenDialog.Filter:=VectorFileFormatsFilter;
      SaveDialog.Filter:=VectorFileFormatsSaveFilter;
      with ActionList do for i:=0 to ActionCount-1 do
         if Actions[i] is TCustomAction then
            with TCustomAction(Actions[i]) do Hint:=Caption;
      ApplyFSAA;
      ApplyFaceCull;
      ApplyBgColor;
      ApplyFPS;

      if ParamCount>0 then
         DoOpen(ParamStr(1));

      nthShow:=True;
   end;
end;

procedure TMain.GLSceneViewerBeforeRender(Sender: TObject);
begin
   THiddenLineShader(hlShader).LinesColor:=VectorMake(107/256, 123/256, 173/256, 1);
   THiddenLineShader(hlShader).BackgroundColor:=ConvertWinColor(GLSceneViewer.Buffer.BackgroundColor);
   {
   if not GL.ARB_multisample then begin
      MIAADefault.Checked:=True;
      MSAA2x.Enabled:=False;
      MSAA4X.Enabled:=False;
      MSAA8X.Enabled:=False;
      MSAA16X.Enabled:=False;
      CSAA8X.Enabled:=False;
      CSAA16X.Enabled:=False;
   end;
   }
end;

procedure TMain.GLSceneViewerAfterRender(Sender: TObject);
begin
   ApplyFSAA;
   Screen.Cursor:=crDefault;
end;

procedure TMain.MIAboutClick(Sender: TObject);
begin
   ShowMessage( 'GLSViewer - Simple OpenGL Mesh Viewer'#13#10
               +'Copyright 2002 Eric Grange'#13#10#13#10
               +'A freeware Delphi program based on...'#13#10#13#10
               +'GLScene: 3D view, 3D file formats support'#13#10
               +'http://glscene.org'#13#10#13#10
               +'GraphicEx: 2D image file formats support'#13#10
               +'http://www.delphi-gems.com/')
end;

procedure TMain.GLSceneViewerMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FormMouseWheel(Sender,Shift,WheelDelta,MousePos,Handled);
end;

procedure TMain.DoResetCamera;
var
   objSize : Single;
begin
   DCTarget.Position.AsVector:=NullHmgPoint;
   GLCamera.Position.SetPoint(7, 3, 5);
   FreeForm.Position.AsVector:=NullHmgPoint;
   FreeForm.Up.Assign(DCAxis.Up);
   FreeForm.Direction.Assign(DCAxis.Direction);

   objSize:=FreeForm.BoundingSphereRadius;
   if objSize>0 then begin
      if objSize<1 then begin
         GLCamera.SceneScale:=1/objSize;
         objSize:=1;
      end else GLCamera.SceneScale:=1;
      GLCamera.AdjustDistanceToTarget(objSize*0.27);
      GLCamera.DepthOfView:=1.5*GLCamera.DistanceToTarget+2*objSize;
   end;
end;

procedure TMain.ApplyShadeModeToMaterial(aMaterial : TGLMaterial);
begin
   with aMaterial do begin
      if ACShadeSmooth.Checked then begin
         GLSceneViewer.Buffer.Lighting:=True;
         GLSceneViewer.Buffer.ShadeModel:=smSmooth;
         aMaterial.PolygonMode:=pmFill;
      end else if ACFlatShading.Checked then begin
         GLSceneViewer.Buffer.Lighting:=True;
         GLSceneViewer.Buffer.ShadeModel:=smFlat;
         aMaterial.PolygonMode:=pmFill;
      end else if ACFlatLined.Checked then begin
         GLSceneViewer.Buffer.Lighting:=True;
         GLSceneViewer.Buffer.ShadeModel:=smFlat;
         aMaterial.PolygonMode:=pmLines;
      end else if ACHiddenLines.Checked then begin
         GLSceneViewer.Buffer.Lighting:=False;
         GLSceneViewer.Buffer.ShadeModel:=smSmooth;
         aMaterial.PolygonMode:=pmLines;
      end else if ACWireframe.Checked then begin
         GLSceneViewer.Buffer.Lighting:=False;
         GLSceneViewer.Buffer.ShadeModel:=smSmooth;
         aMaterial.PolygonMode:=pmLines;
      end;
   end;
end;

procedure TMain.ApplyShadeMode;
var
   i : Integer;
begin
   with GLMaterialLibrary.Materials do for i:=0 to Count-1 do begin
      ApplyShadeModeToMaterial(Items[i].Material);
      if (ACHiddenLines.Checked) or (ACFlatLined.Checked) then
         Items[i].Shader:=hlShader
      else Items[i].Shader:=nil;
   end;
   GLSceneViewer.Buffer.Lighting:=ACLighting.Checked;
   FreeForm.StructureChanged;
end;

procedure TMain.ApplyFSAA;
begin
   with GLSceneViewer.Buffer do begin
      if MIAADefault.Checked then
         AntiAliasing:=aaDefault
      else if MSAA2X.Checked then
         AntiAliasing:=aa2x
      else if MSAA4X.Checked then
         AntiAliasing:=aa4x
      else if MSAA8X.Checked then
         AntiAliasing:=aa8x
      else if MSAA16X.Checked then
         AntiAliasing:=aa16x
      else if CSAA8X.Checked then
         AntiAliasing:=csa8x
      else if CSAA16X.Checked then
         AntiAliasing:=csa16x;
   end;
end;

procedure TMain.ApplyFaceCull;
begin
   with GLSceneViewer.Buffer do begin
      if ACCullFace.Checked then begin
         FaceCulling:=True;
         ContextOptions:=ContextOptions-[roTwoSideLighting];
      end else begin
         FaceCulling:=False;
         ContextOptions:=ContextOptions+[roTwoSideLighting];
      end;
   end;
end;

procedure TMain.ApplyBgColor;
var
   bmp : TBitmap;
   col : TColor;
begin
   bmp:=TBitmap.Create;
   try
      bmp.Width:=16;
      bmp.Height:=16;
      col:=ColorToRGB(ColorDialog.Color);
      GLSceneViewer.Buffer.BackgroundColor:=col;
      with bmp.Canvas do begin
         Pen.Color:=col xor $FFFFFF;
         Brush.Color:=col;
         Rectangle(0, 0, 16, 16);
      end;
      MIBgColor.Bitmap:=bmp;
   finally
      bmp.Free;
   end;
end;

procedure TMain.ApplyTexturing;
var
   i : Integer;
begin
   with GLMaterialLibrary.Materials do for i:=0 to Count-1 do begin
      with Items[i].Material.Texture do begin
         if Enabled then
            Items[i].Tag:=Integer(True);
         Enabled:=Boolean(Items[i].Tag) and ACTexturing.Checked;
      end;
   end;
   FreeForm.StructureChanged;
end;

procedure TMain.ApplyFPS;
begin
   if ACFPS.Checked then begin
      Timer.Enabled:=True;
      GLCadencer.Enabled:=True;
   end else begin
      Timer.Enabled:=False;
      GLCadencer.Enabled:=False;
      StatusBar.Panels[1].Text:='--- FPS';
   end;
end;

procedure TMain.SetupFreeFormShading;
var
   i : Integer;
   libMat : TGLLibMaterial;
begin
   with GLMaterialLibrary do begin
      if Materials.Count=0 then begin
         FreeForm.Material.MaterialLibrary:=GLMaterialLibrary;
         libMat:=Materials.Add;
         FreeForm.Material.LibMaterialName:=libMat.Name;
         libMat.Material.FrontProperties.Diffuse.Red:=0;
      end;
      for i:=0 to Materials.Count-1 do
         with Materials[i].Material do BackProperties.Assign(FrontProperties);
   end;
   ApplyShadeMode;
   ApplyTexturing;
   ApplyFPS;
end;

procedure TMain.DoOpen(const fileName : String);
var
   min, max : TAffineVector;
   aCur: String;
begin
   if not FileExists(fileName) then Exit;

   Screen.Cursor:=crHourGlass;

   Caption:='GLSViewer - '+ExtractFileName(fileName);

   FreeForm.MeshObjects.Clear;
   GLMaterialLibrary.Materials.Clear;

   aCur := GetCurrentDir;
   SetCurrentDir(ExtractFileDir(fileName));

   FreeForm.LoadFromFile(fileName);

   SetCurrentDir(aCur);

   SetupFreeFormShading;

   StatusBar.Panels[0].Text:=IntToStr(FreeForm.MeshObjects.TriangleCount)+' tris';
   StatusBar.Panels[2].Text:=fileName;
   ACSaveTextures.Enabled:=(GLMaterialLibrary.Materials.Count>0);
   MIOpenTexLib.Enabled:=(GLMaterialLibrary.Materials.Count>0);
   lastFileName:=fileName;
   lastLoadWithTextures:=ACTexturing.Enabled;

   FreeForm.GetExtents(min, max);
   if min.X<>Infinity then
      begin
       with CubeExtents do begin
          CubeWidth:=max.X-min.X;
          CubeHeight:=max.Y-min.Y;
          CubeDepth:=max.Z-min.Z;
          Position.AsAffineVector:=VectorLerp(min, max, 0.5);
          end;
      end;

   DoResetCamera;
end;

procedure TMain.ACOpenExecute(Sender: TObject);
begin
   if OpenDialog.Execute then
      DoOpen(OpenDialog.FileName);
end;

procedure TMain.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
   md:=True;
end;

procedure TMain.GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
   d : Single;
begin
   if md and (Shift<>[]) then begin
      if ssLeft in Shift then
         if ssShift in Shift then
            GLCamera.MoveAroundTarget((my-y)*0.1, (mx-x)*0.1)
         else GLCamera.MoveAroundTarget(my-y, mx-x)
      else if ssRight in Shift then begin
         d:=GLCamera.DistanceToTarget*0.01*(x-mx+y-my);
         if IsKeyDown('x') then
            FreeForm.Translate(d, 0, 0)
         else if IsKeyDown('y') then
            FreeForm.Translate(0, d, 0)
         else if IsKeyDown('z') then
            FreeForm.Translate(0, 0, d)
         else begin
            if ssShift in Shift then
               GLCamera.RotateObject(FreeForm, (my-y)*0.1, (mx-x)*0.1)
            else GLCamera.RotateObject(FreeForm, my-y, mx-x);
         end;
      end;
      mx:=x; my:=y;
   end;
end;

procedure TMain.GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   md:=False;
end;

procedure TMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   if FreeForm.MeshObjects.Count>0 then begin
      GLCamera.AdjustDistanceToTarget(Power(1.05, -WheelDelta/120));
      GLCamera.DepthOfView:=2*GLCamera.DistanceToTarget+2*FreeForm.BoundingSphereRadius;
   end;
   Handled:=True;
end;

procedure TMain.ACZoomInExecute(Sender: TObject);
var
   h : Boolean;
begin
   FormMouseWheel(Self, [], 120*4, Point(0, 0), h);
end;

procedure TMain.ACZoomOutExecute(Sender: TObject);
var
   h : Boolean;
begin
   FormMouseWheel(Self, [], -120*4, Point(0, 0), h);
end;

procedure TMain.ACExitExecute(Sender: TObject);
begin
   Close;
end;

procedure TMain.ACShadeSmoothExecute(Sender: TObject);
begin
   ApplyShadeMode;
end;

procedure TMain.MIAADefaultClick(Sender: TObject);
begin
   (Sender as TMenuItem).Checked:=True;
   ApplyFSAA;
end;

procedure TMain.ACResetViewExecute(Sender: TObject);
begin
   DoResetCamera;
end;

procedure TMain.ACCullFaceExecute(Sender: TObject);
begin
   ACCullFace.Checked:=not ACCullFace.Checked;
   ApplyFaceCull;
end;

procedure TMain.MIBgColorClick(Sender: TObject);
begin
   if ColorDialog.Execute then
      ApplyBgColor;
end;

procedure TMain.GLMaterialLibraryTextureNeeded(Sender: TObject;
  var textureFileName: String);
begin
   if not ACTexturing.Enabled then
      textureFileName:='';
end;

procedure TMain.ACTexturingExecute(Sender: TObject);
begin
   ACTexturing.Checked:=not ACTexturing.Checked;
   if ACTexturing.Checked then
      if lastLoadWithTextures then
         ApplyTexturing
      else begin
         DoOpen(lastFileName);
      end
   else ApplyTexturing;
end;

procedure TMain.MIFileClick(Sender: TObject);
begin
   MIPickTexture.Enabled:=(GLMaterialLibrary.Materials.Count>0);
end;

procedure TMain.MIPickTextureClick(Sender: TObject);
begin
   if OpenPictureDialog.Execute then begin
      with GLMaterialLibrary.Materials do begin
         with Items[Count-1] do begin
            Tag:=1;
            Material.Texture.Image.LoadFromFile(OpenPictureDialog.FileName);
            Material.Texture.Enabled:=True;
         end;
      end;
      ApplyTexturing;
   end;
end;

procedure TMain.MIOpenTexLibClick(Sender: TObject);
var
   i : Integer;
begin
   if ODTextures.Execute then with GLMaterialLibrary do begin
      LoadFromFile(ODTextures.FileName);
      for i:=0 to Materials.Count-1 do
         with Materials[i].Material do BackProperties.Assign(FrontProperties);
      ApplyShadeMode;
      ApplyTexturing;
   end;
end;

procedure TMain.ACInvertNormalsExecute(Sender: TObject);
var
   i : Integer;
begin
   with FreeForm.MeshObjects do
      for i:=0 to Count-1 do
         Items[i].Normals.Scale(-1);
   FreeForm.StructureChanged;
end;

procedure TMain.ACReverseRenderingOrderExecute(Sender: TObject);
var
   i, j, n : Integer;
   fg : TFaceGroup;
begin
   with FreeForm.MeshObjects do begin
      // invert meshobjects order
      for i:=0 to (Count div 2) do
         Exchange(i, Count-1-i);
      // for each mesh object
      for i:=0 to Count-1 do with Items[i] do begin
         // invert facegroups order
         n:=FaceGroups.Count;
         for j:=0 to (n div 2) do
            Exchange(j, n-1-j);
         // for each facegroup
         for j:=0 to n-1 do begin
            fg:=FaceGroups[j];
            fg.Reverse;
         end;
      end;
   end;
   FreeForm.StructureChanged;
end;

procedure TMain.ACSaveAsExecute(Sender: TObject);
var
   ext : String;
begin
   if SaveDialog.Execute then begin
      ext:=ExtractFileExt(SaveDialog.FileName);
      if ext='' then
         SaveDialog.FileName:=ChangeFileExt(SaveDialog.FileName,
            '.'+GetVectorFileFormats.FindExtByIndex(SaveDialog.FilterIndex, False, True));
      if GetVectorFileFormats.FindFromFileName(SaveDialog.FileName)=nil then
         ShowMessage('Unsupported or unspecified file extension.')
      else FreeForm.SaveToFile(SaveDialog.FileName);
   end;
end;

procedure TMain.ACSaveAsUpdate(Sender: TObject);
begin
   ACSaveAs.Enabled:=(FreeForm.MeshObjects.Count>0);
end;

procedure TMain.ACConvertToIndexedTrianglesExecute(Sender: TObject);
var
   v : TAffineVectorList;
   i : TIntegerList;
   m : TMeshObject;
   fg : TFGVertexIndexList;
begin
   v:=FreeForm.MeshObjects.ExtractTriangles;
   try
      i:=BuildVectorCountOptimizedIndices(v);
      try
         RemapAndCleanupReferences(v, i);
         IncreaseCoherency(i, 12);
         i.Capacity:=i.Count;
         FreeForm.MeshObjects.Clean;
         m:=TMeshObject.CreateOwned(FreeForm.MeshObjects);
         m.Vertices:=v;
         m.BuildNormals(i, momTriangles);
         m.Mode:=momFaceGroups;
         fg:=TFGVertexIndexList.CreateOwned(m.FaceGroups);
         fg.VertexIndices:=i;
         fg.Mode:=fgmmTriangles;
         FreeForm.StructureChanged;
      finally
         i.Free;
      end;
   finally
      v.Free;
   end;
   GLMaterialLibrary.Materials.Clear;
   SetupFreeFormShading;
end;

procedure TMain.ACStripifyExecute(Sender: TObject);
var
   i : Integer;
   mo : TMeshObject;
   fg : TFGVertexIndexList;
   strips : TPersistentObjectList;
begin
   ACConvertToIndexedTriangles.Execute;
   mo:=FreeForm.MeshObjects[0];
   fg:=(mo.FaceGroups[0] as TFGVertexIndexList);
   strips:=StripifyMesh(fg.VertexIndices, mo.Vertices.Count, True);
   try
      fg.Free;
      for i:=0 to strips.Count-1 do begin
         fg:=TFGVertexIndexList.CreateOwned(mo.FaceGroups);
         fg.VertexIndices:=(strips[i] as TIntegerList);
         if i=0 then
            fg.Mode:=fgmmTriangles
         else fg.Mode:=fgmmTriangleStrip;
      end;
   finally
      strips.Free;
   end;
end;

procedure TMain.ACOptimizeExecute(Sender: TObject);
begin
   OptimizeMesh(FreeForm.MeshObjects, [mooVertexCache, mooSortByMaterials]);
   FreeForm.StructureChanged;
   SetupFreeFormShading;
end;

procedure TMain.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   if Self.Focused then
      GLSceneViewer.Invalidate;
end;

procedure TMain.ACFPSExecute(Sender: TObject);
begin
   ACFPS.Checked:=not ACFPS.Checked;
   ApplyFPS;
end;

procedure TMain.ACLightingExecute(Sender: TObject);
begin
   ACLighting.Checked:=not ACLighting.Checked;
//   TBLighting
   ApplyShadeMode;
end;

procedure TMain.TimerTimer(Sender: TObject);
begin
   StatusBar.Panels[1].Text:=Format('%.1f FPS', [GLSceneViewer.FramesPerSecond]);
   GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TMain.ACSaveTexturesExecute(Sender: TObject);
begin
   if SDTextures.Execute then
      GLMaterialLibrary.SaveToFile(SDTextures.FileName);
end;

end.
