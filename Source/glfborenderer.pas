//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFBO<p>

   Implements FBO support for GLScene.

   Original author of the unit is Riz.
   Modified by C4 and YarUnderoaker (hope, I didn't miss anybody).

   <b>History : </b><font size=-1><ul>
      <li>02/06/10 - Yar - Replace OpenGL functions to OpenGLAdapter
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>15/02/10 - Yar - Added notification of freeing RootObject
      <li>22/01/10 - Yar - Added ClearOptions, Level, Layer, PostGenerateMipmap
                             UseBufferBackground moved to coUseBufferBackground
      <li>14/12/09 - DaStr - Fixed memory leak (thanks YarUnderoaker)
      <li>11/11/09 - DaStr - Added $I GLScene.inc
      <li>09/11/09 - DaStr - Initial version (contributed to GLScene)
   </ul></font>
}
unit GLFBORenderer;

interface

{$I GLScene.inc}

uses
  Classes, GLVectorGeometry, GLScene, GLTexture, GLContext, GLFBO, GLColor,
  GLMaterial, GLRenderContextInfo, GLState, GLSLog;

type
  TGLEnabledRenderBuffer = (erbDepth, erbStencil);
  TGLEnabledRenderBuffers = set of TGLEnabledRenderBuffer;

  TGLFBOTargetVisibility = (tvDefault, tvFBOOnly);

  TGLFBOClearOption = (coColorBufferClear, coDepthBufferClear,
    coStencilBufferClear, coUseBufferBackground);
  TGLFBOClearOptions = set of TGLFBOClearOption;

  TGLFBORenderer = class(TGLBaseSceneObject, IGLMaterialLibrarySupported)
  private
    FFbo: TGLFrameBuffer;
    FDepthRBO: TGLDepthRBO;
    FStencilRBO: TGLStencilRBO;
    FColorAttachment: Integer;
    FRendering: Boolean;

    FHasColor: Boolean;
    FHasDepth: Boolean;
    FHasStencil: Boolean;

    FChanged: Boolean;

    FMaterialLibrary: TGLMaterialLibrary;
    FColorTextureName: TGLLibMaterialName;
    FDepthTextureName: TGLLibMaterialName;
    FWidth: Integer;
    FHeight: Integer;
    FForceTextureDimensions: Boolean;
    FStencilPrecision: TGLStencilPrecision;
    FRootObject: TGLBaseSceneObject;
    FRootVisible: Boolean;
    FCamera: TGLCamera;
    FEnabledRenderBuffers: TGLEnabledRenderBuffers;
    FTargetVisibility: TGLFBOTargetVisibility;
    FBeforeRender: TNotifyEvent;
    FPostInitialize: TNotifyEvent;
    FAfterRender: TNotifyEvent;
    FPreInitialize: TNotifyEvent;
    FBackgroundColor: TGLColor;
    FClearOptions: TGLFBOClearOptions;
    FAspect: Single;
    FSceneScaleFactor: Single;
    FUseLibraryAsMultiTarget: Boolean;
    FPostGenerateMipmap: Boolean;

    // implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TGLMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    procedure SetDepthTextureName(const Value: TGLLibMaterialName);
    procedure SetColorTextureName(const Value: TGLLibMaterialName);
    procedure SetForceTextureDimentions(const Value: Boolean);
    procedure SetHeight(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetLayer(const Value: Integer);
    function GetLayer: Integer;
    procedure SetLevel(const Value: Integer);
    function GetLevel: Integer;
    procedure SetStencilPrecision(const Value: TGLStencilPrecision);
    procedure SetRootObject(const Value: TGLBaseSceneObject);
    function GetViewport: TRectangle;
    procedure SetCamera(const Value: TGLCamera);
    procedure SetEnabledRenderBuffers(const Value: TGLEnabledRenderBuffers);
    procedure SetTargetVisibility(const Value: TGLFBOTargetVisibility);
    procedure SetBackgroundColor(const Value: TGLColor);
    function StoreSceneScaleFactor: Boolean;
    function StoreAspect: Boolean;
    procedure SetUseLibraryAsMultiTarget(Value: Boolean);
    procedure SetPostGenerateMipmap(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InitializeFBO;

    procedure ForceDimensions(Texture: TGLTexture);

    procedure RenderToFBO(var ARci: TRenderContextInfo);

    procedure ApplyCamera;
    procedure UnApplyCamera;

    procedure DoBeforeRender;
    procedure DoAfterRender;
    procedure DoPreInitialize;
    procedure DoPostInitialize;

    property HasColor: Boolean read FHasColor;
    property HasDepth: Boolean read FHasDepth;
    property HasStencil: Boolean read FHasStencil;

    property Viewport: TRectangle read GetViewport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StructureChanged; override;

    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf: Boolean;
      ARenderChildren: Boolean); override;

    {: Layer (also cube map face) is activated only on
       the volume textures, texture array and cube map.
       You can select the layer during the drawing to. }
    property Layer: Integer read GetLayer write SetLayer;
    {: Mipmap Level where will be rendering }
    property Level: Integer read GetLevel write SetLevel;

  published
    {: force texture dimensions when initializing
       only works with TGLBlankImage and TGLFloatDataImage, otherwise does nothing }
    property ForceTextureDimensions: Boolean read FForceTextureDimensions write
      SetForceTextureDimentions default True;

    property Width: Integer read FWidth write SetWidth default 256;
    property Height: Integer read FHeight write SetHeight default 256;

    property Aspect: Single read FAspect write FAspect stored StoreAspect;

    property ColorTextureName: TGLLibMaterialName read FColorTextureName write
      SetColorTextureName;

    property DepthTextureName: TGLLibMaterialName read FDepthTextureName write
      SetDepthTextureName;
    property MaterialLibrary: TGLMaterialLibrary read GetMaterialLibrary write
      SetMaterialLibrary;

    property BackgroundColor: TGLColor read FBackgroundColor write
      SetBackgroundColor;
    property ClearOptions: TGLFBOClearOptions read FClearOptions write
      FClearOptions;

    {: camera used for rendering to the FBO
       if not assigned, use the active view's camera }
    property Camera: TGLCamera read FCamera write SetCamera;

    {: adjust the scene scale of the camera so that the rendering
       becomes independent of the width of the fbo renderer
       0 = disabled }
    property SceneScaleFactor: Single read FSceneScaleFactor write
      FSceneScaleFactor stored StoreSceneScaleFactor;

    {: root object used when rendering to the FBO
       if not assigned, uses itself as root and renders the child objects to the FBO }
    property RootObject: TGLBaseSceneObject read FRootObject write
      SetRootObject;

    {: determines if target is rendered to FBO only or rendered normally
       in FBO only mode, if RootObject is assigned, the RootObject's Visible flag is modified
       in default mode, if RootObject is not assigned, children are rendered normally after being
       rendered to the FBO }
    property TargetVisibility: TGLFBOTargetVisibility read FTargetVisibility
      write SetTargetVisibility default tvDefault;

    {: Enables the use of a render buffer if a texture is not assigned }
    property EnabledRenderBuffers: TGLEnabledRenderBuffers read
      FEnabledRenderBuffers write SetEnabledRenderBuffers;

    {: use stencil buffer }
    property StencilPrecision: TGLStencilPrecision read FStencilPrecision write
      SetStencilPrecision default spDefault;

    {: called before rendering to the FBO }
    property BeforeRender: TNotifyEvent read FBeforeRender write FBeforeRender;
    {: called after the rendering to the FBO }
    property AfterRender: TNotifyEvent read FAfterRender write FAfterRender;
    {: Called before the FBO is initialized
       the FBO is bound before calling this event }
    property PreInitialize: TNotifyEvent read FPreInitialize write
      FPreInitialize;
    {: Called after the FBO is initialized, but before any rendering
       the FBO is bound before calling this event }
    property PostInitialize: TNotifyEvent read FPostInitialize write
      FPostInitialize;

    property UseLibraryAsMultiTarget: Boolean read FUseLibraryAsMultiTarget write
      SetUseLibraryAsMultiTarget default False;

    {: Control mipmap generation after rendering
       texture must have MinFilter with mipmaping }
    property PostGenerateMipmap: Boolean read FPostGenerateMipmap write
      SetPostGenerateMipmap default true;
  end;

implementation

uses
  SysUtils,
  OpenGL1x, OpenGLTokens,
  GLVectorTypes,
  GLMultisampleImage;

  { TGLFBORenderer }

procedure TGLFBORenderer.ApplyCamera;
var
  sc: Single;
begin
  if assigned(Camera) then
  begin
    GL.MatrixMode(GL_PROJECTION);
    GL.PushMatrix;
    GL.LoadIdentity;
    sc := FCamera.SceneScale;
    if FSceneScaleFactor > 0 then
      FCamera.SceneScale := Width / FSceneScaleFactor;
    FCamera.ApplyPerspective(Viewport, Width, Height, 96); // 96 is default dpi
    FCamera.SceneScale := sc;

    GL.MatrixMode(GL_MODELVIEW);
    GL.PushMatrix;
    GL.LoadIdentity;
    GL.Scalef(1.0 / FAspect, 1.0, 1.0);
    FCamera.Apply;
  end
  else
  begin
    GL.MatrixMode(GL_MODELVIEW);
    GL.PushMatrix;
    GL.Scalef(1.0 / FAspect, 1.0, 1.0);
  end;
end;

constructor TGLFBORenderer.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := [osDirectDraw, osNoVisibilityCulling];
  FFbo := TGLFrameBuffer.Create;
  FBackgroundColor := TGLColor.Create(Self);
  FUseLibraryAsMultiTarget := False;
  FForceTextureDimensions := True;
  FWidth := 256;
  FHeight := 256;
  FEnabledRenderBuffers := [erbDepth];
  FClearOptions := [coColorBufferClear, coDepthBufferClear,
    coStencilBufferClear, coUseBufferBackground];
  FAspect := 1.0;
  FSceneScaleFactor := 0.0;
  FPostGenerateMipmap := true;
  StructureChanged;
end;

destructor TGLFBORenderer.Destroy;
begin
  FFbo.Free;
  FDepthRBO.Free;
  FStencilRBO.Free;
  FBackgroundColor.Free;
  inherited;
end;

procedure TGLFBORenderer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FRootObject) and (Operation = opRemove) then
    FRootObject := nil;
end;

procedure TGLFBORenderer.DoAfterRender;
begin
  if assigned(FAfterRender) then
    FAfterRender(Self);
end;

procedure TGLFBORenderer.DoBeforeRender;
begin
  if assigned(FBeforeRender) then
    FBeforeRender(Self);
end;

procedure TGLFBORenderer.DoPostInitialize;
begin
  if assigned(FPreInitialize) then
    FPreInitialize(Self);
end;

procedure TGLFBORenderer.DoPreInitialize;
begin
  if assigned(FPostInitialize) then
    FPostInitialize(Self);
end;

procedure TGLFBORenderer.DoRender(var ARci: TRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
begin
  if (csDesigning in ComponentState) then
    Exit;

  RenderToFBO(ARci);

  if (not assigned(FRootObject)) and (TargetVisibility = tvDefault) and
    ARenderChildren then
  begin
    RenderChildren(0, Count - 1, ARci);
  end;
end;

procedure TGLFBORenderer.ForceDimensions(Texture: TGLTexture);
var
  bi: TGLBlankImage;
  mi: TGLMultisampleImage;
begin
  if Texture.Image is TGLBlankImage then
  begin
    bi := TGLBlankImage(Texture.Image);
    bi.Width := Width;
    bi.Height := Height;
  end
  else if Texture.Image is TGLMultisampleImage then
  begin
    mi := TGLMultisampleImage(Texture.Image);
    mi.Width := Width;
    mi.Height := Height;
  end;
end;

function TGLFBORenderer.GetViewport: TRectangle;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Width := Width;
  Result.Height := Height;
end;

procedure TGLFBORenderer.InitializeFBO;
const
  cDrawBuffers: array[0..15] of GLenum =
    (
    GL_COLOR_ATTACHMENT0_EXT,
    GL_COLOR_ATTACHMENT1_EXT,
    GL_COLOR_ATTACHMENT2_EXT,
    GL_COLOR_ATTACHMENT3_EXT,
    GL_COLOR_ATTACHMENT4_EXT,
    GL_COLOR_ATTACHMENT5_EXT,
    GL_COLOR_ATTACHMENT6_EXT,
    GL_COLOR_ATTACHMENT7_EXT,
    GL_COLOR_ATTACHMENT8_EXT,
    GL_COLOR_ATTACHMENT9_EXT,
    GL_COLOR_ATTACHMENT10_EXT,
    GL_COLOR_ATTACHMENT11_EXT,
    GL_COLOR_ATTACHMENT12_EXT,
    GL_COLOR_ATTACHMENT13_EXT,
    GL_COLOR_ATTACHMENT14_EXT,
    GL_COLOR_ATTACHMENT15_EXT
    );
var
  colorTex: TGLTexture;
  depthTex: TGLTexture;
  I: Integer;
  maxAttachment: TGLint;
  maxSize: TGLint;
begin
  for I := 0 to MaxColorAttachments - 1 do
    FFbo.DetachTexture(I);

  GL.GetIntegerv(GL_MAX_RENDERBUFFER_SIZE, @maxSize);
  if Width>maxSize then
  begin
    FWidth := maxSize;
    GLSLogger.LogWarning(Name+'.Width out of GL_MAX_RENDERBUFFER_SIZE');
  end;
  if Height>maxSize then
  begin
    FHeight := maxSize;
    GLSLogger.LogWarning(Name+'.Height out of GL_MAX_RENDERBUFFER_SIZE');
  end;

  FFbo.Width := Width;
  FFbo.Height := Height;

  FFbo.Bind;
  DoPreInitialize;
  FFbo.Unbind;

  colorTex := FMaterialLibrary.TextureByName(ColorTextureName);
  depthTex := FMaterialLibrary.TextureByName(DepthTextureName);

  FHasColor := False;
  FHasDepth := False;
  FHasStencil := False;
  FColorAttachment := 0;

  if FUseLibraryAsMultiTarget then
  begin
    if not (GL.ARB_draw_buffers or GL.ATI_draw_buffers) then
    begin
      GLSLogger.LogError('Hardware do not support MRT');
      Visible := False;
      Abort;
    end;
    GL.GetIntegerv(GL_MAX_COLOR_ATTACHMENTS, @maxAttachment);

    // Multicolor attachments
    for I := 0 to FMaterialLibrary.Materials.Count - 1 do
    begin
      colorTex := FMaterialLibrary.Materials[I].Material.Texture;
      // Skip depth texture
      if colorTex = depthTex then
        Continue;
      if ForceTextureDimensions then
        ForceDimensions(colorTex);

      if FColorAttachment >= maxAttachment then
      begin
        GLSLogger.LogError('Number of color attachments out of GL_MAX_COLOR_ATTACHMENTS');
        Visible := False;
        Abort;
      end;

      FFbo.AttachTexture(FColorAttachment, colorTex);
      Inc(FColorAttachment);
    end;
    FHasColor := FColorAttachment > 0;
  end
  else
  begin
    // One color attachment
    if Assigned(colorTex) then
    begin
      if ForceTextureDimensions then
        ForceDimensions(colorTex);
      FFbo.AttachTexture(0, colorTex);
      Inc(FColorAttachment);
      FHasColor := True;
    end;
  end;

  if Assigned(depthTex) then
  begin
    if ForceTextureDimensions then
      ForceDimensions(depthTex);
    FFbo.AttachDepthTexture(depthTex);
    FDepthRBO.Free;
    FDepthRBO := nil;
    FHasDepth := True;
  end
  else if erbDepth in EnabledRenderBuffers then
  begin
    if not Assigned(FDepthRBO) then
      FDepthRBO := TGLDepthRBO.Create;

    FDepthRBO.Width := Width;
    FDepthRBO.Height := Height;

    FFbo.AttachDepthBuffer(FDepthRBO);
    FHasDepth := True;
  end
  else
  begin
    FFbo.DetachDepthBuffer;
    FDepthRBO.Free;
    FDepthRBO := nil;
  end;

  if erbStencil in EnabledRenderBuffers then
  begin
    if not assigned(FStencilRBO) then
    begin
      FStencilRBO := TGLStencilRBO.Create;
    end;

    FStencilRBO.StencilPrecision := FStencilPrecision;
    FStencilRBO.Width := Width;
    FStencilRBO.Height := Height;

    FFbo.AttachStencilBuffer(FStencilRBO);
    FHasStencil := True;
  end
  else
  begin
    FFbo.DetachStencilBuffer;
    FStencilRBO.Free;
    FStencilRBO := nil;
  end;
  FFbo.Bind;

  if FColorAttachment = 0 then
  begin
    GL.DrawBuffer(GL_NONE);
    GL.ReadBuffer(GL_NONE);
  end
  else
    GL.DrawBuffers(FColorAttachment, @cDrawBuffers);

  DoPostInitialize;
  FFbo.Unbind;

  FChanged := False;
  GL.CheckError;
end;

procedure TGLFBORenderer.RenderToFBO(var ARci: TRenderContextInfo);

  function GetClearBits: cardinal;
  begin
    Result := 0;
    if HasColor and (coColorBufferClear in FClearOptions) then
      Result := Result or GL_COLOR_BUFFER_BIT;
    if HasDepth and (coDepthBufferClear in FClearOptions) then
      Result := Result or GL_DEPTH_BUFFER_BIT;
    if HasStencil and (coStencilBufferClear in FClearOptions) then
      Result := Result or GL_STENCIL_BUFFER_BIT;
  end;

type
  TGLStoredStates = record
    ColorClearValue: TColorVector;
    ColorWriteMask: TColorMask;
    Tests: TGLStates;
  end;

  function StoreStates: TGLStoredStates;
  begin
    Result.ColorClearValue := ARci.GLStates.ColorClearValue;
    Result.ColorWriteMask := ARci.GLStates.ColorWriteMask[0];
    Result.Tests := [stDepthTest, stStencilTest] * ARci.GLStates.States;
  end;

  procedure RestoreStates(const aStates: TGLStoredStates);
  begin
    ARci.GLStates.ColorClearValue := aStates.ColorClearValue;
    ARci.GLStates.SetColorMask(aStates.ColorWriteMask);
    if stDepthTest in aStates.Tests then
      ARci.GLStates.Enable(stDepthTest)
    else
      ARci.GLStates.Disable(stDepthTest);

    if stStencilTest in aStates.Tests then
      ARci.GLStates.Enable(stStencilTest)
    else
      ARci.GLStates.Disable(stStencilTest);
  end;

var
  backColor: TColorVector;
  buffer: TGLSceneBuffer;
  savedStates: TGLStoredStates;
  w, h: Integer;
  s: string;
begin
  // prevent recursion
  if not TGLFramebufferHandle.IsSupported or FRendering then
    Exit;

  FRendering := True;
  if (not assigned(FFbo)) or FChanged then
    InitializeFBO;

  try
    savedStates := StoreStates;
    ApplyCamera;
    DoBeforeRender;
    FFbo.Bind;
    if FFbo.GetStringStatus(s) <> fsComplete then
    begin
      GLSLogger.LogError('Framebuffer error: ' + s);
      Visible := False;
      Abort;
    end;

    if Assigned(Camera) then
      Camera.Scene.SetupLights(ARci.GLStates.MaxLights);

    w := Width;
    h := Height;
    if FFBO.Level > 0 then
    begin
      w := w shr FFBO.Level;
      h := h shr FFBO.Level;
      if w = 0 then
        w := 1;
      if h = 0 then
        h := 1;
    end;
    ARci.GLStates.ViewPort := Vector4iMake(0, 0, w, h);
    buffer := ARci.buffer as TGLSceneBuffer;

    if HasColor then
      ARci.GLStates.SetColorMask(cAllColorComponents)
    else
      ARci.GLStates.SetColorMask([]);

    ARci.GLStates.DepthWriteMask := HasDepth;

    if HasStencil then
      ARci.GLStates.Enable(stStencilTest)
    else
      ARci.GLStates.Disable(stStencilTest);

    if coUseBufferBackground in FClearOptions then
    begin
      backColor := ConvertWinColor(buffer.BackgroundColor);
      backColor[3] := buffer.BackgroundAlpha;
      ARci.GLStates.ColorClearValue := backColor;
    end
    else
    begin
      ARci.GLStates.ColorClearValue := FBackgroundColor.Color;
    end;

    GL.Clear(GetClearBits);

    FFbo.PreRender;
    // render to fbo
    if Assigned(RootObject) then
    begin
      // if object should only be rendered to the fbo
      // ensure it's visible before rendering to fbo
      if TargetVisibility = tvFBOOnly then
        RootObject.Visible := True;
      RootObject.Render(ARci);
      // then make it invisible afterwards
      if TargetVisibility = tvFBOOnly then
        RootObject.Visible := False;
    end
    else if (Count > 0) then
      RenderChildren(0, Count - 1, ARci);
    FFbo.PostRender(FPostGenerateMipmap);

    RestoreStates(savedStates);
    ARci.GLStates.ViewPort :=
      Vector4iMake(0, 0, ARci.viewPortSize.cx, ARci.viewPortSize.cy);

    UnApplyCamera;
  finally
    FFbo.Unbind;
    FRendering := False;

    DoAfterRender;
  end;
end;

procedure TGLFBORenderer.SetBackgroundColor(const Value: TGLColor);
begin
  FBackgroundColor.Assign(Value);
end;

procedure TGLFBORenderer.SetCamera(const Value: TGLCamera);
begin
  if FCamera <> Value then
  begin
    FCamera := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetColorTextureName(const Value: TGLLibMaterialName);
begin
  if FColorTextureName <> Value then
  begin
    FColorTextureName := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetDepthTextureName(const Value: TGLLibMaterialName);
begin
  if FDepthTextureName <> Value then
  begin
    FDepthTextureName := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetEnabledRenderBuffers(const Value:
  TGLEnabledRenderBuffers);
begin
  if FEnabledRenderBuffers <> Value then
  begin
    FEnabledRenderBuffers := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetForceTextureDimentions(const Value: Boolean);
begin
  if FForceTextureDimensions <> Value then
  begin
    FForceTextureDimensions := Value;
    StructureChanged;
  end;
end;

// GetMaterialLibrary
//

function TGLFBORenderer.GetMaterialLibrary: TGLMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLFBORenderer.SetMaterialLibrary(const Value: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    FMaterialLibrary := Value;
    StructureChanged;
  end;
end;

// SetUseLibraryAsMultiTarget
//

procedure TGLFBORenderer.SetUseLibraryAsMultiTarget(Value: Boolean);
begin
  if FUseLibraryAsMultiTarget <> Value then
  begin
    FUseLibraryAsMultiTarget := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetPostGenerateMipmap(const Value: Boolean);
begin
  if FPostGenerateMipmap <> Value then
    FPostGenerateMipmap := Value;
end;

procedure TGLFBORenderer.SetRootObject(const Value: TGLBaseSceneObject);
begin
  if FRootObject <> Value then
  begin
    if Assigned(FRootObject) then
      FRootObject.RemoveFreeNotification(Self);
    FRootObject := Value;
    if Assigned(FRootObject) then
      FRootObject.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetStencilPrecision(const Value: TGLStencilPrecision);
begin
  if FStencilPrecision <> Value then
  begin
    FStencilPrecision := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetTargetVisibility(const Value:
  TGLFBOTargetVisibility);
begin
  if FTargetVisibility <> Value then
  begin
    if assigned(RootObject) then
    begin
      if (TargetVisibility = tvFBOOnly) then
      begin
        // we went from fbo only, restore root's old visibility
        RootObject.Visible := FRootVisible;
      end
      else
      begin
        // we're going to fbo only, save root visibility for later
        FRootVisible := RootObject.Visible;
      end;
    end;

    FTargetVisibility := Value;
    StructureChanged;
  end;
end;

// StoreSceneScaleFactor
//

function TGLFBORenderer.StoreSceneScaleFactor: Boolean;
begin
  Result := (FSceneScaleFactor <> 0.0);
end;

// StoreAspect
//

function TGLFBORenderer.StoreAspect: Boolean;
begin
  Result := (FAspect <> 1.0);
end;

procedure TGLFBORenderer.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetLayer(const Value: Integer);
begin
  if Value <> FFBO.Layer then
  begin
    if FRendering or FChanged then
      FFBO.Layer := Value
    else
    begin
      FFBO.Bind;
      FFBO.Layer := Value;
      FFBO.Unbind;
    end;
  end;
end;

function TGLFBORenderer.GetLayer: Integer;
begin
  Result := FFbo.Layer;
end;

procedure TGLFBORenderer.SetLevel(const Value: Integer);
var
  w, h: Integer;
begin
  if Value <> FFBO.Level then
  begin
    if FRendering or FChanged then
    begin
      FFBO.Level := Value;
      w := Width;
      h := Height;
      if FFBO.Level > 0 then
      begin
        w := w shr FFBO.Level;
        h := h shr FFBO.Level;
        if w = 0 then
          w := 1;
        if h = 0 then
          h := 1;
        CurrentGLContext.GLStates.ViewPort := Vector4iMake(0, 0, w, h);
      end;
    end
    else
    begin
      FFBO.Bind;
      FFBO.Level := Value;
      FFBO.Unbind;
    end;
  end;
end;

function TGLFBORenderer.GetLevel: Integer;
begin
  Result := FFbo.Level;
end;

procedure TGLFBORenderer.StructureChanged;
begin
  FChanged := True;
  inherited;
end;

procedure TGLFBORenderer.UnApplyCamera;
begin
  if assigned(Camera) then
  begin
    GL.MatrixMode(GL_PROJECTION);
    GL.PopMatrix;

    GL.MatrixMode(GL_MODELVIEW);
    GL.PopMatrix;
  end
  else
  begin
    GL.MatrixMode(GL_MODELVIEW);
    GL.PopMatrix;
  end;
end;

initialization

  RegisterClasses([TGLFBORenderer]);

end.
