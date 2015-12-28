//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLGLXContext<p>

   GLX specific Context.<p>

   <b>History : </b><font size=-1><ul>
      <li>18/06/10 - Yar - Improved memory context and context sharing
      <li>11/06/10 - Yar - Fixed uses section after lazarus-0.9.29.26033 release
      <li>06/06/10 - Yar - Fixes for Linux x64. DoActivate method now check contexts difference
      <li>21/04/10 - Yar - Added support for GLX versions lower than 1.3
                           (by Rustam Asmandiarov aka Predator)
      <li>06/04/10 - Yar - Update to GLX 1.3-1.4, added PBuffer, forward context creation
                           (by Rustam Asmandiarov aka Predator)
      <li>07/11/09 - DaStr - Improved FPC compatibility (BugtrackerID = 2893580)
                             (thanks Predator)
      <li>10/06/09 - DanB - Added to main GLScene CVS repository (from GLScene-Lazarus).
      <li>14/01/05 - CU - Creation
   </ul></font>
}
unit GLGLXContext;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils, LCLType,
  GLCrossPlatform, GLContext, OpenGL1x, OpenGLTokens,
  x, xlib, xutil;

type
  TGLXFBConfigArray = array[0..MaxInt div (SizeOf(GLXFBConfig) * 2)] of
    GLXFBConfig;
  PGLXFBConfigArray = ^TGLXFBConfigArray;

  // TGLGLXContext
  //
  {: A context driver for GLX. }
  TGLGLXContext = class(TGLContext)
  private
    { Private Declarations }
    FDisplay: PDisplay;
    FCurScreen: Integer;
    FDC: GLXDrawable;
    FRC, FShareContext: GLXContext;
    FHPBUFFER: GLXPBuffer;
    FCurXWindow: HWND;
    FiAttribs: packed array of Integer;
    nret: Integer;
    fbConfigs: PGLXFBConfigArray;
    fNewContext: boolean;
    procedure ChooseGLXFormat;
    function CreateTempWnd: TWindow;
    procedure DestroyTmpWnd(AWin: TWindow);
    function _glXMakeCurrent(dpy: PDisplay; draw: GLXDrawable; ctx: GLXContext):boolean;
  protected
    { Protected Declarations }
    procedure ClearIAttribs;
    procedure FreeIAttribs;
    procedure AddIAttrib(attrib, value: Integer);
    procedure ChangeIAttrib(attrib, newValue: Integer);
    procedure DropIAttrib(attrib: Integer);

    procedure DestructionEarlyWarning(sender: TObject);

    {: DoGetHandles must be implemented in child classes,
       and return the display + window }
    procedure DoGetHandles(outputDevice: HWND; out XWin: HWND); virtual;
      abstract;
    procedure GetHandles(outputDevice: HWND);
    procedure DoCreateContext(outputDevice: HWND); override;
    procedure DoCreateMemoryContext(outputDevice: HWND; width, height:
      Integer; BufferCount: integer); override;
    procedure DoShareLists(aContext: TGLContext); override;
    procedure DoDestroyContext; override;
    procedure DoActivate; override;
    procedure DoDeactivate; override;

    property DC: GLXDrawable read FDC;
    property RenderingContext: GLXContext read FRC;
    property CurXWindow: HWND read FCurXWindow;
  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    function IsValid: Boolean; override;
    procedure SwapBuffers; override;

    function RenderOutputDevice: Integer; override;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// -----------------------------------------------------------------

uses
  GLState, GLSLog;

resourcestring
  cForwardContextFailsed = 'Can not create OpenGL 3.x Forward Context';

  // ------------------
  // ------------------ TGLGLXContext ------------------
  // ------------------

{$IFNDEF GLS_MULTITHREAD}
var
{$ELSE}
threadvar
{$ENDIF}
  vLastConfigs: PGLXFBConfigArray;
  vLastConfigsNumber: GLint;
  vLastVendor: TGLString;

var
  ForwardContextAttribList: array[0..6] of Integer = (
    GLX_CONTEXT_MAJOR_VERSION_ARB, 3,
    GLX_CONTEXT_MINOR_VERSION_ARB, 0,
    GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
    None);

procedure TGLGLXContext.ClearIAttribs;
begin
  SetLength(FiAttribs, 1);
  FiAttribs[0] := 0;
end;

procedure TGLGLXContext.FreeIAttribs;
begin
  SetLength(FiAttribs, 0);
end;

procedure TGLGLXContext.AddIAttrib(attrib, value: Integer);
var
  n: Integer;
begin
  n := Length(FiAttribs);
  SetLength(FiAttribs, n + 2);
  FiAttribs[n - 1] := attrib;
  FiAttribs[n] := value;
  FiAttribs[n + 1] := 0;
end;

procedure TGLGLXContext.ChangeIAttrib(attrib, newValue: Integer);
var
  i: Integer;
begin
  i := 0;
  while i < Length(FiAttribs) do
  begin
    if FiAttribs[i] = attrib then
    begin
      FiAttribs[i + 1] := newValue;
      Exit;
    end;
    Inc(i, 2);
  end;
  AddIAttrib(attrib, newValue);
end;

procedure TGLGLXContext.DropIAttrib(attrib: Integer);
var
  i: Integer;
begin
  i := 0;
  while i < Length(FiAttribs) do
  begin
    if FiAttribs[i] = attrib then
    begin
      Inc(i, 2);
      while i < Length(FiAttribs) do
      begin
        FiAttribs[i - 2] := FiAttribs[i];
        Inc(i);
      end;
      SetLength(FiAttribs, Length(FiAttribs) - 2);
      Exit;
    end;
    Inc(i, 2);
  end;
end;

// Create Temp Window And GLContext 1.1
//

function TGLGLXContext.CreateTempWnd: TWindow;
const
  Attribute: array[0..8] of Integer = (
    GLX_RGBA, GL_TRUE,
    GLX_RED_SIZE, 1,
    GLX_GREEN_SIZE, 1,
    GLX_BLUE_SIZE, 1,
    0);
var
  vi: PXvisualInfo;
begin
  // Lets create temporary window with glcontext
  Result := XCreateSimpleWindow(FDisplay, XRootWindow(FDisplay, FCurScreen),
    0, 0, 1, 1, 0, // need to define some realties dimensions,
    // otherwise the context will not work
    XBlackPixel(FDisplay, FCurScreen),
    XWhitePixel(FDisplay, FCurScreen));
  // XMapWindow(FDisplay, win); // For the test, to see micro window
  XFlush(FDisplay); // Makes XServer execute commands
  vi := glXChooseVisual(FDisplay, FCurScreen, Attribute);
  if vi <> nil then
    FRC := glXCreateContext(FDisplay, vi, nil, true);
  if FRC <> nil then
    glXMakeCurrent(FDisplay, Result, FRC);
  if vi <> nil then
    Xfree(vi);
end;

//Free Window and GLContext
//

procedure TGLGLXContext.DestroyTmpWnd(AWin: TWindow);
begin
  if FDisplay = nil then
    Exit;

  if FRC <> nil then
  begin
    glXMakeCurrent(FDisplay, 0, nil);
    glXDestroyContext(FDisplay, FRC);
  end;

  if @AWin <> nil then
  begin
    XDestroyWindow(FDisplay, AWin);
    XFlush(FDisplay);
  end;
end;

function TGLGLXContext._glXMakeCurrent(dpy: PDisplay; draw: GLXDrawable;
  ctx: GLXContext): boolean;
begin
  if fNewContext then
    Result := glXMakeContextCurrent(dpy, draw, draw, ctx)
  else
    Result := glXMakeCurrent(dpy, draw, ctx);
end;

// ChooseGLXFormat
//

procedure TGLGLXContext.ChooseGLXFormat;
var
  fFBConfigs: PGLXFBConfigArray;
  fnelements: Integer;

  function GetFixedAttribute(Attrib: TGLInt; Param: integer): Integer;
  var
    I, Res, OverRes: integer;
  begin
    {: Appointment of a function to look for equal or approximate values
       of attributes from the list glx.
      If you just ask all the attributes
      that the user can put it out of ignorance
      Access Violation could appear as the list will be empty. }
    Result := -1;
    OverRes := -1;
    for i := 0 to fnelements - 1 do
    begin
      glXGetFBConfigAttrib(FDisplay, fFBConfigs[i], Attrib, @Res);
      if (Res > 0) and (Res <= Param) then
        Result := res;
      if (Res > param) and (OverRes < Res) then
        OverRes := res;
    end;
    if (Result = -1) and (i = fnelements - 1) then
      Result := OverRes;
  end;
const
  cAAToSamples: array[aaDefault..csa16xHQ] of Integer =
    (0, 0, 2, 2, 4, 4, 6, 8, 16, 8, 8, 16, 16);
begin
  // Temporarily create a list of available attributes
  ffbConfigs := glXChooseFBConfig(FDisplay, FCurScreen, @FiAttribs[0],
    @fnelements);

  // Not work!
  if ffbConfigs = nil then
    raise EGLContext.Create('Failed to accept attributes');

  ColorBits := GetFixedAttribute(GLX_BUFFER_SIZE, ColorBits);
  AddIAttrib(GLX_BUFFER_SIZE, ColorBits);

  if AlphaBits > 0 then
  begin
    AlphaBits := GetFixedAttribute(GLX_ALPHA_SIZE, AlphaBits);
    AddIAttrib(GLX_ALPHA_SIZE, AlphaBits);
  end
  else
    AddIAttrib(GLX_ALPHA_SIZE, 0);

  DepthBits := GetFixedAttribute(GLX_DEPTH_SIZE, DepthBits);
  AddIAttrib(GLX_DEPTH_SIZE, DepthBits);

  if AuxBuffers > 0 then
  begin
    // Even if it is 0 anyway will select something from the list FBConfigs!
    AuxBuffers := GetFixedAttribute(GLX_AUX_BUFFERS, AuxBuffers);
    AddIAttrib(GLX_AUX_BUFFERS, AuxBuffers);
  end;

  if (rcoDoubleBuffered in Options) then
    AddIAttrib(GLX_DOUBLEBUFFER, GL_TRUE);

  //Stereo not support see glxinfo

  if StencilBits > 0 then
  begin
    StencilBits := GetFixedAttribute(GLX_STENCIL_SIZE, StencilBits);
    AddIAttrib(GLX_STENCIL_SIZE, StencilBits);
  end;

  { if AccumBits>0 then
      AccumBits:=GetFixedAttribute(GLX_ACCUM_RED_SIZE, round(AccumBits/4))+
            GetFixedAttribute(GLX_ACCUM_GREEN_SIZE, round(AccumBits/4))+
            GetFixedAttribute(GLX_ACCUM_BLUE_SIZE, round(AccumBits/4))+
            GetFixedAttribute(GLX_ACCUM_ALPHA_SIZE, round(AccumBits/4)) ; }
  if AccumBits > 0 then
  begin
    AddIAttrib(GLX_ACCUM_RED_SIZE, GetFixedAttribute(GLX_ACCUM_RED_SIZE,
      round(AccumBits / 4)));
    AddIAttrib(GLX_ACCUM_GREEN_SIZE, GetFixedAttribute(GLX_ACCUM_GREEN_SIZE,
      round(AccumBits / 4)));
    AddIAttrib(GLX_ACCUM_BLUE_SIZE, GetFixedAttribute(GLX_ACCUM_BLUE_SIZE,
      round(AccumBits / 4)));
    AddIAttrib(GLX_ACCUM_ALPHA_SIZE, GetFixedAttribute(GLX_ACCUM_ALPHA_SIZE,
      round(AccumBits / 4)));
  end;
  if GLX_ARB_multisample then
    if (AntiAliasing <> aaDefault) and (AntiAliasing <> aaNone) then
    begin
      AddIAttrib(GLX_SAMPLE_BUFFERS_ARB, GL_TRUE);
      AddIAttrib(GLX_SAMPLES_ARB, GetFixedAttribute(GLX_SAMPLES_ARB,
        cAAToSamples[AntiAliasing]));
    end
    else
      AddIAttrib(GLX_SAMPLE_BUFFERS_ARB, GL_FALSE);

  XFree(ffbConfigs);
  ffbConfigs := glXChooseFBConfig(FDisplay, FCurScreen, @FiAttribs[0],
    @fnelements);

  if ffbConfigs = nil then
  begin
    DropIAttrib(GLX_SAMPLE_BUFFERS_ARB);
    DropIAttrib(GLX_SAMPLES_ARB);
    DropIAttrib(GLX_ACCUM_RED_SIZE);
    DropIAttrib(GLX_ACCUM_GREEN_SIZE);
    DropIAttrib(GLX_ACCUM_BLUE_SIZE);
    DropIAttrib(GLX_ACCUM_ALPHA_SIZE);
  end;
  XFree(ffbConfigs);
end;

procedure TGLGLXContext.DestructionEarlyWarning(sender: TObject);
begin
  DestroyContext;
end;

procedure TGLGLXContext.GetHandles(outputDevice: HWND);
begin
  DoGetHandles(outputDevice, FCurXWindow);
end;

// DoCreateContext
//

procedure TGLGLXContext.DoCreateContext(outputDevice: HWND);
var
  FWin: TWindow;
  vi: PXvisualInfo;
begin
  // Just in case it didn't happen already.
  if not InitOpenGL then
    RaiseLastOSError;

  FDisplay := XOpenDisplay(nil);

  if FDisplay = nil then
    raise EGLContext.Create('Failed connect to XServer');

  {$IFDEF GLS_LOGGING}
  GLSLogger.Log('GLGLXContext: DoCreateContext->Were connected to XServer');
  {$ENDIF}

  FCurScreen := XDefaultScreen(FDisplay);

  FWin := CreateTempWnd;
  {$IFDEF GLS_LOGGING}
  GLSLogger.Log('GLGLXContext: DoCreateContext->Have created a time window');
  {$ENDIF}
  if glGetString(GL_VENDOR) <> vLastVendor then
  begin
    ReadExtensions;
    ReadImplementationProperties;
    vLastVendor := glGetString(GL_VENDOR);
  end;
  DestroyTmpWnd(fWin);
  {$IFDEF GLS_LOGGING}
  GLSLogger.Log('GLGLXContext: DoCreateContext->Have deleted a time window');
  {$ENDIF}

  GetHandles(outputDevice);
  FDC := CurXWindow; //FDC - TWindow
                     //   |- PBuffer
  {$IFDEF GLS_LOGGING}
  GLSLogger.Log('GLGLXContext: DoCreateContext->Handle it is received');
  {$ENDIF}
  fNewContext := GLX_VERSION_1_3 or GLX_VERSION_1_4;
  if fNewContext then
  begin
      AddIAttrib(GLX_X_RENDERABLE, GL_True);
      AddIAttrib(GLX_RENDER_TYPE, GLX_RGBA_BIT);
      AddIAttrib(GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT);
      ChooseGLXFormat;
      fbConfigs := glXChooseFBConfig(FDisplay, FCurScreen, @FiAttribs[0], @nret);
      if fbConfigs = nil then
        raise EGLContext.Create('Failed to accept attributes');
      {$IFDEF GLS_LOGGING}
      GLSLogger.Log('GLGLXContext: DoCreateContext->GLXFormat it is choosed');
      {$ENDIF}
      FRC := glXCreateNewContext(FDisplay, fbConfigs[0],
                      GLX_RGBA_TYPE, FShareContext, true);
      XFree(fbConfigs);
  end
    else
      begin
        //Create Old Context
        AddIAttrib(GLX_RGBA,GL_TRUE);
        AddIAttrib(GLX_RED_SIZE, round(ColorBits/4));
        AddIAttrib(GLX_GREEN_SIZE, round(ColorBits/4));
        AddIAttrib(GLX_BLUE_SIZE, round(ColorBits/4));
        if AlphaBits>0 then
          AddIAttrib(GLX_ALPHA_SIZE, AlphaBits);
        AddIAttrib(GLX_DEPTH_SIZE, DepthBits);
        if StencilBits>0 then
          AddIAttrib(GLX_STENCIL_SIZE, StencilBits);
        if AccumBits>0 then
        begin
          AddIAttrib(GLX_ACCUM_RED_SIZE, round(AccumBits/4));
          AddIAttrib(GLX_ACCUM_GREEN_SIZE, round(AccumBits/4));
          AddIAttrib(GLX_ACCUM_BLUE_SIZE, round(AccumBits/4));
        end;
        if AuxBuffers>0 then
          AddIAttrib(GLX_AUX_BUFFERS, AuxBuffers);
        if (rcoDoubleBuffered in Options) then
          AddIAttrib(GLX_DOUBLEBUFFER,GL_TRUE);
        vi := glXChooseVisual(FDisplay, FCurScreen, @FiAttribs[0]);
        if vi = nil then
          raise EGLContext.Create('Failed to accept attributes');
        {$IFDEF GLS_LOGGING}
        GLSLogger.Log('GLGLXContext: DoCreateContext->GLXFormat it is choosed');
        {$ENDIF}
        FRC := glXCreateContext(FDisplay, vi, FShareContext, true);
        XFree(vi);
      end;
  if RenderingContext = nil then
    raise EGLContext.Create('Failed to create rendering context');
  if PtrUInt(RenderingContext) = GLX_BAD_CONTEXT then
    raise EGLContext.Create('Bad context');
  {$IFDEF GLS_LOGGING}
   GLSLogger.Log('GLGLXContext: DoCreateContext->RenderingContext it is created');
  {$ENDIF}

  Activate;

  if GLStates.ForwardContext then
  begin
    if not fNewContext then
      raise EGLContext.Create('Functions glXChooseFBConfig or glXGetFBConfigAttrib or glXCreateNewContext have not been loaded. It is required GLX above 1.2');
    {$IFDEF GLS_LOGGING}
    GLSLogger.Log('GLGLXContext: DoActivate->Activating Forward Context');
    {$ENDIF}
    if FRC <> nil then
    begin
      glXDestroyContext(FDisplay, FRC);
      {$IFDEF GLS_LOGGING}
      GLSLogger.Log('GLGLXContext: DoActivate->Old Context it is Destoyd');
      {$ENDIF}
    end;
    if @glXCreateContextAttribsARB = nil then
      raise EGLContext.Create(cForwardContextFailsed);

    if GL.VERSION_4_0 then
      ForwardContextAttribList[1] := 4
    else if GL.VERSION_3_3 then
      ForwardContextAttribList[3] := 3
    else if GL.VERSION_3_2 then
      ForwardContextAttribList[3] := 2
    else if GL.VERSION_3_1 then
      ForwardContextAttribList[3] := 1;

    fbConfigs := glXChooseFBConfig(FDisplay, FCurScreen, nil, @nret);
    {$IFDEF GLS_LOGGING}
    GLSLogger.Log('GLGLXContext: DoActivate->GLXFormat it is choosed');
    {$ENDIF}
    FRC := glXCreateContextAttribsARB(FDisplay, fbConfigs[0],
      FShareContext, True, @ForwardContextAttribList);
    if RenderingContext = nil then
      raise EGLContext.Create(cForwardContextFailsed);
    if not _glXMakeCurrent(FDisplay, FDC, FRC) then
      raise EGLContext.Create(cContextActivationFailed);
    {$IFDEF GLS_LOGGING}
    GLSLogger.Log('GLGLXContext: DoActivate->Forward rendering context it is created');
    {$ENDIF}
    XFree(fbConfigs);
    FGL.Initialize;
  end;
  // If we are using AntiAliasing, adjust filtering hints
  if AntiAliasing in [aa2xHQ, aa4xHQ, csa8xHQ, csa16xHQ] then
    // Hint for nVidia HQ modes (Quincunx etc.)
    GLStates.MultisampleFilterHint := hintNicest
  else
    GLStates.MultisampleFilterHint := hintDontCare;

  Deactivate;
end;

// DoCreateMemoryContext
//

procedure TGLGLXContext.DoCreateMemoryContext(outputDevice: HWND; width,
  height: Integer; BufferCount: integer);
var
  FHPBufferAttribList: array[0..6] of Integer = (
    GLX_PBUFFER_WIDTH, 256,
    GLX_PBUFFER_HEIGHT, 256,
    GLX_LARGEST_BUFFER, GL_False,
    none);
  fMaxWidth, TempW, fMaxHeight, TempH: Integer;
  fwin: TWindow;
begin
  // Just in case it didn't happen already.
  if not InitOpenGL then
    RaiseLastOSError;

  FDisplay := XOpenDisplay(nil);

  if FDisplay = nil then
  begin
    raise EGLContext.Create('Failed connect to XServer');
    Exit;
  end;
  {$IFDEF GLS_LOGGING}
  GLSLogger.Log('GLGLXContext: DoCreateContext->Were connected to XServer');
  {$ENDIF}
  FCurScreen := XDefaultScreen(FDisplay);

  FWin := CreateTempWnd;
  {$IFDEF GLS_LOGGING}
  GLSLogger.Log('GLGLXContext: DoCreateContext->Have created a time window');
  {$ENDIF}
  // The extension function addresses are unique for each pixel format. All rendering
  // contexts of a given pixel format share the same extension function addresses.
  if glGetString(GL_VENDOR) <> vLastVendor then
  begin
    ReadExtensions;
    ReadImplementationProperties;
    vLastVendor := glGetString(GL_VENDOR);
  end;
  DestroyTmpWnd(fWin);
{$IFDEF GLS_LOGGING}
  GLSLogger.Log('GLGLXContext :DoCreateContext->Have deleted a time window');
{$ENDIF}

  fNewContext := GLX_VERSION_1_3 or GLX_VERSION_1_4;
  if fNewContext then
  begin
    if (@glXChooseFBConfig = nil)
      or (@glXGetFBConfigAttrib = nil)
      or (@glXCreateNewContext = nil) then
      raise
        EGLContext.Create('Functions glXChooseFBConfig or glXGetFBConfigAttrib or glXCreateNewContext have not been loaded.');
    AddIAttrib(GLX_X_RENDERABLE, GL_True);
    AddIAttrib(GLX_RENDER_TYPE, GLX_RGBA_BIT);
    AddIAttrib(GLX_DRAWABLE_TYPE, GLX_PBUFFER_BIT);
    ChooseGLXFormat;
    fbConfigs := glXChooseFBConfig(FDisplay, FCurScreen, @FiAttribs[0], @nret);
    // Not work!
    if fbConfigs = nil then
      raise EGLContext.Create('Failed to accept attributes');
    {$IFDEF GLS_LOGGING}
    GLSLogger.Log('GLGLXContext: DoCreateContext->GLXFormat it is Choosed');
    {$ENDIF}
    FHPBUFFER := glXCreatePbuffer(FDisplay, fbConfigs[0], @FHPBufferAttribList);
    if FHPBUFFER <> 0 then
    begin
      FDC := FHPBUFFER;
      glXGetFBConfigAttrib(FDisplay, fbConfigs[0], GLX_MAX_PBUFFER_HEIGHT,
        @fMaxHeight);
      glXGetFBConfigAttrib(FDisplay, fbConfigs[0], GLX_MAX_PBUFFER_WIDTH,
        @fMaxWidth);
      TempW := Width;
      TempH := Height;
      if Width > fMaxWidth then
        TempW := fMaxWidth;
      if Height > fMaxHeight then
        TempH := fMaxHeight;
      glXQueryDrawable(FDisplay, FDC, GLX_PBUFFER_WIDTH, @TempW);
      glXQueryDrawable(FDisplay, FDC, GLX_PBUFFER_HEIGHT, @TempH);
    end
    else
      raise Exception.Create('Unabled to create pbuffer.');
    {$IFDEF GLS_LOGGING}
    GLSLogger.Log('GLGLXContext: DoCreateContext->PBuffer it is Created');
    {$ENDIF}
    FRC := glXCreateNewContext(FDisplay, fbConfigs[0],
      GLX_RGBA_TYPE, FShareContext, true);
    if RenderingContext = nil then
      raise EGLContext.Create('Failed to create rendering context');
    if PtrUInt(RenderingContext) = GLX_BAD_CONTEXT then
      raise EGLContext.Create('Bad context');
    {$IFDEF GLS_LOGGING}
    GLSLogger.Log('GLGLXContext: DoCreateContext->RenderingContext it is Created');
    {$ENDIF}
    XFree(fbConfigs);
  end
  else
    raise
        EGLContext.Create('For Memory Context required GLX above 1.2');

  Activate;

  if BufferCount > 1 then
    FGL.DrawBuffers(BufferCount, @MRT_BUFFERS);

  Deactivate;
end;

// DoShareLists
//

procedure TGLGLXContext.DoShareLists(aContext: TGLContext);
var
  otherRC: GLXContext;
begin
  if aContext is TGLGLXContext then
  begin
    otherRC := TGLGLXContext(aContext).RenderingContext;
    if RenderingContext <> nil then
    begin
      if (RenderingContext <> otherRC) then
      begin
        DestroyContext;
        FShareContext:= otherRC;
      end;
    end
    else
      FShareContext := otherRC;
  end
  else
    raise Exception.Create(cIncompatibleContexts);
end;

procedure TGLGLXContext.DoDestroyContext;
begin
  try
    {$IFDEF GLS_LOGGING}
    GLSLogger.Log('GLGLXContext: DoDestroyContext');
    {$ENDIF}
    if not Assigned(FDisplay) then
      raise EGLContext.Create('Lost connection XServer');
    if (glXGetCurrentContext() = FRC) and
      (not _glXMakeCurrent(FDisplay, 0, nil)) then
      raise EGLContext.Create(cContextDeactivationFailed);
    glXDestroyContext(FDisplay, FRC);
    {$IFDEF GLS_LOGGING}
    GLSLogger.Log('GLGLXContext: DoDestroyContext->RenderingContext it is Destroyd');
    {$ENDIF}
    if FHPBUFFER <> 0 then
    begin
      glXDestroyPbuffer(FDisplay, FHPBUFFER);
      FHPBUFFER := 0;
      {$IFDEF GLS_LOGGING}
      GLSLogger.Log('GLGLXContext: DoDestroyContext->RenderingContext it is Destroyd');
      {$ENDIF}
    end;
    FRC := nil;
    FDC := 0;
    FShareContext := nil;
    XCloseDisplay(FDisplay);
    FCurScreen := 0;
  finally
    if vLastConfigs <> nil then
      FreeMem(vLastConfigs, vLastConfigsNumber);
    vLastConfigs := nil;
    vLastConfigsNumber := 0;
  end;
end;

// DoActivate
//

procedure TGLGLXContext.DoActivate;
var
  vVendor: string;
  vConfigs: PGLXFBConfigArray;
  N: TGLint;
begin
  if not _glXMakeCurrent(FDisplay, FDC, FRC) then
    raise EGLContext.Create(cContextActivationFailed);

  if not FGL.IsInitialized then
    FGL.Initialize;

  // The extension function addresses are unique for each pixel format. All rendering
  // contexts of a given pixel format share the same extension function addresses.
  if fNewContext then
  begin
    vConfigs := glXChooseFBConfig(FDisplay, FCurScreen, nil, @N);
    if (N <> vLastConfigsNumber) or not CompareMem(vConfigs, vLastConfigs, N) then
    begin
      vVendor := string(FGL.GetString(GL_VENDOR));
      if vVendor <> vLastVendor then
      begin
        ReadExtensions;
        ReadImplementationProperties;
        vLastVendor :=  vVendor;
      end
      else
      begin
        ReadGLXExtensions;
        ReadGLXImplementationProperties;
      end;
      vLastConfigsNumber := N;
      ReallocMem(vLastConfigs, N);
      Move(vConfigs^, vLastConfigs^, N);
    end;
    XFree(vConfigs);
  end
  else
  begin
    raise EGLContext.Create('Low GLX version not yet supported')
  end;
end;

// Deactivate
//

procedure TGLGLXContext.DoDeactivate;
begin
  if not _glXMakeCurrent(FDisplay, 0, nil) then
    raise EGLContext.Create(cContextDeactivationFailed);
end;

constructor TGLGLXContext.Create;
begin
  inherited Create;
  ClearIAttribs;
end;

destructor TGLGLXContext.Destroy;
begin
{$IFDEF GLS_MULTITHREAD}
  vLastVendor := '';
{$ENDIF}
  inherited Destroy;
end;

// IsValid
//

function TGLGLXContext.IsValid: Boolean;
begin
  Result := (FRC <> nil);
end;

// SwapBuffers
//

procedure TGLGLXContext.SwapBuffers;
begin
  if (FDC <> 0) and (rcoDoubleBuffered in Options) then
    glXSwapBuffers(FDisplay, FDC);
end;

function TGLGLXContext.RenderOutputDevice: Integer;
begin
  Result := 0;
end;

end.
