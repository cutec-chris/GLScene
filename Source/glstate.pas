//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLState<p>

   Tools for managing an application-side cache of OpenGL state.<p>

 <b>History : </b><font size=-1><ul>
      <li>03/08/10 - DaStr - Restored deprecated SetGLFrontFaceCW() function
      <li>16/06/10 - YP  - Out of range fix, increasing FListStates by 2 must be done in a while loop
      <li>16/06/10 - YP  - Out of range fix, MAX_HARDWARE_LIGHT can be up to 16
      <li>31/05/10 - Yar - Replace OpenGL1x functions to OpenGLAdapter, not complete yet.
      <li>01/05/10 - Yar - Added cashing to Vertex Array Object
      <li>22/04/10 - Yar - GLState revision
      <li>11/04/10 - Yar - Added NewList, EndList, InsideList
      <li>08/05/10 - DanB - Added TGLStateCache.SetColorMask
      <li>05/03/10 - DanB - Added initial functions/properties for caching all
                            OpenGL 3.2 state, not complete yet.
      <li>22/02/10 - DanB - added SetGLCurrentProgram
      <li>22/02/10 - Yar - Added more control of states
      <li>13/05/07 - fig - Added stTexture3D (GL_TEXTURE_3D)
      <li>19/12/06 - DaStr - GetGLCurrentTexture, ResetGLTexture added to TGLStateCache
      <li>04/10/04 - NC - Added stTextureRect (GL_TEXTURE_RECTANGLE_NV)
      <li>07/01/04 - EG - Introduced TGLStateCache
      <li>05/09/03 - EG - Creation from GLMisc split
   </ul></font>
}

// TODO: Proper client-side pushing + popping of state, in OpenGL 3+ contexts,
//       rather than using glPushAttrib + glPopAttrib.
// TODO: Proper support for textures, taking into account that they probably
//       won't be linked to texture units in some future version of OpenGL.
// TODO: Once more of GLScene is cache-aware, enable some of the checks before
//       changing OpenGL state (where we will gain a speed increase).
// TODO: Cache some relevant legacy state
// TODO: improve binding objects to binding points
// TODO: decide how to implement the new Enable* options (without going above
//       32 elements in sets if possible, which would be slower in 32bit Delphi)
// TODO: remove stTexture1D, 2D, etc from TGLState if possible, since they are
//       per texture-unit + also deprecated in OpenGL 3+

unit GLState;

interface

{$I GLScene.inc}
{$UNDEF GLS_OPENGL_DEBUG}

uses
  Classes,
  GLVectorTypes,
  GLVectorGeometry,
  SysUtils,
  OpenGL1x,OpenGLTokens,
  GLTextureFormat,
  GLSLog;

const
  GLS_VERTEX_ATTR_NUM = 16;

type

  TGLStateType = (sttCurrent, sttPoint, sttLine, sttPolygon, sttPolygonStipple,
    sttPixelMode, sttLighting, sttFog, sttDepthBuffer, sttAccumBuffer,
    sttStencilBuffer, sttViewport, sttTransform, sttEnable, sttColorBuffer,
    sttHint, sttEval, sttList, sttTexture, sttScissor,
    sttMultisample);
  TGLStateTypes = set of TGLStateType;

const
  cAllAttribBits = [low(TGLStateType)..High(TGLStateType)];

type

  // TGLState
  //
//: Reflects all relevant (binary) states of OpenGL subsystem
  TGLState = (stAlphaTest, stAutoNormal,
    stBlend, stColorMaterial, stCullFace, stDepthTest, stDither,
    stFog, stLighting, stLineSmooth, stLineStipple,
    stIndexLogicOp, stColorLogicOp, stNormalize, stPointSmooth, stPointSprite,
    stPolygonSmooth, stPolygonStipple, stScissorTest, stStencilTest,
    stPolygonOffsetPoint, stPolygonOffsetLine, stPolygonOffsetFill,
    stDepthClamp);

  TGLStates = set of TGLState;

  TComparisonFunction = (cfNever, cfAlways, cfLess, cfLEqual, cfEqual,
    cfGreater, cfNotEqual, cfGEqual);
  TStencilFunction = TComparisonFunction;
  TDepthFunction = TComparisonFunction;

  TBlendFunction = (bfZero, bfOne,
    bfSrcColor, bfOneMinusSrcColor, bfDstColor, bfOneMinusDstColor,
    bfSrcAlpha, bfOneMinusSrcAlpha, bfDstAlpha, bfOneMinusDstAlpha,
    bfConstantColor, bfOneMinusConstantColor,
    bfConstantAlpha, bfOneMinusConstantAlpha,
    bfSrcAlphaSat);

  TDstBlendFunction = bfZero..bfOneMinusConstantAlpha;

  TBlendEquation = (beAdd, beSubtract, beReverseSubtract, beMin, beMax);

  TStencilOp = (soKeep, soZero, soReplace, soIncr, soDecr, soInvert, soIncrWrap,
    soDecrWrap);

  TLogicOp = (loClear, loAnd, loAndReverse, loCopy, loAndInverted, loNoOp,
    loXOr, loOr, loNor, loEquiv, loInvert, loOrReverse, loCopyInverted,
    loOrInverted, loNAnd, loSet);

  TQueryType = (qrySamplesPassed, qryPrimitivesGenerated,
    qryTransformFeedbackPrimitivesWritten, qryTimeElapsed {EXT});
  // TFaceWinding
  //
//: Describe what kind of winding has a front face
  TFaceWinding = (fwCounterClockWise, fwClockWise);

  TPolygonMode = (pmFill, pmLines, pmPoints);

  TCullFaceMode = (cmFront, cmBack, cmFrontAndBack);
  //  TSingleCullFaceMode = cmFront..cmBack;

  TColorComponent = (ccRed, ccGreen, ccBlue, ccAlpha);
  TColorMask = set of TColorComponent;

const
  cAllColorComponents = [ccRed, ccGreen, ccBlue, ccAlpha];
  MAX_HARDWARE_LIGHT = 16;
  MAX_HARDWARE_TEXTURE_UNIT = 48;

type

  THintType = (hintDontCare, hintFastest, hintNicest);

  TVAOStates = record
    FArrayBufferBinding: TGLuint;
    FElementBufferBinding: TGLuint;
    FAttribEnabling: array[0..GLS_VERTEX_ATTR_NUM - 1] of TGLboolean;
  end;

  // TGLStateCache
  //
  {: Manages an application-side cache of OpenGL states and parameters.<p>
     Purpose of this class is to eliminate redundant state and parameter
     changes, and there will typically be no more than one state cache per
     OpenGL context. }
  TGLStateCache = class
  private
    { Private Declarations }
    // Legacy state
    FFrontBackColors: array[0..1, 0..3] of TVector;
    FFrontBackShininess: array[0..1] of Integer;
    FAlphaFunc: TComparisonFunction;
    FAlphaRef: TGLclampf;
    FPolygonBackMode: TPolygonMode; // Front + back have same polygon mode

    // Lighting state
    FMaxLights: GLuint;
    FLightEnabling: array[0..MAX_HARDWARE_LIGHT - 1] of Boolean;
    FLightAmbient: array[0..MAX_HARDWARE_LIGHT - 1] of TVector;
    FLightDiffuse: array[0..MAX_HARDWARE_LIGHT - 1] of TVector;
    FLightSpecular: array[0..MAX_HARDWARE_LIGHT - 1] of TVector;
    FSpotCutoff: array[0..MAX_HARDWARE_LIGHT - 1] of Single;
    FConstantAtten: array[0..MAX_HARDWARE_LIGHT - 1] of Single;
    FLinearAtten: array[0..MAX_HARDWARE_LIGHT - 1] of Single;
    FQuadAtten: array[0..MAX_HARDWARE_LIGHT - 1] of Single;

    FColorWriting: Boolean; // TODO: change to per draw buffer (FColorWriteMask)
    FStates: TGLStates;
    FListStates: array of TGLStateTypes;
    FCurrentList: TGLuint;
    FTextureMatrixIsIdentity: Boolean;
    FForwardContext: Boolean;

    // Vertex Array Data state
    FVertexArrayBinding: TGLuint;
    FVAOStates: array of TVAOStates;
    FTextureBufferBinding: TGLuint;
    FEnablePrimitiveRestart: TGLboolean;
    FPrimitiveRestartIndex: TGLuint;

    // Transformation state
    FViewPort: TVector4i;
    FDepthRange: array[0..1] of TGLclampd;
    FEnableClipDistance: array[0..7] of TGLboolean;
    FEnableDepthClamp: TGLboolean;

    // Coloring state
    FClampReadColor: TGLenum; // GL_FIXED_ONLY
    FProvokingVertex: TGLenum; // GL_LAST_VERTEX_CONVENTION

    // Rasterization state
    FPointSize: TGLfloat;
    FPointFadeThresholdSize: TGLfloat;
    FPointSpriteCoordOrigin: TGLenum; // GL_UPPER_LEFT
    FLineWidth: Single;
    FLineStippleFactor: TGLint;
    FLineStipplePattern: TGLushort;

    FEnableLineSmooth: TGLboolean;
    FEnableCullFace: TGLboolean;
    FCullFaceMode: TCullFaceMode;
    FFrontFace: TFaceWinding;
    FEnablePolygonSmooth: TGLboolean;
    FPolygonMode: TPolygonMode;
    FPolygonOffsetFactor: TGLfloat;
    FPolygonOffsetUnits: TGLfloat;
    FEnablePolygonOffsetPoint: TGLboolean;
    FEnablePolygonOffsetLine: TGLboolean;
    FEnablePolygonOffsetFill: TGLboolean;

    // Multisample state
    FEnableMultisample: TGLboolean;
    FEnableSampleAlphaToCoverage: TGLboolean;
    FEnableSampleAlphaToOne: TGLboolean;
    FEnableSampleCoverage: TGLboolean;
    FSampleCoverageValue: TGLfloat;
    FSampleCoverageInvert: TGLboolean;
    FEnableSampleMask: TGLboolean;
    FSampleMaskValue: array[0..15] of TGLbitfield;

    // Texture state
    FMaxTextureImageUnits: TGLuint;
    FTextureBinding: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1, TGLTextureTarget] of TGLuint;

    // Active texture state
    FActiveTexture: TGLint; // 0 .. Max_texture_units
    FActiveTextureEnabling: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1, TGLTextureTarget] of Boolean;

    // Pixel operation state
    FEnableScissorTest: TGLboolean;
    FScissorBox: TVector4i;

    FEnableStencilTest: TGLboolean;

    FStencilFunc: TStencilFunction;
    FStencilValueMask: TGLuint;
    FStencilRef: TGLint;
    FStencilFail: TStencilOp;
    FStencilPassDepthFail: TStencilOp;
    FStencilPassDepthPass: TStencilOp;

    FStencilBackFunc: TStencilFunction;
    FStencilBackValueMask: TGLuint;
    FStencilBackRef: TGLuint;
    FStencilBackFail: TStencilOp;
    FStencilBackPassDepthPass: TStencilOp;
    FStencilBackPassDepthFail: TStencilOp;

    FEnableDepthTest: TGLboolean;
    FDepthFunc: TDepthFunction;

    FEnableBlend: array[0..15] of TGLboolean;

    FBlendSrcRGB: TBlendFunction;
    FBlendSrcAlpha: TBlendFunction;
    FBlendDstRGB: TDstBlendFunction;
    FBlendDstAlpha: TDstBlendFunction;

    FBlendEquationRGB: TBlendEquation;
    FBlendEquationAlpha: TBlendEquation;
    FBlendColor: TVector;

    FEnableFramebufferSRGB: TGLboolean;
    FEnableDither: TGLboolean;
    FEnableColorLogicOp: TGLboolean;

    FLogicOpMode: TLogicOp;

    // Framebuffer control state
    FColorWriteMask: array[0..15] of TColorMask;
    FDepthWriteMask: TGLBoolean;
    FStencilWriteMask: TGLuint;
    FStencilBackWriteMask: TGLuint;
    FColorClearValue: TVector;
    FDepthClearValue: TGLfloat;
    FStencilClearValue: TGLuint;

    // Framebuffer state
    FDrawFrameBuffer: TGLuint;
    FReadFrameBuffer: TGLuint;

    // Renderbuffer state
    FRenderBuffer: TGLuint;

    // Pixels state
    FUnpackSwapBytes: TGLboolean;
    FUnpackLSBFirst: TGLboolean;
    FUnpackImageHeight: TGLuint;
    FUnpackSkipImages: TGLuint;
    FUnpackRowLength: TGLuint;
    FUnpackSkipRows: TGLuint;
    FUnpackSkipPixels: TGLuint;
    FUnpackAlignment: TGLuint;
    FPackSwapBytes: TGLboolean;
    FPackLSBFirst: TGLboolean;
    FPackImageHeight: TGLuint;
    FPackSkipImages: TGLuint;
    FPackRowLength: TGLuint;
    FPackSkipRows: TGLuint;
    FPackSkipPixels: TGLuint;
    FPackAlignment: TGLuint;

    FPixelPackBufferBinding: TGLuint;
    FPixelUnpackBufferBinding: TGLuint;

    // Program state
    FCurrentProgram: TGLuint;
    FUniformBufferBinding: TGLuint;

    // Vector + Geometry Shader state
    FCurrentVertexAttrib: array[0..15] of TVector;
    FEnableProgramPointSize: TGLboolean;

    // Transform Feedback state
    FTransformFeedbackBufferBinding: TGLuint;

    // Hints state
    FTextureCompressionHint: THintType;
    FPolygonSmoothHint: THintType;
    FFragmentShaderDerivitiveHint: THintType;
    FLineSmoothHint: THintType;
    FMultisampleFilterHint: THintType;

    // Misc state
    FCurrentQuery: array[TQueryType] of TGLuint;
    FCopyReadBufferBinding: TGLuint;
    FCopyWriteBufferBinding: TGLuint;
    FEnableTextureCubeMapSeamless: TGLboolean;
    FInsideList: Boolean;

  protected
    { Protected Declarations }
    // Vertex Array Data state
    procedure SetVertexArrayBinding(const Value: TGLuint);
    function GetArrayBufferBinding: TGLuint;
    procedure SetArrayBufferBinding(const Value: TGLuint);
    function GetElementBufferBinding: TGLuint;
    procedure SetElementBufferBinding(const Value: TGLuint);
    function GetEnableVertexAttribArray(I: TGLuint): TGLboolean;
    procedure SetEnableVertexAttribArray(I: TGLuint; Value: TGLboolean);
    function GetEnablePrimitiveRestart: TGLboolean;
    function GetPrimitiveRestartIndex: TGLuint;
    procedure SetEnablePrimitiveRestart(const enabled: TGLboolean);
    procedure SetPrimitiveRestartIndex(const index: TGLuint);
    procedure SetTextureBufferBinding(const Value: TGLuint);
    // Transformation state
    procedure SetViewPort(const Value: TVector4i);
    function GetEnableClipDistance(ClipDistance: Cardinal): TGLboolean;
    procedure SetEnableClipDistance(Index: Cardinal; const Value: TGLboolean);
    procedure SetDepthRangeFar(const Value: TGLclampd);
    procedure SetDepthRangeNear(const Value: TGLclampd);
    procedure SetEnableDepthClamp(const enabled: TGLboolean);
    // Coloring state
    procedure SetClampReadColor(const Value: TGLenum);
    procedure SetProvokingVertex(const Value: TGLenum);
    // Rasterization state
    procedure SetPointSize(const Value: TGLfloat);
    procedure SetPointFadeThresholdSize(const Value: TGLfloat);
    procedure SetPointSpriteCoordOrigin(const Value: TGLenum);
    procedure SetLineWidth(const Value: TGLfloat);
    procedure SetLineStippleFactor(const Value: TGLint);
    procedure SetLineStipplePattern(const Value: TGLushort);

    procedure SetEnableLineSmooth(const Value: TGLboolean);
    procedure SetEnableCullFace(const Value: TGLboolean);
    procedure SetCullFaceMode(const Value: TCullFaceMode);
    procedure SetFrontFace(const Value: TFaceWinding);
    procedure SetEnablePolygonSmooth(const Value: TGLboolean);
    procedure SetPolygonMode(const Value: TPolygonMode);
    procedure SetPolygonOffsetFactor(const Value: TGLfloat);
    procedure SetPolygonOffsetUnits(const Value: TGLfloat);
    procedure SetEnablePolygonOffsetPoint(const Value: TGLboolean);
    procedure SetEnablePolygonOffsetLine(const Value: TGLboolean);
    procedure SetEnablePolygonOffsetFill(const Value: TGLboolean);
    // Multisample state
    procedure SetEnableMultisample(const Value: TGLboolean);
    procedure SetEnableSampleAlphaToCoverage(const Value: TGLboolean);
    procedure SetEnableSampleAlphaToOne(const Value: TGLboolean);
    procedure SetEnableSampleCoverage(const Value: TGLboolean);
    procedure SetSampleCoverageValue(const Value: TGLfloat);
    procedure SetSampleCoverageInvert(const Value: TGLboolean);
    procedure SetEnableSampleMask(const Value: TGLboolean);
    function GetSampleMaskValue(Index: Integer): TGLbitfield;
    procedure SetSampleMaskValue(Index: Integer; const Value: TGLbitfield);
    // Texture state
    function GetMaxTextureImageUnits: Integer;
    function GetTextureBinding(Index: Integer; target: TGLTextureTarget):
      TGLuint;
    procedure SetTextureBinding(Index: Integer; target: TGLTextureTarget;
      const Value: TGLuint);
    function GetActiveTextureEnabled(Target: TGLTextureTarget): Boolean;
    procedure SetActiveTextureEnabled(Target: TGLTextureTarget; const Value:
      Boolean);
    // Active texture
    procedure SetActiveTexture(const Value: TGLint);
    // Pixel operations
    procedure SetEnableScissorTest(const Value: TGLboolean);
    procedure SetScissorBox(const Value: TVector4i);
    procedure SetEnableStencilTest(const Value: TGLboolean);
    procedure SetEnableDepthTest(const Value: TGLboolean);
    procedure SetDepthFunc(const Value: TDepthFunction);
    function GetEnableBlend(Index: Integer): TGLboolean;
    procedure SetEnableBlend(Index: Integer; const Value: TGLboolean);
    procedure SetBlendColor(const Value: TVector);
    procedure SetEnableFramebufferSRGB(const Value: TGLboolean);
    procedure SetEnableDither(const Value: TGLboolean);
    procedure SetEnableColorLogicOp(const Value: TGLboolean);
    procedure SetLogicOpMode(const Value: TLogicOp);
    // Framebuffer control
    function GetColorWriteMask(Index: Integer): TColorMask;
    procedure SetColorWriteMask(Index: Integer; const Value: TColorMask);
    procedure SetDepthWriteMask(const Value: TGLboolean);
    procedure SetStencilWriteMask(const Value: TGLuint);
    procedure SetStencilBackWriteMask(const Value: TGLuint);
    procedure SetColorClearValue(const Value: TVector);
    procedure SetDepthClearValue(const Value: TGLfloat);
    procedure SetStencilClearValue(const Value: TGLuint);
    // Framebuffer
    procedure SetDrawFrameBuffer(const Value: TGLuint);
    procedure SetReadFrameBuffer(const Value: TGLuint);
    // Renderbuffer
    procedure SetRenderBuffer(const Value: TGLuint);
    // Pixels
    procedure SetUnpackSwapBytes(const Value: TGLboolean);
    procedure SetUnpackLSBFirst(const Value: TGLboolean);
    procedure SetUnpackImageHeight(const Value: TGLuint);
    procedure SetUnpackSkipImages(const Value: TGLuint);
    procedure SetUnpackRowLength(const Value: TGLuint);
    procedure SetUnpackSkipRows(const Value: TGLuint);
    procedure SetUnpackSkipPixels(const Value: TGLuint);
    procedure SetUnpackAlignment(const Value: TGLuint);
    procedure SetPackSwapBytes(const Value: TGLboolean);
    procedure SetPackLSBFirst(const Value: TGLboolean);
    procedure SetPackImageHeight(const Value: TGLuint);
    procedure SetPackSkipImages(const Value: TGLuint);
    procedure SetPackRowLength(const Value: TGLuint);
    procedure SetPackSkipRows(const Value: TGLuint);
    procedure SetPackSkipPixels(const Value: TGLuint);
    procedure SetPackAlignment(const Value: TGLuint);
    procedure SetPixelPackBufferBinding(const Value: TGLuint);
    procedure SetPixelUnpackBufferBinding(const Value: TGLuint);
    // Program
    procedure SetCurrentProgram(const Value: TGLuint);
    procedure SetUniformBufferBinding(const Value: TGLuint);
    // Vector + Geometry Shader state
    function GetCurrentVertexAttrib(Index: Integer): TVector;
    procedure SetCurrentVertexAttrib(Index: Integer; const Value: TVector);
    procedure SetEnableProgramPointSize(const Value: TGLboolean);
    // Transform Feedback state
    procedure SetTransformFeedbackBufferBinding(const Value: TGLuint);
    // Hints
    procedure SetLineSmoothHint(const Value: THintType);
    procedure SetPolygonSmoothHint(const Value: THintType);
    procedure SetTextureCompressionHint(const Value: THintType);
    procedure SetFragmentShaderDerivitiveHint(const Value: THintType);
    procedure SetMultisampleFilterHint(const Value: THintType);
    // Misc
    function GetCurrentQuery(Index: TQueryType): TGLuint;
    //    procedure SetCurrentQuery(Index: TQueryType; const Value: TGLuint);
    procedure SetCopyReadBufferBinding(const Value: TGLuint);
    procedure SetCopyWriteBufferBinding(const Value: TGLuint);
    procedure SetEnableTextureCubeMapSeamless(const Value: TGLboolean);
    // Ligting
    function GetMaxLights: Integer;
    function GetLightEnabling(I: Integer): Boolean;
    procedure SetLightEnabling(I: Integer; Value: Boolean);
    function GetLightAmbient(I: Integer): TVector;
    procedure SetLightAmbient(I: Integer; const Value: TVector);
    function GetLightDiffuse(I: Integer): TVector;
    procedure SetLightDiffuse(I: Integer; const Value: TVector);
    function GetLightSpecular(I: Integer): TVector;
    procedure SetLightSpecular(I: Integer; const Value: TVector);
    function GetSpotCutoff(I: Integer): Single;
    procedure SetSpotCutoff(I: Integer; const Value: Single);
    function GetConstantAtten(I: Integer): Single;
    procedure SetConstantAtten(I: Integer; const Value: Single);
    function GetLinearAtten(I: Integer): Single;
    procedure SetLinearAtten(I: Integer; const Value: Single);
    function GetQuadAtten(I: Integer): Single;
    procedure SetQuadAtten(I: Integer; const Value: Single);
  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    procedure PushAttrib(stateTypes: TGLStateTypes);
    procedure PopAttrib();

    procedure Enable(const aState: TGLState);
    procedure Disable(const aState: TGLState);
    procedure PerformEnable(const aState: TGLState);
    procedure PerformDisable(const aState: TGLState);

    procedure SetGLState(const aState : TGLState); deprecated;
    procedure UnSetGLState(const aState : TGLState); deprecated;
    procedure ResetGLPolygonMode; deprecated;
    procedure ResetGLMaterialColors; deprecated;
    procedure ResetGLTexture(const TextureUnit: Integer); deprecated;
    procedure ResetGLCurrentTexture; deprecated;
    procedure SetGLFrontFaceCW; deprecated;
    procedure ResetGLFrontFace; deprecated;
    procedure ResetAll; deprecated;

    {: Adjusts material colors for a face. }
    procedure SetGLMaterialColors(const aFace: TCullFaceMode;
      const emission, ambient, diffuse, specular: TVector;
      const shininess: Integer);
    {: Adjusts material alpha channel for a face. }
    procedure SetGLMaterialAlphaChannel(const aFace: TGLEnum; const alpha:
      TGLFloat);

    {: Lighting states }
    property MaxLights: Integer read GetMaxLights;
    property LightEnabling[Index: Integer]: Boolean read GetLightEnabling write
    SetLightEnabling;
    property LightAmbient[Index: Integer]: TVector read GetLightAmbient write
    SetLightAmbient;
    property LightDiffuse[Index: Integer]: TVector read GetLightDiffuse write
    SetLightDiffuse;
    property LightSpecular[Index: Integer]: TVector read GetLightSpecular write
    SetLightSpecular;
    property LightSpotCutoff[Index: Integer]: Single read GetSpotCutoff write
    SetSpotCutoff;
    property LightConstantAtten[Index: Integer]: Single read GetConstantAtten
    write SetConstantAtten;
    property LightLinearAtten[Index: Integer]: Single read GetLinearAtten write
    SetLinearAtten;
    property LightQuadraticAtten[Index: Integer]: Single read GetQuadAtten write
    SetQuadAtten;

    {: Blending states }
    procedure SetGLAlphaFunction(func: TComparisonFunction; ref: TGLclampf);

    // Vertex Array Data state
    {: The currently bound array buffer (calling glVertexAttribPointer
       locks this buffer to the currently bound VBO). }
    property VertexArrayBinding: TGLuint read FVertexArrayBinding write
      SetVertexArrayBinding;
    {: Reset VAO states, tipicaly used when VAO created }
    procedure ResetVertexArrayStates(Index: TGLuint);
    {: The currently bound vertex buffer object (VAO). }
    property ArrayBufferBinding: TGLuint read GetArrayBufferBinding write
      SetArrayBufferBinding;
    {: The currently bound element buffer object (EBO). }
    property ElementBufferBinding: TGLuint read GetElementBufferBinding write
      SetElementBufferBinding;
    {: Enables/Disables vertex attribute arrays. }
    property EnableVertexAttribArray[Index: TGLuint]: TGLBoolean read
    GetEnableVertexAttribArray write SetEnableVertexAttribArray;
    {: Determines whether primitive restart is turned on or off. }
    property EnablePrimitiveRestart: TGLboolean read GetEnablePrimitiveRestart
      write SetEnablePrimitiveRestart;
    {: The index Value that causes a primitive restart. }
    property PrimitiveRestartIndex: TGLuint read GetPrimitiveRestartIndex write
      SetPrimitiveRestartIndex;
    {: The currently bound texture buffer object (TBO). }
    property TextureBufferBinding: TGLuint read FTextureBufferBinding write
      SetTextureBufferBinding;

    // Transformation state
    {: The viewport. }
    property ViewPort: TVector4i read FViewPort write SetViewPort;
    {: Modifies the near + far clipping planes. }
    procedure SetDepthRange(const ZNear, ZFar: TGLclampd);
    {: The near clipping plane distance. }
    property DepthRangeNear: TGLclampd read FDepthRange[0] write
      SetDepthRangeNear;
    {: The far clipping plane distance. }
    property DepthRangeFar: TGLclampd read FDepthRange[1] write
      SetDepthRangeFar;
    {: Enables/Disables each of the clip distances, used in shaders. }
    property EnableClipDistance[Index: Cardinal]: TGLboolean read
    GetEnableClipDistance write SetEnableClipDistance;
    {: Enables/Disables depth clamping. }
    property EnableDepthClamp: TGLboolean read FEnableDepthClamp write
      SetEnableDepthClamp;

    // Coloring state
    {: Controls read color clamping. }
    property ClampReadColor: TGLenum read FClampReadColor write
      SetClampReadColor;
    {: The provoking vertex used in flat shading.  All the vertices of each
       primitive will the same value determined by this property. }
    property ProvokingVertex: TGLenum read FProvokingVertex write
      SetProvokingVertex;

    // Rasterization state
    {: The default point size, used when EnableProgramPointSize = false. }
    property PointSize: TGLfloat read FPointSize write SetPointSize;
    {: If multisampling is enabled, this can control when points are faded out.}
    property PointFadeThresholdSize: TGLfloat read FPointFadeThresholdSize write
      SetPointFadeThresholdSize;
    {: The texture coordinate origin of point sprites. }
    property PointSpriteCoordOrigin: TGLenum read FPointSpriteCoordOrigin write
      SetPointSpriteCoordOrigin;
    {: The line width. }
    property LineWidth: TGLfloat read FLineWidth write SetLineWidth;
    {: The line stipple. }
    property LineStippleFactor: TGLint read FLineStippleFactor write
      SetLineStippleFactor;
    {: The line stipple. }
    property LineStipplePattern: TGLushort read FLineStipplePattern write
      SetLineStipplePattern;
    {: Enable/Disable line smoothing. }
    property EnableLineSmooth: TGLboolean read FEnableLineSmooth write
      SetEnableLineSmooth;
    {: Enable/Disable face culling. }
    property EnableCullFace: TGLboolean read FEnableCullFace write
      SetEnableCullFace;
    {: Selects which faces to cull: front, back or front+back.}
    property CullFaceMode: TCullFaceMode read FCullFaceMode write
      SetCullFaceMode;
    {: The winding direction that indicates a front facing primitive. }
    property FrontFace: {TGLenum} TFaceWinding read FFrontFace write
    SetFrontFace;
    // Enables/Disables polygon smoothing.
    property EnablePolygonSmooth: TGLboolean read FEnablePolygonSmooth write
      SetEnablePolygonSmooth;
    {: Whether polygons appear filled, lines or points. }
    property PolygonMode: TPolygonMode read FPolygonMode write SetPolygonMode;
    {: Scales the maximum depth of the polygon. }
    property PolygonOffsetFactor: TGLfloat read FPolygonOffsetFactor write
      SetPolygonOffsetFactor;
    {: Scales an implementation-dependent constant that relates to the usable
       resolution of the depth buffer. }
    property PolygonOffsetUnits: TGLfloat read FPolygonOffsetUnits write
      SetPolygonOffsetUnits;
    {: Set polygon offset. }
    procedure SetPolygonOffset(const factor, units: TGLfloat);
    {: Enable/Disable polygon offset for polygons in point mode. }
    property EnablePolygonOffsetPoint: TGLboolean read FEnablePolygonOffsetPoint
      write SetEnablePolygonOffsetPoint;
    {: Enable/Disable polygon offset for polygons in line mode. }
    property EnablePolygonOffsetLine: TGLboolean read FEnablePolygonOffsetLine
      write SetEnablePolygonOffsetLine;
    {: Enable/Disable polygon offset for polygons in fill mode. }
    property EnablePolygonOffsetFill: TGLboolean read FEnablePolygonOffsetFill
      write SetEnablePolygonOffsetFill;

    // Multisample state
    {: Enable/Disable multisampling. }
    property EnableMultisample: TGLboolean read FEnableMultisample write
      SetEnableMultisample;
    {: Enable/Disable sample alpha to coverage. }
    property EnableSampleAlphaToCoverage: TGLboolean read
      FEnableSampleAlphaToCoverage write SetEnableSampleAlphaToCoverage;
    {: Enable/Disable sample alpha to one. }
    property EnableSampleAlphaToOne: TGLboolean read FEnableSampleAlphaToOne
      write SetEnableSampleAlphaToOne;
    {: Enable/Disable sample coverage. }
    property EnableSampleCoverage: TGLboolean read FEnableSampleCoverage write
      SetEnableSampleCoverage;
    {: Sample coverage Value. }
    property SampleCoverageValue: TGLfloat read FSampleCoverageValue write
      SetSampleCoverageValue;
    {: Inverts sample coverage Value. }
    property SampleCoverageInvert: TGLboolean read FSampleCoverageInvert write
      SetSampleCoverageInvert;
    {: Set sample coverage. }
    procedure SetSampleCoverage(const Value: TGLfloat; invert: TGLboolean);
    {: Enable/Disable sample mask. }
    property EnableSampleMask: TGLboolean read FEnableSampleMask write
      SetEnableSampleMask;
    {: Sample mask values. }
    property SampleMaskValue[Index: Integer]: TGLbitfield read GetSampleMaskValue
    write SetSampleMaskValue;

    // Textures
    {: Textures bound to each texture unit + binding point. }
    property TextureBinding[Index: Integer; target: TGLTextureTarget]: TGLuint
    read GetTextureBinding write SetTextureBinding;
    property ActiveTextureEnabled[Target: TGLTextureTarget]: Boolean read
    GetActiveTextureEnabled write SetActiveTextureEnabled;
    property MaxTextureImageUnits: Integer read GetMaxTextureImageUnits;
    // TODO: GL_TEXTURE_BUFFER_DATA_STORE_BINDING ?

    // Active texture
    {: The active texture unit.  Valid values are 0 .. Max texture units. }
    property ActiveTexture: TGLint read FActiveTexture write SetActiveTexture;

    // Pixel operations
    {: Enables/Disables scissor test. }
    property EnableScissorTest: TGLboolean read FEnableScissorTest write
      SetEnableScissorTest;
    {: The bounding box used in scissor test. }
    property ScissorBox: TVector4i read FScissorBox write SetScissorBox;
    {: Enables/Disables stencil test. }
    property EnableStencilTest: TGLboolean read FEnableStencilTest write
      SetEnableStencilTest;
    {: The stencil function.  Determines the comparison function to be used
       when comparing the reference + stored stencil values.  }
    property StencilFunc: TStencilFunction read FStencilFunc;
    // write SetStencilFunc;
  {: The stencil value mask.  Masks both the reference + stored stencil
     values. }
    property StencilValueMask: TGLuint read FStencilValueMask;
    // write SetStencilValueMask;
  {: The stencil reference value.  Clamped to 0..255 with an 8 bit stencil. }
    property StencilRef: TGLint read FStencilRef; // write SetStencilRef;
    {: The operation to perform when stencil test fails. }
    property StencilFail: TStencilOp read FStencilFail; // write SetStencilFail;
    {: The operation to perform when stencil test passes + depth test fails. }
    property StencilPassDepthFail: TStencilOp read FStencilPassDepthFail;
    // write SetStencilPassDepthFail;
  {: The operation to perform when stencil test passes + depth test passes. }
    property StencilPassDepthPass: TStencilOp read FStencilPassDepthPass;
    // write SetStencilPassDepthPass;

  {: The stencil back function.  Determines the comparison function to be
     used when comparing the reference + stored stencil values on back
     facing primitives. }
    property StencilBackFunc: TStencilFunction read FStencilBackFunc;
    // write SetStencilBackFunc;
  {: The stencil back value mask.  Masks both the reference + stored stencil
     values. }
    property StencilBackValueMask: TGLuint read FStencilBackValueMask;
    // write SetStencilBackValueMask;
  {: The stencil back reference value.  Clamped to 0..255 with an 8 bit
     stencil. }
    property StencilBackRef: TGLuint read FStencilBackRef;
    // write SetStencilBackRef;
  {: The operation to perform when stencil test fails on back facing
     primitives. }
    property StencilBackFail: TStencilOp read FStencilBackFail;
    // write SetStencilBackFail;
  {: The operation to perform when stencil test passes + depth test fails on
     back facing primitives. }
    property StencilBackPassDepthFail: TStencilOp read
      FStencilBackPassDepthFail;
    // write SetStencilBackPassDepthFail;
  {: The operation to perform when stencil test passes + depth test passes on
     back facing primitives. }
    property StencilBackPassDepthPass: TStencilOp read
      FStencilBackPassDepthPass;
    // write SetStencilBackPassDepthPass;
  {: Used to set stencil Function, Reference + Mask values, for both front +
     back facing primitives. }
    procedure SetStencilFunc(const func: TStencilFunction; const ref: TGLint;
      const mask: TGLuint);
    {: Used to set stencil Function, Reference + Mask values for either the
       front or back facing primitives (or both, which is the same as calling
       SetStencilFunc). }
    procedure SetStencilFuncSeparate(const face: TCullFaceMode;
      const func: TStencilFunction; const ref: TGLint; const mask: TGLuint);
    {: Used to set the StencilFail, StencilPassDepthFail + StencilPassDepthPass
       in one go. }
    procedure SetStencilOp(const fail, zfail, zpass: TStencilOp);
    {: Used to set the StencilFail, StencilPassDepthFail + StencilPassDepthPass
       in one go, for either front or back facing primitives. }
    procedure SetStencilOpSeparate(const face: TCullFaceMode; const sfail,
      dpfail, dppass: TStencilOp);

    {: Enables/disables depth testing. }
    property EnableDepthTest: TGLboolean read FEnableDepthTest write
      SetEnableDepthTest;
    {: The depth function.  Used to determine whether to keep a fragment or
       discard it, depending on the current value stored in the depth buffer. }
    property DepthFunc: TDepthFunction read FDepthFunc write SetDepthFunc;
    {: Enables/disables blending for each draw buffer. }
    property EnableBlend[Index: Integer]: TGLboolean read GetEnableBlend write
    SetEnableBlend;
    {: The weighting factor used in blending equation, for source RGB. }
    property BlendSrcRGB: TBlendFunction read FBlendSrcRGB;
    // write SetBlendSrcRGB;
  {: The weighting factor used in blending equation, for source alpha. }
    property BlendSrcAlpha: TBlendFunction read FBlendSrcAlpha;
    // write SetBlendSrcAlpha;
  {: The weighting factor used in blending equation, for destination RGB. }
    property BlendDstRGB: TDstBlendFunction read FBlendDstRGB;
    // write SetBlendDstRGB;
  {: The weighting factor used in blending equation, for destination alpha. }
    property BlendDstAlpha: TDstBlendFunction read FBlendDstAlpha;
    // write SetBlendDstAlpha;
  {: Sets the weighting factors to be used by the blending equation, for
     both color + alpha. }
    procedure SetBlendFunc(const Src: TBlendFunction;
      const Dst: TDstBlendFunction);
    {: Sets the weighting factors to be used by the blending equation, with
       separate values used for color + alpha components. }
    procedure SetBlendFuncSeparate(const SrcRGB: TBlendFunction;
      const DstRGB: TDstBlendFunction; const SrcAlpha: TBlendFunction;
      const DstAlpha: TDstBlendFunction);
    {: The blending equation.  Determines how the incoming source fragment's
       RGB are combined with the destination RGB. }
    property BlendEquationRGB: TBlendEquation read FBlendEquationRGB;
    // write SetBlendEquationRGB;
  {: The blending equation.  Determines how the incoming source fragment's
     alpha values are combined with the destination alpha values. }
    property BlendEquationAlpha: TBlendEquation read FBlendEquationAlpha;
    // write SetBlendEquationAlpha;
  {: Sets the blend equation for RGB + alpha to the same value. }
    procedure SetBlendEquation(const mode: TBlendEquation);
    {: Sets the blend equations for RGB + alpha separately. }
    procedure SetBlendEquationSeparate(const modeRGB, modeAlpha:
      TBlendEquation);
    {: A constant blend color, that can be used in the blend equation. }
    property BlendColor: TVector read FBlendColor write SetBlendColor;
    {: Enables/disables framebuffer SRGB. }
    property EnableFramebufferSRGB: TGLboolean read FEnableFramebufferSRGB write
      SetEnableFramebufferSRGB;
    {: Enables/disables dithering. }
    property EnableDither: TGLboolean read FEnableDither write SetEnableDither;
    {: Enables/disables color logic op. }
    property EnableColorLogicOp: TGLboolean read FEnableColorLogicOp write
      SetEnableColorLogicOp;
    {: Logic op mode. }
    property LogicOpMode: TLogicOp read FLogicOpMode write SetLogicOpMode;

    // Framebuffer control
    {: The color write mask, for each draw buffer. }
    property ColorWriteMask[Index: Integer]: TColorMask read GetColorWriteMask
    write SetColorWriteMask;
    {: Set the color write mask for all draw buffers. }
    procedure SetColorMask(mask: TColorMask);
    {: The depth write mask. }
    property DepthWriteMask: TGLBoolean read FDepthWriteMask write
      SetDepthWriteMask;
    {: The stencil write mask. }
    property StencilWriteMask: TGLuint read FStencilWriteMask write
      SetStencilWriteMask;
    {: The stencil back write mask. }
    property StencilBackWriteMask: TGLuint read FStencilBackWriteMask write
      SetStencilBackWriteMask;
    {: The color clear value. }
    property ColorClearValue: TVector read FColorClearValue write
      SetColorClearValue;
    {: The depth clear value. }
    property DepthClearValue: TGLfloat read FDepthClearValue write
      SetDepthClearValue;
    {: The stencil clear value. }
    property StencilClearValue: TGLuint read FStencilClearValue write
      SetStencilClearValue;

    // Framebuffer
    {: Framebuffer to be used for draw operations, 0 = default framebuffer. }
    property DrawFrameBuffer: TGLuint read FDrawFrameBuffer write
      SetDrawFrameBuffer;
    {: Framebuffer to be used for read operations, 0 = default framebuffer. }
    property ReadFrameBuffer: TGLuint read FReadFrameBuffer write
      SetReadFrameBuffer;
    {: set both draw + read framebuffer. }
    procedure SetFrameBuffer(const Value: TGLuint);
    //property FrameBuffer: TGLuint read FDrawFrameBuffer write SetFrameBuffer;

    // Renderbuffer
    {: Currently bound render buffer. }
    property RenderBuffer: TGLuint read FRenderBuffer write SetRenderBuffer;

    // Pixels
    {: Controls whether byte swapping occurs during pixel unpacking. }
    property UnpackSwapBytes: TGLboolean read FUnpackSwapBytes write
      SetUnpackSwapBytes;
    {: Whether unpacked data is required with LSB (least significant bit) first. }
    property UnpackLSBFirst: TGLboolean read FUnpackLSBFirst write
      SetUnpackLSBFirst;
    {: Unpack image height. }
    property UnpackImageHeight: TGLuint read FUnpackImageHeight write
      SetUnpackImageHeight;
    {: Unpack skip images. }
    property UnpackSkipImages: TGLuint read FUnpackSkipImages write
      SetUnpackSkipImages;
    {: Unpack row length. }
    property UnpackRowLength: TGLuint read FUnpackRowLength write
      SetUnpackRowLength;
    {: Unpack skip rows. }
    property UnpackSkipRows: TGLuint read FUnpackSkipRows write
      SetUnpackSkipRows;
    {: Unpack skip pixels. }
    property UnpackSkipPixels: TGLuint read FUnpackSkipPixels write
      SetUnpackSkipPixels;
    {: Unpack alignment. }
    property UnpackAlignment: TGLuint read FUnpackAlignment write
      SetUnpackAlignment;
    {: Controls whether byte swapping occurs during pixel packing. }
    property PackSwapBytes: TGLboolean read FPackSwapBytes write
      SetPackSwapBytes;
    {: Whether packed data is required with LSB (least significant bit) first. }
    property PackLSBFirst: TGLboolean read FPackLSBFirst write SetPackLSBFirst;
    {: Pack image height. }
    property PackImageHeight: TGLuint read FPackImageHeight write
      SetPackImageHeight;
    {: Pack skip images. }
    property PackSkipImages: TGLuint read FPackSkipImages write
      SetPackSkipImages;
    {: Pack row length. }
    property PackRowLength: TGLuint read FPackRowLength write SetPackRowLength;
    {: Pack skip rows. }
    property PackSkipRows: TGLuint read FPackSkipRows write SetPackSkipRows;
    {: Pack skip pixels. }
    property PackSkipPixels: TGLuint read FPackSkipPixels write
      SetPackSkipPixels;
    {: Pack alignment. }
    property PackAlignment: TGLuint read FPackAlignment write SetPackAlignment;
    {: Buffer bound for pixel packing (eg. ReadPixels). }
    property PixelPackBufferBinding: TGLuint read FPixelPackBufferBinding
      write SetPixelPackBufferBinding;
    {: Buffer bound for pixel unpacking (eg. Tex*Image). }
    property PixelUnpackBufferBinding: TGLuint read FPixelUnpackBufferBinding
      write SetPixelUnpackBufferBinding;

    // Program
    {: Currently bound program. }
    property CurrentProgram: TGLuint read FCurrentProgram write
      SetCurrentProgram;
    {: Currently bound uniform buffer. }
    property UniformBufferBinding: TGLuint read FUniformBufferBinding
      write SetUniformBufferBinding;

    // Vector + Geometry Shader state
    {: Default values to be used when a vertex array is not used for that
       attribute. }
    property CurrentVertexAttrib[Index: Integer]: TVector
    read GetCurrentVertexAttrib write SetCurrentVertexAttrib;
    {: Enables/disables program point size. }
    property EnableProgramPointSize: TGLboolean read FEnableProgramPointSize
      write SetEnableProgramPointSize;

    // Transform Feedback state
    {: Currently bound transform feedbac buffer. }
    property TransformFeedbackBufferBinding: TGLuint
      read FTransformFeedbackBufferBinding write
      SetTransformFeedbackBufferBinding;

    // Hints
    {: Line smooth hint. }
    property LineSmoothHint: THintType read FLineSmoothHint write
      SetLineSmoothHint;
    {: Polygon smooth hint. }
    property PolygonSmoothHint: THintType read FPolygonSmoothHint write
      SetPolygonSmoothHint;
    {: Texture compression hint. }
    property TextureCompressionHint: THintType
      read FTextureCompressionHint write SetTextureCompressionHint;
    {: Fragment shader derivitive hint. }
    property FragmentShaderDerivitiveHint: THintType
      read FFragmentShaderDerivitiveHint write SetFragmentShaderDerivitiveHint;
    property MultisampleFilterHint: THintType read FMultisampleFilterHint
      write SetMultisampleFilterHint;

    // Misc
    {: Current queries. }
    property CurrentQuery[Index: TQueryType]: TGLuint read GetCurrentQuery;
    {: Begins a query of "Target" type.  "Value" must be a valid query object. }
    procedure BeginQuery(const Target: TQueryType; const Value: TGLuint);
    {: Ends current query of type "Target". }
    procedure EndQuery(const Target: TQueryType);
    {: The buffer currently bound to the copy read buffer binding point, this
       is an extra binding point provided so that you don't need to overwrite
       other binding points to copy between buffers. }
    property CopyReadBufferBinding: TGLuint read FCopyReadBufferBinding
      write SetCopyReadBufferBinding;
    {: The buffer currently bound to the copy write buffer binding point, this
       is an extra binding point provided so that you don't need to overwrite
       other binding points to copy between buffers. }
    property CopyWriteBufferBinding: TGLuint read FCopyWriteBufferBinding
      write SetCopyWriteBufferBinding;
    {: Enables/Disables seamless texture cube maps. }
    property EnableTextureCubeMapSeamless: TGLboolean read
      FEnableTextureCubeMapSeamless write SetEnableTextureCubeMapSeamless;
    {: Indicates the current presence within the list. }
    property InsideList: Boolean read FInsideList;
    {: Begin new display list. }
    procedure NewList(list: TGLuint; mode: TGLEnum);
    {: End display list. }
    procedure EndList;
    {: Call display list. }
    procedure CallList(list: TGLuint);

    {: Defines the OpenGL texture matrix.<p>
       Assumed texture mode is GL_MODELVIEW. }
    procedure SetGLTextureMatrix(const matrix: TMatrix);
    procedure ResetGLTextureMatrix;

    // note: needs to change to per draw-buffer
    procedure SetGLColorWriting(flag: Boolean);

    {: Inverts front face winding (CCW/CW). }
    procedure InvertGLFrontFace;

    // read only properties
    property States: TGLStates read FStates;

    {: True for ignore deprecated and removed features in OpenGL 3x }
    property ForwardContext: Boolean read FForwardContext
      write FForwardContext;
  end;

type
  TStateRecord = record
    GLConst: TGLEnum;
    GLDeprecated: Boolean;
  end;

const
{$WARN SYMBOL_DEPRECATED OFF}
  cGLStateTypeToGLEnum: array[TGLStateType] of TGLenum = (
    GL_CURRENT_BIT, GL_POINT_BIT, GL_LINE_BIT, GL_POLYGON_BIT,
    GL_POLYGON_STIPPLE_BIT, GL_PIXEL_MODE_BIT, GL_LIGHTING_BIT, GL_FOG_BIT,
    GL_DEPTH_BUFFER_BIT, GL_ACCUM_BUFFER_BIT, GL_STENCIL_BUFFER_BIT,
    GL_VIEWPORT_BIT, GL_TRANSFORM_BIT, GL_ENABLE_BIT, GL_COLOR_BUFFER_BIT,
    GL_HINT_BIT, GL_EVAL_BIT, GL_LIST_BIT, GL_TEXTURE_BIT, GL_SCISSOR_BIT,
    GL_MULTISAMPLE_BIT);

{$WARN SYMBOL_DEPRECATED ON}
  cGLStateToGLEnum: array[TGLState] of TStateRecord =
    ((GLConst: GL_ALPHA_TEST; GLDeprecated: True),
    (GLConst: GL_AUTO_NORMAL; GLDeprecated: True),
    (GLConst: GL_BLEND; GLDeprecated: False),
    (GLConst: GL_COLOR_MATERIAL; GLDeprecated: True),
    (GLConst: GL_CULL_FACE; GLDeprecated: False),
    (GLConst: GL_DEPTH_TEST; GLDeprecated: False),
    (GLConst: GL_DITHER; GLDeprecated: False),
    (GLConst: GL_FOG; GLDeprecated: True),
    (GLConst: GL_LIGHTING; GLDeprecated: True),
    (GLConst: GL_LINE_SMOOTH; GLDeprecated: True),
    (GLConst: GL_LINE_STIPPLE; GLDeprecated: True),
    (GLConst: GL_INDEX_LOGIC_OP; GLDeprecated: True),
    (GLConst: GL_COLOR_LOGIC_OP; GLDeprecated: False),
    (GLConst: GL_NORMALIZE; GLDeprecated: True),
    (GLConst: GL_POINT_SMOOTH; GLDeprecated: True),
    (GLConst: GL_POINT_SPRITE; GLDeprecated: True),
    (GLConst: GL_POLYGON_SMOOTH; GLDeprecated: True),
    (GLConst: GL_POLYGON_STIPPLE; GLDeprecated: True),
    (GLConst: GL_SCISSOR_TEST; GLDeprecated: False),
    (GLConst: GL_STENCIL_TEST; GLDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_POINT; GLDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_LINE; GLDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_FILL; GLDeprecated: False),
    (GLConst: GL_DEPTH_CLAMP; GLDeprecated: False)
    );

  cGLTexTypeToGLEnum: array[TGLTextureTarget] of TGLenum =
    (GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY,
    GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_BUFFER,
    GL_TEXTURE_CUBE_MAP, GL_TEXTURE_2D_MULTISAMPLE,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_CUBE_MAP_ARRAY);

  cGLQueryTypeToGLEnum: array[TQueryType] of TGLenum =
    (GL_SAMPLES_PASSED, GL_PRIMITIVES_GENERATED,
    GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN,
    GL_TIME_ELAPSED_EXT);

  cGLStencilOpToGLEnum: array[TStencilOp] of TGLenum =
    (GL_KEEP, GL_ZERO, GL_REPLACE, GL_INCR, GL_DECR, GL_INVERT, GL_INCR_WRAP,
    GL_DECR_WRAP);

  cGLLogicOpToGLEnum: array[TLogicOp] of TGLEnum =
    (GL_CLEAR, GL_AND, GL_AND_REVERSE, GL_COPY, GL_AND_INVERTED, GL_NOOP,
    GL_XOR, GL_OR, GL_NOR, GL_EQUIV, GL_INVERT, GL_OR_REVERSE,
    GL_COPY_INVERTED, GL_OR_INVERTED, GL_NAND, GL_SET);

  cGLComparisonFunctionToGLEnum: array[TComparisonFunction] of TGLenum =
    (GL_NEVER, GL_ALWAYS, GL_LESS, GL_LEQUAL, GL_EQUAL, GL_GREATER,
    GL_NOTEQUAL, GL_GEQUAL);

  cGLBlendFunctionToGLEnum: array[TBlendFunction] of TGLenum =
    (GL_ZERO, GL_ONE, GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR, GL_DST_COLOR,
    GL_ONE_MINUS_DST_COLOR, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA,
    GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA, GL_CONSTANT_COLOR,
    GL_ONE_MINUS_CONSTANT_COLOR, GL_CONSTANT_ALPHA,
    GL_ONE_MINUS_CONSTANT_ALPHA, GL_SRC_ALPHA_SATURATE {valid for src only});

  cGLBlendEquationToGLEnum: array[TBlendEquation] of TGLEnum =
    (GL_FUNC_ADD, GL_FUNC_SUBTRACT, GL_FUNC_REVERSE_SUBTRACT, GL_MIN,
    GL_MAX);

  cGLFaceWindingToGLEnum: array[TFaceWinding] of TGLenum =
    (GL_CCW, GL_CW);

  cGLPolygonModeToGLEnum: array[TPolygonMode] of TGLEnum =
    (GL_FILL, GL_LINE, GL_POINT);

  cGLCullFaceModeToGLEnum: array[TCullFaceMode] of TGLEnum =
    (GL_FRONT, GL_BACK, GL_FRONT_AND_BACK);

  cGLHintToGLEnum: array[THintType] of TGLEnum =
    (GL_DONT_CARE, GL_FASTEST, GL_NICEST);
  //------------------------------------------------------
  //------------------------------------------------------
  //------------------------------------------------------
implementation
//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------

uses
  GLContext, GLColor;

resourcestring
  glsStateCashMissing = 'States cash missing: ';

  // ------------------
  // ------------------ TGLStateCache ------------------
  // ------------------

procedure TGLStateCache.BeginQuery(const Target: TQueryType; const Value:
  TGLuint);
begin
  Assert(FCurrentQuery[Target] = 0, 'Can only have one query (of each type)' +
    ' running at a time');
  // Assert(glIsQuery(Value), 'Not a valid query');
 //  if Value<>FCurrentQuery[Target] then
  begin
    FCurrentQuery[Target] := Value;
    GL.BeginQuery(cGLQueryTypeToGLEnum[Target], Value);
  end;
end;

// Create
//

constructor TGLStateCache.Create;
var
  I: Integer;
begin
  inherited;
  SetLength(FListStates, 128);
  FCurrentList := 0;

  // Material colors
  FFrontBackColors[0][0] := clrBlack;
  FFrontBackColors[0][1] := clrGray20;
  FFrontBackColors[0][2] := clrGray80;
  FFrontBackColors[0][3] := clrBlack;
  FFrontBackShininess[0] := 0;

  FFrontBackColors[1][0] := clrBlack;
  FFrontBackColors[1][1] := clrGray20;
  FFrontBackColors[1][2] := clrGray80;
  FFrontBackColors[1][3] := clrBlack;
  FFrontBackShininess[1] := 0;

  // Lighting
  FMaxLights := 0;
  for I := 0 to High(FLightEnabling) do
  begin
    FLightEnabling[I] := False;
    FLightAmbient[I] := clrBlack;
    FLightDiffuse[I] := clrWhite;
    FLightSpecular[I] := clrBlack;
    FSpotCutoff[I] := 180;
    FConstantAtten[I] := 1;
    FLinearAtten[I] := 0;
    FQuadAtten[I] := 0;
  end;

  FTextureMatrixIsIdentity := False;
  FForwardContext := False;

  // Vertex Array Data state
  FVertexArrayBinding := 0;
  SetLength(FVAOStates, 128);
  FTextureBufferBinding := 0;

  // Transformation state
  // FViewPort := Rect(0,0,0,0);  // (0, 0, Width, Height)
  FDepthRange[0] := 0.0;
  FDepthRange[1] := 1.0;

  FillChar(FEnableClipDistance, sizeof(FEnableClipDistance), $00);
  FEnableDepthClamp := false;

  // Coloring state
  FClampReadColor := GL_FIXED_ONLY;
  FProvokingVertex := GL_LAST_VERTEX_CONVENTION;

  // Rasterization state
  FPointSize := 1.0;
  FPointFadeThresholdSize := 1.0;
  FPointSpriteCoordOrigin := GL_UPPER_LEFT;
  FLineWidth := 1.0;
  FLineStippleFactor := 1;
  FLineStipplePattern := $FFFF;
  FEnableLineSmooth := false;
  FEnableCullFace := false;
  FCullFaceMode := cmBack;
  FFrontFace := fwCounterClockWise;
  FEnablePolygonSmooth := false;
  FPolygonMode := pmFill;
  FPolygonOffsetFactor := 0.0;
  FPolygonOffsetUnits := 0.0;
  FEnablePolygonOffsetPoint := false;
  FEnablePolygonOffsetLine := false;
  FEnablePolygonOffsetFill := false;

  // Multisample state
  FEnableMultisample := true;
  FEnableSampleAlphaToCoverage := false;
  FEnableSampleAlphaToOne := false;
  FEnableSampleCoverage := false;
  FSampleCoverageValue := 1.0;
  FSampleCoverageInvert := false;
  FEnableSampleMask := false;
  FillChar(FSampleMaskValue, sizeof(FSampleMaskValue), $FF);

  // Texture state
  FillChar(FTextureBinding, sizeof(FTextureBinding), $00);
  FillChar(FActiveTextureEnabling, sizeof(FActiveTextureEnabling), $00);

  // Active texture state
  FActiveTexture := 0;

  // Pixel operation state
  FEnableScissorTest := false;
  //    FScissorBox := Rect(0, 0, Width, Height);
  FEnableStencilTest := false;
  FStencilFunc := cfAlways;
  FStencilValueMask := $FFFFFFFF;
  FStencilRef := 0;
  FStencilFail := soKeep;
  FStencilPassDepthFail := soKeep;
  FStencilPassDepthPass := soKeep;

  FStencilBackFunc := cfAlways;
  FStencilBackValueMask := $FFFFFFFF;
  FStencilBackRef := 0;
  FStencilBackFail := soKeep;
  FStencilBackPassDepthPass := soKeep;
  FStencilBackPassDepthFail := soKeep;

  FEnableDepthTest := false;
  FDepthFunc := cfLess;

  FillChar(FEnableBlend, sizeof(FEnableBlend), $0);

  FBlendSrcRGB := bfOne;
  FBlendSrcAlpha := bfOne;
  FBlendDstRGB := bfZero;
  FBlendDstAlpha := bfZero;

  FBlendEquationRGB := beAdd;
  FBlendEquationAlpha := beAdd;
  FBlendColor := NullHmgVector;

  FEnableFramebufferSRGB := false;
  FEnableDither := true;
  FEnableColorLogicOp := false;

  FLogicOpMode := loCopy;

  // Framebuffer control state
//    for I := 0 to Length(FColorWriteMask) - 1 do
//      FColorWriteMask[i] := [ccRed, ccGreen, ccBlue, ccAlpha];
  FillChar(FColorWriteMask, sizeof(FColorWriteMask), $F);
  FDepthWriteMask := True;
  FStencilWriteMask := $FFFFFFFF;
  FStencilBackWriteMask := $FFFFFFFF;
  FColorClearValue := NullHmgVector;
  FDepthClearValue := 1.0;
  FStencilClearValue := 0;

  // Framebuffer state
  FDrawFrameBuffer := 0;
  FReadFrameBuffer := 0;

  // Renderbuffer state
  FRenderBuffer := 0;

  // Pixels state
  FUnpackSwapBytes := false;
  FUnpackLSBFirst := false;
  FUnpackImageHeight := 0;
  FUnpackSkipImages := 0;
  FUnpackRowLength := 0;
  FUnpackSkipRows := 0;
  FUnpackSkipPixels := 0;
  FUnpackAlignment := 4;
  FPackSwapBytes := False;
  FPackLSBFirst := False;
  FPackImageHeight := 0;
  FPackSkipImages := 0;
  FPackRowLength := 0;
  FPackSkipRows := 0;
  FPackSkipPixels := 0;
  FPackAlignment := 4;

  FPixelPackBufferBinding := 0;
  FPixelUnpackBufferBinding := 0;

  // Program state
  FCurrentProgram := 0;
  FUniformBufferBinding := 0;

  // Vector + Geometry Shader state
  for I := 0 to Length(FCurrentVertexAttrib) - 1 do
    FCurrentVertexAttrib[I] := NullHmgPoint;
  FEnableProgramPointSize := false;

  // Transform Feedback state
  FTransformFeedbackBufferBinding := 0;

  // Hints state
  FTextureCompressionHint := hintDontCare;
  FPolygonSmoothHint := hintDontCare;
  FFragmentShaderDerivitiveHint := hintDontCare;
  FLineSmoothHint := hintDontCare;

  // Misc state
  FillChar(FCurrentQuery, sizeof(FCurrentQuery), $00);
  FCopyReadBufferBinding := 0;
  FCopyWriteBufferBinding := 0;
  FEnableTextureCubeMapSeamless := false;
  FInsideList := False;
end;

// Destroy
//

destructor TGLStateCache.Destroy;
begin
  inherited;
end;

procedure TGLStateCache.EndQuery(const Target: TQueryType);
begin
  Assert(FCurrentQuery[Target] <> 0, 'No query running');
  FCurrentQuery[Target] := 0;
  GL.EndQuery(cGLQueryTypeToGLEnum[Target]);
end;

// Enable
//

procedure TGLStateCache.Enable(const aState: TGLState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit;
  if not (aState in FStates) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      Include(FStates, aState);
{$IFDEF GLS_OPENGL_DEBUG}
    if GL.IsEnabled(cGLStateToGLEnum[aState].GLConst) then
      GLSLogger.LogError(glsStateCashMissing + 'Enable');
{$ENDIF}
    GL.Enable(cGLStateToGLEnum[aState].GLConst);
  end;
end;

// Disable
//

procedure TGLStateCache.Disable(const aState: TGLState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit;
  if (aState in FStates) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      Exclude(FStates, aState);
{$IFDEF GLS_OPENGL_DEBUG}
    if not GL.IsEnabled(cGLStateToGLEnum[aState].GLConst) then
      GLSLogger.LogError(glsStateCashMissing + 'Disable');
{$ENDIF}
    GL.Disable(cGLStateToGLEnum[aState].GLConst);
    if aState = stColorMaterial then
    begin
      GL.Materialfv(GL_FRONT, GL_EMISSION, @FFrontBackColors[0][0]);
      GL.Materialfv(GL_FRONT, GL_AMBIENT, @FFrontBackColors[0][1]);
      GL.Materialfv(GL_FRONT, GL_DIFFUSE, @FFrontBackColors[0][2]);
      GL.Materialfv(GL_FRONT, GL_SPECULAR, @FFrontBackColors[0][3]);
      GL.Materiali(GL_FRONT, GL_SHININESS, FFrontBackShininess[0]);

      GL.Materialfv(GL_BACK, GL_EMISSION, @FFrontBackColors[1][0]);
      GL.Materialfv(GL_BACK, GL_AMBIENT, @FFrontBackColors[1][1]);
      GL.Materialfv(GL_BACK, GL_DIFFUSE, @FFrontBackColors[1][2]);
      GL.Materialfv(GL_BACK, GL_SPECULAR, @FFrontBackColors[1][3]);
      GL.Materiali(GL_BACK, GL_SHININESS, FFrontBackShininess[1]);
    end;
  end;
end;

// PerformEnable
//

procedure TGLStateCache.PerformEnable(const aState: TGLState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit;
  Include(FStates, aState);
  GL.Enable(cGLStateToGLEnum[aState].GLConst);
end;

// PerformDisable
//

procedure TGLStateCache.PerformDisable(const aState: TGLState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit;
  Exclude(FStates, aState);
  GL.Disable(cGLStateToGLEnum[aState].GLConst);
end;

procedure TGLStateCache.PopAttrib;
begin
  // TODO: replace with proper client side push/pop
  GL.PopAttrib();
end;

procedure TGLStateCache.PushAttrib(stateTypes: TGLStateTypes);
var
  tempFlag: TGLuint;
  I: Integer;
begin
  // TODO: replace with proper client side push/pop
  tempFlag := 0;
  for I := Integer(Low(TGLStateType)) to Integer(high(TGLStateType)) do
  begin
    if TGLStateType(I) in stateTypes then
    begin
      tempFlag := tempFlag or cGLStateTypeToGLEnum[TGLStateType(I)];
    end;
  end;
  GL.PushAttrib(tempFlag);
end;

// SetGLMaterialColors
//

procedure TGLStateCache.SetGLMaterialColors(const aFace: TCullFaceMode;
  const emission, ambient, diffuse, specular: TVector;
  const shininess: Integer);
var
  i: Integer;
  currentFace: TGLenum;
begin
  if FForwardContext then
    exit;
  Assert((aFace = cmFront) or (aFace = cmBack),
    'Only cmFront or cmBack supported');
  i := Integer(aFace);
  currentFace := cGLCullFaceModeToGLEnum[aFace];

  if (FFrontBackShininess[i] <> shininess)
    or FInsideList then
  begin
    GL.Materiali(currentFace, GL_SHININESS, shininess);
    if not FInsideList then
      FFrontBackShininess[i] := shininess;
  end;
  if not AffineVectorEquals(FFrontBackColors[i][0], emission)
    or FInsideList then
  begin
    GL.Materialfv(currentFace, GL_EMISSION, @emission);
    if not FInsideList then
      SetVector(FFrontBackColors[i][0], emission);
  end;
  if not AffineVectorEquals(FFrontBackColors[i][1], ambient)
    or FInsideList then
  begin
    GL.Materialfv(currentFace, GL_AMBIENT, @ambient);
    if not FInsideList then
      SetVector(FFrontBackColors[i][1], ambient);
  end;
  if not VectorEquals(FFrontBackColors[i][2], diffuse)
    or FInsideList then
  begin
    GL.Materialfv(currentFace, GL_DIFFUSE, @diffuse);
    if not FInsideList then
      SetVector(FFrontBackColors[i][2], diffuse);
  end;
  if not AffineVectorEquals(FFrontBackColors[i][3], specular)
    or FInsideList then
  begin
    GL.Materialfv(currentFace, GL_SPECULAR, @specular);
    if not FInsideList then
      SetVector(FFrontBackColors[i][3], specular);
  end;
  if FInsideList then
    Include(FListStates[FCurrentList], sttLighting);
end;

// SetGLMaterialAlphaChannel
//

procedure TGLStateCache.SetGLMaterialAlphaChannel(const aFace: TGLEnum; const
  alpha: TGLFloat);
var
  i: Integer;
  color: TVector4f;
begin
  if FForwardContext then
    exit;
  i := aFace - GL_FRONT;
  if (FFrontBackColors[i][2][3] <> alpha) or FInsideList then
  begin

    if FInsideList then
    begin
      Include(FListStates[FCurrentList], sttLighting);
      color := FFrontBackColors[i][2];
    end
    else
    begin
      FFrontBackColors[i][2][3] := alpha;
      color[3] := alpha;
    end;
    GL.Materialfv(aFace, GL_DIFFUSE, @color);
  end;
end;

procedure TGLStateCache.SetActiveTexture(const Value: TGLint);
begin
  if (Value <> FActiveTexture) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttTexture)
    else
      FActiveTexture := Value;
    GL.ActiveTexture(GL_TEXTURE0 + Value);
  end;
end;

procedure TGLStateCache.SetVertexArrayBinding(const Value: TGLuint);
begin
  if Value <> FVertexArrayBinding then
  begin
    if Integer(Value) > Length(FVAOStates) then
      SetLength(FVAOStates, 2 * Length(FVAOStates));
    if Value = 0 then
      FVAOStates[0] := FVAOStates[FVertexArrayBinding];
    FVertexArrayBinding := Value;
    GL.BindVertexArray(Value);
  end;
end;

procedure TGLStateCache.ResetVertexArrayStates(Index: TGLuint);
begin
  FillChar(FVAOStates[Index], SizeOf(FVAOStates[Index]), $00);
  if FVertexArrayBinding = Index then
    FVertexArrayBinding := 0;
end;

function TGLStateCache.GetArrayBufferBinding: TGLuint;
begin
  Result := FVAOStates[FVertexArrayBinding].FArrayBufferBinding;
end;

procedure TGLStateCache.SetArrayBufferBinding(const Value: TGLuint);
begin
  if Value <> FVAOStates[FVertexArrayBinding].FArrayBufferBinding then
  begin
    FVAOStates[FVertexArrayBinding].FArrayBufferBinding := Value;
    GL.BindBuffer(GL_ARRAY_BUFFER, Value);
  end;
end;

function TGLStateCache.GetElementBufferBinding: TGLuint;
begin
  Result := FVAOStates[FVertexArrayBinding].FElementBufferBinding;
end;

procedure TGLStateCache.SetElementBufferBinding(const Value: TGLuint);
begin
  if Value <> FVAOStates[FVertexArrayBinding].FElementBufferBinding then
  begin
    FVAOStates[FVertexArrayBinding].FElementBufferBinding := Value;
    GL.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, Value);
  end;
end;

function TGLStateCache.GetEnableVertexAttribArray(I: TGLuint): TGLboolean;
begin
  Result := FVAOStates[FVertexArrayBinding].FAttribEnabling[I];
end;

procedure TGLStateCache.SetEnableVertexAttribArray(I: TGLuint; Value:
  TGLboolean);
begin
  Assert(I < GLS_VERTEX_ATTR_NUM, 'Out of maximum attribute index');

  if Value <> FVAOStates[FVertexArrayBinding].FAttribEnabling[I] then
  begin
    FVAOStates[FVertexArrayBinding].FAttribEnabling[I] := Value;
    if Value then
      GL.EnableVertexAttribArray(I)
    else
      GL.DisableVertexAttribArray(I);
  end;
end;

function TGLStateCache.GetEnablePrimitiveRestart: TGLboolean;
begin
  Result := FEnablePrimitiveRestart;
end;

procedure TGLStateCache.SetEnablePrimitiveRestart(const enabled: TGLboolean);
begin
  if enabled <> FEnablePrimitiveRestart then
  begin
    FEnablePrimitiveRestart := enabled;
    if GL.NV_primitive_restart or FForwardContext then
    begin
      if enabled then
        GL.Enable(GL_PRIMITIVE_RESTART)
      else
        GL.Disable(GL_PRIMITIVE_RESTART);
    end;
  end;
end;

function TGLStateCache.GetPrimitiveRestartIndex: TGLuint;
begin
  Result := FPrimitiveRestartIndex;
end;

procedure TGLStateCache.SetPrimitiveRestartIndex(const index: TGLuint);
begin
  if index <> FPrimitiveRestartIndex then
  begin
    if GL.NV_primitive_restart or FForwardContext then
    begin
      FPrimitiveRestartIndex := index;
      GL.PrimitiveRestartIndex(index)
    end;
  end;
end;

procedure TGLStateCache.SetEnableProgramPointSize(const Value: TGLboolean);
begin
  if Value <> FEnableProgramPointSize then
  begin
    FEnableProgramPointSize := Value;
    if Value then
      GL.Enable(GL_PROGRAM_POINT_SIZE)
    else
      GL.Disable(GL_PROGRAM_POINT_SIZE);
  end;
end;

procedure TGLStateCache.SetBlendColor(const Value: TVector);
begin
  if not VectorEquals(Value, FBlendColor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FBlendColor := Value;
    GL.BlendColor(Value[0], Value[1], Value[2], Value[3]);
  end;
end;

procedure TGLStateCache.SetBlendEquationSeparate(const modeRGB, modeAlpha:
  TBlendEquation);
begin
  if (modeRGB <> FBlendEquationRGB) or (modeAlpha <> FBlendEquationAlpha)
    or FInsideList then
  begin
    FBlendEquationRGB := modeRGB;
    FBlendEquationAlpha := modeAlpha;
    glBlendEquationSeparate(cGLBlendEquationToGLEnum[modeRGB],
      cGLBlendEquationToGLEnum[modeAlpha]);
  end;
  if FInsideList then
    Include(FListStates[FCurrentList], sttColorBuffer);
end;

procedure TGLStateCache.SetBlendEquation(const mode: TBlendEquation);
begin
  if (mode <> FBlendEquationRGB) or (mode <> FBlendEquationAlpha)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendEquationRGB := mode;
      FBlendEquationAlpha := mode;
    end;
    GL.BlendEquation(cGLBlendEquationToGLEnum[mode]);
  end;
end;

procedure TGLStateCache.SetBlendFunc(const Src: TBlendFunction;
  const Dst: TDstBlendFunction);
begin
  if (Src <> FBlendSrcRGB) or (Dst <> FBlendDstRGB) or FInsideList then
  begin
    FBlendSrcRGB := Src;
    FBlendDstRGB := Dst;
    FBlendSrcAlpha := Src;
    FBlendSrcAlpha := Dst;
    GL.BlendFunc(cGLBlendFunctionToGLEnum[Src], cGLBlendFunctionToGLEnum[Dst]);
  end;
end;

procedure TGLStateCache.SetBlendFuncSeparate(const SrcRGB: TBlendFunction;
  const DstRGB: TDstBlendFunction; const SrcAlpha: TBlendFunction;
  const DstAlpha: TDstBlendFunction);
begin
  if (SrcRGB <> FBlendSrcRGB) or (DstRGB <> FBlendDstRGB) or
    (SrcAlpha <> FBlendSrcAlpha) or (DstAlpha <> FBlendDstAlpha)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendSrcRGB := SrcRGB;
      FBlendDstRGB := DstRGB;
      FBlendSrcAlpha := SrcAlpha;
      FBlendDstAlpha := DstAlpha;
    end;
    glBlendFuncSeparate(cGLBlendFunctionToGLEnum[SrcRGB],
      cGLBlendFunctionToGLEnum[DstRGB],
      cGLBlendFunctionToGLEnum[SrcAlpha],
      cGLBlendFunctionToGLEnum[DstAlpha]);
  end;
end;

procedure TGLStateCache.SetClampReadColor(const Value: TGLenum);
begin
  if (Value <> FClampReadColor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FClampReadColor := Value;
    glClampColor(GL_CLAMP_READ_COLOR, Value);
  end;
end;

procedure TGLStateCache.SetColorWriteMask(Index: Integer;
  const Value: TColorMask);
begin
  if FColorWriteMask[Index] <> Value then
  begin
    FColorWriteMask[Index] := Value;
    glColorMaski(Index, ccRed in Value, ccGreen in Value, ccBlue in Value,
      ccAlpha in Value);
  end;
end;

procedure TGLStateCache.SetCopyReadBufferBinding(const Value: TGLuint);
begin
  if Value <> FCopyReadBufferBinding then
  begin
    FCopyReadBufferBinding := Value;
    GL.BindBuffer(GL_COPY_READ_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetCopyWriteBufferBinding(const Value: TGLuint);
begin
  if Value <> FCopyWriteBufferBinding then
  begin
    FCopyWriteBufferBinding := Value;
    GL.BindBuffer(GL_COPY_WRITE_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetCullFaceMode(const Value: TCullFaceMode);
begin
  if (Value <> FCullFaceMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FCullFaceMode := Value;
    GL.CullFace(cGLCullFaceModeToGLEnum[Value]);
  end;

end;

procedure TGLStateCache.SetCurrentProgram(const Value: TGLuint);
begin
  if Value <> FCurrentProgram then
  begin
    FCurrentProgram := Value;
    GL.UseProgram(Value);
  end;
end;

procedure TGLStateCache.SetTextureBufferBinding(const Value: TGLuint);
begin
  if Value <> FTextureBufferBinding then
  begin
    FTextureBufferBinding := Value;
    GL.BindBuffer(GL_TEXTURE_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetCurrentVertexAttrib(Index: Integer;
  const Value: TVector);
begin
  if not VectorEquals(Value, FCurrentVertexAttrib[Index]) then
  begin
    FCurrentVertexAttrib[Index] := Value;
    GL.VertexAttrib4fv(Index, @Value[0]);
  end;
end;

procedure TGLStateCache.SetDepthClearValue(const Value: TGLfloat);
begin
  if (Value <> FDepthClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthClearValue := Value;
    GL.ClearDepth(Value);
  end;

end;

procedure TGLStateCache.SetDepthFunc(const Value: TDepthFunction);
begin
  if (Value <> FDepthFunc) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthFunc := Value;
    GL.DepthFunc(cGLComparisonFunctionToGLEnum[Value]);
  end;

end;

procedure TGLStateCache.SetDepthRange(const ZNear, ZFar: TGLclampd);
begin
  if (ZNear <> FDepthRange[0]) or (ZFar <> FDepthRange[1])
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
    begin
      FDepthRange[0] := ZNear;
      FDepthRange[1] := ZFar;
    end;
    GL.DepthRange(ZNear, ZFar);
  end;
end;

procedure TGLStateCache.SetDepthRangeFar(const Value: TGLclampd);
begin
  if (Value <> FDepthRange[1]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FDepthRange[1] := Value;
    GL.DepthRange(FDepthRange[0], Value);
  end;
end;

procedure TGLStateCache.SetDepthRangeNear(const Value: TGLclampd);
begin
  if (Value <> FDepthRange[0]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FDepthRange[0] := Value;
    GL.DepthRange(Value, FDepthRange[1]);
  end;
end;

procedure TGLStateCache.SetDepthWriteMask(const Value: TGLboolean);
begin
  if (Value <> FDepthWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthWriteMask := Value;
    GL.DepthMask(Value);
  end;
end;

procedure TGLStateCache.SetDrawFrameBuffer(const Value: TGLuint);
begin
  if Value <> FDrawFrameBuffer then
  begin
    FDrawFrameBuffer := Value;
    GL.BindFramebuffer(GL_DRAW_FRAMEBUFFER, Value);
  end;
end;

procedure TGLStateCache.SetEnableBlend(Index: Integer;
  const Value: TGLboolean);
begin
  if FEnableBlend[Index] <> Value then
  begin
    FEnableBlend[Index] := Value;
    if Value then
      glEnablei(GL_BLEND, Index)
    else
      glDisablei(GL_BLEND, Index);
  end;
end;

procedure TGLStateCache.SetEnableClipDistance(Index: Cardinal;
  const Value: TGLboolean);
begin
  if FEnableClipDistance[Index] <> Value then
  begin
    FEnableClipDistance[Index] := Value;
    if Value then
      GL.Enable(GL_CLIP_DISTANCE0 + Index)
    else
      GL.Disable(GL_CLIP_DISTANCE0 + Index);
  end;
end;

procedure TGLStateCache.SetEnableColorLogicOp(const Value: TGLboolean);
begin
  if Value <> FEnableColorLogicOp then
  begin
    FEnableColorLogicOp := Value;
    if Value then
      GL.Enable(GL_COLOR_LOGIC_OP)
    else
      GL.Disable(GL_COLOR_LOGIC_OP);
  end;
end;

procedure TGLStateCache.SetEnableCullFace(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableDepthClamp(const enabled: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableDepthTest(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableDither(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableFramebufferSRGB(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableLineSmooth(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableMultisample(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnablePolygonOffsetFill(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnablePolygonOffsetLine(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnablePolygonOffsetPoint(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnablePolygonSmooth(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableSampleAlphaToCoverage(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableSampleCoverage(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableSampleMask(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableSampleAlphaToOne(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableScissorTest(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableStencilTest(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetFragmentShaderDerivitiveHint(const Value: THintType);
begin
  if Value <> FFragmentShaderDerivitiveHint then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FFragmentShaderDerivitiveHint := Value;
    GL.Hint(GL_FRAGMENT_SHADER_DERIVATIVE_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetMultisampleFilterHint(const Value: THintType);
begin
  if GL.NV_multisample_filter_hint then
    if Value <> FMultisampleFilterHint then
    begin
      if FInsideList then
        Include(FListStates[FCurrentList], sttHint)
      else
        FMultisampleFilterHint := Value;
      GL.Hint(GL_MULTISAMPLE_FILTER_HINT_NV, cGLHintToGLEnum[Value]);
    end;
end;

procedure TGLStateCache.SetFrameBuffer(const Value: TGLuint);
begin
  if (Value <> FDrawFrameBuffer) or (Value <> FReadFrameBuffer)
    or FInsideList then
  begin
    FDrawFrameBuffer := Value;
    FReadFrameBuffer := Value;
    GL.BindFramebuffer(GL_FRAMEBUFFER, Value);
  end;
end;

procedure TGLStateCache.SetFrontFace(const Value: TFaceWinding);
begin
  if (Value <> FFrontFace) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FFrontFace := Value;
    GL.FrontFace(cGLFaceWindingToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetGLAlphaFunction(func: TComparisonFunction;
  ref: TGLclampf);
begin
  if FForwardContext then
    exit;
  if (FAlphaFunc <> func) or (FAlphaRef <> ref)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FAlphaFunc := func;
      FAlphaRef := ref;
    end;
    GL.AlphaFunc(cGLComparisonFunctionToGLEnum[func], ref);
  end;
end;

function TGLStateCache.GetColorWriteMask(Index: Integer): TColorMask;
begin
  Result := FColorWriteMask[Index];
end;

function TGLStateCache.GetCurrentQuery(Index: TQueryType): TGLuint;
begin
  Result := FCurrentQuery[Index];
end;

function TGLStateCache.GetCurrentVertexAttrib(Index: Integer): TVector;
begin
  Result := FCurrentVertexAttrib[Index];
end;

function TGLStateCache.GetEnableBlend(Index: Integer): TGLboolean;
begin
  Result := FEnableBlend[Index];
end;

function TGLStateCache.GetEnableClipDistance(
  ClipDistance: Cardinal): TGLboolean;
begin
  Result := FEnableClipDistance[ClipDistance];
end;

function TGLStateCache.GetSampleMaskValue(Index: Integer): TGLbitfield;
begin
  Result := FSampleMaskValue[Index];
end;

function TGLStateCache.GetMaxTextureImageUnits: Integer;
begin
  if FMaxTextureImageUnits = 0 then
    GL.GetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, @FMaxTextureImageUnits);
  Result := Integer(FMaxTextureImageUnits);
end;

function TGLStateCache.GetTextureBinding(Index: Integer;
  target: TGLTextureTarget): TGLuint;
begin
  Result := FTextureBinding[Index, target];
end;

// SetGLTextureMatrix
//

procedure TGLStateCache.SetGLTextureMatrix(const matrix: TMatrix);
begin
  if FForwardContext then
    exit;
  FTextureMatrixIsIdentity := False;
  GL.MatrixMode(GL_TEXTURE);
  GL.LoadMatrixf(PGLFloat(@matrix[0][0]));
  GL.MatrixMode(GL_MODELVIEW);
  if FInsideList then
    Include(FListStates[FCurrentList], sttTransform);
end;

procedure TGLStateCache.SetLineSmoothHint(const Value: THintType);
begin
  if (Value <> FLineSmoothHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FLineSmoothHint := Value;
    GL.Hint(GL_LINE_SMOOTH_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetLineWidth(const Value: TGLfloat);
begin
  // note: wide lines no longer deprecated (see OpenGL spec)
  if (Value <> FLineWidth) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineWidth := Value;
    GL.LineWidth(Value);
  end;
end;

procedure TGLStateCache.SetLineStippleFactor(const Value: TGLint);
begin
  if (Value <> FLineStippleFactor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineStippleFactor := Value;
    GL.LineStipple(Value, FLineStipplePattern);
  end;
end;

procedure TGLStateCache.SetLineStipplePattern(const Value: TGLushort);
begin
  if (Value <> FLineStipplePattern) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineStipplePattern := Value;
    GL.LineStipple(FLineStippleFactor, Value);
  end;
end;

procedure TGLStateCache.SetLogicOpMode(const Value: TLogicOp);
begin
  if (Value <> FLogicOpMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FLogicOpMode := Value;
    GL.LogicOp(cGLLogicOpToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetPackAlignment(const Value: TGLuint);
begin
  if Value <> FPackAlignment then
  begin
    FPackAlignment := Value;
    GL.PixelStoref(GL_PACK_ALIGNMENT, Value);
  end;
end;

procedure TGLStateCache.SetPackImageHeight(const Value: TGLuint);
begin
  if Value <> FPackImageHeight then
  begin
    FPackImageHeight := Value;
    GL.PixelStoref(GL_PACK_IMAGE_HEIGHT, Value);
  end;
end;

procedure TGLStateCache.SetPackLSBFirst(const Value: TGLboolean);
begin
  if Value <> FPackLSBFirst then
  begin
    FPackLSBFirst := Value;
    GL.PixelStorei(GL_PACK_LSB_FIRST, byte(Value));
  end;
end;

procedure TGLStateCache.SetPackRowLength(const Value: TGLuint);
begin
  if Value <> FPackRowLength then
  begin
    FPackRowLength := Value;
    GL.PixelStoref(GL_PACK_ROW_LENGTH, Value);
  end;
end;

procedure TGLStateCache.SetPackSkipImages(const Value: TGLuint);
begin
  if Value <> FPackSkipImages then
  begin
    FPackSkipImages := Value;
    GL.PixelStoref(GL_PACK_SKIP_IMAGES, Value);
  end;
end;

procedure TGLStateCache.SetPackSkipPixels(const Value: TGLuint);
begin
  if Value <> FPackSkipPixels then
  begin
    FPackSkipPixels := Value;
    GL.PixelStoref(GL_PACK_SKIP_PIXELS, Value);
  end;
end;

procedure TGLStateCache.SetPackSkipRows(const Value: TGLuint);
begin
  if Value <> FPackSkipRows then
  begin
    FPackSkipRows := Value;
    GL.PixelStoref(GL_PACK_SKIP_ROWS, Value);
  end;
end;

procedure TGLStateCache.SetPackSwapBytes(const Value: TGLboolean);
begin
  if Value <> FPackSwapBytes then
  begin
    FPackSwapBytes := Value;
    GL.PixelStorei(GL_PACK_SWAP_BYTES, byte(Value));
  end;
end;

procedure TGLStateCache.SetPixelPackBufferBinding(const Value: TGLuint);
begin
  if Value <> FPixelPackBufferBinding then
  begin
    FPixelPackBufferBinding := Value;
    GL.BindBuffer(GL_PIXEL_PACK_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetPixelUnpackBufferBinding(const Value: TGLuint);
begin
  if Value <> FPixelUnpackBufferBinding then
  begin
    FPixelUnpackBufferBinding := Value;
    GL.BindBuffer(GL_PIXEL_UNPACK_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetPointFadeThresholdSize(const Value: TGLfloat);
begin
  if (Value <> FPointFadeThresholdSize) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointFadeThresholdSize := Value;
    glPointParameterf(GL_POINT_FADE_THRESHOLD_SIZE, Value);
  end;
end;

procedure TGLStateCache.SetPointSize(const Value: TGLfloat);
begin
  if (Value <> FPointSize) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointSize := Value;
    GL.PointSize(Value);
  end;
end;

procedure TGLStateCache.SetPointSpriteCoordOrigin(const Value: TGLenum);
begin
  if (Value <> FPointSpriteCoordOrigin) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointSpriteCoordOrigin := Value;
    glPointParameterf(GL_POINT_SPRITE_COORD_ORIGIN, Value);
  end;
end;

procedure TGLStateCache.SetPolygonMode(const Value: TPolygonMode);
begin
  if (Value <> FPolygonMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
    begin
      FPolygonMode := Value;
      FPolygonBackMode := Value;
    end;
    GL.PolygonMode(GL_FRONT_AND_BACK, cGLPolygonModeToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetPolygonOffset(const factor, units: TGLfloat);
begin
  if (factor <> FPolygonOffsetFactor) or (units <> FPolygonOffsetUnits)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
    begin
      FPolygonOffsetFactor := factor;
      FPolygonOffsetUnits := units;
    end;
    GL.PolygonOffset(factor, units);
  end;
end;

procedure TGLStateCache.SetPolygonOffsetFactor(const Value: TGLfloat);
begin
  if (Value <> FPolygonOffsetFactor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FPolygonOffsetFactor := Value;
    GL.PolygonOffset(Value, FPolygonOffsetUnits);
  end;
end;

procedure TGLStateCache.SetPolygonOffsetUnits(const Value: TGLfloat);
begin
  if (Value <> FPolygonOffsetUnits) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FPolygonOffsetUnits := Value;
    GL.PolygonOffset(FPolygonOffsetFactor, Value);
  end;
end;

procedure TGLStateCache.SetPolygonSmoothHint(const Value: THintType);
begin
  if (Value <> FPolygonSmoothHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FPolygonSmoothHint := Value;
    GL.Hint(GL_POLYGON_SMOOTH_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetProvokingVertex(const Value: TGLenum);
begin
  if Value <> FProvokingVertex then
  begin
    FProvokingVertex := Value;
    glProvokingVertex(Value);
  end;
end;

procedure TGLStateCache.SetReadFrameBuffer(const Value: TGLuint);
begin
  if Value <> FReadFrameBuffer then
  begin
    FReadFrameBuffer := Value;
    GL.BindFramebuffer(GL_READ_FRAMEBUFFER, Value);
  end;
end;

procedure TGLStateCache.SetRenderBuffer(const Value: TGLuint);
begin
  if Value <> FRenderBuffer then
  begin
    FRenderBuffer := Value;
    GL.BindRenderbuffer(GL_RENDERBUFFER, Value);
  end;
end;

procedure TGLStateCache.SetSampleCoverage(const Value: TGLfloat;
  invert: TGLboolean);
begin
  if (Value <> FSampleCoverageValue) or (invert <> FSampleCoverageInvert)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
    begin
      FSampleCoverageValue := Value;
      FSampleCoverageInvert := invert;
    end;
    glSampleCoverage(Value, invert);
  end;
end;

procedure TGLStateCache.SetSampleCoverageInvert(const Value: TGLboolean);
begin
  if (Value <> FSampleCoverageInvert) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleCoverageInvert := Value;
    glSampleCoverage(FSampleCoverageValue, Value);
  end;
end;

procedure TGLStateCache.SetSampleCoverageValue(const Value: TGLfloat);
begin
  if (Value <> FSampleCoverageValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleCoverageValue := Value;
    glSampleCoverage(Value, FSampleCoverageInvert);
  end;
end;

procedure TGLStateCache.SetSampleMaskValue(Index: Integer;
  const Value: TGLbitfield);
begin
  if (FSampleMaskValue[Index] <> Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleMaskValue[Index] := Value;
    glSampleMaski(Index, Value);
  end;
end;

procedure TGLStateCache.SetScissorBox(const Value: TVector4i);
begin
  if not VectorEquals(FScissorBox, Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttScissor)
    else
      FScissorBox := Value;
    GL.Scissor(Value[0], Value[1], Value[2], Value[3]);
  end;
end;

procedure TGLStateCache.SetStencilBackWriteMask(const Value: TGLuint);
begin
  if (Value <> FStencilBackWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilBackWriteMask := Value;
    // TODO: ignore if unsupported
    if GL_VERSION_2_0 then
      glStencilMaskSeparate(GL_BACK, Value);
  end;
end;

procedure TGLStateCache.SetStencilClearValue(const Value: TGLuint);
begin
  if (Value <> FStencilClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilClearValue := Value;
    glClearStencil(Value);
  end;
end;

procedure TGLStateCache.SetColorClearValue(const Value: TVector);
begin
  if not VectorEquals(Value, FColorClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FColorClearValue := Value;
    GL.ClearColor(Value[0], Value[1], Value[2], Value[3]);
  end;
end;

procedure TGLStateCache.SetColorMask(mask: TColorMask);
var
  i: integer;
begin
  // it might be faster to keep track of whether all draw buffers are same
  // value or not, since using this is probably more common than setting
  // the color write mask for individual draw buffers
  if FInsideList then
    Include(FListStates[FCurrentList], sttColorBuffer)
  else
    for I := low(FColorWriteMask) to high(FColorWriteMask) do
    begin
      FColorWriteMask[I] := mask;
    end;
  GL.ColorMask(ccRed in mask, ccGreen in mask, ccBlue in mask, ccAlpha in mask);
end;

procedure TGLStateCache.SetStencilFuncSeparate(const face: TCullFaceMode;
  const func: TStencilFunction; const ref: TGLint; const mask: TGLuint);
begin
  if FInsideList then
    Include(FListStates[FCurrentList], sttStencilBuffer)
  else
    case face of
      cmFront:
        begin
          FStencilFunc := func;
          FStencilRef := ref;
          FStencilValueMask := mask;
        end;
      cmBack:
        begin
          FStencilBackFunc := func;
          FStencilBackRef := ref;
          FStencilBackValueMask := mask;
        end;
      cmFrontAndBack:
        begin
          FStencilFunc := func;
          FStencilRef := ref;
          FStencilValueMask := mask;
          FStencilBackFunc := func;
          FStencilBackRef := ref;
          FStencilBackValueMask := mask;
        end;
    end;

  glStencilFuncSeparate(cGLCullFaceModeToGLEnum[face],
    cGLComparisonFunctionToGLEnum[func], ref, mask);
  //if (func<>FStencilFunc)or(ref<>FStencilRef)or(mask<>FStencilValueMask) then
end;

procedure TGLStateCache.SetStencilFunc(const func: TStencilFunction; const ref:
  TGLint;
  const mask: TGLuint);
begin
  if (func <> FStencilFunc) or (ref <> FStencilRef) or (mask <>
    FStencilValueMask)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
    begin
      FStencilFunc := func;
      FStencilRef := ref;
      FStencilValueMask := mask;
    end;
    GL.StencilFunc(cGLComparisonFunctionToGLEnum[func], ref, mask);
  end;
end;

procedure TGLStateCache.SetStencilOp(const fail, zfail, zpass: TStencilOp);
begin
  if FInsideList then
    Include(FListStates[FCurrentList], sttStencilBuffer)
  else
  begin
    FStencilFail := fail;
    FStencilPassDepthFail := zfail;
    FStencilPassDepthPass := zpass;
  end;
  GL.StencilOp(cGLStencilOpToGLEnum[fail],
    cGLStencilOpToGLEnum[zfail],
    cGLStencilOpToGLEnum[zpass]);
end;

procedure TGLStateCache.SetStencilOpSeparate(const face: TCullFaceMode; const
  sfail, dpfail,
  dppass: TStencilOp);
begin
  if FInsideList then
    Include(FListStates[FCurrentList], sttStencilBuffer)
  else
    case face of
      cmFront:
        begin
          FStencilFail := sfail;
          FStencilPassDepthFail := dpfail;
          FStencilPassDepthPass := dppass;
        end;
      cmBack:
        begin
          FStencilBackFail := sfail;
          FStencilBackPassDepthFail := dpfail;
          FStencilBackPassDepthPass := dppass;
        end;
      cmFrontAndBack:
        begin
          FStencilFail := sfail;
          FStencilPassDepthFail := dpfail;
          FStencilPassDepthPass := dppass;
          FStencilBackFail := sfail;
          FStencilBackPassDepthFail := dpfail;
          FStencilBackPassDepthPass := dppass;
        end;
    end;

  glStencilOpSeparate(cGLCullFaceModeToGLEnum[face],
    cGLStencilOpToGLEnum[sfail],
    cGLStencilOpToGLEnum[dpfail],
    cGLStencilOpToGLEnum[dppass]);
end;

procedure TGLStateCache.SetStencilWriteMask(const Value: TGLuint);
begin
  if (Value <> FStencilWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[Value], sttStencilBuffer)
    else
      FStencilValueMask := Value;
    glStencilMaskSeparate(GL_FRONT, Value);
  end;
end;

procedure TGLStateCache.SetTextureBinding(Index: Integer; target:
  TGLTextureTarget;
  const Value: TGLuint);
var
  lastActiveTexture: TGLuint;
begin
  if (Value <> FTextureBinding[Index, target]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttTexture)
    else
      FTextureBinding[Index, target] := Value;
    lastActiveTexture := ActiveTexture;
    ActiveTexture := Index;
    GL.BindTexture(cGLTexTypeToGLEnum[target], Value);
    ActiveTexture := lastActiveTexture;
  end;
end;

function TGLStateCache.GetActiveTextureEnabled(Target: TGLTextureTarget):
  Boolean;
begin
  Result := FActiveTextureEnabling[FActiveTexture][Target];
end;

procedure TGLStateCache.SetActiveTextureEnabled(Target: TGLTextureTarget;
  const Value: Boolean);
var
  glTarget: TGLEnum;
begin
  glTarget := DecodeGLTextureTarget(Target);
  if FForwardContext or not IsTargetSupported(glTarget) then
    exit;
  if (Value <> FActiveTextureEnabling[FActiveTexture][Target])
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      FActiveTextureEnabling[FActiveTexture][Target] := Value;
    if Value then
      GL.Enable(glTarget)
    else
      GL.Disable(glTarget);
  end;
end;

procedure TGLStateCache.SetTextureCompressionHint(const Value: THintType);
begin
  if (Value <> FTextureCompressionHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FTextureCompressionHint := Value;
    GL.Hint(GL_TEXTURE_COMPRESSION_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetTransformFeedbackBufferBinding(const Value: TGLuint);
begin
  if (Value <> FTransformFeedbackBufferBinding) or FInsideList then
  begin
    FTransformFeedbackBufferBinding := Value;
    GL.BindBuffer(GL_TRANSFORM_FEEDBACK_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetEnableTextureCubeMapSeamless(const Value:
  TGLboolean);
begin
  if Value <> FEnableTextureCubeMapSeamless then
  begin
    FEnableTextureCubeMapSeamless := Value;
    if Value = true then
      GL.Enable(GL_TEXTURE_CUBE_MAP_SEAMLESS)
    else
      GL.Disable(GL_TEXTURE_CUBE_MAP_SEAMLESS);
  end;
end;

procedure TGLStateCache.NewList(list: TGLuint; mode: TGLEnum);
var
  I: TGLuint;
begin
  Assert(mode = GL_COMPILE,
    'Compile & executing not supported by TGLStateCache');
  FCurrentList := list - 1;
  while High(FListStates) < Integer(FCurrentList) do
    SetLength(FListStates, 2 * Length(FListStates));

  FListStates[FCurrentList] := [];
  FInsideList := True;
  // Reset VBO binding and client attribute
  if GL_ARB_vertex_buffer_object then
  begin
    ArrayBufferBinding := 0;
    ElementBufferBinding := 0;
    for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      EnableVertexAttribArray[I] := False;
  end;
  GL.NewList(list, mode)
end;

procedure TGLStateCache.EndList;
begin
  GL.EndList;
  FInsideList := False;
end;

procedure TGLStateCache.CallList(list: TGLuint);
begin
  PushAttrib(FListStates[list - 1]);
  GL.CallList(list);
  PopAttrib;
end;

procedure TGLStateCache.SetUniformBufferBinding(const Value: TGLuint);
begin
  if (Value <> FUniformBufferBinding) or FInsideList then
  begin
    FUniformBufferBinding := Value;
    GL.BindBuffer(GL_UNIFORM_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetUnpackAlignment(const Value: TGLuint);
begin
  if Value <> FUnpackAlignment then
  begin
    FUnpackAlignment := Value;
    GL.PixelStoref(GL_UNPACK_ALIGNMENT, Value);
  end;
end;

procedure TGLStateCache.SetUnpackImageHeight(const Value: TGLuint);
begin
  if Value <> FUnpackImageHeight then
  begin
    FUnpackImageHeight := Value;
    GL.PixelStoref(GL_UNPACK_IMAGE_HEIGHT, Value);
  end;
end;

procedure TGLStateCache.SetUnpackLSBFirst(const Value: TGLboolean);
begin
  if Value <> FUnpackLSBFirst then
  begin
    FUnpackLSBFirst := Value;
    GL.PixelStorei(GL_UNPACK_LSB_FIRST, byte(Value));
  end;
end;

procedure TGLStateCache.SetUnpackRowLength(const Value: TGLuint);
begin
  if Value <> FUnpackRowLength then
  begin
    FUnpackRowLength := Value;
    GL.PixelStoref(GL_UNPACK_ROW_LENGTH, Value);
  end;
end;

procedure TGLStateCache.SetUnpackSkipImages(const Value: TGLuint);
begin
  if Value <> FUnpackSkipImages then
  begin
    FUnpackSkipImages := Value;
    GL.PixelStoref(GL_UNPACK_SKIP_IMAGES, Value);
  end;
end;

procedure TGLStateCache.SetUnpackSkipPixels(const Value: TGLuint);
begin
  if Value <> FUnpackSkipPixels then
  begin
    FUnpackSkipPixels := Value;
    GL.PixelStoref(GL_UNPACK_SKIP_PIXELS, Value);
  end;
end;

procedure TGLStateCache.SetUnpackSkipRows(const Value: TGLuint);
begin
  if Value <> FUnpackSkipRows then
  begin
    FUnpackSkipRows := Value;
    GL.PixelStoref(GL_UNPACK_SKIP_ROWS, Value);
  end;
end;

procedure TGLStateCache.SetUnpackSwapBytes(const Value: TGLboolean);
begin
  if Value <> FUnpackSwapBytes then
  begin
    FUnpackSwapBytes := Value;
    GL.PixelStorei(GL_UNPACK_SWAP_BYTES, byte(Value));
  end;
end;

procedure TGLStateCache.SetViewPort(const Value: TVector4i);
begin
  if not VectorEquals(Value, FViewPort) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FViewPort := Value;
    GL.Viewport(Value[0], Value[1], Value[2], Value[3]);
  end;
end;

function TGLStateCache.GetMaxLights: Integer;
begin
  if FMaxLights = 0 then
    GL.GetIntegerv(GL_MAX_LIGHTS, @FMaxLights);
  Result := FMaxLights;
end;

function TGLStateCache.GetLightEnabling(I: Integer): Boolean;
begin
  Result := FLightEnabling[I];
end;

procedure TGLStateCache.SetLightEnabling(I: Integer; Value: Boolean);
begin
  if FForwardContext then
    exit;
  if (FLightEnabling[I] <> Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightEnabling[I] := Value;
    if Value then
      GL.Enable(GL_LIGHT0 + I)
    else
      GL.Disable(GL_LIGHT0 + I);
  end;
end;

function TGLStateCache.GetLightAmbient(I: Integer): TVector;
begin
  Result := FLightAmbient[I];
end;

procedure TGLStateCache.SetLightAmbient(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightAmbient[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightAmbient[I] := Value;
    GL.Lightfv(GL_LIGHT0 + I, GL_AMBIENT, @Value);
  end;
end;

function TGLStateCache.GetLightDiffuse(I: Integer): TVector;
begin
  Result := FLightDiffuse[I];
end;

procedure TGLStateCache.SetLightDiffuse(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightDiffuse[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightDiffuse[I] := Value;
    GL.Lightfv(GL_LIGHT0 + I, GL_DIFFUSE, @Value);
  end;
end;

function TGLStateCache.GetLightSpecular(I: Integer): TVector;
begin
  Result := FLightSpecular[I];
end;

procedure TGLStateCache.SetLightSpecular(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightSpecular[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightSpecular[I] := Value;
    GL.Lightfv(GL_LIGHT0 + I, GL_SPECULAR, @Value);
  end;
end;

function TGLStateCache.GetSpotCutoff(I: Integer): Single;
begin
  Result := FSpotCutoff[I];
end;

procedure TGLStateCache.SetSpotCutoff(I: Integer; const Value: Single);
begin
  if (Value <> FSpotCutoff[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FSpotCutoff[I] := Value;
    GL.Lightfv(GL_LIGHT0 + I, GL_SPOT_CUTOFF, @Value);
  end;
end;

function TGLStateCache.GetConstantAtten(I: Integer): Single;
begin
  Result := FConstantAtten[I];
end;

procedure TGLStateCache.SetConstantAtten(I: Integer; const Value: Single);
begin
  if (Value <> FConstantAtten[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FConstantAtten[I] := Value;
    GL.Lightfv(GL_LIGHT0 + I, GL_CONSTANT_ATTENUATION, @Value);
  end;
end;

function TGLStateCache.GetLinearAtten(I: Integer): Single;
begin
  Result := FLinearAtten[I];
end;

procedure TGLStateCache.SetLinearAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLinearAtten[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLinearAtten[I] := Value;
    GL.Lightfv(GL_LIGHT0 + I, GL_LINEAR_ATTENUATION, @Value);
  end;
end;

function TGLStateCache.GetQuadAtten(I: Integer): Single;
begin
  Result := FQuadAtten[I];
end;

procedure TGLStateCache.SetQuadAtten(I: Integer; const Value: Single);
begin
  if (Value <> FQuadAtten[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FQuadAtten[I] := Value;
    GL.Lightfv(GL_LIGHT0 + I, GL_QUADRATIC_ATTENUATION, @Value);
  end;
end;

// ResetGLTextureMatrix
//

procedure TGLStateCache.ResetGLTextureMatrix;
begin
  if FForwardContext then
    exit;
  if not FTextureMatrixIsIdentity then
  begin
    GL.MatrixMode(GL_TEXTURE);
    GL.LoadIdentity;
    GL.MatrixMode(GL_MODELVIEW);
    FTextureMatrixIsIdentity := True;
  end;
end;

// SetGLColorIgnoring
//

procedure TGLStateCache.SetGLColorWriting(flag: Boolean);
begin
  if (FColorWriting <> flag) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FColorWriting := flag;
    GL.ColorMask(flag, flag, flag, flag);
  end;
end;

// InvertGLFrontFace
//

procedure TGLStateCache.InvertGLFrontFace;
begin
  if FFrontFace = fwCounterClockWise then
    FrontFace := fwClockWise
  else
    FrontFace := fwCounterClockWise;
end;

// SetGLState
//
procedure TGLStateCache.SetGLState(const aState : TGLState);
begin
	Enable(aState);
end;

// UnSetGLState
//
procedure TGLStateCache.UnSetGLState(const aState : TGLState);
begin
	Disable(aState);
end;

// ResetGLPolygonMode
//

procedure TGLStateCache.ResetGLPolygonMode;
begin
  GL.PolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  FPolygonMode := pmFill;
  FPolygonBackMode := pmFill;
end;

// ResetGLMaterialColors
//

procedure TGLStateCache.ResetGLMaterialColors;
begin
  GL.Materialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @clrGray20);
  GL.Materialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @clrGray80);
  GL.Materialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @clrBlack);
  GL.Materialfv(GL_FRONT_AND_BACK, GL_EMISSION, @clrBlack);
  GL.Materiali(GL_FRONT_AND_BACK, GL_SHININESS, 0);
  FillChar(FFrontBackColors, SizeOf(FFrontBackColors), 127);
  FFrontBackShininess[0] := 0;
  FFrontBackShininess[1] := 0;
end;

// ResetGLTexture
//

procedure TGLStateCache.ResetGLTexture(const TextureUnit: Integer);
var
  t: TGLTextureTarget;
  glTarget: TGLEnum;
begin
  GL.ActiveTexture(GL_TEXTURE0 + TextureUnit);
  for t := Low(TGLTextureTarget) to High(TGLTextureTarget) do
  begin
    glTarget := DecodeGLTextureTarget(t);
    if IsTargetSupported(glTarget) then
    begin
      GL.BindTexture(glTarget, 0);
      FTextureBinding[TextureUnit, t] := 0;
    end;
  end;
  GL.ActiveTexture(GL_TEXTURE0);
  FActiveTexture := 0;
end;

// ResetGLCurrentTexture
//

procedure TGLStateCache.ResetGLCurrentTexture;
var
  a: TGLint;
  t: TGLTextureTarget;
  glTarget: TGLEnum;
begin
  if GL.ARB_multitexture then
  begin
    for a := MaxTextureImageUnits - 1 to 0 do
    begin
      GL.ActiveTexture(GL_TEXTURE0 + a);
      for t := Low(TGLTextureTarget) to High(TGLTextureTarget) do
      begin
        glTarget := DecodeGLTextureTarget(t);
        if IsTargetSupported(glTarget) then
        begin
          GL.BindTexture(glTarget, 0);
          FTextureBinding[a, t] := 0;
        end;
      end;
    end;
  end
  else
    for t := Low(TGLTextureTarget) to High(TGLTextureTarget) do
    begin
      glTarget := DecodeGLTextureTarget(t);
      if IsTargetSupported(glTarget) then
      begin
        GL.BindTexture(glTarget, 0);
        FTextureBinding[0, t] := 0;
      end;
    end;
end;

// ResetGLFrontFace
//

procedure TGLStateCache.ResetGLFrontFace;
begin
  GL.FrontFace(GL_CCW);
  FFrontFace := fwCounterClockWise;
end;


procedure TGLStateCache.SetGLFrontFaceCW;
begin
  if FFrontFace = fwCounterClockWise then
  begin
    GL.FrontFace(GL_CW);
    FFrontFace := fwClockWise;
  end;
end;

// ResetAll
//

procedure TGLStateCache.ResetAll;
begin
{$WARN SYMBOL_DEPRECATED OFF}
  ResetGLPolygonMode;
  ResetGLMaterialColors;
  ResetGLCurrentTexture;
  ResetGLFrontFace;
{$WARN SYMBOL_DEPRECATED ON}
end;


end.

