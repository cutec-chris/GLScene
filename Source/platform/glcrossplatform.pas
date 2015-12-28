//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLCrossPlatform<p>

   Cross platform support functions and types for GLScene.<p>

   Ultimately, *no* cross-platform or cross-version defines should be present
   in the core GLScene units, and have all moved here instead.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>15/06/10 - Yar - Replace Shell to fpSystem
      <li>04/03/10 - DanB - Added CharInSet, for Delphi versions < 2009
      <li>07/01/10 - DaStr - Bugfixed GetDeviceCapabilities() for Unix
                             (thanks Predator)
      <li>17/12/09 - DaStr - Moved screen utility functions to GLScreen.pas
                             (thanks Predator)
      <li>11/11/09 - DaStr - Added GLS_FONT_CHARS_COUNT constant
      <li>07/11/09 - DaStr - Improved FPC compatibility (BugtrackerID = 2893580)
                             (thanks Predator)
      <li>24/08/09 - DaStr - Added IncludeTrailingPathDelimiter for Delphi 5
      <li>03/06/09 - DanB - Re-added Sleep procedure, for Delphi 5
      <li>07/05/09 - DanB - Added FindUnitName (to provide functionality of TObject.UnitName,
                            on prior versions of Delphi)
      <li>24/03/09 - DanB - Moved Dialog utility functions to GLUtils.pas, new ShowHTMLUrl procedure
      <li>19/03/09 - DanB - Removed some Kylix IFDEFs, and other changes mostly affecting D5/FPC
      <li>29/05/08 - DaStr - Added StrToFloatDef(), TryStrToFloat()
      <li>10/04/08 - DaStr - Added TGLComponent (BugTracker ID = 1938988)
      <li>07/04/08 - DaStr - Added IsInfinite, IsNan
      <li>18/11/07 - DaStr - Added ptrInt and PtrUInt types (BugtrackerID = 1833830)
                              (thanks Dje and Burkhard Carstens)
      <li>06/06/07 - DaStr - Added WORD type
                             Got rid of GLTexture.pas dependancy
                             Moved GetRValue, GetGValue, GetBValue, InitWinColors
                               to GLColor.pas (BugtrackerID = 1732211)
      <li>02/04/07 - DaStr - Added MakeSubComponent
                             Fixed some IFDEFs to separate FPC from Kylix
      <li>25/03/07 - DaStr - Replaced some UNIX IFDEFs with KYLIX
                             Added IdentToColor, ColorToIdent, ColorToString,
                                   AnsiStartsText, IsSubComponent
                             Added TPoint, PPoint, TRect, PRect, TPicture, TGraphic,
                                   TBitmap, TTextLayout, TMouseButton, TMouseEvent,
                                   TKeyEvent, TKeyPressEvent
                             Added IInterface, S_OK, E_NOINTERFACE,
                                   glKey_PRIOR, glKey_NEXT, glKey_CONTROL
      <li>24/03/07 - DaStr - Added TPenStyle, TPenMode, TBrushStyle, more color constants,
                             Added "Application" function
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTracekrID=1681585)
      <li>08/07/04 - LR - Added clBlack
      <li>03/07/04 - LR - Added constant for Keyboard (glKey_TAB, ...)
                          Added function GLOKMessageBox to avoid the uses of Forms
                          Added other abstraction calls
                          Added procedure ShowHTMLUrl for unit Info.pas
                          Added GLShowCursor, GLSetCursorPos, GLGetCursorPos,
                          GLGetScreenWidth, GLGetScreenHeight for GLNavigation
                          Added GLGetTickCount for GLFPSMovement
      <li>28/06/04 - LR - Added TGLTextLayout, GLLoadBitmapFromInstance
                          Added GetDeviceCapabilities to replace the old function
      <li>30/05/03 - EG - Added RDTSC and RDTSC-based precision timing for non-WIN32
      <li>22/01/02 - EG - Added OpenPictureDialog, ApplicationTerminated
      <li>07/01/02 - EG - Added QuestionDialog and SavePictureDialog,
                          Added PrecisionTimer funcs
      <li>06/12/01 - EG - Added several abstraction calls
      <li>31/08/01 - EG - Creation
	</ul></font>
}
unit GLCrossPlatform;

interface

{$include GLScene.inc}

//   Tips: Delphi 5 doesn't contain StrUtils.pas, Types.pas, so don't include
//         these files in uses clauses.

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UNIX} Unix, xlib, x,{$ENDIF}
  Classes, SysUtils, Graphics, Controls, Forms, Dialogs
  {$IFDEF FPC} ,LCLType {$ELSE}, Consts{$ENDIF}
  {$IFNDEF GLS_COMPILER_5_DOWN}, StrUtils, Types{$ENDIF}
  ;

type
{$IFNDEF FPC}
  // These new types were added to be able to cast pointers to integers
  // in 64 bit mode, because in FPC "Integer" type is always 32 bit
  // (or 16 bit in Pascal mode), but in Delphi it is platform-specific and
  // can be 16, 32 or 64 bit.
  ptrInt  = Integer;
  PtrUInt = Cardinal;
{$ENDIF}

   // Several aliases to shield us from the need of ifdef'ing between
   // the "almost cross-platform" units like Graphics/QGraphics etc.
   // Gives a little "alien" look to names, but that's the only way around :(

   // DaStr: Actually, there is a way around, see TPenStyle for example.

   TGLPoint = TPoint;
   PGLPoint = ^TGLPoint;
   TGLRect = TRect;
   PGLRect = ^TGLRect;
   TDelphiColor = TColor;

   TGLPicture = TPicture;
   TGLGraphic = TGraphic;
   TGLBitmap = TBitmap;
   TGraphicClass = class of TGraphic;

   TGLTextLayout = (tlTop, tlCenter, tlBottom); // idem TTextLayout;

   TGLMouseButton = (mbLeft, mbRight, mbMiddle); // idem TMouseButton;
   TGLMouseEvent = procedure(Sender: TObject; Button: TGLMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;
   TGLMouseMoveEvent = TMouseMoveEvent;
   TGLKeyEvent = TKeyEvent;
   TGLKeyPressEvent = TKeyPressEvent;

{$IFDEF GLS_DELPHI_5}
   EGLOSError = EWin32Error;
{$ELSE}
   EGLOSError = EOSError;
//   {$IFDEF FPC}
//      EGLOSError = EWin32Error;
//   {$ELSE}
//      EGLOSError = EOSError;
//   {$ENDIF}
{$ENDIF}

{$IFDEF GLS_DELPHI_5_DOWN}
  IInterface = IUnknown;
{$ENDIF}

// A work-around a Delphi5 interface bug.
{$IFDEF GLS_DELPHI_5_DOWN}
  TGLComponent = class(TComponent, IInterface);
{$ELSE}
  TGLComponent = class(TComponent);
{$ENDIF}

{$IFDEF GLS_DELPHI_5_DOWN}
  DWORD = Windows.DWORD;
  TPoint = Windows.TPoint;
  PPoint = Windows.PPoint;
  TRect = Windows.TRect;
  PRect = Windows.PRect;
{$ELSE}
  {$IFDEF FPC}
  DWORD = System.DWORD;
  TPoint = Types.TPoint;
  PPoint = ^TPoint;
  TRect = Types.TRect;
  PRect = ^TRect;
  {$ELSE}
  DWORD = Types.DWORD;
  TPoint = Types.TPoint;
  PPoint = Types.PPoint;
  TRect = Types.TRect;
  PRect = Types.PRect;
  {$ENDIF}
{$ENDIF}

const
{$IFDEF GLS_DELPHI_5_DOWN}
  S_OK = Windows.S_OK;
  E_NOINTERFACE = Windows.E_NOINTERFACE;
{$ENDIF}

{$IFDEF WIN32}
   glpf8Bit = pf8bit;
   glpf24bit = pf24bit;
   glpf32Bit = pf32bit;
   glpfDevice = pfDevice;
{$ENDIF}
{$IFDEF UNIX}
   glpf8Bit = pf8bit;
   glpf24bit = pf32bit;
   glpf32Bit = pf32bit;
   glpfDevice = pf32bit;
{$ENDIF}

// standard keyboard
  glKey_TAB = VK_TAB;
  glKey_SPACE = VK_SPACE;
  glKey_RETURN = VK_RETURN;
  glKey_DELETE = VK_DELETE;
  glKey_LEFT = VK_LEFT;
  glKey_RIGHT = VK_RIGHT;
  glKey_HOME = VK_HOME;
  glKey_END = VK_END;
  glKey_CANCEL = VK_CANCEL;
  glKey_UP = VK_UP;
  glKey_DOWN = VK_DOWN;
  glKey_PRIOR = VK_PRIOR;
  glKey_NEXT = VK_NEXT;
  glKey_CONTROL = VK_CONTROL;

// TPenStyle.
 {$IFDEF FPC}
  //FPC doesn't support TPenStyle "psInsideFrame", so provide an alternative
  psInsideFrame = psSolid;
 {$ENDIF}

// Several define from unit Consts
const
{$IFDEF FPC}
  glsAllFilter: string = 'All';
{$ELSE}
  glsAllFilter: string = sAllFilter;
{$ENDIF}

{$IFDEF GLS_COMPILER_2009_UP}
  GLS_FONT_CHARS_COUNT = 2024;
{$else}
  GLS_FONT_CHARS_COUNT = 256;
{$ENDIF}

function GLPoint(const x, y : Integer) : TGLPoint;

{: Builds a TColor from Red Green Blue components. }
function RGB(const r, g, b : Byte) : TColor;

function GLRect(const aLeft, aTop, aRight, aBottom : Integer) : TGLRect;
{: Increases or decreases the width and height of the specified rectangle.<p>
   Adds dx units to the left and right ends of the rectangle and dy units to
   the top and bottom. }
procedure InflateGLRect(var aRect : TGLRect; dx, dy : Integer);
procedure IntersectGLRect(var aRect : TGLRect; const rect2 : TGLRect);

procedure RaiseLastOSError;

{: Number of pixels per logical inch along the screen width for the device.<p>
   Under Win32 awaits a HDC and returns its LOGPIXELSX. }
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
{: Number of bits per pixel for the current desktop resolution. }
function GetCurrentColorDepth : Integer;
{: Returns the number of color bits associated to the given pixel format. }
function PixelFormatToColorBits(aPixelFormat : TPixelFormat) : Integer;

{: Returns the bitmap's scanline for the specified row. }
function BitmapScanLine(aBitmap : TGLBitmap; aRow : Integer) : Pointer;

{$IFDEF GLS_DELPHI_5_DOWN}
{: Suspends thread execution for length milliseconds.<p>
   If length is zero, only the remaining time in the current thread's time
   slice is relinquished. }
procedure Sleep(length : Cardinal);

{: IncludeTrailingPathDelimiter returns the path without a PathDelimiter
  ('\' or '/') at the end.  This function is MBCS enabled. }
function IncludeTrailingPathDelimiter(const S: string): string;
{$ENDIF}

{: Returns the current value of the highest-resolution counter.<p>
   If the platform has none, should return a value derived from the highest
   precision time reference available, avoiding, if possible, timers that
   allocate specific system resources. }
procedure QueryPerformanceCounter(var val : Int64);
{: Returns the frequency of the counter used by QueryPerformanceCounter.<p>
   Return value is in ticks per second (Hz), returns False if no precision
   counter is available. }
function QueryPerformanceFrequency(var val : Int64) : Boolean;

{: Starts a precision timer.<p>
   Returned value should just be considered as 'handle', even if it ain't so.
   Default platform implementation is to use QueryPerformanceCounter and
   QueryPerformanceFrequency, if higher precision references are available,
   they should be used. The timer will and must be stopped/terminated/released
   with StopPrecisionTimer. }
function StartPrecisionTimer : Int64;
{: Computes time elapsed since timer start.<p>
   Return time lap in seconds. }
function PrecisionTimerLap(const precisionTimer : Int64) : Double;
{: Computes time elapsed since timer start and stop timer.<p>
   Return time lap in seconds. }
function StopPrecisionTimer(const precisionTimer : Int64) : Double;
{: Returns the number of CPU cycles since startup.<p>
   Use the similarly named CPU instruction. }
function RDTSC : Int64;

function GLOKMessageBox(const Text, Caption: string): Integer;
procedure GLLoadBitmapFromInstance(Instance: LongInt; ABitmap: TBitmap; AName: string);
procedure ShowHTMLUrl(Url: String);
function GLGetTickCount:int64;

{$IFDEF GLS_DELPHI_5_DOWN}
function ColorToString(Color: TColor): string;
{$ENDIF}

// StrUtils.pas
function AnsiStartsText(const ASubText, AText: string): Boolean;

// Classes.pas
function IsSubComponent(const AComponent: TComponent): Boolean;
procedure MakeSubComponent(const AComponent: TComponent; const Value: Boolean);

{$IFDEF GLS_DELPHI_5_DOWN}
// SysUtils.pas
function StrToFloatDef(const S: string; const Default: Extended): Extended; overload;
function TryStrToFloat(const S: string; out Value: Extended): Boolean; overload;
function TryStrToFloat(const S: string; out Value: Double): Boolean; overload;
function TryStrToFloat(const S: string; out Value: Single): Boolean; overload;

// Math.pas
function IsNan(const AValue: Double): Boolean; overload;
function IsNan(const AValue: Single): Boolean; overload;
function IsNan(const AValue: Extended): Boolean; overload;
function IsInfinite(const AValue: Double): Boolean;
{$ENDIF}

{$IFDEF GLS_DELPHI_7_UP}
function FindUnitName(anObject: TObject): string; overload;
function FindUnitName(aClass: TClass): string; overload;
{$ENDIF}

{$IFNDEF GLS_COMPILER_2009_UP}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
{$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
uses
  ShellApi;
{$ENDIF}
{$IFDEF UNIX}

{$ENDIF}

var
   vInvPerformanceCounterFrequency : Double;
   vInvPerformanceCounterFrequencyReady : Boolean = False;

function IsSubComponent(const AComponent: TComponent): Boolean;
begin
{$IFDEF GLS_DELPHI_5_DOWN}
  Result := False; // AFAIK Delphi 5 does not know what is a SubComponent.
{$ELSE}
  Result := (csSubComponent in AComponent.ComponentStyle);
{$ENDIF}
end;

procedure MakeSubComponent(const AComponent: TComponent; const Value: Boolean);
begin
{$IFDEF GLS_DELPHI_5_DOWN}
 // AFAIK Delphi 5 does not know what is a SubComponent, so ignore this.
{$ELSE}
  AComponent.SetSubComponent(Value);
{$ENDIF}
end;

{$IFDEF GLS_DELPHI_5_DOWN}
function StrToFloatDef(const S: string; const Default: Extended): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := Default;
end;

function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended)
end;

function TryStrToFloat(const S: string; out Value: Double): Boolean;
var
  LValue: Extended;
begin
  Result := TextToFloat(PChar(S), LValue, fvExtended);
  if Result then
    Value := LValue;
end;

function TryStrToFloat(const S: string; out Value: Single): Boolean;
var
  LValue: Extended;
begin
  Result := TextToFloat(PChar(S), LValue, fvExtended);
  if Result then
    Value := LValue;
end;

function IsNan(const AValue: Single): Boolean;
begin
  Result := ((PLongWord(@AValue)^ and $7F800000)  = $7F800000) and
            ((PLongWord(@AValue)^ and $007FFFFF) <> $00000000);
end;

function IsNan(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) <> $0000000000000000);
end;

function IsNan(const AValue: Extended): Boolean;
type
  TExtented = packed record
    Mantissa: Int64;
    Exponent: Word;
  end;
  PExtended = ^TExtented;
begin
  Result := ((PExtended(@AValue)^.Exponent and $7FFF)  = $7FFF) and
            ((PExtended(@AValue)^.Mantissa and $7FFFFFFFFFFFFFFF) <> 0);
end;

function IsInfinite(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000) = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) = $0000000000000000);
end;
{$ENDIF}

{$IFDEF GLS_DELPHI_5_DOWN}
function ColorToString(Color: TColor): string;
begin
  // Taken from Delphi7 Graphics.pas
  if not ColorToIdent(Color, Result) then
    FmtStr(Result, '%s%.8x', [HexDisplayPrefix, Color]);
end;
{$ENDIF}

function AnsiStartsText(const ASubText, AText: string): Boolean;
{$IFDEF GLS_DELPHI_5_DOWN}
var
  P: PChar;
  L, L2: Integer;
begin
  P := PChar(AText);
  L := Length(ASubText);
  L2 := Length(AText);
  if L > L2 then
    Result := False
  else
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      P, L, PChar(ASubText), L) = 2;
end;
{$ELSE}
begin
  Result := StrUtils.AnsiStartsText(ASubText, AText);
end;
{$ENDIF}

function GLOKMessageBox(const Text, Caption: string): Integer;
begin
  Result := Application.MessageBox(PChar(Text), PChar(Caption), MB_OK);
end;

procedure GLLoadBitmapFromInstance(Instance: LongInt; ABitmap: TBitmap; AName: string);
begin
{$IFDEF MSWINDOWS}
  ABitmap.Handle := LoadBitmap(Instance, PChar(AName));
{$ENDIF}
{$IFDEF UNIX}
  ABitmap.LoadFromResourceName(Instance, PChar(AName));
{$ENDIF}
end;

function GLGetTickCount:int64;
begin
{$IFDEF MSWINDOWS}
  result := GetTickCount;
{$ENDIF}
{$IFDEF UNIX}
  QueryPerformanceCounter(result);
{$ENDIF}
end;

procedure ShowHTMLUrl(Url: String);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(Url), Nil, Nil, SW_SHOW);
{$ENDIF}
{$IFDEF UNIX}
  fpSystem(PChar('env xdg-open ' + Url));
{$ENDIF}
end;

// GLPoint
//
function GLPoint(const x, y : Integer) : TGLPoint;
begin
   Result.X:=x;
   Result.Y:=y;
end;

// RGB
//
function RGB(const r, g, b : Byte) : TColor;
begin
   Result:=(b shl 16) or (g shl 8) or r;
end;

// GLRect
//
function GLRect(const aLeft, aTop, aRight, aBottom : Integer) : TGLRect;
begin
   Result.Left:=aLeft;
   Result.Top:=aTop;
   Result.Right:=aRight;
   Result.Bottom:=aBottom;
end;

// InflateRect
//
procedure InflateGLRect(var aRect : TGLRect; dx, dy : Integer);
begin
   aRect.Left:=aRect.Left-dx;
   aRect.Right:=aRect.Right+dx;
   if aRect.Right<aRect.Left then
      aRect.Right:=aRect.Left;
   aRect.Top:=aRect.Top-dy;
   aRect.Bottom:=aRect.Bottom+dy;
   if aRect.Bottom<aRect.Top then
      aRect.Bottom:=aRect.Top;
end;

// IntersectGLRect
//
procedure IntersectGLRect(var aRect : TGLRect; const rect2 : TGLRect);
var
   a : Integer;
begin
   if (aRect.Left>rect2.Right) or (aRect.Right<rect2.Left)
      or (aRect.Top>rect2.Bottom) or (aRect.Bottom<rect2.Top) then begin
      // no intersection
      a:=0;
      aRect.Left:=a;
      aRect.Right:=a;
      aRect.Top:=a;
      aRect.Bottom:=a;
   end else begin
      if aRect.Left<rect2.Left then
         aRect.Left:=rect2.Left;
      if aRect.Right>rect2.Right then
         aRect.Right:=rect2.Right;
      if aRect.Top<rect2.Top then
         aRect.Top:=rect2.Top;
      if aRect.Bottom>rect2.Bottom then
         aRect.Bottom:=rect2.Bottom;
   end;
end;

// RaiseLastOSError
//
procedure RaiseLastOSError;
var
   e : EGLOSError;
begin
   {$IFDEF FPC}
   e:=EGLOSError.Create('OS Error : '+SysErrorMessage(GetLastOSError));
   {$ELSE}
   e:=EGLOSError.Create('OS Error : '+SysErrorMessage(GetLastError));
   {$ENDIF}
   raise e;
end;

type
  TDeviceCapabilities = record
    Xdpi, Ydpi: integer;        // Number of pixels per logical inch.
    Depth: integer;             // The bit depth.
    NumColors: integer;         // Number of entries in the device's color table.
  end;

function GetDeviceCapabilities: TDeviceCapabilities;
{$IFDEF MSWINDOWS}
var
  Device: HDC;
begin
  Device := GetDC(0);
  try
    result.Xdpi := GetDeviceCaps(Device,LOGPIXELSX);
    result.Ydpi := GetDeviceCaps(Device,LOGPIXELSY);
    result.Depth := GetDeviceCaps(Device,BITSPIXEL);
    result.NumColors := GetDeviceCaps(Device,NUMCOLORS);
  finally
    ReleaseDC(0, Device);
  end;
end;
{$ELSE}
var dpy: PDisplay;
begin
  dpy := XOpenDisplay(nil);
  Result.Depth := DefaultDepth(dpy,DefaultScreen(dpy));
  XCloseDisplay(dpy);

  Result.Xdpi := 96;
  Result.Ydpi := 96;
  Result.NumColors := 1;
end;
{$ENDIF}

// GetDeviceLogicalPixelsX
//
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
begin
  result := GetDeviceCapabilities().Xdpi;
end;

// GetCurrentColorDepth
//
function GetCurrentColorDepth : Integer;
begin
  result := GetDeviceCapabilities().Depth;
end;

// PixelFormatToColorBits
//
function PixelFormatToColorBits(aPixelFormat : TPixelFormat) : Integer;
begin
   case aPixelFormat of
      pfCustom {$IFDEF WIN32}, pfDevice{$ENDIF} :  // use current color depth
         Result:=GetCurrentColorDepth;
      pf1bit  : Result:=1;
{$IFDEF WIN32}
      pf4bit  : Result:=4;
      pf15bit : Result:=15;
{$ENDIF}
      pf8bit  : Result:=8;
      pf16bit : Result:=16;
      pf32bit : Result:=32;
   else
      Result:=24;
   end;
end;

// BitmapScanLine
//
function BitmapScanLine(aBitmap : TGLBitmap; aRow : Integer) : Pointer;
begin
{$IFDEF FPC}
   Assert(False, 'BitmapScanLine unsupported');
   Result:=nil;
{$ELSE}
   Result:=aBitmap.ScanLine[aRow];
{$ENDIF}
end;

{$IFDEF GLS_DELPHI_5_DOWN}
// Sleep
//
procedure Sleep(length : Cardinal);
begin
   Windows.Sleep(length);
end;

// IncludeTrailingPathDelimiter
//
function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := IncludeTrailingBackslash(S);
end;
{$ENDIF} // GLS_DELPHI_5_DOWN

// QueryPerformanceCounter
//
{$IFDEF UNIX}
  {$IFDEF FPC}
   var
     vProgStartSecond : int64;

   procedure Init_vProgStartSecond;
   var
     tz:timeval;
   begin
     fpgettimeofday(@tz, nil);
     vProgStartSecond := tz.tv_sec;
   end;
  {$ENDIF}
{$ENDIF}

procedure QueryPerformanceCounter(var val : Int64);
{$IFDEF WIN32}
begin
   Windows.QueryPerformanceCounter(val);
{$ELSE}
   {$IFDEF FPC}
   var
     tz: timeval;
   begin
     //val:=round(now*MSecsPerDay);
     fpgettimeofday(@tz, nil);
     val := tz.tv_sec - vProgStartSecond;
     val := val * 1000000;
     val := val + tz.tv_usec;
   {$ELSE}
   begin
     val := RDTSC;
   {$ENDIF}
{$ENDIF}
end;

// QueryPerformanceFrequency
//
function QueryPerformanceFrequency(var val : Int64) : Boolean;
{$IFDEF WIN32}
begin
   Result:=Boolean(Windows.QueryPerformanceFrequency(val));
end;
{$ELSE}
  {$IFDEF FPC}
  begin
    val := 1000000;
    Result := True;
  end;
  {$ELSE}
  var
    startCycles, endCycles : Int64;
    aTime, refTime : TDateTime;
  begin
   aTime:=Now;
   while aTime=Now do ;
   startCycles:=RDTSC;
   refTime:=Now;
   while refTime=Now do ;
   endCycles:=RDTSC;
   aTime:=Now;
   val:=Round((endCycles-startCycles)/((aTime-refTime)*(3600*24)));
   Result:=True;
  end;
  {$ENDIF}
{$ENDIF}

// StartPrecisionTimer
//
function StartPrecisionTimer : Int64;
begin
   QueryPerformanceCounter(Result);
end;

// PrecisionTimeLap
//
function PrecisionTimerLap(const precisionTimer : Int64) : Double;
begin
   // we can do this, because we don't really stop anything
   Result:=StopPrecisionTimer(precisionTimer);
end;

// StopPrecisionTimer
//
function StopPrecisionTimer(const precisionTimer : Int64) : Double;
var
   cur, freq : Int64;
begin
   QueryPerformanceCounter(cur);
   if not vInvPerformanceCounterFrequencyReady then begin
      QueryPerformanceFrequency(freq);
      vInvPerformanceCounterFrequency:=1.0/freq;
      vInvPerformanceCounterFrequencyReady:=True;
   end;
   Result:=(cur-precisionTimer)*vInvPerformanceCounterFrequency;
end;

// RDTSC
//
function RDTSC : Int64;
{$IFDEF FPC}
begin
  raise exception.create('Using GLCrossPlatform.RDTSC is a bad idea!');
end;
{$ELSE}
asm
   db $0f, $31
end;
{$ENDIF}

{$IFDEF GLS_DELPHI_7_UP}
{$IFNDEF GLS_COMPILER_2009_UP}
type
  PClassData = ^TClassData;
  TClassData = record
    ClassType: TClass;
    ParentInfo: Pointer;
    PropCount: SmallInt;
    UnitName: ShortString;
  end;
{$ENDIF}

function FindUnitName(anObject: TObject): string;
{$IFDEF GLS_COMPILER_2009_UP}
begin
  if Assigned(anObject) then
    Result := anObject.UnitName
  else
    Result:='';
end;
{$ELSE}
var
  LClassInfo: Pointer;
begin
  Result:='';
  if anObject=nil then Exit;

  LClassInfo := anObject.ClassInfo;
  if LClassInfo <> nil then
    Result := String(PClassData(Integer(LClassInfo) + 2 + PByte(Integer(LClassInfo) + 1)^).UnitName);
end;
{$ENDIF}

function FindUnitName(aClass: TClass): string;
{$IFDEF GLS_COMPILER_2009_UP}
begin
  if Assigned(aClass) then
    Result := aClass.UnitName
  else
    Result:='';
end;
{$ELSE}
var
  LClassInfo: Pointer;
begin
  Result:='';
  if aClass=nil then Exit;

  LClassInfo := aClass.ClassInfo;
  if LClassInfo <> nil then
    Result := String(PClassData(Integer(LClassInfo) + 2 + PByte(Integer(LClassInfo) + 1)^).UnitName);
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF GLS_COMPILER_2009_UP}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;

function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := (C < #$0100) and (AnsiChar(C) in CharSet);
end;
{$ENDIF}


initialization
{$IFDEF FPC}
  {$IFDEF UNIX}
  Init_vProgStartSecond;
  {$ENDIF}
{$ENDIF}
end.
