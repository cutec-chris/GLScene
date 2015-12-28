//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLGameMenu<p>

   Manages a basic game menu UI<p>

	<b>History : </b><font size=-1><ul>
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>04/09/07 - DaStr - Fixed memory leak in TGLGameMenu
                              (BugtrackerID = 1787617) (thanks Pierre Lemerle)
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
      <li>26/03/07 - DaveK - back to TGLSceneObject for Material support
      <li>16/02/07 - DaStr & DaveK - TGLGameMenu.MouseMenuSelect bugfixed (again)
                             Component made descendant of TGLBaseSceneObject
                             IGLMaterialLibrarySupported added
      <li>20/12/06 - DaStr - TGLGameMenu.MouseMenuSelect bugfixed (thanks to Predator)
      <li>03/27/06 - DaveK - added mouse selection support
      <li>03/03/05 - EG - Creation
   </ul></font>
}
unit GLGameMenu;

interface

{$I GLScene.inc}

uses Classes, GLScene, GLMaterial, GLBitmapFont, GLCrossPlatform, GLColor,
     GLRenderContextInfo;

type

   // TGLGameMenuScale
   //
   TGLGameMenuScale = (gmsNormal, gms1024x768);

   // TGLGameMenu
   //
   {: Classic game menu interface made of several lines.<p> }
   TGLGameMenu = class(TGLSceneObject, IGLMaterialLibrarySupported)
      private
         { Private Properties }
         FItems : TStrings;
         FSelected : Integer;
         FFont : TGLCustomBitmapFont;
         FMarginVert, FMarginHorz, FSpacing : Integer;
         FMenuScale : TGLGameMenuScale;
         FBackColor : TGLColor;
         FInactiveColor, FActiveColor, FDisabledColor : TGLColor;
         FMaterialLibrary : TGLMaterialLibrary;
         FTitleMaterialName : TGLLibMaterialName;
         FTitleWidth, FTitleHeight : Integer;
         FOnSelectedChanged : TNotifyEvent;
         FBoxTop, FBoxBottom, FBoxLeft, FBoxRight: Integer;
         FMenuTop: integer;
         //implementing IGLMaterialLibrarySupported
         function GetMaterialLibrary: TGLMaterialLibrary;
		protected
         { Protected Properties }
         procedure SetMenuScale(AValue : TGLGameMenuScale);
         procedure SetMarginHorz(AValue : Integer);
         procedure SetMarginVert(AValue : Integer);
         procedure SetSpacing(AValue : Integer);
         procedure SetFont(AValue : TGLCustomBitmapFont);
         procedure SetBackColor(AValue : TGLColor);
         procedure SetInactiveColor(AValue : TGLColor);
         procedure SetActiveColor(AValue : TGLColor);
         procedure SetDisabledColor(AValue : TGLColor);
         function  GetEnabled(AIndex : Integer) : Boolean;
         procedure SetEnabled(AIndex : Integer; AValue : Boolean);
         procedure SetItems(AValue : TStrings);
         procedure SetSelected(AValue : Integer);
         function  GetSelectedText : String;
         procedure SetMaterialLibrary(AValue : TGLMaterialLibrary);
         procedure SetTitleMaterialName(const AValue : String);
         procedure SetTitleWidth(AValue : Integer);
         procedure SetTitleHeight(AValue : Integer);

         procedure ItemsChanged(Sender : TObject);

		public
         { Public Properties }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
		   procedure BuildList(var rci : TRenderContextInfo); override;

         property Enabled[AIndex : Integer] : Boolean read GetEnabled write SetEnabled;
         property SelectedText : String read GetSelectedText;

         procedure SelectNext;
         procedure SelectPrev;

         procedure MouseMenuSelect(const X, Y: integer);

		published
         { Published Properties }
         property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;

         property MenuScale : TGLGameMenuScale read FMenuScale write SetMenuScale default gmsNormal;
         property MarginHorz : Integer read FMarginHorz write SetMarginHorz default 16;
         property MarginVert : Integer read FMarginVert write SetMarginVert default 16;
         property Spacing : Integer read FSpacing write SetSpacing default 16;
         property Font : TGLCustomBitmapFont read FFont write SetFont;

         property TitleMaterialName : String read FTitleMaterialName write SetTitleMaterialName;
         property TitleWidth : Integer read FTitleWidth write SetTitleWidth default 0;
         property TitleHeight : Integer read FTitleHeight write SetTitleHeight default 0;

         property BackColor : TGLColor read FBackColor write SetBackColor;
         property InactiveColor : TGLColor read FInactiveColor write SetInactiveColor;
         property ActiveColor : TGLColor read FActiveColor write SetActiveColor;
         property DisabledColor : TGLColor read FDisabledColor write SetDisabledColor;

         property Items : TStrings read FItems write SetItems;
         property Selected : Integer read FSelected write SetSelected default -1;
         property OnSelectedChanged : TNotifyEvent read FOnSelectedChanged write FOnSelectedChanged;

         // these are the extents of the menu
         property BoxTop: integer read FBoxTop;
         property BoxBottom: integer read FBoxBottom;
         property BoxLeft: integer read FBoxLeft;
         property BoxRight: integer read FBoxRight;
         // this is the top of the first menu item
         property MenuTop: integer read FMenuTop;

        //publish other stuff from TGLBaseSceneObject
         property ObjectsSorting;
         property VisibilityCulling;
         property Position;
         property Visible;
         property OnProgress;
         property Behaviours;
         property Effects;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, GLCanvas, OpenGL1x, OpenGLTokens;

// ------------------
// ------------------ TGLGameMenu ------------------
// ------------------

// Create
//
constructor TGLGameMenu.Create(AOwner: TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FItems:=TStringList.Create;
   TStringList(FItems).OnChange:=ItemsChanged;
   FSelected:=-1;
   FMarginHorz:=16;
   FMarginVert:=16;
   FSpacing:=16;
   FMenuScale:=gmsNormal;
   FBackColor:=TGLColor.CreateInitialized(Self, clrTransparent, NotifyChange);
   FInactiveColor:=TGLColor.CreateInitialized(Self, clrGray75, NotifyChange);
   FActiveColor:=TGLColor.CreateInitialized(Self, clrWhite, NotifyChange);
   FDisabledColor:=TGLColor.CreateInitialized(Self, clrGray60, NotifyChange);
end;

// Destroy
//
destructor TGLGameMenu.Destroy;
begin
   inherited;
   FItems.Free;
   Font:=nil;
   FBackColor.Free;
   FInactiveColor.Free;
   FActiveColor.Free;
   FDisabledColor.Free;
end;

// Notification
//
procedure TGLGameMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited;
   if Operation=opRemove then begin
      if AComponent=Font then Font:=nil;
      if AComponent=MaterialLibrary then MaterialLibrary:=nil;
   end;
end;

// BuildList
//
procedure TGLGameMenu.BuildList(var rci : TRenderContextInfo);
var
   canvas : TGLCanvas;
   buffer : TGLSceneBuffer;
   i, w, h, tw, y : Integer;
   color : TColorVector;
   libMat : TGLLibMaterial;
begin
   if Font=nil then Exit;
   case MenuScale of
      gmsNormal : begin
         buffer:=TGLSceneBuffer(rci.buffer);
         canvas:=TGLCanvas.Create(buffer.Width, buffer.Height);
      end;
      gms1024x768 : canvas:=TGLCanvas.Create(1024, 768);
   else
      canvas:=nil;
      Assert(False);
   end;
   try
      // determine extents
      h:=FItems.Count*(Font.CharHeight+Spacing)-Spacing+MarginVert*2;
      if TitleHeight>0 then
         h:=h+TitleHeight+Spacing;
      w:=TitleWidth;
      for i:=0 to FItems.Count-1 do begin
         tw:=Font.TextWidth(FItems[i]);
         if tw>w then w:=tw;
      end;
      w:=w+2*MarginHorz;

      // calculate boundaries for user
      FBoxLeft   := Round(Position.X - w / 2);
      FBoxTop    := Round(Position.Y - h / 2);
      FBoxRight  := Round(Position.X + w / 2);
      FBoxBottom := Round(Position.Y + h / 2);

      // paint back
      if BackColor.Alpha>0 then begin
         canvas.PenColor:=BackColor.AsWinColor;
         canvas.PenAlpha:=BackColor.Alpha;
         canvas.FillRect(FBoxLeft, FBoxTop, FBoxRight, FBoxBottom);
      end;

      canvas.StopPrimitive;

      // paint items
      y:=Round(Position.Y-h / 2+MarginVert);
      if TitleHeight>0 then begin
         if (TitleMaterialName<>'') and (MaterialLibrary<>nil) and (TitleWidth>0) then begin
            libMat:=MaterialLibrary.LibMaterialByName(TitleMaterialName);
            if libMat<>nil then begin
               libMat.Apply(rci);
               repeat
               glBegin(GL_QUADS);
                  glTexCoord2f(0, 0);  glVertex2f(Position.X-TitleWidth div 2, y+TitleHeight);
                  glTexCoord2f(1, 0);  glVertex2f(Position.X+TitleWidth div 2, y+TitleHeight);
                  glTexCoord2f(1, 1);  glVertex2f(Position.X+TitleWidth div 2, y);
                  glTexCoord2f(0, 1);  glVertex2f(Position.X-TitleWidth div 2, y);
               glEnd;
               until (not libMat.UnApply(rci));
            end;
         end;
         y:=y+TitleHeight+Spacing;
         FMenuTop := y;
      end
      else
        FMenuTop := y + Spacing;

      for i:=0 to FItems.Count-1 do begin
         tw:=Font.TextWidth(FItems[i]);
         if not Enabled[i] then
            color:=DisabledColor.Color
         else if i=Selected then
            color:=ActiveColor.Color
         else color:=InactiveColor.Color;
         Font.TextOut(rci, Position.X-tw div 2, y, FItems[i], color);
         y:=y+Font.CharHeight+Spacing;
      end;
   finally
      canvas.Free;
   end;
   glEnable(GL_BLEND); // to match rci change
end;

// SelectNext
//
procedure TGLGameMenu.SelectNext;
var
   i : Integer;
begin
   i:=Selected;
   repeat
      i:=i+1;
   until (i>=Items.Count) or Enabled[i];
   if (i<Items.Count) and (i<>Selected) then
      Selected:=i;
end;

// SelectPrev
//
procedure TGLGameMenu.SelectPrev;
var
   i : Integer;
begin
   i:=Selected;
   repeat
      i:=i-1;
   until (i<0) or Enabled[i];
   if (i>=0) and (i<>Selected) then
      Selected:=i;
end;

// SetMenuScale
//
procedure TGLGameMenu.SetMenuScale(AValue : TGLGameMenuScale);
begin
   if FMenuScale<>AValue then begin
      FMenuScale:=AValue;
      StructureChanged;
   end;
end;

// SetMarginHorz
//
procedure TGLGameMenu.SetMarginHorz(AValue : Integer);
begin
   if FMarginHorz<>AValue then begin
      FMarginHorz:=AValue;
      StructureChanged;
   end;
end;

// SetMarginVert
//
procedure TGLGameMenu.SetMarginVert(AValue : Integer);
begin
   if FMarginVert<>AValue then begin
      FMarginVert:=AValue;
      StructureChanged;
   end;
end;

// SetSpacing
//
procedure TGLGameMenu.SetSpacing(AValue : Integer);
begin
   if FSpacing<>AValue then begin
      FSpacing:=AValue;
      StructureChanged;
   end;
end;

// SetFont
//
procedure TGLGameMenu.SetFont(AValue : TGLCustomBitmapFont);
begin
   if FFont<>nil then
      FFont.RemoveFreeNotification(Self);
   FFont:=AValue;
   if FFont<>nil then
      FFont.FreeNotification(Self);
end;

// SetBackColor
//
procedure TGLGameMenu.SetBackColor(AValue : TGLColor);
begin
   FBackColor.Assign(AValue);
end;

// SetInactiveColor
//
procedure TGLGameMenu.SetInactiveColor(AValue : TGLColor);
begin
   FInactiveColor.Assign(AValue);
end;

// SetActiveColor
//
procedure TGLGameMenu.SetActiveColor(AValue : TGLColor);
begin
   FActiveColor.Assign(AValue);
end;

// SetDisabledColor
//
procedure TGLGameMenu.SetDisabledColor(AValue : TGLColor);
begin
   FDisabledColor.Assign(AValue);
end;

// GetEnabled
//
function TGLGameMenu.GetEnabled(AIndex : Integer) : Boolean;
begin
  Result:=not Boolean(pointer(FItems.Objects[AIndex]));
end;

// SetEnabled
//
procedure TGLGameMenu.SetEnabled(AIndex : Integer; AValue : Boolean);
begin
   FItems.Objects[AIndex]:=TObject(pointer(ord(not AValue)));
   StructureChanged;
end;

// SetItems
//
procedure TGLGameMenu.SetItems(AValue : TStrings);
begin
   FItems.Assign(AValue);
   SetSelected(Selected);
end;

// SetSelected
//
procedure TGLGameMenu.SetSelected(AValue : Integer);
begin
   if AValue<-1 then AValue:=-1;
   if AValue>=FItems.Count then AValue:=FItems.Count-1;
   if AValue<>FSelected then begin
      FSelected:=AValue;
      StructureChanged;
      if Assigned(FOnSelectedChanged) then
         FOnSelectedChanged(Self);
   end;
end;

// GetSelectedText
//
function TGLGameMenu.GetSelectedText : String;
begin
   if Cardinal(Selected)<Cardinal(FItems.Count) then
      Result:=FItems[Selected]
   else Result:='';
end;

// SetMaterialLibrary
//
procedure TGLGameMenu.SetMaterialLibrary(AValue : TGLMaterialLibrary);
begin
   if FMaterialLibrary<>nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
   FMaterialLibrary:=AValue;
   if FMaterialLibrary<>nil then
      FMaterialLibrary.FreeNotification(Self);
end;

// SetTitleMaterialName
//
procedure TGLGameMenu.SetTitleMaterialName(const AValue : String);
begin
   if FTitleMaterialName<>AValue then begin
      FTitleMaterialName:=AValue;
      StructureChanged;
   end;
end;

// SetTitleWidth
//
procedure TGLGameMenu.SetTitleWidth(AValue : Integer);
begin
   if AValue<0 then AValue:=0;
   if FTitleWidth<>AValue then begin
      FTitleWidth:=AValue;
      StructureChanged;
   end;
end;

// SetTitleHeight
//
procedure TGLGameMenu.SetTitleHeight(AValue : Integer);
begin
   if AValue<0 then AValue:=0;
   if FTitleHeight<>AValue then begin
      FTitleHeight:=AValue;
      StructureChanged;
   end;
end;

// ItemsChanged
//
procedure TGLGameMenu.ItemsChanged(Sender : TObject);
begin
   SetSelected(FSelected);
   StructureChanged;
end;

// MouseMenuSelect
//
procedure TGLGameMenu.MouseMenuSelect(const X, Y: integer);
begin
  if (X >= BoxLeft)  and (Y >= MenuTop) and
     (X <= BoxRight) and (Y <= BoxBottom) then
  begin
    Selected := (Y - FMenuTop) div (Font.CharHeight + FSpacing);
  end
  else
    Selected := -1;
end;

// GetMaterialLibrary
//
function TGLGameMenu.GetMaterialLibrary: TGLMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClass(TGLGameMenu);

end.
