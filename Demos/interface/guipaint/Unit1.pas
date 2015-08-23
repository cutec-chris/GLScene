{: The Simple Paint Program using Gui components from GLScene.<p>

   To use the Gui you must place a TGLGUILayout component on your form,
   this is a storage for Gui layouts within a single texture, each layout
   is identified by a name. All Layouts can be used with any Gui Component,
   however features like a forms title might look awkward on a checkbox...

   For correct usage a Material Library should be used.

   If a BitmapFont or WindowsFont is applied to a layout it is auto set to
   the components, however it is perfectly valid to set it to someother font.

   To Interface correctly, handlers must send the mouse and key events
   to a root gui component, this can be any of the keyboard aware gui
   components or a RootControl(a tramsparent component with no other purpose)
   Plus the root gui component should have its DoChanges invoked, this
   makes form movement and other mouse/key events changes fluintly
   match the rendering process, eg no flicker.

   All other Gui component must be below the root control in the GLScene
   hierachy, this is a rule which allows you to seperate gui from the
   rest of glscene and save some clock cycles on <tab> operations.
   Plus mouse events...

   For a more thorough look on the gui please check out my article on
   the caperaven site: http://caperaven.co.za
   under "Online documentation" called "Gui Interface"

   The two BMP's pen.bmp and brush.bmp have been published by Borland in the
   Doc/GraphEx demo that comes with Delphi.
   The rest is MPL as part of the GLScene project.

   NOTICE IS YOU HAVE A LOW FRAME RATE and you feel the drawing is lagging to
   far behind, try setting GLCanvas.MaxInvalidRenderCount to a lower value in
   the formcreate event.


	<b>History : </b><font size=-1><ul>
      <li>01/05/03 - JAJ - Creation
	</ul></font>
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLHUDObjects, GLObjects, GLCadencer, ExtCtrls,
  GLBitmapFont, GLViewer, GLWindowsFont, Menus, GLWindows, GLGui,
  GLTexture, GLCrossPlatform, LResources, GLScene, GLMaterial;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    WindowsBitmapFont1: TGLWindowsBitmapFont;
    MainMenu1: TMainMenu;
    Font1: TMenuItem;
    WindowsFont1: TMenuItem;
    FontDialog1: TFontDialog;
    GLGuiLayout1: TGLGuiLayout;
    GLForm1: TGLForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    BrushButton: TGLButton;
    PenButton: TGLButton;
    GLPanel1: TGLPanel;
    GLCanvas: TGLCustomControl;
    WhiteButton: TGLButton;
    BlackButton: TGLButton;
    RedButton: TGLButton;
    GreenButton: TGLButton;
    BlueButton: TGLButton;
    GuiRoot: TGLBaseControl;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure WindowsFont1Click(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GLCanvasMouseDown(Sender: TObject;
      Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLCanvasMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure GLCanvasMouseUp(Sender: TObject;
      Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLCanvasRender(sender: TGLCustomControl; Bitmap: TBitmap);
    procedure FormCreate(Sender: TObject);
    procedure WhiteButtonButtonClick(Sender: TObject);
    procedure BlackButtonButtonClick(Sender: TObject);
    procedure RedButtonButtonClick(Sender: TObject);
    procedure GreenButtonButtonClick(Sender: TObject);
    procedure BlueButtonButtonClick(Sender: TObject);
    procedure PenButtonButtonClick(Sender: TObject);
    procedure BrushButtonButtonClick(Sender: TObject);
    procedure GLCanvasAcceptMouseQuery(Sender: TGLBaseControl;
      Shift: TShiftState; Action: TGLMouseAction; Button: TGLMouseButton; X,
      Y: Integer; var accept: Boolean);
    procedure GLForm1Moving(Sender: TGLForm; var Left, Top: Single);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    StartX   : Integer;
    StartY   : Integer;
    CurrentX : Integer;
    CurrentY : Integer;
  end;

var
  Form1: TForm1;

implementation


procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  // set frame rate to 10 when program is not focused to reduce cpu usage...
  If Form1.Focused then
    GLCadencer1.SleepLength := 0
  else GLCadencer1.SleepLength := 100;

  // make things move a little
  GLForm1.DoChanges;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.WindowsFont1Click(Sender: TObject);
begin
   FontDialog1.Font:=WindowsBitmapFont1.Font;
   if FontDialog1.Execute then
      WindowsBitmapFont1.Font:=FontDialog1.Font;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GuiRoot.MouseDown(Sender,TGLMouseButton(Button),Shift,X,Y);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  GuiRoot.MouseMove(Sender,Shift,X,Y);
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GuiRoot.MouseUp(Sender,TGLMouseButton(Button),Shift,X,Y);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GuiRoot.KeyDown(Sender,Key,Shift);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  GuiRoot.KeyPress(Sender,Key);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GuiRoot.KeyUp(Sender,Key,Shift);
end;

procedure TForm1.GLCanvasMouseDown(Sender: TObject;
  Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Button = mbLeft then
  Begin
    // Make sure all mouse events are sent to the canvas before other GuiComponents, see GLCanvasAcceptMouseQuery.
    GuiRoot.ActiveControl := GLCanvas;
    // Set a status not to send mouse message to child components if any, see GLCanvasAcceptMouseQuery.
    GLCanvas.KeepMouseEvents := True;
    StartX := X;
    StartY := Y;
  End;
end;

procedure TForm1.GLCanvasMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  CurrentX := X;
  CurrentY := Y;
end;

procedure TForm1.GLCanvasMouseUp(Sender: TObject;
  Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Button = mbLeft then
  Begin
    StartX := -1;
    StartY := -1;
    // Set normal mouse message handling, see GLCanvasAcceptMouseQuery.
    GuiRoot.ActiveControl := Nil;
    // Set that childs are allowed to get mouse events, meant for then, see GLCanvasAcceptMouseQuery.
    GLCanvas.KeepMouseEvents := False;
  End;
end;

procedure TForm1.GLCanvasRender(sender: TGLCustomControl; Bitmap: TBitmap);
begin
  Bitmap.Width := Round(GLCanvas.width);
  Bitmap.Height := Round(GLCanvas.height);
  If StartX <> -1 then
  Begin
    Bitmap.Canvas.MoveTo(StartX-Round(Sender.left),StartY-Round(Sender.top));
    Bitmap.Canvas.LineTo(CurrentX-Round(Sender.left),CurrentY-Round(Sender.top));
    StartX := CurrentX;
    StartY := CurrentY;
  End;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLCanvas.MaxInvalidRenderCount := 40;
  StartX := -1;
end;

procedure TForm1.WhiteButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Color := clWhite;
end;

procedure TForm1.BlackButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Color := clBlack;
end;

procedure TForm1.RedButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Color := clRed;
end;

procedure TForm1.GreenButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Color := clGreen;
end;

procedure TForm1.BlueButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Color := clBlue;
end;

procedure TForm1.PenButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Width := 1;
end;

procedure TForm1.BrushButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Width := 5;
end;

procedure TForm1.GLCanvasAcceptMouseQuery(Sender: TGLBaseControl;
  Shift: TShiftState; Action: TGLMouseAction; Button: TGLMouseButton; X,
  Y: Integer; var accept: Boolean);
begin
// Sender.KeepMouseEvents is set when drawing,
// if drawing this component, gets mouse events even if they are out of bounds!
  If Sender.KeepMouseEvents then Accept := True;
end;

procedure TForm1.GLForm1Moving(Sender: TGLForm; var Left, Top: Single);
begin
// make sure the form isn't moved out of bounds...

  If Left > GLSceneViewer1.width-32 then
    Left := GLSceneViewer1.width-32;

  If Left+Sender.width < 32 then
    Left := 32-Sender.Width;

  If Top > GLSceneViewer1.Height-32 then
    Top := GLSceneViewer1.Height-32;

  If Top < 0 then
    Top := 0;
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  If OpenDialog1.Execute then
  begin
    GLCanvas.Bitmap.LoadFromFile(OpenDialog1.FileName);
  End;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  If SaveDialog1.Execute then
  begin
    GLCanvas.Bitmap.SaveToFile(SaveDialog1.FileName);
  End;
end;

initialization
  {$i Unit1.lrs}

end.
