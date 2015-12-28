// FMain
{: Main form
}
unit FMain;

{$MODE Delphi}

interface

uses
  LCLProc,LCLIntf,LMessages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLLCLViewer, GLVectorGeometry,
  GLVectorFileObjects, GLTexture, ApplicationFileIO, UAirplane, GLCadencer,
  GLTerrainRenderer, GLHeightData, GLHeightTileFileHDS, GLSkyBox, GLFileWAV,
  GLFileMP3, GLTexCombineShader, UAirBlastEngine, UAirBlastControler,
  UABControlerUI, GLParticleFX, GLPerlinPFX, UGameEngine, GLCanvas, GLSound,
  GLSMFMOD, UABVoice, DToolBox, GLBitmapFont, GLWindowsFont, ExtCtrls,
  GLGameMenu, GLHUDObjects, GLMaterial, GLCoordinates, GLCrossPlatform,
  GLBaseClasses,FileUtil,LCLType;

type
  TMain = class(TForm)
    GLScene: TGLScene;
    SceneViewer: TGLSceneViewer;
    LSSun: TGLLightSource;
    MaterialLibrary: TGLMaterialLibrary;
    ApplicationFileIO: TApplicationFileIO;
    GLCadencer: TGLCadencer;
    HTF: TGLHeightTileFileHDS;
    TerrainRenderer: TGLTerrainRenderer;
    SkyBox: TGLSkyBox;
    MLSkyBox: TGLMaterialLibrary;
    GLTexCombineShader: TGLTexCombineShader;
    ParticleFXRenderer: TGLParticleFXRenderer;
    PFXSmoke: TGLPerlinPFXManager;
    DCRoot: TGLDummyCube;
    PFXFire: TGLPerlinPFXManager;
    GLSMFMOD: TGLSMFMOD;
    GLSoundLibrary: TGLSoundLibrary;
    DCProxies: TGLDummyCube;
    PFXDust: TGLCustomSpritePFXManager;
    Timer: TTimer;
    DCMenu: TGLDummyCube;
    WBFMenu: TGLWindowsBitmapFont;
    TIMenuVoiceDelay: TTimer;
    IMLogo: TImage;
    TISplash: TTimer;
    HSCover: TGLHUDSprite;
    GameMenu: TGLGameMenu;
    GameOverMenu: TGLGameMenu;
    AbortGameMenu: TGLGameMenu;
    DCSortedRoot: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
    function ApplicationFileIOFileStream(const fileName: string;
      mode: Word): TStream;
    function ApplicationFileIOFileStreamExists(
      const fileName: string): Boolean;
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TimerTimer(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TIMenuVoiceDelayTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TISplashTimer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure GameMenuSelectedChanged(Sender: TObject);
  private
    { Private declarations }
    procedure GameEngineSpawn(engine: TAirBlastEngine; airplane: TABAirplane);
    procedure DoOptionsDlg;
    procedure GameMenuMainMenu;
    procedure GameOverMenuSetup;
    procedure GameAbortMenuSetup;
    procedure StartGame(const fileName: string);
  public
    { Public declarations }
    FDeltaTimeBuffer: Double;
    FGameEngine: TAirBlastEngine;
    FRecorder: TABControlerRecorder;
    FPlayback: TABControlerPlayback;
    FDebugMode, FInTutorial: Boolean;
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

uses GLFile3DS, Jpeg, GLKeyboard, FConfigControls, FOptionsDlg, UABUtils,
GLScreen;

const
  cPaths = 'Terrains;Models;Skymap;Textures;Missions;Sounds;Voices;Music';

procedure SetTexImageName(ml: TGLMaterialLibrary; const matName, fileName:
  string);
var
  libMat: TGLLibMaterial;
  img: TGLPicFileImage;
begin
  libMat := ml.LibMaterialByName(matName);
  libMat.Material.Texture.ImageClassName := TGLPicFileImage.ClassName;
  img := TGLPicFileImage(libMat.Material.Texture.Image);
  img.PictureFileName := fileName;
end;

procedure TMain.FormCreate(Sender: TObject);
var
  searchRec: TSearchRec;
begin
  //   FDebugMode:=True;

  SetCurrentDirUTF8(ExtractFilePath(Application.ExeName)); { *Konvertiert von SetCurrentDir* }
  SceneViewer.Cursor := crNone;

  HTF.HTFFileName := 'terrains'+DirectorySeparator+'desert.htf';
  HTF.MaxPoolSize := 16 * 1024 * 1024;

  SetTexImageName(MaterialLibrary, 'Terrain', 'Desert.jpg');
  SetTexImageName(MaterialLibrary, 'Detail', 'Detail1.jpg');
  SetTexImageName(MaterialLibrary, 'Detail2', 'Detail.jpg');
  SetTexImageName(MaterialLibrary, 'Title', 'AirBlast.jpg');
  SetTexImageName(MaterialLibrary, 'Victory', 'Victory.jpg');
  SetTexImageName(MaterialLibrary, 'Defeat', 'Defeat.jpg');

  SetTexImageName(MLSkyBox, 'north', 'North.jpg');
  SetTexImageName(MLSkyBox, 'east', 'East.jpg');
  SetTexImageName(MLSkyBox, 'south', 'South.jpg');
  SetTexImageName(MLSkyBox, 'west', 'West.jpg');
  SetTexImageName(MLSkyBox, 'top', 'Top.jpg');

{$WARNINGS OFF}
  FindFirstUTF8('Sounds'+DirectorySeparator+'*.*',faArchive,searchRec); { *Konvertiert von FindFirst* }
{$WARNINGS ON}
  repeat
    GLSoundLibrary.Samples.AddFile('Sounds'+DirectorySeparator + searchRec.Name,
      ChangeFileExt(searchRec.Name, ''))
  until FindNextUTF8(searchRec) { *Konvertiert von FindNext* } <> 0;
  FindCloseUTF8(searchRec); { *Konvertiert von FindClose* }

  GLSMFMOD.Active := True;

  FGameEngine := TAirBlastEngine.Create;
  FGameEngine.SoundLibrary := GLSoundLibrary;
  FGameEngine.MaterialLibrary := MaterialLibrary;
  FGameEngine.TerrainRenderer := TerrainRenderer;
  FGameEngine.PFXRenderer := ParticleFXRenderer;
  FGameEngine.Cadencer := GLCadencer;
  FGameEngine.PFXSmoke := PFXSmoke;
  FGameEngine.PFXFire := PFXFire;
  FGameEngine.PFXDust := PFXDust;
  FGameEngine.SceneRoot := DCRoot;
  FGameEngine.SortedSceneRoot := DCSortedRoot;
  FGameEngine.ProxiesRoot := DCProxies;
  FGameEngine.SoundManager := GLSMFMOD;
  FGameEngine.OnSpawnAirplane := GameEngineSpawn;

  if FileExistsUTF8('AirBlast.options') then
    begin
      FGameEngine.Options.LoadFromFile('AirBlast.options');
      FGameEngine.ApplyOptions;
    end;
  SceneViewer.Buffer.AntiAliasing := FGameEngine.OptionsFSAA;

  if not FDebugMode then
  begin
    if FGameEngine.Options.Values['VSync'] = 'Y' then
      SceneViewer.VSync := vsmSync;
    BorderStyle := bsNone;
    //SetFullscreenMode(FGameEngine.OptionsScreenResolution, 100);
    SetBounds(0, 0, Screen.Width, Screen.Height); //}
    IMLogo.Picture.LoadFromFile('Textures'+DirectorySeparator+'GLScene.bmp');
    IMLogo.Left := (ClientWidth - IMLogo.Width) div 2;
    IMLogo.Top := (ClientHeight - IMLogo.Height) div 2;
    StartGame('Demo.mis');
    GameMenuMainMenu;
  end
  else
  begin
    FGameEngine.SetupEventsDebugStuff;
    StartGame('Endurance.mis');
  end;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  RestoreDefaultMode;

  FGameEngine.Free;

  {   if FRecorder<>nil then
        FRecorder.Log.SaveToFile('flight.log');  //}
end;

procedure TMain.FormShow(Sender: TObject);
begin
  if not SceneViewer.Visible then
  begin
    HSCover.Width := ClientWidth;
    HSCover.Height := ClientHeight;
    HSCover.Position.SetPoint(ClientWidth * 0.5, ClientHeight * 0.5, 0);
    with HSCover.Material.Texture do
    begin
      MappingSCoordinates.X := 1 / 512;
      MappingSCoordinates.W := 0.5;
      MappingTCoordinates.Y := 1 / 128;
      MappingTCoordinates.W := 0.5;
    end;
  end;
end;

procedure TMain.FormPaint(Sender: TObject);
begin
  TISplash.Enabled := True;
  Self.OnPaint := nil;
end;

procedure TMain.TISplashTimer(Sender: TObject);
begin
  SceneViewer.SetBounds(0, 0, ClientWidth, ClientHeight);
  SceneViewer.Visible := True;
  TISplash.Enabled := False;
end;

procedure TMain.GameEngineSpawn(engine: TAirBlastEngine; airplane: TABAirplane);
begin
  if not FDebugMode then
    Exit;

  if airplane.Name = 'Player' then
  begin
    if FRecorder = nil then
      FRecorder := TABControlerRecorder.Create;
    airplane.Controler.Controler := FRecorder; //}
    {      if FPlayback=nil then begin
             FPlayback:=TABControlerPlayback.Create;
             FPlayback.Log.LoadFromFile('flight.log');
          end;
          (airplane.Controler as TABControlerUI).Active:=False;
          airplane.Controler.Controler:=FPlayback;
          FPlayback.PlaybackControler:=TABControler(airplane.Controler);  // }
  end;

  if airplane.Controler is TABControlerWaypoints then
    TABControlerWaypoints(airplane.Controler).CreateGLLines(DCRoot);
end;

// DoOptionsDlg
//

procedure TMain.DoOptionsDlg;
begin
  FGameEngine.Pause;
  FGameEngine.StopVoice('GameMenu');
  OptionsDlg.Execute('AirBlast.options');
  FGameEngine.Options.LoadFromFile('AirBlast.options');
  FGameEngine.ApplyOptions;
  FGameEngine.Resume;
end;

procedure TMain.GameMenuSelectedChanged(Sender: TObject);
begin
  if DCMenu.Visible then
  begin
    TIMenuVoiceDelay.Enabled := False;
    TIMenuVoiceDelay.Enabled := True;
    FGameEngine.Play2DSound('Menu_Click.wav');
  end;
end;

procedure TMain.GameMenuMainMenu;
begin
  with GameMenu do
  begin
    Items.CommaText :=
      '"Instant Action","Enter Tournament","Tutorial","Options","Exit Game"';
    Selected := 0;
    Enabled[0] := (FGameEngine.Options.Values['TutorialPassed'] = 'Y');
    Enabled[1] := False;
  end;
  GameMenu.Visible := True;
  DCMenu.Visible := True;
end;

// GameOverMenuSetup
//

procedure TMain.GameOverMenuSetup;

  function PaddedConcat(str1, str2: string): string;
  var
    n: Integer;
  begin
    n := 1;
    repeat
      Result := str1 + StringOfChar(' ', n) + str2;
      Inc(n);
    until WBFMenu.TextWidth(Result) > 800;
  end;

var
  i, k, iTeam, totalScore, nbDeaths: Integer;
  buf: string;
  items: TStrings;
begin
  items := GameOverMenu.Items;
  items.Clear;
  items.Add('');
  // global results
  case FGameEngine.GameResult of
    grVictory:
      begin
        GameOverMenu.TitleMaterialName := 'Victory';
        items.Add('You have won the match...');
        FGameEngine.PlayVoice('GameMenu', 'You have won the match');
        FGameEngine.PlayMusic('Simplicity_Calpomatt.wav', True);
      end;
    grDefeat:
      begin
        GameOverMenu.TitleMaterialName := 'Defeat';
        items.Add('You have lost the match...');
        FGameEngine.PlayVoice('GameMenu', 'You have lost the match');
        FGameEngine.PlayMusic('Unforsaken_Calpomatt.wav', True);
      end;
  else
    GameOverMenu.TitleMaterialName := 'Title';
    items.Add('Match was tied');
    FGameEngine.PlayMusic('Unforsaken_Calpomatt.wav', True);
  end;
  items.Add('Rating');
  items.Add('');
  // scorecard
  k := 0;
  iTeam := 0;
  nbDeaths := 0;
  for i := 0 to FGameEngine.TeamCount - 1 do
  begin
    if FGameEngine.Teams[i].Name = 'Player' then
    begin
      k := k + FGameEngine.Teams[i].Score;
      iTeam := i;
    end
    else
      nbDeaths := nbDeaths + FGameEngine.Teams[i].Score;
  end;
  totalScore := k * 1000;
  buf := IntToStr(k * 1000);
  if k >= 0 then
    buf := '+' + buf;
  items.Add(PaddedConcat('Kills Score (x1000)', buf));
  items.Add(PaddedConcat('Losses (x1000)', '-' + IntToStr(nbDeaths * 1000)));
  totalScore := totalScore - nbDeaths * 1000;
  with FGameEngine.Teams[iTeam] do
  begin
    totalScore := totalScore - FireRecord[frtMissile] * 100;
    items.Add(PaddedConcat('Missiles Fired (x100)', '-' +
      IntToStr(FireRecord[frtMissile] * 100)));
    totalScore := totalScore - FireRecord[frtGunRound];
    items.Add(PaddedConcat('Gun Rounds (x1)', '-' +
      IntToStr(FireRecord[frtGunRound])));
    totalScore := totalScore - FireRecord[frtDecoy] * 25;
    items.Add(PaddedConcat('Decoys Dropped (x25)', '-' +
      IntToStr(FireRecord[frtDecoy] * 25)));
  end;
  items.Add('');
  items.Add('Total Score');
  items.Add(IntToStr(totalScore));
  items.Add('');
  items.Add('Ok');

  items[2] := FGameEngine.RatingForScore(totalScore);

  for i := 0 to GameOverMenu.Items.Count - 2 do
    GameOverMenu.Enabled[i] := False;

  GameOverMenu.Selected := GameOverMenu.Items.Count - 1;
end;

// GameAbortMenuSetup
//

procedure TMain.GameAbortMenuSetup;
begin
  AbortGameMenu.Enabled[0] := False;
  AbortGameMenu.Enabled[1] := False;
  AbortGameMenu.Selected := 2;
end;

// StartGame
//

procedure TMain.StartGame(const fileName: string);
begin
  TIMenuVoiceDelay.Enabled := False;
  FGameEngine.StopVoices;
  DCMenu.Visible := False;
  GameMenu.Visible := False;

  FGameEngine.Clear;
  FGameEngine.LoadFromFile('Missions'+DirectorySeparator+fileName);

  with FGameEngine.AddViewerCam do
  begin
    ViewerBuffer := SceneViewer.Buffer;
    MobileName := 'Player';
    GLSMFMOD.Listener := Camera;
  end;

  FGameEngine.Startup;
  FGameEngine.Progress(0.01);
  FGameEngine.Progress(0.01);
  GLCadencer.Enabled := True;
end;

procedure TMain.TIMenuVoiceDelayTimer(Sender: TObject);
begin
  FGameEngine.PlayVoice('GameMenu', GameMenu.SelectedText);
  TIMenuVoiceDelay.Enabled := False;
end;

function TMain.ApplicationFileIOFileStream(const fileName: string;
  mode: Word): TStream;
begin
  Result := TFileStream.Create(FindInPaths(fileName, cPaths), mode);
end;

function TMain.ApplicationFileIOFileStreamExists(
  const fileName: string): Boolean;
begin
  Result := (FindInPaths(fileName, cPaths) <> '');
end;

procedure TMain.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if not SceneViewer.Visible then
    Exit;
  if not GameOverMenu.Visible then
  begin
    if HSCover.Visible then
    begin
      HSCover.AlphaChannel := HSCover.AlphaChannel - 0.01;
      if HSCover.AlphaChannel <= 0 then
        HSCover.Visible := False;
    end;

    PFXSmoke.Rotation := newTime * 3;
    FDeltaTimeBuffer := FDeltaTimeBuffer + deltaTime;
    while FDeltaTimeBuffer >= 0.01 do
    begin
      FGameEngine.Progress(0.01);
      FDeltaTimeBuffer := FDeltaTimeBuffer - 0.01;
    end;
  end
  else
    FGameEngine.Progress(0);

  if FGameEngine.Status = gesCompleted then
  begin
    if not GameOverMenu.Visible then
    begin
      if FInTutorial then
      begin
        FGameEngine.Options.Values['TutorialPassed'] := 'Y';
        FGameEngine.Options.SaveToFile('AirBlast.options');
        StartGame('Demo.mis');
        GameMenuMainMenu;
      end
      else
      begin
        FGameEngine.StopVoices;
        FGameEngine.StopMusic;
        GameMenu.Visible := False;
        GameOverMenu.Visible := True;
        GameOverMenuSetup;
      end;
      DCMenu.Visible := True;
    end;
  end;
end;

procedure TMain.TimerTimer(Sender: TObject);
var
  playerMobile: TMobile;
begin
  playerMobile := FGameEngine.MobileByName('Player');
  if playerMobile <> nil then
    Caption := Format('%.1f, %.1f, %.1f / ', [playerMobile.Position[0],
      playerMobile.Position[1], playerMobile.Position[2]])
  else
    Caption := '';
  Caption := Caption + SceneViewer.FramesPerSecondText + ' / ' +
    IntToStr(TerrainRenderer.LastTriangleCount) + ' tris'
    + ' / ' + IntToStr(ParticleFXRenderer.LastParticleCount) + ' particles'
    + Format(' (%.1f ms sort)', [ParticleFXRenderer.LastSortTime]);
  SceneViewer.ResetPerformanceMonitor;
end;

procedure TMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  sl: TStrings;
begin
  case Key of
    VK_F2: DoOptionsDlg;
    VK_ESCAPE:
      begin
        if DCMenu.Visible then
        begin
          if AbortGameMenu.Visible then
          begin
            AbortGameMenu.Selected := 3;
            Key := VK_RETURN;
          end
          else
            Close;
        end
        else
        begin
          GameAbortMenuSetup;
          FGameEngine.Pause;
          GLCadencer.Enabled := False;
          AbortGameMenu.Visible := True;
          DCMenu.Visible := True;
        end;
      end;
    Ord('X'), VK_F4: if ssAlt in Shift then
        Close;
    222: if FDebugMode then
      begin
        // WayPoint log helper
        sl := TStringList.Create;
        if FileExistsUTF8('path.log') { *Konvertiert von FileExists* } then
          sl.LoadFromFile('path.log');
        if Trim(sl.text) = '' then
          sl.Clear;
        sl.Add('WP' + IntToStr(sl.Count) + '=' +
          Vector3ToString(FGameEngine.MobileByName('Player').position));
        sl.SaveToFile('path.log');
        sl.Free;
      end;
  end;
  if DCMenu.Visible then
  begin
    if GameMenu.Visible then
    begin
      case Key of
        VK_DOWN: GameMenu.SelectNext;
        VK_UP: GameMenu.SelectPrev;
        VK_RETURN:
          begin
            GameMenuSelectedChanged(Self);
            if GameMenu.SelectedText = 'Options' then
              DoOptionsDlg
            else if GameMenu.SelectedText = 'Exit Game' then
            begin
              TIMenuVoiceDelay.Enabled := False;
              FGameEngine.StopVoice('GameMenu');
              Close;
            end
            else if GameMenu.SelectedText = 'Back' then
              GameMenuMainMenu
            else if GameMenu.SelectedText = 'Instant Action' then
            begin
              GameMenu.Items.CommaText :=
                '"Back","1 vs 1","1 vs 1 Furball","1 vs 1 Firestorm","Endurance"';
            end
            else if GameMenu.SelectedText = 'Tutorial' then
            begin
              StartGame('Tutorial.mis');
              FInTutorial := True;
            end
            else if GameMenu.SelectedText = '1 vs 1' then
            begin
              StartGame('1vs1.mis');
              FGameEngine.PlayVoice('GameMenu', '1 vs 1');
              FGameEngine.PlayVoice('GameMenu', 'Match Beginning');
            end
            else if GameMenu.SelectedText = '1 vs 1 Furball' then
            begin
              StartGame('1vs1-Furball.mis');
              FGameEngine.PlayVoice('GameMenu', '1 vs 1 Furball');
              FGameEngine.PlayVoice('GameMenu', 'Match Beginning');
            end
            else if GameMenu.SelectedText = '1 vs 1 Firestorm' then
            begin
              StartGame('1vs1-Firestorm.mis');
              FGameEngine.PlayVoice('GameMenu', '1 vs 1 Firestorm');
              FGameEngine.PlayVoice('GameMenu', 'Match Beginning');
            end
            else if GameMenu.SelectedText = 'Endurance' then
            begin
              StartGame('Endurance.mis');
            end;
          end;
      end;
    end;
    if GameOverMenu.Visible then
    begin
      case Key of
        VK_DOWN: GameOverMenu.SelectNext;
        VK_UP: GameOverMenu.SelectPrev;
        VK_RETURN:
          begin
            GameOverMenu.Visible := False;
            StartGame('Demo.mis');
            GameMenuMainMenu;
          end;
      end;
    end;
    if AbortGameMenu.Visible then
    begin
      case Key of
        VK_DOWN: AbortGameMenu.SelectNext;
        VK_UP: AbortGameMenu.SelectPrev;
        VK_RETURN:
          begin
            AbortGameMenu.Visible := False;
            if AbortGameMenu.SelectedText = 'Yes' then
            begin
              StartGame('Demo.mis');
              GameMenuMainMenu;
            end
            else
            begin
              DCMenu.Visible := False;
              GLCadencer.Enabled := True;
              FGameEngine.Resume;
            end;
          end;
      end;
    end;
  end;
  Key := 0;
end;

procedure TMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  KeyboardNotifyWheelMoved(wheelDelta);
end;

end.

