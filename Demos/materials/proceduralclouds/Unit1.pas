{: Procedural Texture Demo / Tobias Peirick }
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GLScene, GLObjects, GLTexture, GLHUDObjects,
  GLCadencer, GLLCLViewer, GLProcTextures, Spin, ComCtrls, Buttons,
  GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    Panel1: TPanel;
    Label1: TLabel;
    CBFormat: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    CBCompression: TComboBox;
    Label5: TLabel;
    RBDefault: TRadioButton;
    RBDouble: TRadioButton;
    LAUsedMemory: TLabel;
    RBQuad: TRadioButton;
    LARGB32: TLabel;
    LACompression: TLabel;
    GLCadencer1: TGLCadencer;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Label6: TLabel;
    CheckBox2: TCheckBox;
    GLPlane1: TGLPlane;
    TrackBar1: TTrackBar;
    Timer1: TTimer;
    CloudRandomSeedUsedEdit: TEdit;
    CloudImageSizeUsedEdit: TEdit;
    UseCloudFileCB: TCheckBox;
    CloudFileOpenBtn: TSpeedButton;
    CloudFileUsedEdit: TEdit;
    MakeAndSaveCloudNoiseFile: TSpeedButton;
    Label61: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure GLSceneViewer1AfterRender(Sender: TObject);
    procedure CBFormatChange(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure TrackBar1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloudFileOpenBtnClick(Sender: TObject);
    procedure MakeAndSaveCloudNoiseFileClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    newSelection: boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLTextureFormat;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CBFormat.ItemIndex := 3;
  CBCompression.ItemIndex := 0;
  CBFormatChange(Sender);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  if CheckBox1.Checked then
    TGLProcTextureNoise(GLPlane1.Material.Texture.Image).NoiseAnimate(deltaTime);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  GLPlane1.XTiles := TrackBar1.Position;
  GLPlane1.YTiles := TrackBar1.Position;
  {EnvColor clrLightBlue   TextureMode Blend}
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1AfterRender(Sender: TObject);
var
  rgb: integer;
begin
  // update compression stats, only the 1st time after a new selection
  if newSelection then
    with GLPlane1.Material.Texture do
    begin
      rgb := Image.Width * Image.Height * 4;
      LARGB32.Caption := Format('RGBA 32bits would require %d kB', [rgb div 1024]);
      LAUsedMemory.Caption := Format('Required memory : %d kB',
        [TextureImageRequiredMemory div 1024]);
      LACompression.Caption := Format('Compression ratio : %d %%',
        [100 - 100 * TextureImageRequiredMemory div rgb]);
      newSelection := False;
    end;
end;

procedure TForm1.CBFormatChange(Sender: TObject);
var
  aPERM: array [0..255] of byte;
  outfile: Textfile;
  s: string;
  i: integer;
begin
  // adjust settings from selection and reload the texture map
  with GLPlane1.Material.Texture do
  begin
    if (UseCloudFileCB.Checked and (FileExists(CloudFileUsedEdit.Text))) then
    begin
      try
        AssignFile(outfile, CloudFileUsedEdit.Text);   { File selected in dialog box }
        Reset(outfile);
        Readln(outfile, s{'Cloud Base V1.0'});
        for I := 0 to 255 do
        begin
          Readln(outfile, s);
          aPERM[I] := StrToInt(s);
        end;
      finally
        CloseFile(outfile);
      end;
      TGLProcTextureNoise(Image).SetPermFromData(aPERM);
    end
    else
      TGLProcTextureNoise(Image).SetPermToDefault;
    TextureFormat := TGLTextureFormat(integer(tfRGB) + CBFormat.ItemIndex);
    Compression := TGLTextureCompression(integer(tcNone) + CBCompression.ItemIndex);
    TGLProcTextureNoise(Image).MinCut := SpinEdit1.Value;
    TGLProcTextureNoise(Image).NoiseSharpness := SpinEdit2.Value / 100;
    TGLProcTextureNoise(Image).Height := StrToInt(CloudImageSizeUsedEdit.Text);
    TGLProcTextureNoise(Image).Width := StrToInt(CloudImageSizeUsedEdit.Text);
    TGLProcTextureNoise(Image).NoiseRandSeed := StrToInt(CloudRandomSeedUsedEdit.Text);
    ;
    TGLProcTextureNoise(Image).Seamless := CheckBox2.Checked;

    if RBDefault.Checked then
    begin
      GLPlane1.Width := 50;
      GLPlane1.Height := 50;
    end
    else if RBDouble.Checked then
    begin
      GLPlane1.Width := 100;
      GLPlane1.Height := 100;
    end
    else
    begin
      GLPlane1.Width := 400;
      GLPlane1.Height := 400;
    end;
  end;
  newSelection := True;
end;




procedure TForm1.CloudFileOpenBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud base (*.clb)|*.clb';
  OpenDialog1.InitialDir := ExtractFilePath(Application.Exename);
  OpenDialog1.Filename := '*.clb';
  if OpenDialog1.Execute then
  begin
    CloudFileUsedEdit.Text := OpenDialog1.Filename;
  end;
end;

procedure TForm1.MakeAndSaveCloudNoiseFileClick(Sender: TObject);
var
  aPERM: array [0..255] of byte;
  outfile: Textfile;
  i: integer;

  procedure RandomPerm;
  var
    Idiot, Count, More, Less, again: integer;
  begin
    MakeAndSaveCloudNoiseFile.Caption := IntToStr(0);
    Application.ProcessMessages;
    for Idiot := 0 to 255 do
    begin
      aPERM[Idiot] := Random(256);
      //Label61.Caption:= inttostr(Idiot);
      //Application.ProcessMessages;
    end;
    Count := 0;
    repeat
      again := 0;
      Less := Random(256);
      for Idiot := 0 to Count do
      begin
        more := aPERM[Idiot];
        if (Less = more) then
          Inc(again);
      end;
      Label61.Caption := IntToStr(again); //these can be removed.. just for debugging
      Application.ProcessMessages;
      if (again = 0) then
      begin
        aPERM[Count + 1] := Less;
        Inc(Count);
        MakeAndSaveCloudNoiseFile.Caption :=
          IntToStr(Less) + ',' + IntToStr(Count);
        Application.ProcessMessages;
      end;
    until Count = 255;
  end;

begin
  SaveDialog1.Filter := 'Cloud base (*.clb)|*.clb';
  SaveDialog1.InitialDir := ExtractFilePath(Application.Exename);
  SaveDialog1.DefaultExt := 'rnd';
  SaveDialog1.Filename := '*.clb';
  if (SaveDialog1.Execute) then
  begin
    if UpperCase(ExtractFileExt(SaveDialog1.FileName)) = '.CLB' then
    begin
      Application.ProcessMessages;
      Randomize;
      RandomPerm;
      try
        AssignFile(outfile, SaveDialog1.FileName);   { File selected in dialog box }
        Rewrite(outfile);
        Writeln(outfile, 'Cloud Base V1.0');
        for I := 0 to 255 do
          Writeln(outfile, IntToStr(aPERM[I]));
      finally
        CloseFile(outfile);
      end;
      Label61.Caption := 'Done';
      MakeAndSaveCloudNoiseFile.Caption := '';
    end;
  end;
end;


end.

