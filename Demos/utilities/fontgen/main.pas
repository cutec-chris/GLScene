unit main;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLBitmapFont, GLWindowsFont, StdCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    Button1: TButton;
    FontDialog1: TFontDialog;
    GroupBox2: TGroupBox;
    Image1: TImage;
    GroupBox3: TGroupBox;
    SaveDialog1: TSaveDialog;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FFont: TGLStoredBitmapFont;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  FontDialog1.Font := Panel1.Font;
  if FontDialog1.Execute then
  begin
    Panel1.Font := FontDialog1.Font;
    FFont.Font := FontDialog1.Font;
    FFont.LoadWindowsFont;

    Image1.Picture.Assign(FFont.Glyphs);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FFont := TGLStoredBitmapFont.Create(Self);
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    FFont.SaveToFile(SaveDialog1.FileName);
  end;
end;

end.
