program CgTexture;

{$MODE Delphi}



{%File 'cg_texture.fp.cg'}
{%File 'cg_texture.vp.cg'}

uses
  Interfaces,
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
