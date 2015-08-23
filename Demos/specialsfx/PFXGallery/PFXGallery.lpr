program PFXGallery;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  UPFXGallery in 'UPfxGallery.pas' {FrmMain};

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
