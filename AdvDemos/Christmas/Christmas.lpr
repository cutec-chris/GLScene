{: GLScene Christmas "ScreenSaver".<p>

   Won't save your screen from anything though ;)<p>
   If placed in windows\system32 directory you need also to copy data there

   The scene is made up from a few meshes, some GLScene objects, several
   lens-flares and particle effects components, and a 2 text. BASS is used
   for the sound part (3D positionned fire loop, and mp3 playback).<br>
   Wrapped gifts appear around christmas every year.<p>

   Assembled from bits from the web, should be royalty free, but I don't have
   the means to check... so if you have clues about any of them:<p>

   Models: from 3DCafe.com<br>
   Textures: various origins, some from 3dtextures.fr.st, others made by me<br>
   Music: unknown origin, was in a "royalty free" download package<br>

   http://glscene.org
}
program Christmas;

{$MODE Delphi}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Forms, Interfaces,
  GLSound,
  FMain in 'FMain.pas' {Main};

{$E .scr}

{.$R *.res}

begin
   // don't complain about missing sound support
   vVerboseGLSMErrors:=False;
   Application.Initialize;
   Application.Title := 'GLScene Christmas 2014';
   Application.CreateForm(TMain, Main);
  Application.Run;
end.
