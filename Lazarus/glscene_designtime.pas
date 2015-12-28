{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_designtime;

interface

uses
  FLibMaterialPickerLCL, FMaterialEditorFormLCL, FRColorEditorLCL, 
  FRFaceEditorLCL, FRMaterialPreviewLCL, FRTextureEditLCL, FRTrackBarEditLCL, 
  FVectorEditorLCL, FXCollectionEditorLCL, GLSceneEditLCL, GLSceneRegisterLCL, 
  RegisterXCollection, GLObjectManager, FInfoLCL, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLSceneRegisterLCL', @GLSceneRegisterLCL.Register);
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register);
end;

initialization
  RegisterPackage('GLScene_designtime', @Register);
end.
