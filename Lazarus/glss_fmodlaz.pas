{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLSS_FMODLaz;

interface

uses
  GLSMFMOD, fmoderrors, fmodpresets, fmodtypes, fmoddyn, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLSMFMOD', @GLSMFMOD.Register);
end;

initialization
  RegisterPackage('GLSS_FMODLaz', @Register);
end.
