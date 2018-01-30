{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit jTiledPanel; 

interface

uses
  janTiledPanel, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('janTiledPanel', @janTiledPanel.Register); 
end; 

initialization
  RegisterPackage('jTiledPanel', @Register); 
end.
