{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit jPanelButton; 

interface

uses
  janPanelButton, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('janPanelButton', @janPanelButton.Register); 
end; 

initialization
  RegisterPackage('jPanelButton', @Register); 
end.
