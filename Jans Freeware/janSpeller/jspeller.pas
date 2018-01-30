{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit jspeller; 

interface

uses
  janSpeller, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('janSpeller', @janSpeller.Register); 
end; 

initialization
  RegisterPackage('jSpeller', @Register); 
end.
