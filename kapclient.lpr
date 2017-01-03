program kapclient;

{$mode objfpc}
{$h+}
{$define usecthreads}

uses
  {$ifdef unix}
  {$ifdef usecthreads}
  cthreads,
  {$endif}
  {$endif}
  interfaces,
  forms,
  kapclientform, pca9685;

{$R *.res}

begin
  application.title:='KAPcake';
  requirederivedformresource := true;
  application.initialize;
  application.createform(tkapform, kapform);
  application.run;
end.
