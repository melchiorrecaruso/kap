program kapclient;

{$mode objfpc}
{$h+}

uses
  {$ifdef unix}
  {$ifdef usecthreads}
  cthreads,
  {$endif}
  {$endif}
  interfaces,
  forms, lazcontrols,
  kapclientform;

{$r *.res}

begin
  requirederivedformresource := True;
  application.initialize;
  Application.CreateForm(TKapForm, KapForm);
  application.run;
end.
