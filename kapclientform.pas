unit kapclientform;

{$mode objfpc}
{$h+}

interface

uses
  classes, sysutils, fileutil, dividerbevel, forms, controls, graphics,
  dialogs, extctrls, stdctrls, buttons, comctrls;

type

  { tkapcam }

  tkapcam = class(tthread)
  private
    previewfn: ansistring;
    procedure enablebtn;
    procedure disablebtn;
    procedure showpreview;
    procedure clearpreview;
  protected
    procedure execute; override;
  public
    constructor create(createsuspended : boolean);
  end;

  { tkapform }

  tkapform = class(tform)
    autocheckbox: tcheckbox;
    selftimer: ttimer;
    upbitbtn: tbitbtn;
    downbitbtn: tbitbtn;
    rightbitbtn: tbitbtn;
    leftbitbtn: tbitbtn;
    actionbitbtn: tbitbtn;
    monitorimage: timage;
    monitorpanel: tpanel;
    procedure actionbitbtnclick(sender: tobject);
    procedure autocheckboxeditingdone(sender: tobject);
    procedure selftimertimer(sender: tobject);
    procedure formcreate(sender: tobject);
  private
    { private declarations }
  public
    { public declarations }
  end;


var
  kapcam:  tkapcam;
  kapform: tkapform;

implementation

{$r *.lfm}

uses
  libkapclient,
  process;

{ tkapcam }

constructor tkapcam.create(createsuspended : boolean);
begin
  freeonterminate := true;
  inherited create(createsuspended);
  previewfn := extractfilepath(application.exename) + 'previw.jpg';
end;

procedure tkapcam.execute;
var
  p: tprocess;
  s: ansistring;
  e: ansistring;
begin
  synchronize(@disablebtn);
  deletefile(previewfn);
  runcommand('fswebcam',[
    '-r' ,'640x480',
    '--skip',  '15',
    '--no-banner'  ,
    '--jpeg',  '95',
    previewfn], e);

  synchronize(@clearpreview);
  synchronize(@showpreview);
  synchronize(@enablebtn);
end;

procedure tkapcam.clearpreview;
begin
  with kapform do
  begin
    monitorimage.picture.clear;
    monitorimage.proportional := true;
    monitorimage.stretch      := true;
  end;
end;

procedure tkapcam.showpreview;
begin
  if fileexists(previewfn) then
    kapform.monitorimage.picture.loadfromfile(previewfn);
end;

procedure tkapcam.enablebtn;
begin
  with kapform do
  begin
    upbitbtn    .enabled := true;
    downbitbtn  .enabled := true;
    leftbitbtn  .enabled := true;
    rightbitbtn .enabled := true;
    actionbitbtn.enabled := true;
  end;
end;

procedure tkapcam.disablebtn;
begin
  with kapform do
  begin
    upbitbtn    .enabled := false;
    downbitbtn  .enabled := false;
    leftbitbtn  .enabled := false;
    rightbitbtn .enabled := false;
    actionbitbtn.enabled := false;
  end;
end;

{ tkapform }

procedure tkapform.formcreate(sender: tobject);
begin
  kapcam := nil;
end;

procedure tkapform.actionbitbtnclick(sender: tobject);
begin
  kapcam := tkapcam.create(true);
  kapcam.resume;
end;

procedure tkapform.autocheckboxeditingdone(sender: tobject);
begin
  selftimer.enabled := autocheckbox.checked;
end;

procedure tkapform.selftimertimer(sender: tobject);
begin
  if actionbitbtn.enabled then
    actionbitbtn.click;
end;


end.

