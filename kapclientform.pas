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
    procedure DownBitBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LeftBitBtnClick(Sender: TObject);
    procedure RightBitBtnClick(Sender: TObject);
    procedure selftimertimer(sender: tobject);
    procedure formcreate(sender: tobject);
    procedure UpBitBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;


var
  kapcam:  tkapcam;
  kapform: tkapform;
  kapinit: boolean;

implementation

{$r *.lfm}

uses
  libkapclient, process, wiringpi;

// common routines

procedure updatebtn(value: boolean);
begin
  kapform.upbitbtn    .enabled := true;
  kapform.downbitbtn  .enabled := true;
  kapform.leftbitbtn  .enabled := true;
  kapform.rightbitbtn .enabled := true;
  kapform.actionbitbtn.enabled := true;
end;

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
  digitalwrite(P11, HIGH);
  runcommand('fswebcam',[
    '-r' ,'640x480',
    '--skip',  '15',
    '--no-banner'  ,
    '--jpeg',  '95',
    previewfn], e);
  digitalwrite(P12, HIGH);
  if fileexists(previewfn) then
  begin
    digitalwrite(P15, HIGH);
    synchronize(@clearpreview);
    digitalwrite(P16, HIGH);
    synchronize(@showpreview);
    digitalwrite(P18, HIGH);
    deletefile(previewfn);
    delay(1000);
  end;

  digitalwrite(P11, LOW);
  digitalwrite(P12, LOW);
  digitalwrite(P15, LOW);
  digitalwrite(P16, LOW);
  digitalwrite(P18, LOW);

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
  kapform.monitorimage.picture.loadfromfile(previewfn);
end;

procedure tkapcam.enablebtn;
begin
  updatebtn(true);
end;

procedure tkapcam.disablebtn;
begin
  updatebtn(false);
end;

{ tkapform }

procedure tkapform.formcreate(sender: tobject);
begin
  kapcam  := nil;
  kapinit := wiringPiSetup <> -1;
  if kapinit then
  begin
    pinMode(P11, OUTPUT);
    pinMode(P12, OUTPUT);
    pinMode(P15, OUTPUT);
    pinMode(P16, OUTPUT);
    pinMode(P18, OUTPUT);
  end;
end;

procedure tkapform.formdestroy(sender: tobject);
begin
  if kapinit then
  begin
    digitalwrite(P11, LOW);
    digitalwrite(P12, LOW);
    digitalwrite(P15, LOW);
    digitalwrite(P16, LOW);
    digitalwrite(P18, LOW);
  end;
end;

procedure tkapform.upbitbtnclick(sender: tobject);
begin
  if kapinit then
  begin
    updatebtn(false);
    digitalwrite(P11, HIGH);
    delay(1000);
    digitalwrite(P11, LOW);
    updatebtn(true);
  end;
end;

procedure tkapform.downbitbtnclick(sender: tobject);
begin
  if kapinit then
  begin
    updatebtn(false);
    digitalwrite(P18, HIGH);
    delay(1000);
    digitalwrite(P18, LOW);
    updatebtn(true);
  end;
end;

procedure tkapform.leftbitbtnclick(sender: tobject);
begin
  if kapinit then
  begin
    updatebtn(false);
    digitalwrite(P12, HIGH);
    delay(1000);
    digitalwrite(P12, LOW);
    updatebtn(true);
  end;
end;

procedure tkapform.rightbitbtnclick(sender: tobject);
begin
  if kapinit then
  begin
    updatebtn(false);
    digitalwrite(P16, HIGH);
    delay(1000);
    digitalwrite(P16, LOW);
    updatebtn(true);
  end;
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

