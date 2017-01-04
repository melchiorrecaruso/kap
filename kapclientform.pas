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
    procedure autotest;
  protected
    procedure execute; override;
  public
    constructor create(createsuspended : boolean);
  end;

  { tkapform }

  tkapform = class(tform)
    autocheckbox: tcheckbox;
    Bevel1: TBevel;
    ResetBitBtn: TBitBtn;
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
    procedure ResetBitBtnClick(Sender: TObject);
    procedure RightBitBtnClick(Sender: TObject);
    procedure selftimertimer(sender: tobject);
    procedure formcreate(sender: tobject);
    procedure UpBitBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  kapsrv_maxvalue = 2.60;
  kapsrv_minvalue = 0.50;
  kapsrv_rstvalue = 1.55;
  kapsrv_incvalue = 0.10;
  kapsrv_freq     = 50;

var
  kapcam:  tkapcam;
  kapform: tkapform;
  kapinit: boolean;
  kapsrv0: double;
  kapsrv1: double;

implementation

{$r *.lfm}

uses
  libkapclient, math, process, wiringpi, pca9685;

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

  synchronize(@autotest);

  runcommand('fswebcam',[
    '-r' ,'640x480',
    '--skip',  '15',
    '--no-banner'  ,
    '--jpeg',  '95',
    previewfn], e);
  if fileexists(previewfn) then
  begin
    delay(1000);
    synchronize(@clearpreview);
    synchronize(@showpreview);
    deletefile(previewfn);
  end;

  digitalwrite(P11, LOW);
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

procedure tkapcam.autotest;
begin
  pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(kapsrv_maxvalue, kapsrv_freq));
  pwmWrite(PCA9685_PIN_BASE + 1, calcTicks(kapsrv_minvalue, kapsrv_freq));

  delay(800);

  pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(kapsrv_minvalue, kapsrv_freq));
  pwmWrite(PCA9685_PIN_BASE + 1, calcTicks(kapsrv_maxvalue, kapsrv_freq));

  delay(800);

  pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(kapsrv_rstvalue, kapsrv_freq));
  pwmWrite(PCA9685_PIN_BASE + 1, calcTicks(kapsrv_rstvalue, kapsrv_freq));
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
    kapinit := pca9685Setup(PCA9685_PIN_BASE, PCA9685_ADDRESS, kapsrv_freq) <> -1;

  if kapinit then
  begin
    pinMode(P11, OUTPUT);

    kapsrv0 := kapsrv_rstvalue;
    pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(kapsrv0, kapsrv_freq));

    kapsrv1 := kapsrv_rstvalue;
    pwmWrite(PCA9685_PIN_BASE + 1, calcTicks(kapsrv1, kapsrv_freq));
  end;
end;

procedure tkapform.formdestroy(sender: tobject);
begin
  if kapinit then
  begin
    digitalwrite(P11, LOW);
  end;
end;

procedure tkapform.upbitbtnclick(sender: tobject);
begin
  updatebtn(false);
  if kapinit then
  begin
    digitalwrite(P11, HIGH);

    kapsrv0 := min(kapsrv_maxvalue, kapsrv0 + kapsrv_incvalue);
    pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(kapsrv0, kapsrv_freq));

    delay(250);
    digitalwrite(P11, LOW);
  end;
  updatebtn(true);
end;

procedure tkapform.downbitbtnclick(sender: tobject);
begin
  updatebtn(false);
  if kapinit then
  begin
    digitalwrite(P11, HIGH);

    kapsrv0 := max(kapsrv_minvalue, kapsrv0 - kapsrv_incvalue);
    pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(kapsrv0, kapsrv_freq));

    delay(250);
    digitalwrite(P11, LOW);
  end;
  updatebtn(true);
end;

procedure tkapform.leftbitbtnclick(sender: tobject);
begin
  updatebtn(false);
  if kapinit then
  begin
    digitalwrite(P11, HIGH);

    kapsrv1 := min(kapsrv_maxvalue, kapsrv1 + kapsrv_incvalue);
    pwmWrite(PCA9685_PIN_BASE + 1, calcTicks(kapsrv1, kapsrv_freq));

    delay(250);
    digitalwrite(P11, LOW);
  end;
  updatebtn(true);
end;

procedure tkapform.rightbitbtnclick(sender: tobject);
begin
  updatebtn(false);
  if kapinit then
  begin
    digitalwrite(P11, HIGH);

    kapsrv1 := max(kapsrv_minvalue, kapsrv1 - kapsrv_incvalue);
    pwmWrite(PCA9685_PIN_BASE + 1, calcTicks(kapsrv1, kapsrv_freq));

    delay(250);
    digitalwrite(P11, LOW);
  end;
  updatebtn(true);
end;

procedure tkapform.ResetBitBtnClick(Sender: TObject);
begin
  updatebtn(false);
  if kapinit then
  begin
    digitalwrite(P11, HIGH);

    kapsrv0 := kapsrv_rstvalue;
    pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(kapsrv0, kapsrv_freq));

    kapsrv1 := kapsrv_rstvalue;
    pwmWrite(PCA9685_PIN_BASE + 1, calcTicks(kapsrv1, kapsrv_freq));

    delay(250);
    digitalwrite(P11, LOW);

  end;
  updatebtn(true);
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

