unit kapclientform;

{$mode objfpc}
{$h+}

interface

uses
  classes, sysutils, process, fileutil, dividerbevel, forms, controls, graphics,
  dialogs, extctrls, stdctrls, buttons;

type
  { TKapForm }

  TKapForm = class(tform)
    AutoCheckBox: TCheckBox;
    Process1: TProcess;
    CamTimer: TTimer;
    UpBitBtn: TBitBtn;
    DownBitBtn: TBitBtn;
    RightBitBtn: TBitBtn;
    LeftBitBtn: TBitBtn;
    ActionBitBtn: TBitBtn;
    MonitorImage: TImage;
    MonitorPanel: TPanel;
    procedure ActionBitBtnClick(Sender: TObject);
    procedure CamTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure enablebtn(value: boolean);
  public
    { public declarations }
  end;

var
  kapform: tkapform;

implementation

{$r *.lfm}

uses
  libkapclient;

{ tkapform }

procedure tkapform.formcreate(sender: tobject);
begin

end;

procedure tkapform.enablebtn(value: boolean);
begin
  upbitbtn    .enabled := value;
  downbitbtn  .enabled := value;
  leftbitbtn  .enabled := value;
  rightbitbtn .enabled := value;
  actionbitbtn.enabled := value;
end;

procedure tkapform.actionbitbtnclick(sender: tobject);
var
  cam: tprocess;
  fn:  string;
begin
  fn := includetrailingbackslash(getcurrentdir) + 'images/previw.jpg';

  enablebtn(false);
  try
    cam := tprocess.create(nil);
    cam.currentdirectory := getcurrentdir;
    cam.commandline := 'fswebcam -r 640x480 -s 20 --no-banner ' + fn;
    cam.options     := [powaitonexit];
    cam.execute;
    cam.free;
  except
  end;
  enablebtn(true);

  monitorimage.picture.clear;
  if fileexists(fn) then
    monitorimage.picture.loadfromfile(fn);
end;

procedure TKapForm.CamTimerTimer(Sender: TObject);
begin
  if autocheckbox.checked then
  begin
    actionbitbtn.click;
  end;
end;

procedure TKapForm.FormDestroy(Sender: TObject);
begin

end;

end.

