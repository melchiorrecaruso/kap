unit kapclientform;

{$mode objfpc}
{$h+}

interface

uses
  classes, sysutils, process, fileutil, DividerBevel, forms, controls, graphics,
  dialogs, extctrls, StdCtrls, Buttons;

type
  { TKapForm }

  TKapForm = class(tform)
    MonitorDBevel: TDividerBevel;
    UpBitBtn: TBitBtn;
    DownBitBtn: TBitBtn;
    RightBitBtn: TBitBtn;
    LeftBitBtn: TBitBtn;
    ActionBitBtn: TBitBtn;
    MonitorImage: TImage;
    MonitorPanel: TPanel;
    procedure ActionBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public
    { public declarations }
  end;

var
  KapForm: TKapForm;

implementation

{$r *.lfm}

uses
  libkapclient;

{ TKapForm }

procedure TKapForm.FormCreate(Sender: TObject);
begin

end;

procedure TKapForm.ActionBitBtnClick(Sender: TObject);
begin
  MonitorImage.Picture.LoadFromFile(GetCam);
end;

procedure TKapForm.FormDestroy(Sender: TObject);
begin

end;

end.

