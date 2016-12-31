unit kapclientform;

{$mode objfpc}
{$h+}

interface

uses
  classes, sysutils, fileutil, DividerBevel, forms, controls, graphics, dialogs,
  extctrls, StdCtrls, Buttons;

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

{ TKapForm }

procedure TKapForm.FormCreate(Sender: TObject);
begin

end;

procedure TKapForm.FormDestroy(Sender: TObject);
begin

end;

end.

