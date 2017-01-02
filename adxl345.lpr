program adxl345;

uses
  wiringPi;

const
   devaddr = $53;

type
  acc_rec = packed record
    x: longint;
    y: longint;
    z: longint;
  end;

procedure adxl345_init(fd: longint);
begin
  wiringPiI2CWriteReg8(fd, $31, $0b);
  wiringPiI2CWriteReg8(fd, $2d, $08);
//wiringPiI2CWriteReg8(fd, $2e, $00);
  wiringPiI2CWriteReg8(fd, $1e, $00);
  wiringPiI2CWriteReg8(fd, $1f, $00);
  wiringPiI2CWriteReg8(fd, $20, $00);

  wiringPiI2CWriteReg8(fd, $21, $00);
  wiringPiI2CWriteReg8(fd, $22, $00);
  wiringPiI2CWriteReg8(fd, $23, $00);

  wiringPiI2CWriteReg8(fd, $24, $01);
  wiringPiI2CWriteReg8(fd, $25, $0f);
  wiringPiI2CWriteReg8(fd, $26, $2b);
  wiringPiI2CWriteReg8(fd, $27, $00);

  wiringPiI2CWriteReg8(fd, $28, $09);
  wiringPiI2CWriteReg8(fd, $29, $ff);
  wiringPiI2CWriteReg8(fd, $2a, $80);
  wiringPiI2CWriteReg8(fd, $2c, $0a);
  wiringPiI2CWriteReg8(fd, $2f, $00);
  wiringPiI2CWriteReg8(fd, $38, $9f);
end;

function adxl345_read_xyz(fd: longint): acc_rec;
var
  x0, y0, z0, x1, y1, z1: byte;
begin
  x0 := $ff - wiringPiI2CReadReg8(fd, $32);
  x1 := $ff - wiringPiI2CReadReg8(fd, $33);
  y0 := $ff - wiringPiI2CReadReg8(fd, $34);
  y1 := $ff - wiringPiI2CReadReg8(fd, $35);
  z0 := $ff - wiringPiI2CReadReg8(fd, $36);
  z1 := $ff - wiringPiI2CReadReg8(fd, $37);

  result.x := (x1 shl 8) + x0;
  result.y := (y1 shl 8) + y0;
  result.z := (z1 shl 8) + z0;
end;

var
  fd: longint;
  acc_xyz: acc_rec;

begin
  fd := wiringPiI2CSetup(devaddr);
  if (-1 = fd) then
  begin
    writeln('I2C device setup error');
    halt;
  end;

  adxl345_init(fd);
  while true do
  begin
    acc_xyz := adxl345_read_xyz(fd);
    writeln('x: ', acc_xyz.x, '  y: ', acc_xyz.y, '  z: ', acc_xyz.z);
    delay(1000);
  end;
end.



