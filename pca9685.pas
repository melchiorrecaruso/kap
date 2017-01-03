unit pca9685;

{$mode objfpc}
{$H+}

interface

uses
  wiringPi;

const
  PIN_ALL = 16;

  // Setup registers
  PCA9685_MODE1     = $0;
  PCA9685_PRESCALE  = $FE;



implementation

procedure myPwmWrite (node: pwiringPiNodeStruct; pin: longint; value: longint);
var
  fd: longint;
  chan: longint;
begin
  fd   := node^.fd;
  chan := 4 * (pin - node^.pinBase);

  wiringPiI2CWriteReg8(fd, LED0_ON_L  + chan, $00);
  wiringPiI2CWriteReg8(fd, LED0_ON_H  + chan, $00);
  wiringPiI2CWriteReg8(fd, LED0_OFF_L + chan, value and $FF);
  wiringPiI2CWriteReg8(fd, LED0_OFF_H + chan, (value shr 8) and $FF);
end;


function pca9685Setup(pinBase: longint; i2cAddress: longint; freq: real): longint;
var
  node: pwiringPiNodeStruct;
  fd: longint;
  settings: longint;
  autoInc: longint;
begin
  // Create a node with 16 pins [0..15] + [16] for all
  node := wiringPiNewNode(pinBase, PIN_ALL + 1);

  // Check if pinBase is available
  result := -1;
  if node <> nil then
  begin
    // Check i2c address
    fd := wiringPiI2CSetup(i2cAddress);
    if (fd < 0) then exit;

    // Setup the chip. Enable auto-increment of registers.
    settings := wiringPiI2CReadReg8(fd, PCA9685_MODE1) and $7F;
    autoInc  := settings or $20;

    wiringPiI2CWriteReg8(fd, PCA9685_MODE1, autoInc);

    // Set frequency of PWM signals. Also ends sleep mode and starts PWM output.
    if freq > 0 then
      pca9685PWMFreq(fd, freq);

    node^.fd	       := fd;
    node->pwmWrite     := @myPwmWrite;
    node->digitalWrite := @myOnOffWrite;
    node->digitalRead  := @myOffRead;
    node->analogRead   := @myOnRead;

    result := fd;
  end;
end;

}


end.

