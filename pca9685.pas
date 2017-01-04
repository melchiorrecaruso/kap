unit pca9685;

{$mode objfpc}
{$H+}

interface

uses
  wiringPi;

const

  // PCA9685 Registers

  PCA9685_MODE1         = $00;
  PCA9685_MODE2         = $01;
  PCA9685_SUBADR1       = $02;
  PCA9685_SUBADR2       = $03;
  PCA9685_SUBADR3       = $04;
  PCA9685_ALLCALLADR    = $05;
  PCA9685_LED0_ON_L     = $06;
  PCA9685_LED0_ON_H     = $07;
  PCA9685_LED0_OFF_L    = $08;
  PCA9685_LED0_OFF_H    = $09;

  // LED1 .. LED15 (0x0A .. 0x45)

  PCA9685_ALL_LED_ON_L  = $FA;
  PCA9685_ALL_LED_ON_H  = $FB;
  PCA9685_ALL_LED_OFF_L = $FC;
  PCA9685_ALL_LED_OFF_H = $FD;
  PCA9685_PRE_SCALE     = $FE;
  PCA9685_TESTMODE      = $FF;

  // Bits in the MODE1 register

  PCA9685_MODE1_RESTART = $80;
  PCA9685_MODE1_EXTCLK  = $40;
  PCA9685_MODE1_AI      = $20;
  PCA9685_MODE1_SLEEP   = $10;
  PCA9685_MODE1_SUB1    = $08;
  PCA9685_MODE1_SUB2    = $04;
  PCA9685_MODE1_SUB3    = $02;
  PCA9685_MODE1_ALLCALL = $01;

  // Bits in the MODE2 register

  PCA9685_MODE2_INVRT   = $10;
  PCA9685_MODE2_OCH     = $08;
  PCA9685_MODE2_OUTDRV  = $04;
  PCA9685_MODE2_OUTNE_1 = $02;
  PCA9685_MODE2_OUTNE_0 = $01;

  PCA9685_OSC_CLOCK     = 25000000.0;
  PCA9685_PRESCALE_DIV  = 4096.0;

  PCA9685_ADDRESS       = $40;
  PCA9685_PIN_BASE      = 300;
  PCA9685_MAX_PWM       = 4096;

  function pca9685Setup(pinBase: longint; i2cAddress: longint; freq: longint): longint;

implementation

procedure pca9685PwmWrite(node: pwiringPiNodeStruct; pin: longint; value: longint); cdecl;
var
  fd: longint;
  chan: longint;
begin
  fd   := node^.fd;
  chan := 4 * (pin - node^.pinBase);

  wiringPiI2CWriteReg8(fd, PCA9685_LED0_ON_L  + chan, $00);
  wiringPiI2CWriteReg8(fd, PCA9685_LED0_ON_H  + chan, $00);
  wiringPiI2CWriteReg8(fd, PCA9685_LED0_OFF_L + chan, (value      ) and $FF);
  wiringPiI2CWriteReg8(fd, PCA9685_LED0_OFF_H + chan, (value shr 8) and $FF);
end;

procedure pca9685PwmReset(fd: longint); cdecl;
begin
  wiringPiI2CWriteReg8(fd, PCA9685_ALL_LED_ON_L,  $00);
  wiringPiI2CWriteReg8(fd, PCA9685_ALL_LED_ON_H,  $00);
  wiringPiI2CWriteReg8(fd, PCA9685_ALL_LED_OFF_L, $00);
  wiringPiI2CWriteReg8(fd, PCA9685_ALL_LED_OFF_H, $00);
end;

procedure pca9685PWMfreq(fd: longint; freq: longint);
var
  prescale: longint;
  mode1: longint;
begin
  // Calculate presacle
  prescale := round(PCA9685_OSC_CLOCK / (PCA9685_PRESCALE_DIV * freq)) - 1;
  // Read MODE1 register
  mode1 := wiringPiI2CReadReg8(fd, PCA9685_MODE1);
  // Write MODE1 register (discard RESTART bit and set SLEEP bit)
  wiringPiI2CWriteReg8(fd, PCA9685_MODE1, (mode1 and $7F) or PCA9685_MODE1_SLEEP);
  // Write PRE_SCALE register
  wiringPiI2CWriteReg8(fd, PCA9685_PRE_SCALE, prescale);
  // Restore MODE1 register
  wiringPiI2CWriteReg8(fd, PCA9685_MODE1, mode1);
  delay(5);
  // Write MODE1 register (set RESTART BIT)
  wiringPiI2CWriteReg8(fd, PCA9685_MODE1, mode1 or PCA9685_MODE1_RESTART);
end;

function pca9685Setup(pinBase: longint; i2cAddress: longint; freq: longint): longint;
var
  fd: longint;
  node: pwiringPiNodeStruct;
begin
  result := -1;
  fd := wiringPiI2CSetup(i2cAddress);
  if fd <> -1 then
  begin
    wiringPiI2CWriteReg8(fd, PCA9685_MODE1, $00);
    wiringPiI2CWriteReg8(fd, PCA9685_MODE2, $04);

    pca9685PWMreset(fd);
    pca9685PWMfreq (fd, freq);

    node := wiringPiNewNode(pinBase, 16);

    node^.fd	       := fd;
    node^.pwmWrite     := @pca9685PwmWrite;
  //node^.digitalWrite := @myOnOffWrite;
  //node^.digitalRead  := @myOffRead;
  //node^.analogRead   := @myOnRead;

    result := fd;
  end;
end;




end.

