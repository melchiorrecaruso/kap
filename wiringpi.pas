(* Pascal wrapper unit for Gordon Henderson wiringPi library.
 * The source can be found at https://http://wiringpi.com
 *
 * wiringpi.pas: version 0.1 by Melchiorre Caruso
 *
 * wiringPi.h:
 *    Arduino like Wiring library for the Raspberry Pi.
 *    Copyright (c) 2012-2016 Gordon Henderson
 ***********************************************************************
 * This file is part of wiringPi:
 *    https://projects.drogon.net/raspberry-pi/wiringpi/
 *
 *    wiringPi is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU Lesser General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    wiringPi is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public License
 *    along with wiringPi.  If not, see <http://www.gnu.org/licenses/>.
 ***********************************************************************
 *)

unit wiringpi;

{$mode objfpc}
{$h+}

{$linklib c}
{$linklib libwiringPi}

interface

const
  // Pin mappings from P1 connector to WiringPi library
  // Px represents to physical pin on the RaspberryPi P1 connector

  // P1 = 3.3V
  // P2 = 5V
  P3  = 8;
  // P4 = 5V
  P5  = 9;
  //P6 = GND
  P7  = 7;
  P8  = 15;
  //P9 = GND
  P10 = 16;
  P11 = 0;
  P12 = 1;
  P13 = 2;
  //P14 = GND
  P15 = 3;
  P16 = 4;
  // P17 = 3.3V
  P18 = 5;
  P19 = 12;
  // P20 = GND
  P21 = 13;
  P22 = 6;
  P23 = 14;
  P24 = 10;
  // P25 = GND
  P26 = 11;
  // pi2
  P27 = 30;
  P28 = 31;
  P29 = 21;
  // P30 = GND
  P31  = 22;
  P32 = 26;
  P33 = 23;
  // P34 = GND
  P35 = 24;
  P36 = 27;
  P37 = 25;
  P38 = 28;
  // P39 = GND
  P40 = 29;

  // Pin modes

  INPUT                  =  0;
  OUTPUT                 =  1;
  PWM_OUTPUT             =  2;
  GPIO_CLOCK             =  3;
  SOFT_PWM_OUTPUT        =  4;
  SOFT_TONE_OUTPUT       =  5;
  PWM_TONE_OUTPUT        =  6;

  LOW                    =  0;
  HIGH                   =  1;

  // Pull up/down/none

  PUD_OFF                =  0;
  PUD_DOWN               =  1;
  PUD_UP                 =  2;

  // PWM

  PWM_MODE_MS            =  0;
  PWM_MODE_BAL           =  1;

  // Interrupt levels

  INT_EDGE_SETUP         =  0;
  INT_EDGE_FALLING       =  1;
  INT_EDGE_RISING        =  2;
  INT_EDGE_BOTH          =  3;

type

  // wiringPiNodeStruct

  pwiringPiNodes = ^wiringPiNodeStruct;
  pwiringPiNodeStruct = ^wiringPiNodeStruct;

  wiringPiNodeStruct = packed record
    pinBase: longint;
    pinMax: longint;
    fd: longint;
    data0: dword;
    data1: dword;
    data2: dword;
    data3: dword;
    pinMode: procedure (node: pwiringPiNodeStruct; pin: longint; mode: longint); cdecl;
    pullUpDnControl: procedure (node: pwiringPiNodeStruct; pin: longint; mode: longint); cdecl;
    digitalRead: function  (node: pwiringPiNodeStruct; pin: longint): longint; cdecl;
    digitalWrite: procedure (node: pwiringPiNodeStruct; pin: longint; value: longint); cdecl;
    pwmWrite: procedure (node: pwiringPiNodeStruct; pin: longint; value: longint); cdecl;
    analogRead: function  (node: pwiringPiNodeStruct; pin: longint): longint; cdecl;
    analogWrite: procedure (node: pwiringPiNodeStruct; pin: longint; value: longint); cdecl;
    next: pwiringPiNodeStruct;
  end;

  // Core wiringPi functions

  function wiringPiFindNode(pin: longint): pwiringPiNodeStruct; cdecl; external;
  function wiringPiNewNode(pinBase: longint; numPins: longint): pwiringPiNodeStruct; cdecl; external;

  function wiringPiSetup    : longint; cdecl; external;
  function wiringPiSetupGpio: longint; cdecl; external;

  procedure pinMode        (pin: longint; mode: longint); cdecl; external;
  procedure pullUpDnControl(pin: longint; pud:  longint); cdecl; external;

  function  digitalRead (pin: longint): longint;        cdecl; external;
  procedure digitalWrite(pin: longint; value: longint); cdecl; external;

  procedure pwmWrite(pin: longint; value: longint); cdecl; external;

  function  analogRead (pin: longint): longint; cdecl; external;
  procedure analogWrite(pin: longint; value: longint); cdecl; external;

  // Raspberry Pi Specifics

  function  digitalReadByte: dword; cdecl; external;
  procedure digitalWriteByte(value:longint); cdecl; external;

  procedure pwmSetMode (mode: longint); cdecl; external;
  procedure pwmSetRange(range: dword); cdecl; external;
  procedure pwmSetClock(divisor: longint); cdecl; external;

  function piBoardRev: longint; cdecl; external;

  function wpiPinToGpio(wpiPin: longint): longint; cdecl; external;

  // Timming

  procedure delay(howLong: dword); cdecl; external;
  procedure delayMicroseconds(howLong: dword); cdecl; external;
  function  millis: dword; cdecl; external;
  function  micros: dword; cdecl; external;

  // Thread Priority

  function piHiPri(pri: longint): longint; cdecl; external;

  // Interrupts

  function wiringPiISR(pin: longint; mode: longint; pcallback: pointer):longint; cdecl; external;

  // SPI Library

  function wiringPiSPISetup (channel, speed: longint): longint; cdecl; external;
  function wiringPiSPIDataRW(channel: longint; data: pointer; len: longint): longint; cdecl; external;

  // I2C Library

  function wiringPiI2CSetup(const devId: longint): longint; cdecl; external;
  function wiringPiI2CRead (fd: longint): longint; cdecl; external;
  function wiringPiI2CWrite(fd, data: longint): longint; cdecl; external;

  function wiringPiI2CReadReg8 (fd, reg: longint): longint; cdecl; external;
  function wiringPiI2CReadReg16(fd, reg: longint): longint; cdecl; external;

  function wiringPiI2CWriteReg8 (fd, reg, data: longint): longint; cdecl; external;
  function wiringPiI2CWriteReg16(fd, reg, data: longint): longint; cdecl; external;

  // Software PWM Library

  function  softPwmCreate(pin, value, range: longint): longint; cdecl; external;
  procedure softPwmWrite (pin, value: longint); cdecl; external;

  // Software Tone Library

  function  softToneCreate(pin: longint): longint; cdecl; external;
  procedure softToneWrite (pin, freq: longint); cdecl; external;

implementation


end.
