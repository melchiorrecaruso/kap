{ Description: Kap library.

  Copyright (C) 2016 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WiTHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABiLiTY or FiTNESS
  FOR A PARTiCULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit libkap;

{$mode objfpc}
{$H+}

{$linklib c}
{$linklib libwiringPi}

interface

const

  // Px represents to physical pin
  // on the RaspberryPi connector
  P11 = 0;
  P12 = 1;
  P13 = 2;


function  libinit:boolean;
procedure libdelay(ms: longword);

procedure pinINPUT (pin: longword);
procedure pinOUTPUT(pin: longword);

procedure pinON (pin: longword);
procedure pinOFF(pin: longword);

procedure pwmON (pin: longword; ms: longword);
procedure pwmOFF(pin: longword; ms: longword);


implementation

const
  // Pin modes
  INPUT	 =  0;
  OUTPUT =  1;

  // Pin value
  LOW    =  0;
  HIGH   =  1;

// ---

function  wiringPiSetup:longint; cdecl; external;

procedure pinMode(pin:longint; mode:longint); cdecl; external;

function  digitalRead (pin:longint):longint; cdecl; external;
procedure digitalWrite(pin:longint; value:longint); cdecl; external;

procedure delay(howLong:dword); cdecl; external;

// ---

function libinit: boolean;
begin
  result := wiringPiSetup <> -1;
end;

procedure libdelay(ms: longword);
begin
  delay(ms);
end;

procedure pinINPUT(pin: longword);
begin
  pinMODE(pin, INPUT);
end;

procedure pinOUTPUT(pin: longword);
begin
  pinMode(pin, OUTPUT);
end;

function getsteps(ms, tc, td: longword): longword;
begin
  result := 0;
  while (result * tc) + ((result + 1) * (result div 2) * td) <= ms do
    inc(result);
end;

procedure pinON(pin: longword);
begin
  pwmON(pin, 0);
end;

procedure pinOFF(pin: longword);
begin
  pwmOFF(pin, 0);
end;

procedure pwmON(pin: longword; ms: longword);
const
  tc = 1;
  td = 2;
var
  i: longint;
begin
  if ms > 0 then
    for i := getsteps(ms -100, tc, td) downto 1 do
    begin
      digitalWrite(pin, HIGH);
      delay(tc);
      digitalWrite(pin, LOW);
      delay(i * td);
    end;
  digitalWrite(pin, HIGH);
end;

procedure pwmOFF(pin: longword; ms: longword);
const
  tc = 1;
  td = 2;
var
  i: longint;
begin
  if ms > 0 then
    for i := 1 to getsteps(ms -100, tc, td) do
    begin
      digitalWrite(pin, HIGH);
      delay(tc);
      digitalWrite(pin, LOW);
      delay(i * td);
    end;
  digitalWrite(pin, LOW);
end;

end.
