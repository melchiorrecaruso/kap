{ Description: Kap library test program.

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

program libtest;

{$mode objfpc}
{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  libkap;

var
  j: longint;

begin
  writeln('RaspberryPi TEST');
  if libinit then
  begin

    pinOUTPUT(P11);
    pinOUTPUT(P12);
    pinOUTPUT(P13);


    pinOFF (P11);
    pinON  (P12);

    for j := 0 to 1 do
    begin

    pwmON (P13, 2500);

    libdelay(2000);

    pinOFF (P13);

    libdelay(2000);

    end;

  end else
    writeln('Error setting up the RaspberryPi');
end.
