{ Description: Kap Client Library.

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

unit libkapclient;

{$mode objfpc}
{$h+}

interface

uses
  sysutils, process;

function getcam: ansistring;


implementation

function getcam: ansistring;
var
  p: tprocess;
begin
  result := includetrailingbackslash(getcurrentdir) + 'kap.jpg';

  p := tprocess.create(nil);
  p.currentdirectory := getcurrentdir;
  p.commandline := 'fswebcam -r 640x480 -S 20 --no-banner ' + result;
  p.options     := [powaitonexit];
  p.execute;
  p.free;


end;



end.
