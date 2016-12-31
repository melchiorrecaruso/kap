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
  tlntsend, ftpsend;

type
  tkapclient = class
  private
    ftelnetsend: ttelnetsend;
    fftpsend: tftpsend;
  public
    constructor create(ahost: string);
    destructor destroy; override;
    procedure sendcommand(acommand: string);
    procedure logout;
    function receivedata: string;
    function login: boolean;
  end;

implementation

{ tkapclient }

constructor tkapclient.create(ahost: string);
begin
  inherited create;
  ftelnetsend := ttelnetsend.create;
  ftelnetsend.targethost := ahost;
  ftelnetsend.targetport := cTelnetProtocol;
end;

destructor tkapclient.destroy;
begin
  ftelnetsend.free;
  inherited destroy;
end;

function tkapclient.login: boolean;
begin
  result := ftelnetsend.login;
end;

procedure tkapclient.logout;
begin
  ftelnetsend.logout;
end;

function tkapclient.receivedata: string;
var
  lpos: integer;
begin
  result := '';

  lpos := 1;
  while (ftelnetsend.sock.canread  (2500)) or
        (ftelnetsend.sock.waitingdata > 0) do
  begin
    ftelnetsend.sock.recvpacket(1000);
    result := result + copy(ftelnetsend.sessionlog,
      lpos, length(ftelnetsend.sessionlog));
    lpos := length(ftelnetsend.sessionlog) + 1;
  end;
end;

procedure tkapclient.sendcommand(acommand: string);
begin
  ftelnetsend.send(acommand + #13);
end;

end.
