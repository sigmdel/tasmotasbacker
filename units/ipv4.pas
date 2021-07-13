unit ipv4;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils;

type
  // Exception raised when convering incorrectly formated IP string
  EIPv4Error = class(Exception);

  { TIPv4 }

  TIPv4 = packed record
    procedure Init(A, B, C, D: byte);        // A is high octet, D the low octet
    procedure FromString(const s: string);             // can raise EIPv4Error
    function TryFromString(const s: string): boolean;  // will not raise exception
    function ToString: string;
    function Succ: TIPv4;  // next valid IP address  - rolls over to 0.0.0.0 from 255.255.255.255
    function Pred: TIPv4;  // previous valid IP address - rolls over to 255.255.255.255 from 0.0.0.0
    procedure Inc;         // increment self to next IP address - rolls over
    procedure Dec;         // decrement self to previous IP address - rolls over
    function Compare(const ip: TIPv4): integer; // if < 0, self precededs ip,
    function IsSame(const ip: TIPv4): boolean;
    class operator=(const ip1, ip2: TIPv4): boolean;
    class operator<(const ip1, ip2: TIPv4): boolean;
    class operator<=(const ip1, ip2: TIPv4): boolean;
    class operator>(const ip1, ip2: TIPv4): boolean;
    class operator>=(const ip1, ip2: TIPv4): boolean;
    class operator and(const ip1, ip2: TIPv4): TIPv4;
    class operator or(const ip1, ip2: TIPv4): TIPv4;
    class operator not(const ip: TIPv4): TIPv4;
    case Integer of
      0: (D, C, B, A: Byte);           // little endian: low to high octet
      1: (Quad: array[0..3] of byte);
      2: (Value: Longword);
  end;


function StrToIPv4(const S: String): TIPv4;
function TryStrToIPv4(const S: String; out ip: TIPv4): boolean;
function ValidIPv4(const S: string): boolean;
function IPv4ToStr(const AIPv4: TIPv4): String;
function IPv4Compare(const AIPv41, AIPv42: TIPv4): Integer;
function IPv4Succ(const aIPv4: TIPv4): TIPv4;
function IPv4Pred(const aIPv4: TIPv4): TIPv4;
function SubnetMask(bits: byte): TIPv4;
procedure SubnetRange(net: TIPv4; bits: byte; out firstIp, lastIP: TIPv4);


implementation

resourcestring
  SInvalidIPv4Value = '''%s'' is not a valid IPv4 address';

procedure IPv4Error(const Message: String);
begin
  raise EIPv4Error.Create(Message);
end;

procedure IPv4ErrorFmt(const Message, IPv4AsString: String);
begin
  raise EIPv4Error.Create(Format(Message, [IPv4AsString]));
end;

{ TIPv4 }

procedure TIPv4.Init(A, B, C, D: byte);
begin
  self.D := D;
  self.C := C;
  self.B := B;
  self.A := A;
end;

procedure TIPv4.FromString(const s: string);
begin
  self := StrToIPv4(s)
end;

function TIPv4.TryFromString(const s: string): boolean;
begin
  result := TryStrToIPv4(s, self)
end;

function TIPv4.ToString: string;
begin
  result :=  IPv4ToStr(self)
end;

function TIPv4.Succ: TIPv4;
begin
  result := IPV4Succ(self)
end;

function TIPv4.Pred: TIPv4;
begin
  result := IPV4Pred(self)
end;

procedure TIPv4.Inc;
begin
  self := Succ
end;

procedure TIPv4.Dec;
begin
  self := Pred
end;

function TIPv4.Compare(const ip: TIPv4): integer;
begin
  result := IPv4Compare(self, ip)
end;

function TIPv4.IsSame(const ip: TIPv4): boolean;
begin
  result := Compare(ip) = 0;
end;

class operator TIPv4.=(const ip1, ip2: TIPv4): boolean;
begin
  result := ip1.isSame(ip2);
end;

class operator TIPv4.<(const ip1, ip2: TIPv4): boolean;
begin
  result := ip1.Compare(ip2) < 0
end;

class operator TIPv4.<=(const ip1, ip2: TIPv4): boolean;
begin
  result := ip1.Compare(ip2) <= 0
end;

class operator TIPv4.>(const ip1, ip2: TIPv4): boolean;
begin
  result := ip1.Compare(ip2) > 0
end;

class operator TIPv4.>=(const ip1, ip2: TIPv4): boolean;
begin
  result := ip1.Compare(ip2) >= 0
end;

class operator TIPv4.and(const ip1, ip2: TIPv4): TIPv4;
begin
  result.value := ip1.value and ip2.value;
end;

class operator TIPv4.or(const ip1, ip2: TIPv4): TIPv4;
begin
  result.value := ip1.value or ip2.value;
end;

class operator TIPv4.not(const ip: TIPv4): TIPv4;
begin
  result.value := not ip.value;
end;

{ older slower version
function StrToIPv4(const S: string): TIPv4;
var
  ss: string;  // sub string to hold each quad value
  p: integer;  // index into S as it is being scanned
  n: integer;  // length of S
  l: integer;  // index into ss as chars are copied from S
  v: integer;  // value of single quad
  r: integer;  // quad index
begin
  n := length(S);
  p := 1;
  r := 3;
  repeat
    // start new quad sub string
    setlength(ss, n);
    l := 0;
    while (p <= n) and (S[p] <> '.') do begin
      // copy digits from S to ss until '.' or EOS
      inc(l);
      ss[l] := S[p];
      inc(p);
    end;
    setlength(ss, l);

    // convert the quad sub string into a binary value
    if TryStrToInt(ss, v) and (v >= 0) and (v <= 255) then begin
      // put the value into the quad and prepare to move on
      result.Quad[r] := v;
      dec(r);
      if (p < n) then inc(p); // skip '.' except at end
    end
    else
      r := -2;  // break out of the while loop with error flag
  until (p > n) or (r < 0);
  if (p <= n) or (r <> -1) then
    IPv4ErrorFmt(SInvalidIPv4Value, S);
end;
}

{ Newer faster version based on
https://codes-sources.commentcamarche.net/source/62056-ipv4-ipv6-vers-chaine-et-chaine-vers-ipv4-ipv6
by f0xi on CodeS-SourceS (2008-09-15)
}
function StrToIPv4(const S: string): TIPv4;
var
  L: integer;  // length of S
  i: integer;  // char index in S
  v: integer;  // value of single quad
  r: integer;  // quad index and error flag (if r < 0)
begin
  result.value := 0;
  L := Length(S);
  r := 3;
  for i := 1 to L do begin
    if S[i] = '.' then begin
      // move to next quad
      dec(r);
      if r < 0 then
        break;    // too many '.'
    end
    else begin
      v := ord(S[i]) - ord('0'); // assume an ascii digit has been found, convert to  binary
      if (v < 0) or (v > 9) then begin
        // not an ascii digit
        r := -2;
        break;
      end;
      v := result.Quad[r]*10 + v;
      if v > 255 then begin
        r := -3;
        break;
      end;
      result.Quad[r] := V
    end;
  end;
  // possible r values:
  //   -3 - the quad value > 255
  //   -2 - char in in ascii digit range
  //   -1 - too many '.'
  //    0 - could be ok as long as not finishing with empty last quad as in  '192.168.8.'
  if (r <> 0) or (S[L] = '.') then
    IPv4ErrorFmt(SInvalidIPv4Value, S);        ;
end;

function TryStrToIPv4(const S: string; out ip: TIPv4): boolean;
begin
  try
    ip := strToIPv4(S);
    result := true;
  except
    ip.Value := 0;
    result := false;
  end;
end;

function ValidIPv4(const S: string): boolean;
var
  ip: TIPv4;
begin
  result := TryStrToIPv4(S, ip);
end;

function IPv4ToStr(const AIPv4: TIPv4): String;
begin
  with AIPv4 do
    Result := Format('%d.%d.%d.%d', [A, B, C, D]);
end;

function IPv4Compare(const AIPv41, AIPv42: TIPv4): Integer;
begin
  if AIPv41.Value = AIPv42.Value then
    Result := 0
  else if AIPv41.Value < AIPv42.Value then
    Result := -1
  else
    Result := 1;
end;

function IPv4Succ(const aIPv4: TIPv4): TIPv4;
begin
  result.value := longword(aIPv4.value + 1)
end;

function IPv4Pred(const aIPv4: TIPv4): TIPv4;
begin
  result.value := longword(aIPv4.value - 1)
end;

function SubnetMask(bits: byte): TIPv4;
var
  i, n: integer;
  mask: longword;
begin
  n := bits;
  if n > 32 then n := 32;
  mask := 0;
  for i := 0 to n-1 do
    mask := mask or ($80000000 >> i);
  result.value := mask;
end;

procedure SubnetRange(net: TIPv4; bits: byte; out firstIp, lastIP: TIPv4);
var
  mask: TIPv4;
begin
  mask := SubnetMask(bits);
  firstIp.value := net.value and mask.value;
  lastIp.value := firstIp.value or (not mask.value);
  { cleaner but marginally slower
  firstIp := net and mask;
  lastIp := firstIp or (not mask);
  }
end;

end.

