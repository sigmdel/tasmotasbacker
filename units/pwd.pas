(*
  String Encryption and Decryption Functions using Free Pascal Blowfish unit

  Based on
    Blowfish, the cryptography unit by leledumbo (June 24, 2012)
    @ http://pascalgeek.blogspot.com/2012/06/encryption-decryption-and-asynchronous.html
*)

unit pwd;

{$mode objfpc}{$H+}

interface

  {Returns the encrypted conversion of value using aKey as the encryption key.
   If aKey is empty, the encryption is performed with the DefaultKey.
   See SetEncryptionKey and GetEncryptionKey.}
function Encrypt(value: string; encode: boolean = true; const aKey: string = ''): string;

  {Returns the decrypted conversion of value using aKey as the encryption key.
   If aKey is empty, the decryption is performed with the DefaultKey.
   See SetEncryptionKey and GetEncryptionKey.}
function Decrypt(value: string; encoded: boolean = true; const aKey: string = ''): string;

  {Sets DefaultKey to aKey. aKey must contain at least one non space character
   otherwise the DefaultKey is not changed.}
procedure SetEncryptionKey(const aKey: string);

  {Sets DefaultKey to the content of a text file. The file must contain
   a single non empty line of text.}
procedure GetEncryptionKey(const filename: string);

implementation

uses
  SysUtils, Classes, base64, BlowFish;

const
  DEFAULT_KEY = 'Tts9tXi3UrKMNBYneptT2T4kx2W9PeimNeFkKpnfGv5';

var
  DefaultKey: string = DEFAULT_KEY;

function Encrypt(value: string;  encode: boolean; const aKey: string): string;
var
  Key: string;
  ss: TStringStream;
  en: TBlowFishEncryptStream;
begin
  result := '';
  if value = '' then
    exit;
  if aKey = '' then
    Key := DefaultKey
  else
    Key := aKey;

  ss := TStringStream.Create('');
  try
    en := TBlowFishEncryptStream.Create(Key, ss);
    try
      en.WriteAnsiString(value);  // must be freed before ss is read!!!
    finally
      en.Free;
    end;
  finally
    result := ss.DataString;
    ss.free;
    if encode then
      result := EncodeStringBase64(result);
  end;
end;

function Decrypt(value: string; encoded: boolean; const aKey: string): string;
var
  Key, src: string;
  ss: TStringStream;
  de: TBlowFishDecryptStream;
begin
  result := '';
  if value = '' then
    exit;
  if aKey = '' then
    Key := DefaultKey
  else
    Key := aKey;

  if encoded then
    src := DecodeStringBase64(value)
  else
    src := value;
  ss := TStringStream.Create(src);
  try
    de := TBlowFishDeCryptStream.Create(Key, ss);
    try
      result :=  de.ReadAnsiString;
    finally
      de.free;
    end;
  finally
    ss.free;
  end;
end;

procedure SetEncryptionKey(const aKey: string);
begin
  if trim(aKey) <> '' then
    DefaultKey := aKey;
end;

procedure GetEncryptionKey(const filename: string);
var
  inf: textfile;
  aKey: string;
begin
  {$i-}
  assign(inf, filename);
  reset(inf);
  {$i+}
  if (IoResult=0) then begin
    read(inf, aKey);
    close(inf);
    SetEncryptionKey(aKey);
  end;
end;

end.

