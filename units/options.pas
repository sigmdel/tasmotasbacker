unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles;

type
  
  { TParams }

  TParams = class
  private
    ini: TIniFile;
    function GetConnectTimeout: integer;
    function GetDateFormat: integer;
    function GetDeviceName: integer;
    function GetDirectory: string;
    function GetExtension: string;
    function GetFilenameFormat: integer;
    function GetHost: string;
    function GetPassword: string;
    function GetPort: integer;
    function GetTopic: string;
    function GetUser: string;
    procedure SetConnectTimeout(AValue: integer);
    procedure SetDateFormat(AValue: integer);
    procedure SetDeviceName(AValue: integer);
    procedure SetDirectory(AValue: string);
    procedure SetExtension(AValue: string);
    procedure SetFilenameFormat(AValue: integer);
    procedure SetHost(AValue: string);
    procedure SetPassword(AValue: string);
    procedure SetPort(AValue: integer);
    procedure SetTopic(AValue: string);
    procedure SetUser(AValue: string);
  public
    constructor create;
    destructor destroy; override;
    property host: string read GetHost write SetHost;
    property port: integer read GetPort write SetPort;
    property user: string read GetUser write SetUser;
    //**** Password saved in plain text !!!! ****
    property password: string read GetPassword write SetPassword;
    property topic: string read GetTopic write SetTopic;
    property directory: string read GetDirectory write SetDirectory;
    property extension: string read GetExtension write SetExtension;
    property dateformat: integer read GetDateFormat write SetDateFormat;
    property FilenameFormat: integer read GetFilenameFormat write SetFilenameFormat;
    property DeviceName: integer read GetDeviceName write SetDeviceName;
    property ConnectTimeout: integer read GetConnectTimeout write SetConnectTimeout;
  end;

var
  params: TParams;

implementation

const
  Soptions = 'Options';
  Shost = 'Host';
  Sport = 'Port';
  Suser = 'User';
  Spassword = 'Password';
  Stopic = 'Topic';
  Sdirectory = 'Directory';
  Sextension = 'Extension';
  SdateFormat = 'DateFormat';
  SfilenameFormat = 'FilenameFormat';
  SdeviceName = 'DeviceName';
  SconnectTimeout = 'ConnectTimeout';

const
  CONFIGFILENAME = 'options.ini';

var
  configfile: string; // see initialization


function Vendor: string;
begin
  result := 'sigmdel';
end;

function  GetAppName: string;
begin
  result := changefileext(extractfilename(paramstr(0)), '');
end;

{ TParams }

function TParams.GetHost: string;
begin
  result := ini.ReadString(Soptions, Shost, '192.168.1.22');
end;

function TParams.GetDateFormat: integer;
begin
  result := ini.ReadInteger(Soptions, SdateFormat, 2);
end;

function TParams.GetConnectTimeout: integer;
begin
  result := ini.ReadInteger(Soptions, SconnectTimeout, 3000);
end;

function TParams.GetDeviceName: integer;
begin
  result := ini.ReadInteger(Soptions, sdeviceName, 0);
end;

function TParams.GetDirectory: string;
begin
  result := ini.ReadString(Soptions, Sdirectory, './backup');
end;

function TParams.GetExtension: string;
begin
  result := ini.ReadString(Soptions, Sextension, '.bin');
end;

function TParams.GetFilenameFormat: integer;
begin
  result := ini.ReadInteger(Soptions, SfilenameFormat, 0);
end;

function TParams.GetPassword: string;
begin
  result := ini.ReadString(Soptions, Spassword, '');
end;

function TParams.GetPort: integer;
begin
  result := ini.ReadInteger(Soptions, Sport, 1883);
end;

function TParams.GetTopic: string;
begin
  result := ini.ReadString(Soptions, Stopic, 'tasmotas');
end;

function TParams.GetUser: string;
begin
  result := ini.ReadString(Soptions, Suser, '');
end;

procedure TParams.SetConnectTimeout(AValue: integer);
begin
  ini.writeInteger(Soptions, SconnectTimeout, AValue);
end;

procedure TParams.SetDateFormat(AValue: integer);
begin
  ini.writeInteger(Soptions, SdateFormat, AValue);
end;

procedure TParams.SetDeviceName(AValue: integer);
begin
  ini.writeInteger(Soptions, SdeviceName, AValue);
end;

procedure TParams.SetDirectory(AValue: string);
begin
  ini.WriteString(Soptions, Sdirectory, AValue);
end;

procedure TParams.SetExtension(AValue: string);
begin
  ini.WriteString(Soptions, Sextension, AValue);
end;

procedure TParams.SetFilenameFormat(AValue: integer);
begin
  ini.writeInteger(Soptions, SfilenameFormat, AValue);
end;

procedure TParams.SetHost(AValue: string);
begin
  ini.WriteString(Soptions, Shost, AValue);
end;

procedure TParams.SetPassword(AValue: string);
begin
  ini.WriteString(Soptions, Spassword, AValue);

end;

procedure TParams.SetPort(AValue: integer);
begin
  ini.WriteInteger(Soptions, Sport, AValue);
end;

procedure TParams.SetTopic(AValue: string);
begin
  ini.WriteString(Soptions, Stopic, AValue);

end;

procedure TParams.SetUser(AValue: string);
begin
  ini.WriteString(Soptions, Suser, AValue);

end;

constructor TParams.create;
begin
  inherited create;
  ini := TIniFile.create(configfile, []);
end;

destructor TParams.destroy;
begin
  ini.free;
  inherited destroy;
end;

initialization
  OnGetVendorName := @Vendor;
  OnGetApplicationName := @GetAppName;
  configfile := GetAppConfigDir(false);
  ForceDirectories(configfile);   // create config directory, report error if false ?
  configfile := IncludeTrailingPathDelimiter(configfile) + CONFIGFILENAME;
  params := TParams.create;
finalization
  params.free;
end.

