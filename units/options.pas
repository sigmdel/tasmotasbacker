unit options;

{$mode objfpc}{$H+}

{$DEFINE INCLUDE_MQTT_OPTIONS}
{ -- $DEFINE INCLUDE_HTTP_OPTIONS}

interface

uses
  Classes, SysUtils, inifiles;

const
  {$IFDEF INCLUDE_MQTT_OPTIONS}
  DEFAULT_HOST = '192.168.1.22';
  DEFAULT_PORT = 1883;
  DEFAULT_PASSWORD = '';
  DEFAULT_USER = '';
  DEFAULT_TOPIC = 'tasmotas, sonoffs';
  {$ENDIF}

  {$IFDEF INCLUDE_HTTP_OPTIONS}
  DEFAULT_SUBNET = '192.168.0.0';
  DEFAULT_SUBNET_BITS = 24;
  DEFAULT_SCAN_ALL_IP = true;
  DEFAULT_FIRST_IP = '192.168.0.100';
  DEFAULT_LAST_IP = '192.168.0.200';
  DEFAULT_SCAN_ATTEMPTS = 2;
  DEFAULT_SCAN_TIMEOUT = 1;
  {$ENDIF}

  // backup options
  DEFAUT_DATE_FORMAT = 2;
  DEFAULT_DEVICE_NAME = 0;
  DEFAULT_BACK_DIRECTORY = 'backup';
  DEFAULT_EXTENSION = '.dmp';
  DEFAULT_FILENAME_FORMAT = 0;
  DEFAULT_DOWNLOAD_ATTEMPTS = 2;
  DEFAULT_DOWNLOAD_TIMEOUT = 4;


type
  
  { TParams }

  TParams = class
  private
    ini: TIniFile;

    {$IFDEF INCLUDE_MQTT_OPTIONS}
    function GetHost: string;
    function GetPort: integer;
    function GetUser: string;
    function GetPassword: string;
    function GetTopic: string;
    procedure SetHost(AValue: string);
    procedure SetPort(AValue: integer);
    procedure SetUser(AValue: string);
    procedure SetPassword(AValue: string);
    procedure SetTopic(AValue: string);
    {$ENDIF}

    {$IFDEF INCLUDE_HTTP_OPTIONS}
    function GetSubnet: string;
    function GetSubnetBits: integer;
    function GetScanAllIP: boolean;
    function GetFirstIP: string;
    function GetLastIP: string;
    function GetScanAttempts: integer;
    function GetScanTimeout: integer;
    procedure SetSubnet(AValue: string);
    procedure SetSubnetBits(AValue: integer);
    procedure SetScanAllIP(AValue: boolean);
    procedure SetFirstIP(AValue: string);
    procedure SetLastIP(AValue: string);
    procedure SetScanAttempts(AValue: integer);
    procedure SetScanTimeout(AValue: integer);
    {$ENDIF}

    function GetDownloadAttempts: integer;
    function GetDownloadTimeout: integer;
    function GetDateFormat: integer;
    function GetDeviceName: integer;
    function GetDirectory: string;
    function GetExtension: string;
    function GetFilenameFormat: integer;

    procedure SetDownloadAttempts(AValue: integer);
    procedure SetDownloadTimeout(AValue: integer);
    procedure SetDateFormat(AValue: integer);
    procedure SetDeviceName(AValue: integer);
    procedure SetDirectory(AValue: string);
    procedure SetExtension(AValue: string);
    procedure SetFilenameFormat(AValue: integer);
  public
    constructor create;
    destructor destroy; override;

    {$IFDEF INCLUDE_MQTT_OPTIONS}
    property host: string read GetHost write SetHost;
    property port: integer read GetPort write SetPort;
    property user: string read GetUser write SetUser;
    property password: string read GetPassword write SetPassword;
    property topic: string read GetTopic write SetTopic;
    {$ENDIF}

    {$IFDEF INCLUDE_HTTP_OPTIONS}
    property Subnet: string read GetSubnet write SetSubnet;
    property SubnetBits: integer read GetSubnetBits write SetSubnetBits;
    property ScanAllIP: boolean read GetScanAllIP write SetScanAllIP;
    property FirstIP: string read GetFirstIP write SetFirstIP;
    property LastIP: string read GetLastIP write SetLastIP;
    property ScanAttempts: integer read GetScanAttempts write SetScanAttempts;
    property ScanTimeout: integer read GetScanTimeout write SetScanTimeout;
    {$ENDIF}

    property Directory: string read GetDirectory write SetDirectory;
    property Extension: string read GetExtension write SetExtension;
    property DateFormat: integer read GetDateFormat write SetDateFormat;
    property FilenameFormat: integer read GetFilenameFormat write SetFilenameFormat;
    property DeviceName: integer read GetDeviceName write SetDeviceName;
    property DownloadAttempts: integer read GetDownloadAttempts write SetDownloadAttempts;
    property DownloadTimeout: integer read GetDownloadTimeout write SetDownloadTimeout;
  end;

var
  params: TParams;

implementation

{$IFDEF INCLUDE_MQTT_OPTIONS}
uses
  pwd;
{$ENDIF}

const
  // never translated these ini file section and entry names
  Soptions = 'Options';

  {$IFDEF INCLUDE_MQTT_OPTIONS}
  Shost = 'Host';
  Sport = 'Port';
  Suser = 'User';
  Spassword = 'Password';
  Stopic = 'Topic';
  {$ENDIF}

  {$IFDEF INCLUDE_HTTP_OPTIONS}
  Ssubnet = 'Subnet';
  SsubnetBits = 'SubnetBits';
  SscanAllIp = 'ScanAllIP';
  SfirstIP = 'FirstIp';
  SlastIP = 'LastIp';
  SscanAttempts = 'ScanAttempts';
  SscanTimeout = 'ScanTimeout';
  {$ENDIF}

  Sdirectory = 'Directory';
  Sextension = 'Extension';
  SdateFormat = 'DateFormat';
  SfilenameFormat = 'FilenameFormat';
  SdeviceName = 'DeviceName';
  SdownloadAttempts = 'DownloadAttempts';
  SdownloadTimeout = 'DownloadTimeout';


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

{$IFDEF INCLUDE_MQTT_OPTIONS}

function TParams.GetHost: string;
begin
  result := ini.ReadString(Soptions, Shost, DEFAULT_HOST);
end;

function TParams.GetUser: string;
begin
  result := Decrypt(ini.ReadString(Soptions, Suser, DEFAULT_USER));
end;

function TParams.GetPassword: string;
begin
  result := Decrypt(ini.ReadString(Soptions, Spassword, DEFAULT_PASSWORD));
end;

function TParams.GetPort: integer;
begin
  result := ini.ReadInteger(Soptions, Sport, DEFAULT_PORT);
end;

function TParams.GetTopic: string;
begin
  result := ini.ReadString(Soptions, Stopic, DEFAULT_TOPIC);
end;

procedure TParams.SetHost(AValue: string);
begin
  ini.WriteString(Soptions, Shost, AValue);
end;

procedure TParams.SetPort(AValue: integer);
begin
  ini.WriteInteger(Soptions, Sport, AValue);
end;

procedure TParams.SetUser(AValue: string);
begin
  ini.WriteString(Soptions, Suser, Encrypt(AValue));
end;

procedure TParams.SetPassword(AValue: string);
begin
  ini.WriteString(Soptions, Spassword, Encrypt(AValue));
end;

procedure TParams.SetTopic(AValue: string);
begin
  ini.WriteString(Soptions, Stopic, AValue);
end;

{$ENDIF}

{$IFDEF INCLUDE_HTTP_OPTIONS}

function TParams.GetSubnet: string;
begin
  result := ini.ReadString(Soptions, Ssubnet, DEFAULT_SUBNET);
end;

function TParams.GetSubnetBits: integer;
begin
  result := ini.ReadInteger(Soptions, SsubnetBits, DEFAULT_SUBNET_BITS);
end;

function TParams.GetScanAllIP: boolean;
begin
  result := ini.ReadBool(Soptions, SscanAllIP, DEFAULT_SCAN_ALL_IP);
end;

function TParams.GetFirstIP: string;
begin
  result := ini.ReadString(Soptions, SfirstIP, DEFAULT_FIRST_IP);
end;

function TParams.GetLastIP: string;
begin
  result := ini.ReadString(Soptions, SlastIp, DEFAULT_LAST_IP);
end;

function TParams.GetScanAttempts: integer;
begin
  result := ini.ReadInteger(Soptions, SscanAttempts, DEFAULT_SCAN_ATTEMPTS);
end;

function TParams.GetScanTimeout: integer;
begin
  result := ini.ReadInteger(Soptions, SscanTimeout, DEFAULT_SCAN_TIMEOUT);
end;

procedure TParams.SetSubnet(AValue: string);
begin
  ini.WriteString(Soptions, Ssubnet, AValue);
end;

procedure TParams.SetSubnetBits(AValue: integer);
begin
  ini.WriteInteger(Soptions, SsubnetBits, AValue);
end;

procedure TParams.SetScanAllIP(AValue: boolean);
begin
  ini.writeBool(Soptions, SscanAllIp, AValue);
end;

procedure TParams.SetFirstIP(AValue: string);
begin
  ini.writeString(Soptions, SfirstIP, Avalue);
end;

procedure TParams.SetLastIP(AValue: string);
begin
  ini.writeString(Soptions, SlastIP, Avalue);
end;

procedure TParams.SetScanAttempts(AValue: integer);
begin
  ini.WriteInteger(Soptions, SscanAttempts, AValue);
end;

procedure TParams.SetScanTimeout(AValue: integer);
begin
  ini.WriteInteger(Soptions, SscanTimeout, AValue);
end;

{$ENDIF}

function TParams.GetDateFormat: integer;
begin
  result := ini.ReadInteger(Soptions, SdateFormat, DEFAUT_DATE_FORMAT);
end;

function TParams.GetDownloadTimeout: integer;
begin
  result := ini.ReadInteger(Soptions, SDownloadTimeout, DEFAULT_Download_TIMEOUT);
end;

function TParams.GetDownloadAttempts: integer;
begin
  result := ini.ReadInteger(Soptions, SDownloadAttempts, DEFAULT_Download_ATTEMPTS);
end;

function TParams.GetDeviceName: integer;
begin
  result := ini.ReadInteger(Soptions, sdeviceName, DEFAULT_DEVICE_NAME);
end;

function TParams.GetDirectory: string;
begin
  result := ini.ReadString(Soptions, Sdirectory, DEFAULT_BACK_DIRECTORY);
end;

function TParams.GetExtension: string;
begin
  result := ini.ReadString(Soptions, Sextension, DEFAULT_EXTENSION);
end;

function TParams.GetFilenameFormat: integer;
begin
  result := ini.ReadInteger(Soptions, SfilenameFormat, DEFAULT_FILENAME_FORMAT);
end;

procedure TParams.SetDownloadAttempts(AValue: integer);
begin
  ini.writeInteger(Soptions, SDownloadAttempts, AValue);
end;

procedure TParams.SetDownloadTimeout(AValue: integer);
begin
  ini.writeInteger(Soptions, SDownloadTimeout, AValue);
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
  {$IFDEF INCLUDE_MQTT_OPTIONS}
  getEncryptionKey(ExtractFilePath(configfile) + 'key.txt');
  {$ENDIF}
finalization
  params.free;
end.

