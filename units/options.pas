unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles;

const
  DEFAULT_DISCOVERY_METHOD = 0;   // MQTT, 1 HTTP, 2 HOST LIST

  // default mqtt options
  DEFAULT_HOST = '192.168.1.22';
  DEFAULT_PORT = 1883;
  DEFAULT_PASSWORD = '';
  DEFAULT_USER = '';
  DEFAULT_TOPIC = 'tasmotas, sonoffs';
  DEFAULT_MQTT_TIMEOUT = 2;

  // default http scan options
  DEFAULT_SUBNET = '192.168.0.0';
  DEFAULT_SUBNET_BITS = 24;
  DEFAULT_SCAN_ALL_IP = true;
  DEFAULT_FIRST_IP = '192.168.0.100';
  DEFAULT_LAST_IP = '192.168.0.200';
  DEFAULT_SCAN_ATTEMPTS = 2;
  DEFAULT_SCAN_TIMEOUT = 1;          // seconds

  // default use host list options
  DEFAULT_INCLUDE_HOST_FILE = false;
  DEFAULT_HOST_FILE_NAME = '';
  DEFAULT_INCLUDE_HOST_LIST  = false;
  DEFAULT_HOST_LIST = '';

  // default backup options
  DEFAUT_DATE_FORMAT = 2;
  DEFAULT_DEVICE_NAME = 0;
  DEFAULT_BACK_DIRECTORY = 'backup';
  DEFAULT_EXTENSION = '.dmp';
  DEFAULT_FILENAME_FORMAT = 0;
  DEFAULT_DOWNLOAD_ATTEMPTS = 2;
  DEFAULT_DOWNLOAD_TIMEOUT = 4;

type
  
  { TParams }

  TOptionListAction = (olaNothing, olaSave, olaErase);

  TParams = class
  private
    ini: TIniFile;

    FDiscoveryMethod: integer;

    FIncludeIPs: TStrings;
    FExcludeIPs: TStrings;
    FExcludeIPsAction: TOptionListAction;
    FIncludeIPsAction: TOptionListAction;

    FHostList: TStrings;
    FHostListAction: TOptionListAction;

    function GetDiscoveryMethod: integer;
    procedure SetDiscoveryMethod(AValue: integer);

    function GetSubnet: string;
    function GetSubnetBits: integer;
    function GetScanAllIP: boolean;
    function GetFirstIP: string;
    function GetLastIP: string;
    function GetScanAttempts: integer;
    function GetScanTimeout: integer;
    procedure SetExcludeIPs(AValue: TStrings);
    procedure SetIncludeIPs(AValue: TStrings);
    procedure SetSubnet(AValue: string);
    procedure SetSubnetBits(AValue: integer);
    procedure SetScanAllIP(AValue: boolean);
    procedure SetFirstIP(AValue: string);
    procedure SetLastIP(AValue: string);
    procedure SetScanAttempts(AValue: integer);
    procedure SetScanTimeout(AValue: integer);

    function GetHost: string;
    function GetPort: integer;
    function GetUser: string;
    function GetPassword: string;
    function GetTopic: string;
    function GetMqttTimeout: integer;
    procedure SetHost(AValue: string);
    procedure SetPort(AValue: integer);
    procedure SetUser(AValue: string);
    procedure SetPassword(AValue: string);
    procedure SetTopic(AValue: string);
    procedure SetMqttTimeout(AValue: integer);

    function GetIncludeHostFile: boolean;
    function GetHostFilename: string;
    function GetIncludeHostList: boolean;
    procedure SetIncludeHostFile(AValue: boolean);
    procedure SetHostFilename(AValue: string);
    procedure SetIncludeHostList(AValue: boolean);
    procedure SetHostList(AValue: TStrings);

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

    property DiscoveryMethod: integer read GetDiscoveryMethod write SetDiscoveryMethod;

    // mqtt options
    property Host: string read GetHost write SetHost;
    property Port: integer read GetPort write SetPort;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Topic: string read GetTopic write SetTopic;
    property MqttTimeout: integer read GetMqttTimeout write SetMqttTimeout;

    // http options
    property Subnet: string read GetSubnet write SetSubnet;
    property SubnetBits: integer read GetSubnetBits write SetSubnetBits;
    property ScanAllIP: boolean read GetScanAllIP write SetScanAllIP;
    property FirstIP: string read GetFirstIP write SetFirstIP;
    property LastIP: string read GetLastIP write SetLastIP;
    property ScanAttempts: integer read GetScanAttempts write SetScanAttempts;
    property ScanTimeout: integer read GetScanTimeout write SetScanTimeout;
    property ExcludeIPs: TStrings read FExcludeIPs write SetExcludeIPs;
    property ExcludeIPsAction: TOptionListAction read FExcludeIPsAction write FExcludeIPsAction;
    property IncludeIPs: TStrings read FIncludeIPs write SetIncludeIPs;
    property IncludeIPsAction: TOptionListAction read FIncludeIPsAction write FIncludeIPsAction;

    // host list options
    property IncludeHostFile: boolean read GetIncludeHostFile write SetIncludeHostFile;
    property HostFileName: string read GetHostFilename write SetHostFilename;
    property IncludeHostList: boolean read GetIncludeHostList write SetIncludeHostList;
    property HostList: TStrings read FHostList write SetHostList;
    property HostListAction: TOptionListAction read FHostListAction write FHostListAction;

    // backup options
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

uses
  pwd;

const
  // never translated these ini file section and entry names
  Soptions = 'Options';
  Sexclude = 'Exclude';
  Sinclude = 'Include';
  Shosts = 'Hosts';

    SdiscoveryMethod = 'DiscoveryMethod';

  Shost = 'Host';
  Sport = 'Port';
  Suser = 'User';
  Spassword = 'Password';
  Stopic = 'Topic';
  SmqttTimeout = 'MqttTimeout';

  Ssubnet = 'Subnet';
  SsubnetBits = 'SubnetBits';
  SscanAllIp = 'ScanAllIP';
  SfirstIP = 'FirstIp';
  SlastIP = 'LastIp';
  SscanAttempts = 'ScanAttempts';
  SscanTimeout = 'ScanTimeout';

  SincludeHostFile = 'IncludeHostFile';
  ShostFilename = 'HostFileName';
  SincludeHostList = 'IncludeHostList';

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


function TParams.GetDiscoveryMethod: integer;
begin
  result := ini.ReadInteger(Soptions, SdiscoveryMethod, DEFAULT_DISCOVERY_METHOD);
end;

procedure TParams.SetDiscoveryMethod(AValue: integer);
begin
  ini.WriteInteger(Soptions, SdiscoveryMethod, AValue);
end;


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

function TParams.GetMqttTimeout: integer;
begin
  result := ini.ReadInteger(Soptions, SmqttTimeout, DEFAULT_MQTT_TIMEOUT);
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

procedure TParams.SetMqttTimeout(AValue: integer);
begin
  ini.WriteInteger(Soptions, SmqttTimeout, AValue);
end;



function TParams.GetIncludeHostFile: boolean;
begin
  result := ini.ReadBool(Soptions, SincludeHostFile, DEFAULT_INCLUDE_HOST_FILE);
end;

function TParams.GetHostFilename: string;
begin
  result := ini.ReadString(Soptions, ShostFilename, DEFAULT_HOST_FILE_NAME);
end;

function TParams.GetIncludeHostList: boolean;
begin
  result := ini.ReadBool(Soptions, SincludeHostList, DEFAULT_INCLUDE_HOST_LIST);
end;

procedure TParams.SetIncludeHostFile(AValue: boolean);
begin
  ini.WriteBool(Soptions, SincludeHostFile, AValue);
end;

procedure TParams.SetHostFilename(AValue: string);
begin
  ini.WriteString(Soptions, ShostFilename, AValue);
end;

procedure TParams.SetIncludeHostList(AValue: boolean);
begin
  ini.WriteBool(Soptions, SincludeHostList, AValue);
end;

procedure TParams.SetHostList(AValue: TStrings);
begin
  FHostList.assign(AValue);
end;


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

procedure TParams.SetExcludeIPs(AValue: TStrings);
begin
  FExcludeIPs.assign(AValue);
end;

procedure TParams.SetIncludeIPs(AValue: TStrings);
begin
  FIncludeIPs.assign(AValue);
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
var
  i: integer;
begin
  inherited create;
  ini := TIniFile.create(configfile, []);
  FIncludeIPs := TStringList.create;
  FExcludeIPs := TStringList.create;
  FHostList := TStringList.create;
  ini.ReadSectionValues(Sexclude, FExcludeIPs);
  ini.ReadSectionValues(Sinclude, FIncludeIPs);
  ini.ReadSectionValues(Shosts, FHostList);
  for i := 0 to FExcludeIPs.count-1 do
    FExcludeIPs[i] := FExcludeIPs.ValueFromIndex[i];
  for i := 0 to FIncludeIPs.count-1 do
    FIncludeIPs[i] := FIncludeIPs.ValueFromIndex[i];
  for i := 0 to FHostList.count-1 do
    FHostList[i] := FHostList.ValueFromIndex[i];
end;

destructor TParams.destroy;
var
  i: integer;
begin
  if ExcludeIPsAction <> olaNothing then begin
    ini.EraseSection(Sexclude);
    if ExcludeIPsAction = olaSave then begin
      for i := 0 to FExcludeIPs.count-1 do
         ini.writeString(Sexclude, Format('%0.3d', [i]), FExcludeIPs[i]);
    end;
  end;
  if IncludeIPsAction <> olaNothing then begin
    ini.EraseSection(Sinclude);
    if IncludeIPsAction = olaSave then begin
      for i := 0 to FIncludeIPs.count-1 do
         ini.writeString(Sinclude, Format('%0.3d', [i]), FIncludeIPs[i]);
    end;
  end;
  if HostListAction <> olaNothing then begin
    ini.EraseSection(Shosts);
    if HostListAction = olaSave then begin
      for i := 0 to FHostList.count-1 do
         ini.writeString(Shosts, Format('%0.3d', [i]), FHostList[i]);
    end;
  end;

  FIncludeIPs.free;
  FExcludeIPs.free;
  FHostList.Free;
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
  getEncryptionKey(ExtractFilePath(configfile) + 'key.txt');
finalization
  params.free;
end.

