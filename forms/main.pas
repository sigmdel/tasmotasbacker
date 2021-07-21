(*
  File: main.pas

  The utility can download the configuration of multiple Tasmota devices

  To identify the Tasmotat devices it can

    1. publish a Status 5 command as an MQTT message and
       obtain the reply from all Tasmota devices connected to the broker

    2. do an HTTP scan to find Tasmota devices on the local area network

    3. use a predefined list of hosts (IP addresses or host names
       mDns (i.e. .local domain) names are not supported).

  To communicate with the MQTT broker the Eclipse mosquitto libraries must be
  installed on the system. If the library is installed, the program will
  work but only methods 2. or 3. can be used to identify the Tasmota devices.

  Uses a modified version of mosquitto-p which is a Free Pascal conversions of
  the libmosquitto header file mosquitto.h and mqttclass.pas by
  Károly Balogh (chainq) @ https://github.com/chainq/mosquitto-p

  The ctypes unit must be loaded at the start of the program. Furthermore,
  the cthreads unit must be loaded first in Linux systems. See the uses
  clause in the project source:

    uses
      {$IFDEF UNIX}{$IFDEF UseCThreads}
      cthreads,
      {$ENDIF}{$ENDIF}
      ctypes, // needed by mosquitto
      ...

  The  UseCThreads is defined by adding -dUseCThreads in the
  Personalised Options of the Compiler Options in Project Options.
*)

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, EditBtn, Grids, Menus, ipv4;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Back2Button: TButton;
    Back2Button1: TButton;
    Back2Button2: TButton;
    Back2Button3: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    IncludeHostFileCheckBox: TCheckBox;
    IncludeHostListCheckBox: TCheckBox;
    HostFileNameEdit: TFileNameEdit;
    ExcludeCheckBox: TCheckBox;
    ExcludeListBox: TListBox;
    FirstIPEdit: TEdit;
    FullRangeRadioButton: TRadioButton;
    HomeLabel: TLabel;
    ImageList1: TImageList;
    IncludeCheckBox: TCheckBox;
    IncludeListBox: TListBox;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    LastIPEdit: TEdit;
    HostListMemo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    N2: TMenuItem;
    Next1Button1: TButton;
    Next1Button2: TButton;
    NextButton1: TButton;
    PageHostList: TPage;
    PageHttpScan: TPage;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    ScanAttemptsEdit: TSpinEdit;
    ScanTimeoutEdit: TSpinEdit;
    SourceLabel: TLabel;
    PageStart: TPage;
    PasswordEdit: TEditButton;
    PopupMenu1: TPopupMenu;
    ResetButton: TButton;
    AllOptionsCheckBox: TCheckBox;
    DateFormatEdit: TComboBox;
    DeviceGrid: TStringGrid;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Next1Button: TButton;
    Next2Button: TButton;
    OptionsButton: TButton;
    BackupButton: TButton;
    Panel1: TPanel;
    QuitButton: TButton;
    BackButton: TButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    SaveButton: TButton;
    CheckBox1: TCheckBox;
    DateEdit: TDateEdit;
    DirectoryEdit: TDirectoryEdit;
    Label14: TLabel;
    PageOptions: TPage;
    OptionsGrid: TStringGrid;
    DownloadAttemptsEdit: TSpinEdit;
    DownloadTimeoutEdit: TSpinEdit;
    MqttTimeoutEdit: TSpinEdit;
    SubnetBitsEdit: TSpinEdit;
    SubnetEdit: TEdit;
    TopicEdit: TEdit;
    HostEdit: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    PageBackupResults: TPage;
    ResGrid: TStringGrid;
    UserEdit: TEdit;
    ExtensionEdit: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Notebook1: TNotebook;
    PageMqttBroker: TPage;
    PageFoundDevices: TPage;
    PageBackupParams: TPage;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    PortEdit: TSpinEdit;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Back2Button1Click(Sender: TObject);
    procedure Back2ButtonClick(Sender: TObject);
    procedure ExcludeCheckBoxChange(Sender: TObject);
    procedure FirstIPEditEditingDone(Sender: TObject);
    procedure ExcludeListBoxDblClick(Sender: TObject);
    procedure HomeLabelClick(Sender: TObject);
    procedure IncludeCheckBoxChange(Sender: TObject);
    procedure IncludeListBoxDblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure Next1Button1Click(Sender: TObject);
    procedure Next1Button2Click(Sender: TObject);
    procedure NextButton1Click(Sender: TObject);
    procedure PageHttpScanBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure PasswordEditButtonClick(Sender: TObject);
    procedure PopupMenu1Close(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure AllOptionsCheckBoxChange(Sender: TObject);
    procedure DateFormatEditChange(Sender: TObject);
    procedure DeviceGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Next1ButtonClick(Sender: TObject);
    procedure Next2ButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure BackupButtonClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure DeviceListClickCheck(Sender: TObject);
    procedure ExtensionEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure PageFoundDevicesBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure PageBackupResultsBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure PageOptionsBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure QuitButtonClick(Sender: TObject);
    procedure GridHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure GridResize(Sender: TObject);
    procedure ResGridDblClick(Sender: TObject);
    procedure ResGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SaveButtonClick(Sender: TObject);
    procedure ScanAttemptsEditChange(Sender: TObject);
    procedure SourceLabelClick(Sender: TObject);
    procedure SubnetBitsEditChange(Sender: TObject);
    procedure SubnetEditDone(Sender: TObject);
    //procedure SubnetEditDone(Sender: TObject);
  private
    popx: integer;
    popy: integer;
    currentCol: integer;
    resizinggrid: boolean;
    updategrid: TObject;
    newdev: boolean;
    fip, lip: TIPv4;
    maxRequestTimeProp: single;
    procedure CalcMaxRequestTime;
    procedure FixupAllIpHint;
    procedure FixupIpRangeHint;
    function GetExtension: string;
    procedure GetDevicesFromMqttBroker;
    procedure GetDevicesWithHttpScan;
    procedure GetDevicesFromList(hosts: TStrings);
    procedure UpdateCheckCount;
    procedure CopyResGridToClipbrd(delim: char);
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  LCLIntf, clipbrd, fileinfo, options, resolve, ssockets, fphttpclient,
  mqttclass, mosquitto, iplistedit, math;

{$R *.lfm}


{$IFDEF DEBUG_HTTP_REQUEST}
  {$DEFINE DO_LOG}
{$ENDIF}

{$IFDEF DEBUG_BACKUP}
  {$DEFINE DO_LOG}
{$ENDIF}

{$IFDEF DEBUG_HTTP_SCAN}
  {$DEFINE DO_LOG}
{$ENDIF}

{$IFDEF DEFINE TIME_HTTP_SCAN}
  {$DEFINE DO_LOG}
{$ENDIF}

{  Utility functions }

{$IFDEF DO_LOG}

// In Linux system, messages sent to the Log function are displayed
// on the standard output. To see the output launch the utility in
// a terminal, not from GUI file namanger.
//
// In Windows, messages  are saved to a log file named tasmotasbacker.log

var
  lastic: longint = 0;
  logopened: boolean = false;

procedure log(msg: string);
var
  tic: QWORD;
  i: integer;
begin
  tic := GetTickCount64;
  if not logopened then begin
     {$IFDEF MSWINDOWS}
     close(StdOut);
     assign(StdOut,  changefileext(application.ExeName, '.log'));
     rewrite(StdOut);
     {$ENDIF}
     writeln(StdOut, 'time':18, 'diff':8,'   ', 'message');
     logopened := true;
  end;
  for i := 1 to length(msg) do
    if (msg[i] < ' ') or (msg[i] > '~') then msg[i] := '?';
  writeln(StdOut, timetostr(now), '.', tic, tic-lastic:8, '   ', msg);
  lastic := tic;
end;
{$ENDIF}

// kludge to give some time to the gui to update itself while
// waiting for responses from the MQTT broker and HTTP requests
procedure Delay(dt: QWORD);
var
  tcount : QWORD;
begin
  tcount := GetTickCount64;
  while (GetTickCount64 - tcount < dt) and (not Application.Terminated) do
    Application.ProcessMessages;
end;

procedure TcpPortAvailable(const host: string; port: integer; timeout: integer = 5000);
begin
  if timeout < 1000 then
    timeout := 1000;
  try
    With TInetSocket.Create(Host, Port, timeout) do begin
      Free;
    end;
  except
    Raise Exception.createFmt('%s:%d not reachable', [host, port]);
  end;
end;

{
TSocketErrorType = (
 0  seHostNotFound,
 1  seCreationFailed,
 2  seBindFailed,
 3  seListenFailed,
 4  seConnectFailed,
 5  seConnectTimeOut,
 6  seAcceptFailed,
 7  seAcceptWouldBlock,
 8  seIOTimeOut);
}
function HttpRequest(const aURL: string; out code: integer; out rawdata: string; maxtries: integer = 3; backoff: integer = 4000): boolean;
var
  i: integer;
  {$IFDEF DEBUG_HTTP_REQUEST}
  gotE: boolean;
  eclass: string;
  emsg: string;
  {$ENDIF}
begin
  {$IFDEF DEBUG_HTTP_REQUEST}
  lastic := GetTickCount64;
  log(Format('HttpRequest(url: %s, maxtries: %d, timeout: %d)', [aURL, maxtries, backoff]));
  {$ENDIF}
  rawdata := '';
  code := -1;
  with TFPHTTPClient.create(nil) do try
    KeepConnection := False;
    for i := 0 to maxtries-1 do begin
      ConnectTimeOut := (i+1)*backoff;
      delay(2);
      {$IFDEF DEBUG_HTTP_REQUEST}
      gotE := false;
      eclass := '';
      emsg := '';
      rawdata := '';
      {$ENDIF}
      try
        rawdata := Get(aURL);
        code := ResponseStatusCode;
        {$IFDEF DEBUG_HTTP_REQUEST}
        log(Format('  request returns after %d tries with code = %d, data = %s', [i+1, code, copy(rawdata, 1, 64)]));
        {$ENDIF}
        exit;
      except
        {$IFDEF DEBUG_HTTP_REQUEST}
        On E: Exception do begin
          gotE := true;
          eclass := E.classname;
          emsg := E.Message;
          if E is ESocketError then
            code := integer(ESocketError(E).Code)
          else
            code := ResponseStatusCode;
        end;
        {$ELSE}
        On  E: ESocketError do
          code := integer(E.Code);
        else
          code := ResponseStatusCode;
        rawdata := ResponseStatusText;
        {$ENDIF}
      end;
      {$IFDEF DEBUG_HTTP_REQUEST}
      if gotE then
        log(Format('  Exception class %s, message %s, code %d, try %d', [eclass, emsg, code, i+1]));
      {$ENDIF};
      if (code = 404) or (code = integer(seConnectFailed)) or (code = integer(seHostNotFound)) then begin
        {$IFDEF DEBUG_HTTP_REQUEST}
        log(Format('  request returns after %d tries with code = %d, data = %s', [i+1, code, copy(rawdata, 1, 64)]));
        {$ENDIF}
        exit;
      end;
      {$IFDEF DEBUG_HTTP_REQUEST}
      log(Format('  http request attemp %d failed with code: %d', [i+1, code]));
      {$ENDIF}
    end; // for loop
  finally
    result := code = 200;
    Free;
  end;
  {$IFDEF DEBUG_HTTP_SCAN}
  log(Format('  http request returns after %d tries with code = %d, data = %s', [i+1, code, copy(rawdata, 1, 64)]));
  {$ENDIF}
end;

// mosquitto library log level
const
  {$IFDEF MSWINDOWS}  // no log in Windows
  MOSQ_LOG = MOSQ_LOG_NONE;
  {$ELSE} // chose one from any log level defined in mosquitto.pas,  for all debug levels
  //MOSQ_LOG = MOSQ_LOG_ALL;
  MOSQ_LOG = MOSQ_LOG_NODEBUG;
  //MOSQ_LOG = MOSQ_LOG_NONE;
  {$ENDIF}

//Save a string as a file
procedure SaveStringToFile(const s, filename: string);
var
  F: TextFile;
begin
  AssignFile(F, filename);
  try
    ReWrite(F);
    Write(F, s);
  finally
    CloseFile(F);
  end;
end;

type
  TThisMQTTConnection = class(TMQTTConnection)
  private
    FThisIP: string;
    FThisTopic: string;
    FThisHostname: string;
    procedure UpdateGUI;
    procedure MessageHandler(const payload: Pmosquitto_message);
  end;

var
  MqttClient: TThisMQTTConnection = nil;
  MqttConfig: TMQTTConfig;

{ TThisMQTTConnection }
procedure TThisMQTTConnection.UpdateGUI;
var
  i, r: integer;
begin
   with MainForm.DeviceGrid do begin
     r := -1;
     for i := 1 to RowCount-1 do
       if cells[1, i] = FThisIP then begin
         r := i;
         break;
       end;
     if r < 0 then begin
       r := RowCount;
       RowCount := RowCount+1;
     end;
     cells[0, r] := '1';
     cells[1, r] := FThisIp;
     cells[2, r] := FThisTopic;
     cells[3, r] := FThisHostname;
     MainForm.newdev := true;
  end;
end;

// make it option to use mqtt-topic or hostname
procedure TThisMQTTConnection.MessageHandler(const payload: Pmosquitto_message);
// topic:  'stat/garage-exterieur/STATUS5'
// payload: '{"StatusNET":{"Hostname":"garage-exterieur","IPAddress":"192.168.0.107","Gateway":... '
var
  msg: string;
  top: string;
  p: integer;
begin
   msg := '';
   with payload^ do begin
      if (payloadlen > 0) then begin
        SetLength(msg,payloadlen);
        Move(payload^,msg[1],payloadlen);
      end
      else
        exit;

      p := pos('"Hostname":"', msg);
      if p < 1 then exit;
      delete(msg, 1, p + 11);
      p := pos('"', msg);
      if p < 1 then exit;
      FThisHostname := copy(msg, 1, p-1);

      p := pos('"IPAddress":"', msg);
      if p < 1 then exit;
      delete(msg, 1, p + 12);
      p := pos('"', msg);
      if p < 1 then exit;
      FThisIP := copy(msg, 1, p-1);

      top := topic;
      delete(top, 1, 5); // delete stat/
      p := pos('/', top);
      if p < 1 then exit;
      FThisTopic := copy(top, 1, p-1);
      Synchronize(@UpdateGui);
   end;
end;

// Converts value seconds into a  'xx minutes xx seconds' string
// if value < 120 then no minutes shown
function SecondsToStr(value: integer): string;
begin
  if value <= 0 then
    result := ''
  else if value < 120 then
    result := Format(' %d seconds', [value])
  else
    result := Format(' %d minutes%s', [value div 60, SecondsToStr(value mod 60)]);
end;

{ TMainForm }

procedure TMainForm.AllOptionsCheckBoxChange(Sender: TObject);
var
  b: string;
  i: integer;
begin
  if AllOptionsCheckBox.Checked then
    b := '1'
  else
    b := '0';
  for i := 0 to OptionsGrid.RowCount-1 do
    OptionsGrid.cells[0, i] := b;
end;

procedure TMainForm.ApplicationProperties1Idle(Sender: TObject;
  var Done: Boolean);
begin
  if newdev then begin
    UpdateCheckCount;
    newdev := false;
    invalidate;
    application.processMessages;
  end;
  if assigned(updategrid) then begin
    GridResize(updateGrid);
    updategrid := nil;
  end;
end;

procedure TMainForm.Back2Button1Click(Sender: TObject);
begin
  Notebook1.PageIndex := 0;
end;

procedure TMainForm.Back2ButtonClick(Sender: TObject);
begin
  DeviceGrid.RowCount := 1;
  Notebook1.PageIndex := 0;
end;

procedure TMainForm.ExcludeCheckBoxChange(Sender: TObject);
begin

end;

procedure TMainForm.BackButtonClick(Sender: TObject);
begin
  Notebook1.PageIndex := 2;
end;

procedure TMainForm.BackupButtonClick(Sender: TObject);
var
  i: integer;
  ip, device, s1, s2: string;
  url, html: String;
  aRow: integer;
  resstring: string;
  code, count: integer;
begin
  Notebook1.PageIndex := 4;
  Notebook1.Invalidate;
  Notebook1.Update;
  Label19.caption := ExpandFilename(DirectoryEdit.Directory);
  application.ProcessMessages;
  screen.Cursor := crHourglass;
  OptionsButton.enabled := false;
  try
    count := 0;
    for i := 1 to DeviceGrid.RowCount-1 do begin
      ip := trim(DeviceGrid.cells[1, i]);
      if radioButton3.checked then
        device := trim(DeviceGrid.cells[2, i])
      else
        device := trim(DeviceGrid.cells[3, i]);
       aRow := ResGrid.RowCount;
       ResGrid.RowCount := aRow + 1;
       ResGrid.Cells[0, aRow] := ip;
       ResGrid.Cells[1, aRow] := device;
       if DeviceGrid.cells[0, i] = '0' then
         resstring := 'not selected'
       else begin
         if radiobutton1.checked then begin
           s1 := trim(device);
           s2 := DateEdit.Text;
         end
         else begin
           s1 := DateEdit.Text;
           s2 := trim(device);
         end;
         device := Format('%s%s%s-%s%s',
           [DirectoryEdit.Directory,  DirectorySeparator, s1, s2, GetExtension]);
         if count = 0 then
           ForceDirectories(DirectoryEdit.Directory);
         url := 'http://' + ip + '/dl';
         html := '';
         {$IFDEF DEBUG_BACKUP}
         log('Backup - reading '+ url);
         {$ENDIF}

         if HttpRequest(url, code, html, DownloadAttemptsEdit.value,
           DownloadTimeoutEdit.value*1000) and (html <> '') then begin
           SaveStringToFile(html, device);
           {$IFDEF DEBUG_BACKUP}
           log(' Saved to ' + device);
           {$ENDIF}
           resstring := Format('saved to %s', [extractFilename(device)]);
         end
         else begin
           resstring := Format('http error %d', [code]);
         end;
         inc(count);
       end;
       ResGrid.Cells[2, aRow] := resstring;
       ResGrid.invalidate;
       ResGrid.Update;
       application.ProcessMessages;
    end;
  finally
    screen.Cursor := crDefault;
  end;
  OptionsButton.enabled := true;
  OptionsButton.setFocus;
end;

procedure TMainForm.CalcMaxRequestTime;
begin
  MaxRequestTimeProp := ScanTimeoutEdit.value * (intpower(2, ScanAttemptsEdit.value) - 1);
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
var
  b: string;
  i: integer;
begin
  if CheckBox1.Checked then
    b := '1'
  else
    b := '0';
  for i := 0 to DeviceGrid.RowCount-1 do
    DeviceGrid.cells[0, i] := b;
  UpdateCheckCount;
end;

procedure TMainForm.CopyResGridToClipbrd(delim: char);
var
  stream: TStringStream;
begin
  stream := TStringStream.create;
  try
    ResGrid.SaveToCSVStream(stream, delim);
    Clipboard.AsText := stream.DataString;
  finally
    stream.free;
  end;
end;

procedure TMainForm.DateFormatEditChange(Sender: TObject);
begin
   DateEdit.DateOrder := TDateOrder(DateFormatEdit.ItemIndex + 1);
end;

procedure TMainForm.DeviceGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
   currentcol := aCol;
   CanSelect := aCol = 0;
end;

procedure TMainForm.DeviceListClickCheck(Sender: TObject);
begin
  UpdateCheckCount;
end;

procedure TMainForm.ExcludeListBoxDblClick(Sender: TObject);
begin
  EditIPList(ExcludeListBox.Items)
end;

procedure TMainForm.HomeLabelClick(Sender: TObject);
begin
  OpenUrl('https://sigmdel.ca/michel/ha/tasmota/tasmota_backups_en.html');  // report
end;

procedure TMainForm.ExtensionEditChange(Sender: TObject);
var
  newext: string;
begin
  newext := GetExtension;
  RadioButton1.Caption := ChangeFileExt(RadioButton1.caption, newext);
  RadioButton2.Caption := ChangeFileExt(RadioButton2.caption, newext);
end;

procedure TMainForm.FirstIPEditEditingDone(Sender: TObject);
begin
  FixupIpRangeHint;
end;

procedure TMainForm.FixupAllIpHint;
var
  mask: TIPv4;
  ttime: integer;
  nips: integer;
begin
  mask := not SubnetMask(SubnetBitsEdit.value);
  nips := mask.value - 2 - ExcludeListBox.Count*ord(ExcludeCheckBox.Checked);
  ttime := round( nips * MaxRequestTimeProp ) ;
  FullRangeRadioButton.Hint := Format('Could take up to %s', [SecondsToStr(ttime)]);
end;

procedure TMainForm.FixupIpRangeHint;
var
  lfip, llip: TIpv4;
  ttime: integer;
  nips: integer;
begin
  if TryStrToIPv4(FirstIpEdit.text, lfip)
  and TryStrToIPv4(LastIpEdit.text, llip) then begin
    nips := llip.value - lfip.value + 1
      - ExcludeListBox.Count*ord(ExcludeCheckBox.Checked)
      + IncludeListBox.Count*ord(IncludeCheckBox.Checked);
    ttime := round( nips * MaxRequestTimeProp ) ;
    RadioButton8.Hint := Format('Could take up to %s', [SecondsToStr(ttime)]);
    label23.Hint := RadioButton8.Hint;
    FirstIPEdit.Hint := RadioButton8.Hint;
    LastIPEdit.Hint := RadioButton8.Hint;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Version : TProgramVersion;
begin
  if GetProgramVersion(Version) then with Version do
    label28.caption := Format('Version %d.%d.%d', [Major,Minor,Revision])
  else
     label28.caption := '';

  RadioButton5.Checked := params.DiscoveryMethod = 0;
  RadioButton6.Checked := params.DiscoveryMethod <> 0;

  HostEdit.Text := params.Host;
  PortEdit.value := params.Port;
  UserEdit.Text := params.User;
  PasswordEdit.Text := params.Password;
  TopicEdit.Text := params.Topic;
  MqttTimeOutEdit.Value := params.MqttTimeout;
  DirectoryEdit.directory := params.Directory;
  ExtensionEdit.text := params.Extension;
  DateFormatEdit.ItemIndex := params.Dateformat;
  DateEdit.DateOrder := TDateOrder(DateFormatEdit.ItemIndex + 1);
  if params.FilenameFormat = 0 then
    RadioButton1.Checked := true
  else
    RadioButton2.Checked := true;
  if params.DeviceName = 0 then
    RadioButton3.Checked := true
  else
    RadioButton4.Checked := true;
  DownloadAttemptsEdit.Value := params.DownloadAttempts;
  DownloadTimeoutEdit.Value := params.DownloadTimeout;

  RadioButton5.Checked := mosquitto_lib_loaded();
  RadioButton6.Checked := not RadioButton5.Checked;
  RadioButton5.Enabled := RadioButton5.checked;

  SubnetEdit.Text := params.Subnet;
  SubnetBitsEdit.value := params.SubnetBits;
  FullRangeRadioButton.Checked := params.ScanAllIP;
  RadioButton8.Checked := not params.ScanAllIP;
  FirstIpEdit.Text := params.FirstIP;
  LastIPEdit.Text := params.LastIP;
  ScanAttemptsEdit.value := params.ScanAttempts;
  ScanTimeoutEdit.value := params.ScanTimeout;

  IncludeListBox.Items.assign(params.IncludeIPs);
  ExcludeListBox.Items.assign(params.ExcludeIPs);
  CalcMaxRequestTime;
  FixupAllIpHint;
  FixupIpRangeHint;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  MqttClient.free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if not resizinggrid then begin
    resizinggrid := true;
    {
    try
      GridResize(DeviceGrid);
      GridResize(ResGrid);
      GridResize(OptionsGrid);
    finally
      invalidate;
    end;
    }
    try
      case Notebook1.PageIndex of
        1: GridResize(DeviceGrid);
        3: GridResize(ResGrid);
        4: GridResize(OptionsGrid);
        else exit;
      end;
      invalidate;
    finally
      resizinggrid := false;
    end;
  end;
end;

procedure TMainForm.GetDevicesFromMqttBroker;
var
  sl: TStringList;
  i: integer;
begin
  FillChar(MqttConfig, sizeof(MqttConfig), 0);
  with MqttConfig do begin
     //ssl := aBroker.SSL;
     //ssl_cacertfile := aBroker.SSLCert;
     hostname := HostEdit.Text;
     port := PortEdit.Value;
     username := UserEdit.Text;
     password := PasswordEdit.Text;
     keepalives := 60;
     //reconnect_delay := aBroker.ReconnectDelay;
     //reconnect_backoff := aBroker.ReconnectBackoff;
  end;
  sl := TStringList.create;
  try
    sl.Delimiter := ',';
    sl.DelimitedText := TopicEdit.Text;
    if sl.count < 1 then
      exit;
    try
      MqttClient := TThisMQTTConnection.Create('mqttClient', MqttConfig, MOSQ_LOG);
      MqttClient.OnMessage := @MqttClient.MessageHandler;
      //MqttClient.AutoReconnect := true;
      MqttClient.Connect;
      MqttClient.Subscribe('stat/+/STATUS5', 0);
      newdev := true;
      for i := 0 to sl.count-1 do begin
        MqttClient.Publish('cmnd/' + sl[i] + '/status', '5', 0, false);
        delay(5);
      end;
    except
      raise;
    end;
  finally
    sl.free;
  end;
end;

procedure TMainForm.GetDevicesWithHttpScan;
const
  URLB = 'http://%s/cm?cmnd=';
var
  myHostname, myTopic, myIps: string;
  code: integer;
  url: string;
  {$IFDEF TIME_HTTP_SCAN}
  deb, fin: TDateTime;
  {$ELSE}
  r: integer;
  {$ENDIF}

  excount: integer;
  incount: integer;
  i, j: integer;
  ip: TIPv4;
  v, fv, lv: longword;


  function FindValue(const key: string; var value: string): boolean;
  var
    p: integer;
  begin
    result := true;
    p := pos('"' + key + '":"', value);
    if p > 0 then begin
      delete(value, 1, p + length(key)+3);
      p := pos('"', value);
      if p > 0 then begin
        delete(value, p, maxint);
        exit;
      end;
    end;
    value := '';
    result := false;
  end;

  procedure CheckIP(aIP: TIPv4);
  begin
    myIps := aip.ToString;
    url := Format(URLB, [myIps]);
    myHostname := '';
    myTopic := '';
    {$IFNDEF TIME_HTTP_SCAN}
    label11.Caption := 'Checking ' + myIps;
    label11.Repaint;
    delay(2);
    {$ENDIF}
    if not HttpRequest(url + 'hostname', code, myHostname, ScanAttemptsEdit.value, ScanTimeOutEdit.value*1000) then
    //if not HttpRequest(url + 'hostname', code, myHostname) then
      exit;
    if not FindValue('Hostname', myHostname) then
      exit;
    if not HttpRequest(url + 'topic', code, myTopic, ScanAttemptsEdit.value, ScanTimeOutEdit.value*1000) then
    //if not HttpRequest(url + 'topic', code, myTopic) then
      exit;
    if not FindValue('Topic', myTopic) then
      exit;
    {$IFNDEF TIME_HTTP_SCAN}
    with DeviceGrid do begin
       r := RowCount;
       RowCount := RowCount+1;
       cells[0, r] := '1';
       cells[1, r] := myIps;
       cells[2, r] := myTopic;
       cells[3, r] := myHostname;
       MainForm.newdev := true;
    end;
    notebook1.invalidate;
    Delay(2);
    {$ENDIF}
  end;

begin
  Next2Button.Enabled := false;
  Screen.Cursor := crHourGlass;
  {$IFDEF TIME_HTTP_SCAN}
  deb := time;
  {$ENDIF}
  excount := 0;
  incount := 0;
  // get valid excluded IPs
  if ExcludeCheckBox.checked then begin
    for i := 0 to ExcludeListBox.count-1 do begin
      if (not ip.TryFromString(ExcludeListBox.Items[i]))
      or (ip < fip) or (ip > lip) then
        continue;
      ExcludeListBox.Items.Objects[excount] := TObject(intptr(ip.value));
      inc(excount);
    end;
  end;
  // get valid incldued IPs
  if IncludeCheckBox.checked then begin
    for i := 0 to IncludeListBox.count-1 do begin
      if (not ip.TryFromString(IncludeListBox.Items[i]))
      or ((fip <= ip) and (ip <= lip)) then
        continue;
      IncludeListBox.Items.Objects[incount] := TObject(intptr(ip.value));
      inc(incount);
    end;
  end;
  fv := fip.value;
  lv := lip.value;
  try
    for v := fv to lv do begin
      ip.value := v;
      // check if it is in excluded IP
      for j := 0 to excount-1 do
        if longword(ptrint(ExcludeListBox.Items.Objects[j])) = v then begin
          ip.value := 0;
          break;
        end;
      if ip.value <> 0 then
        CheckIp(ip);
    end;
    for i := 0 to incount-1 do begin
      ip.value := longword(ptrint(IncludeListBox.Items.Objects[i]));
      CheckIp(ip);
    end;
    {$IFDEF TIME_HTTP_SCAN}
    fin := now;
    log(Format('HTTP scan time: %s ms', [TimeToStr(fin-deb)]));
    {$ENDIF}
  finally
    Screen.Cursor := crDefault;
    next2Button.Enabled := true;
    next2Button.SetFocus;
  end;
end;

{ refactor both GetDevicesFromScan and GetDevicesFromList to
  create a list of IP addresses that is passed on to the actual
  ip scan procedure

  That way it should be possible to avoid duplicate requests
  in FromList

  Need better error reporting about
    host names that cannot be resolved
    invalid IP addresses
}
procedure TMainForm.GetDevicesFromList(hosts: TStrings);
const
  URLB = 'http://%s/cm?cmnd=';
var
  myHostname, myTopic, myIps: string;
  code: integer;
  url: string;
  r: integer;

  i, j: integer;
  ip: TIPv4;
  resolver: THostResolver;

  function FindValue(const key: string; var value: string): boolean;
  var
    p: integer;
  begin
    result := true;
    p := pos('"' + key + '":"', value);
    if p > 0 then begin
      delete(value, 1, p + length(key)+3);
      p := pos('"', value);
      if p > 0 then begin
        delete(value, p, maxint);
        exit;
      end;
    end;
    value := '';
    result := false;
  end;

  function CheckIP(aIP: TIPv4): integer;
  begin
    result := 0;
    myIps := aip.ToString;
    url := Format(URLB, [myIps]);
    myHostname := '';
    myTopic := '';
    label11.Caption := 'Checking ' + myIps;
    label11.Repaint;
    delay(2);
    if not HttpRequest(url + 'hostname', code, myHostname, ScanAttemptsEdit.value, ScanTimeOutEdit.value*1000) then
      result := -1;
    if not FindValue('Hostname', myHostname) then
      result := -1;
    if not HttpRequest(url + 'topic', code, myTopic, ScanAttemptsEdit.value, ScanTimeOutEdit.value*1000) then
      result := -2;
    if not FindValue('Topic', myTopic) then
      result := -2;
    if result = 0 then with DeviceGrid do begin
        r := RowCount;
        RowCount := RowCount+1;
        cells[0, r] := '1';
        cells[1, r] := myIps;
        cells[2, r] := myTopic;
        cells[3, r] := myHostname;
        MainForm.newdev := true;
    end;
    notebook1.invalidate;
    Delay(2);
  end;

begin
  Next2Button.Enabled := false;
  Screen.Cursor := crHourGlass;
  resolver := THostResolver.Create(self);
  ip.value := 0;
  try
    for i := 0 to hosts.count-1 do begin
      if resolver.NameLookup(trim(hosts[i])) then
        ip.value := longword(resolver.Addresses[0])
      else if not ip.TryFromString(trim(hosts[i])) then
        ip.value := 0;
      if ip.value <> 0 then
        CheckIp(ip);
    end;
  finally
    Screen.Cursor := crDefault;
    next2Button.Enabled := true;
    next2Button.SetFocus;
  end;
end;

function TMainForm.GetExtension: string;
begin
  result := ExtensionEdit.Text;
  if (result = '.') then
    result := ''
  else if (result <> '') and (result[1] <> '.') then
    insert('.', result, 1);
end;

procedure TMainForm.GridClick(Sender: TObject);
begin
  with Sender as TStringGrid do begin
    if (currentcol = 0) and (row > 0) then begin
      if Cells[0, Row] = '0' then
         Cells[0, Row] := '1'
      else
         Cells[0, Row] := '0';
    end;
  end;
end;

procedure TMainForm.GridHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  if not resizinggrid then
    GridResize(sender);
end;

procedure TMainForm.GridResize(Sender: TObject);
var
  wd, j: integer;
begin
  with Sender as TStringGrid do begin
    wd := clientwidth - colcount;
    for j := 0 to Colcount-2 do
      dec(wd, colwidths[j]);
    colwidths[colcount-1] := wd;
  end;
end;

procedure TMainForm.IncludeCheckBoxChange(Sender: TObject);
begin
  FixupIpRangeHint;
end;

procedure TMainForm.IncludeListBoxDblClick(Sender: TObject);
begin
  EditIPList(IncludeListBox.Items)
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
var
  delim: char;
begin
  if MenuItem3.checked then
    delim := #9
  else
    delim := ',';
  CopyResGridToClipbrd(delim);
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
  PopupMenu1.popup(popx, popy);
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
begin
  PopupMenu1.popup(popx, popy);
end;

procedure TMainForm.Next1Button1Click(Sender: TObject);
begin
  if radioButton8.checked then begin
    fip.FromString(FirstIpEdit.Text);
    lip.FromString(LastIPEdit.Text);
  end
  else begin
    fip.FromString(SubnetEdit.Text);
    SubnetRange(fip, SubnetBitsEdit.value, fip, lip);
    fip.inc;
    lip.dec;
  end;
  Notebook1.PageIndex := 2;
  CheckBox1.SetFocus;
  Notebook1.invalidate;
  Delay(2);
  GetDevicesWithHttpScan;
end;

procedure TMainForm.Next1Button2Click(Sender: TObject);
var
  hosts: TStringList;
begin
  hosts := TStringList.create;
  try
    if IncludeHostFileCheckBox.Checked then
      hosts.LoadFromFile(HostFileNameEdit.FileName);
    if IncludeHostListCheckBox.Checked then
      hosts.AddText(HostListMemo.Lines.Text);
    if hosts.count < 1 then begin
      OptionsButtonClick(nil);
      exit;
    end;
    Notebook1.PageIndex := 2;
    CheckBox1.SetFocus;
    Notebook1.invalidate;
    Delay(2);
      GetDevicesFromList(hosts);
  finally
    hosts.free;
  end;
end;

procedure TMainForm.NextButton1Click(Sender: TObject);
begin
  if RadioButton5.Checked then
     Notebook1.PageIndex := 1
  else if RadioButton6.Checked then
     Notebook1.PageIndex := 6
  else
     Notebook1.PageIndex := 7;
end;

procedure TMainForm.PageHttpScanBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin

end;

procedure TMainForm.Next1ButtonClick(Sender: TObject);
begin
  screen.cursor := crHourglass;
  try
    TcpPortAvailable(HostEdit.Text, PortEdit.value, MqttTimeoutEdit.value*1000);
    Notebook1.PageIndex := 2;
    CheckBox1.SetFocus;
    Notebook1.invalidate;
    Delay(2);
    GetDevicesFromMqttBroker;
  finally
    screen.cursor := crDefault;
  end;
end;

procedure TMainForm.Next2ButtonClick(Sender: TObject);
begin
  DateEdit.Date := date;
  Notebook1.PageIndex := 3;
  DirectoryEdit.SetFocus;
end;

procedure TMainForm.OptionsButtonClick(Sender: TObject);

const
  pwddots = #$E2#$97#$8F#$E2#$97#$8F#$E2#$97#$8F#$E2#$97#$8F#$E2#$97#$8F
            + #$E2#$97#$8F#$E2#$97#$8F#$E2#$97#$8F#$E2#$97#$8F;

  function ListIPs(list: TStrings): string;
  var
    i: integer;
  begin
    result := '';
    if list.count > 0 then begin
      result := list[0];
      if list.count > 1 then begin
        result := result + ', ' + list[1];
        if list.count > 2 then
          result := result + '...';
      end;
    end;
  end;

  procedure setCb(row: integer; aVal: boolean);  overload;
  var
    v: string;
  begin
    if aVal then
      v := '0'
    else
      v := '1';
    OptionsGrid.cells[0, row] := v;
  end;

  procedure setCb(row: integer; const current: string);
  begin
    setCb(row, OptionsGrid.cells[2, row] = current)
  end;


begin
  with OptionsGrid do begin
    cells[1, 1] := 'Discovery method';
    if RadioButton5.checked then
      cells[2, 1] := 'MQTT query'
    else
      cells[2, 1] := 'HTTP scan';
    setCb(1, RadioButton5.Checked and (params.DiscoveryMethod = 0));

    cells[1, 2] := 'MQTT host';
    cells[2, 2] := HostEdit.Text;
    setCb(2, params.host);

    cells[1, 3] := 'MQTT port';
    cells[2, 3] := PortEdit.Text;
    setCb(3, PortEdit.value = params.port);

    cells[1, 4] := 'MQTT user';
    cells[2, 4] := UserEdit.Text;
    setCb(4, params.user);

    cells[1, 5] := 'MQTT password';
    if length(PasswordEdit.Text) < 1 then
      cells[2, 5] := ''
    else
      cells[2, 5] := pwddots;
    setCb(5, PasswordEdit.Text = params.password);

    cells[1, 6] := 'MQTT topic';
    cells[2, 6] := TopicEdit.Text;
    setCb(6, params.topic);

    cells[1, 7] := 'MQTT timeout';
    cells[2, 7] := MqttTimeoutEdit.Text;
    setCb(7, MqttTimeoutEdit.value = params.MqttTimeout);

    cells[1, 8] := 'Subnet';
    cells[2, 8] := SubnetEdit.Text;
    setCb(8, params.subnet);

    cells[1, 9] := 'Subnet bits';
    cells[2, 9] := SubnetBitsEdit.Text;
    setCb(9, SubnetBitsEdit.value = params.subnetbits);

    cells[1, 10] := 'Scan';
    if FullRangeRadioButton.checked then
      cells[2, 10] := 'All IP in subnet'
    else
      cells[2, 10] := 'IP in range';
    setCb(10, FullRangeRadioButton.checked = params.ScanAllIP);

    cells[1, 11] := 'First IP';
    cells[2, 11] := FirstIPEdit.Text;
    setCb(11, params.FirstIP);

    cells[1, 12] := 'Last IP';
    cells[2, 12] := LastIPEdit.Text;
    setCb(12, params.LastIP);

    cells[1, 13] := 'Exclude IP addresses';
    cells[2, 13] := ListIPs(ExcludeListBox.Items);
    setCb(13, ExcludeListBox.Items.Equals(params.ExcludeIPs));

    cells[1, 14] := 'Include IP addresses';
    cells[2, 14] := ListIPs(IncludeListBox.Items);
    setCb(14, IncludeListBox.Items.Equals(params.IncludeIPs));

    cells[1, 15] := 'Scan connect attempts';
    cells[2, 15] := ScanAttemptsEdit.Text;
    setCb(15, ScanAttemptsEdit.value = params.ScanAttempts);

    cells[1, 16] := 'Scan connect timeout';
    cells[2, 16] := ScanTimeoutEdit.Text;
    setCb(16, ScanTimeOutEdit.value = params.ScanTimeout);

    cells[1, 17] := 'Include hosts in file';
    if IncludeHostFileCheckBox.checked then
      cells[2, 17] := 'yes'
    else
      cells[2, 17] := 'no';
    setCb(17, IncludeHostFileCheckBox.checked = params.IncludeHostFile);

    cells[1, 18] := 'Host file name';
    cells[2, 18] := HostFilenameEdit.Filename;
    setCb(18, params.HostFilename);

    cells[1, 19] := 'Include hosts in list';
    if IncludeHostListCheckBox.checked then
      cells[2, 19] := 'yes'
    else
      cells[2, 19] := 'no';
    setCb(19, IncludeHostListCheckBox.checked = params.IncludeHostList);

    cells[1, 20] := 'Hosts list';
    cells[2, 20] := ListIPs(HostListMemo.Lines);
    setCb(20, HostListMemo.Lines.Equals(params.HostList));

    cells[1, 21] := 'Backup directory';
    cells[2, 21] := DirectoryEdit.Directory;
    setCb(21, params.directory);

    cells[1, 22] := 'Backup extension';
    cells[2, 22] := ExtensionEdit.Text;
    setCb(22, params.extension);

    cells[1, 23] := 'Date format';
    cells[2, 23] := DateformatEdit.Text;
    setCb(23, DateformatEdit.ItemIndex = params.dateformat);

    cells[1, 24] := 'Filename format';
    if radioButton1.Checked then
      cells[2, 24] := ChangeFileExt(radiobutton1.caption, '')
    else
      cells[2, 24] := ChangeFileExt(radiobutton2.caption, '');
    setCb(24, (radioButton1.checked and (params.filenameformat = 0))
           or (radioButton2.checked and (params.filenameformat = 1)));

    cells[1, 25] := 'Device name';
    if radioButton3.Checked then
      cells[2, 25] := 'Topic'
    else
      cells[2, 25] := 'Hostname';
    setCb(25, (radioButton3.checked and (params.devicename = 0))
           or (radioButton4.checked and (params.devicename = 1)));

    cells[1, 26] := 'Download attempts';
    cells[2, 26] := DownloadAttemptsEdit.Text;
    setCb(26, DownloadAttemptsEdit.value = params.DownloadAttempts);

    cells[1, 27] := 'Download timeout';
    cells[2, 27] := DownloadTimeoutEdit.Text;
    setCb(27, DownloadTimeoutEdit.value = params.DownloadTimeout);
  end;
  Notebook1.PageIndex := 5;
  AllOptionsCheckBox.SetFocus;
end;

procedure TMainForm.PageFoundDevicesBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin
  updategrid := DeviceGrid;
end;

procedure TMainForm.PageBackupResultsBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin
  updategrid := ResGrid;
end;

procedure TMainForm.PageOptionsBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin
  updategrid := OptionsGrid;
end;

procedure TMainForm.PasswordEditButtonClick(Sender: TObject);
begin
  with PasswordEdit do begin
     if passwordchar = #0 then begin  // hide password
       passwordchar := '*';
       ImageIndex := 0;
     end
     else begin // show password
       passwordchar := #0;
       ImageIndex := 1;
     end;
   end;
end;

procedure TMainForm.PopupMenu1Close(Sender: TObject);
begin
  PopupMenu1.tag := 0;
end;

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  PopupMenu1.tag := 1;
end;

procedure TMainForm.QuitButtonClick(Sender: TObject);
begin
  close;
end;

procedure TMainForm.ResetButtonClick(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to OptionsGrid.Rowcount-1 do begin
    if OptionsGrid.cells[0, i] = '0' then
       continue;
    case i of
       1: params.DiscoveryMethod := DEFAULT_DISCOVERY_METHOD;
       2: params.Host := DEFAULT_HOST;
       3: params.Port := DEFAULT_PORT;
       4: params.User := DEFAULT_USER;
       5: params.Password := DEFAULT_PASSWORD;
       6: params.Topic := DEFAULT_TOPIC;
       7: params.MqttTimeout := DEFAULT_MQTT_TIMEOUT;
       8: params.Subnet := DEFAULT_SUBNET;
       9: params.SubnetBits := DEFAULT_SUBNET_BITS;
      10: params.ScanAllIP := DEFAULT_SCAN_ALL_IP;
      11: params.FirstiP := DEFAULT_FIRST_IP;
      12: params.LastIP := DEFAULT_LAST_IP;
      13: params.ExcludeIPsAction := olaErase;
      14: params.IncludeIPsAction := olaErase;
      15: params.ScanAttempts := DEFAULT_SCAN_ATTEMPTS;
      16: params.ScanTimeout := DEFAULT_SCAN_TIMEOUT;
      17: params.IncludeHostFile := DEFAULT_INCLUDE_HOST_FILE;
      18: params.HostFilename := DEFAULT_HOST_FILE_NAME;
      19: params.IncludeHostList := DEFAULT_INCLUDE_HOST_LIST;
      20: params.HostListAction := olaErase;
      21: params.Directory := DEFAULT_BACK_DIRECTORY;
      22: params.Extension := DEFAULT_EXTENSION;
      23: params.DateFormat := DEFAUT_DATE_FORMAT;
      24: params.FilenameFormat := DEFAULT_FILENAME_FORMAT;
      25: params.DeviceName := DEFAULT_DEVICE_NAME;
      26: params.DownloadAttempts := DEFAULT_DOWNLOAD_ATTEMPTS;
      27: params.DownloadTimeout := DEFAULT_DOWNLOAD_TIMEOUT;
     end;
  end;
  close;
end;

procedure TMainForm.ResGridDblClick(Sender: TObject);
begin
  CopyResGridToClipbrd(#9);
end;

procedure TMainForm.ResGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (PopupMenu1.tag = 0) and (Button = mbRight) then begin
    popx := left + x;
    popy := top + y;
    PopupMenu1.PopUp(popx, popy);
  end;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to OptionsGrid.Rowcount-1 do begin
    if OptionsGrid.cells[0, i] = '0' then
       continue;
    case i of
       1: if RadioButton5.Checked then
            params.DiscoveryMethod := 0
          else
            params.DiscoveryMethod := 1;
       2: params.Host := OptionsGrid.cells[2, i];
       3: params.Port := PortEdit.value;
       4: params.User := OptionsGrid.cells[2, i];
       5: params.Password := PasswordEdit.Text;
       6: params.Topic := OptionsGrid.cells[2, i];
       7: params.MqttTimeout := MqttTimeoutEdit.value;
       8: params.Subnet := OptionsGrid.cells[2, i];
       9: params.SubnetBits := SubnetBitsEdit.value;
      10: params.ScanAllIP := FullRangeRadioButton.Checked;
      11: params.FirstIP := OptionsGrid.cells[2, i];
      12: params.LastIP := OptionsGrid.cells[2, i];
      13: begin params.ExcludeIPsAction := olaSave; params.ExcludeIPs := ExcludeListBox.Items; end;
      14: begin params.IncludeIPsAction := olaSave; params.IncludeIPs := IncludeListBox.Items; end;
      15: params.ScanAttempts := ScanAttemptsEdit.value;
      16: params.ScanTimeout := ScanTimeoutEdit.value;
      17: params.IncludeHostFile := IncludeHostFileCheckbox.Checked;
      18: params.HostFilename := OptionsGrid.cells[2, i];
      19: params.IncludeHostList := IncludeHostListCheckbox.Checked;
      20: begin params.HostListAction := olaSave; params.HostList := HostListMemo.Lines; end;
      21: params.directory := OptionsGrid.cells[2, i];
      22: params.extension := OptionsGrid.cells[2, i];
      23: params.dateformat := DateFormatEdit.ItemIndex;
      24: if RadioButton1.Checked then
            params.filenameformat := 0
          else
            params.filenameformat := 1;
      25: if RadioButton3.Checked then
            params.devicename := 0
          else
            params.devicename := 1;
      26: params.DownloadAttempts := DownloadAttemptsEdit.value;
      27: params.DownloadTimeout := DownloadTimeoutEdit.value;
     end;
  end;
  close;
end;

procedure TMainForm.ScanAttemptsEditChange(Sender: TObject);
begin
  CalcMaxRequestTime;
  FixupAllIpHint;
  FixupIpRangeHint;
end;

procedure TMainForm.SourceLabelClick(Sender: TObject);
begin
  OpenUrl('https://github.com/sigmdel/tasmotasbacker');  // report error ?
end;


procedure TMainForm.SubnetBitsEditChange(Sender: TObject);
begin
  FixupAllIpHint;
end;

procedure TMainForm.SubnetEditDone(Sender: TObject);
var
  ip, lfip, llip, mask: TIPv4;
begin
  if TryStrToIPv4(SubnetEdit.text, ip) then begin
    mask := SubnetMask(SubnetBitsEdit.value);
    if TryStrToIPv4(FirstIpEdit.text, lfip) then
      lfip := (lfip and not mask) or (ip and mask)
    else
      lfip := (ip and mask).succ;
    if TryStrToIPv4(LastIpEdit.text, llip) then
      llip := (llip and not mask) or (ip and mask)
    else
      llip := ((ip and mask) or (not mask)).pred;
    FirstIPEdit.text := lfip.ToString;
    LastIPEdit.Text := llip.ToString;
  end
end;

procedure TMainForm.UpdateCheckCount;
var
  i, checkcount: integer;
begin
  checkcount := 0;
  for i := 1 to DeviceGrid.RowCount-1 do
    if DeviceGrid.cells[0, i] = '1' then inc(checkcount);
  label11.Caption := Format('%d devices checked / %d total', [checkcount, DeviceGrid.RowCount-1]);
end;

{$IFDEF MSWINDOWS}{$IFDEF DO_LOG}
finalization
  if logopened then
    close(StdOut);
{$ENDIF}{$ENDIF}
end.

