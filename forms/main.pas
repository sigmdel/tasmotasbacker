unit main;

(*
  Uses mosquitto-p a Free Pascal conversions of the libmosquitto header file
  mosquitto.h and mqttclass.pas by Károly Balogh (chainq)
  @ https://github.com/chainq/mosquitto-p

  Requirements: The Eclipse mosquitto libraries must be installed on the
  system. See the README file.

  There is no need to install the mosquitto broker assuming access to
  an MQTT broker is available on the network.

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

{$mode objfpc}{$H+}

interface

// Set these defines to help in determining the timeout and retry
// parameters of the HTTP request function
// See log() function below
//
{ -- $DEFINE DEBUG_HTTP_REQUEST}
{ -- $DEFINE DEBUG_BACKUP}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, EditBtn, Grids, Menus;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Back2Button: TButton;
    ImageList1: TImageList;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    N2: TMenuItem;
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
    OptionsPage: TPage;
    OptionsGrid: TStringGrid;
    DownloadAttemptsEdit: TSpinEdit;
    DownloadTimeoutEdit: TSpinEdit;
    MqttTimeoutEdit: TSpinEdit;
    TopicEdit: TEdit;
    HostEdit: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    BackupResultsPage: TPage;
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
    FoundDevicesPage: TPage;
    BackupParamsPage: TPage;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    PortEdit: TSpinEdit;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Back2ButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
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
    procedure FoundDevicesPageBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure BackupResultsPageBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure OptionsPageBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure QuitButtonClick(Sender: TObject);
    procedure GridHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure GridResize(Sender: TObject);
    procedure ResGridDblClick(Sender: TObject);
    procedure ResGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SaveButtonClick(Sender: TObject);
  private
    popx: integer;
    popy: integer;
    currentCol: integer;
    resizinggrid: boolean;
    updategrid: TObject;
    newdev: boolean;
    function GetExtension: string;
    procedure GetDevicesFromMqttBroker;
    procedure UpdateCheckCount;
    procedure CopyResGridToClipbrd(delim: char);
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  clipbrd, fileinfo, options, ssockets, fphttpclient, mqttclass, mosquitto;

{$R *.lfm}


{$IFDEF DEBUG_HTTP_REQUEST}
  {$DEFINE DO_LOG}
{$ENDIF}

{$IFDEF DEBUG_BACKUP}
  {$DEFINE DO_LOG}
{$ENDIF}

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

procedure TMainForm.Back2ButtonClick(Sender: TObject);
begin
  DeviceGrid.RowCount := 1;
  Notebook1.PageIndex := 0;
end;

procedure TMainForm.BackButtonClick(Sender: TObject);
begin
  Notebook1.PageIndex := 1;
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
  Notebook1.PageIndex := 3;
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

procedure TMainForm.ExtensionEditChange(Sender: TObject);
var
  newext: string;
begin
  newext := GetExtension;
  RadioButton1.Caption := ChangeFileExt(RadioButton1.caption, newext);
  RadioButton2.Caption := ChangeFileExt(RadioButton2.caption, newext);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Version : TProgramVersion;
begin
  if GetProgramVersion(Version) then with Version do
    caption := Format('Tasmotas Backer [%d.%d.%d]', [Major,Minor,Revision])
  else
     caption := 'Tasmotas Backer';
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

procedure TMainForm.Next1ButtonClick(Sender: TObject);
begin
  screen.cursor := crHourglass;
  try
    TcpPortAvailable(HostEdit.Text, PortEdit.value, MqttTimeoutEdit.value*1000);
    Notebook1.PageIndex := 1;
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
  Notebook1.PageIndex := 2;
  DirectoryEdit.SetFocus;
end;

procedure TMainForm.OptionsButtonClick(Sender: TObject);

const
  pwddots = #$E2#$97#$8F#$E2#$97#$8F#$E2#$97#$8F#$E2#$97#$8F#$E2#$97#$8F
            + #$E2#$97#$8F#$E2#$97#$8F#$E2#$97#$8F#$E2#$97#$8F;

  procedure setCb(row: integer; const current: string);
  var
    v: string;
  begin
    if OptionsGrid.cells[2, row] = current then
      v := '0'
    else
      v := '1';
    OptionsGrid.cells[0, row] := v;
  end;

begin
  with OptionsGrid do begin
    cells[1, 1] := 'MQTT host';
    cells[2, 1] := HostEdit.Text;
    setCb(1, params.host);

    cells[1, 2] := 'MQTT port';
    cells[2, 2] := PortEdit.Text;
    if PortEdit.value = params.port then
      cells[0, 2] := '0'
    else
      cells[0, 2] := '1';

    cells[1, 3] := 'MQTT user';
    cells[2, 3] := UserEdit.Text;
    setCb(3, params.user);

    cells[1, 4] := 'MQTT password';
    if length(PasswordEdit.Text) < 1 then
      cells[2, 4] := ''
    else
      cells[2, 4] := pwddots;
    if PasswordEdit.Text = params.password then
      cells[0, 4] := '0'
    else
      cells[0, 4] := '1';

    cells[1, 5] := 'MQTT topic';
    cells[2, 5] := TopicEdit.Text;
    setCb(5, params.topic);

    cells[1, 6] := 'MQTT timeout';
    cells[2, 6] := MqttTimeoutEdit.Text;
    if MqttTimeoutEdit.value = params.MqttTimeout then
      cells[0, 6] := '0'
    else
      cells[0, 6] := '1';

    cells[1, 7] := 'Backup directory';
    cells[2, 7] := DirectoryEdit.Directory;
    setCb(7, params.directory);

    cells[1, 8] := 'Backup extension';
    cells[2, 8] := ExtensionEdit.Text;
    setCb(8, params.extension);

    cells[1, 9] := 'Date format';
    cells[2, 9] := DateformatEdit.Text;
    if DateformatEdit.ItemIndex = params.dateformat then
      cells[0, 9] := '0'
    else
      cells[0, 9] := '1';

    cells[1, 10] := 'Filename format';
    if radioButton1.Checked then
      cells[2, 10] := ChangeFileExt(radiobutton1.caption, '')
    else
      cells[2, 10] := ChangeFileExt(radiobutton2.caption, '');
    if (radioButton1.checked and (params.filenameformat = 0))
    or (radioButton2.checked and (params.filenameformat = 1)) then
      cells[0, 10] := '0'
    else
      cells[0, 10] := '1';

    cells[1, 11] := 'Device name';
    if radioButton3.Checked then
      cells[2, 11] := 'Topic'
    else
      cells[2, 11] := 'Hostname';
    if (radioButton3.checked and (params.devicename = 0))
    or (radioButton4.checked and (params.devicename = 1)) then
      cells[0, 11] := '0'
    else
      cells[0, 11] := '1';

    cells[1, 12] := 'Download attempts';
    cells[2, 12] := DownloadAttemptsEdit.Text;
    if DownloadAttemptsEdit.value = params.DownloadAttempts then
      cells[0, 12] := '0'
    else
      cells[0, 12] := '1';

    cells[1, 13] := 'Download timeout';
    cells[2, 13] := DownloadTimeoutEdit.Text;
    if DownloadTimeoutEdit.value = params.DownloadTimeout then
      cells[0, 13] := '0'
    else
      cells[0, 13] := '1';
  end;
  Notebook1.PageIndex := 4;
  AllOptionsCheckBox.SetFocus;
end;

procedure TMainForm.FoundDevicesPageBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin
  updategrid := DeviceGrid;
end;

procedure TMainForm.BackupResultsPageBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin
  updategrid := ResGrid;
end;

procedure TMainForm.OptionsPageBeforeShow(ASender: TObject; ANewPage: TPage;
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
       1: params.Host := DEFAULT_HOST;
       2: params.Port := DEFAULT_PORT;
       3: params.User := DEFAULT_USER;
       4: params.Password := DEFAULT_PASSWORD;
       5: params.Topic := DEFAULT_TOPIC;
       6: params.MqttTimeout := DEFAULT_MQTT_TIMEOUT;
       7: params.Directory := DEFAULT_BACK_DIRECTORY;
       8: params.Extension := DEFAULT_EXTENSION;
       9: params.DateFormat := DEFAUT_DATE_FORMAT;
      10: params.FilenameFormat := DEFAULT_FILENAME_FORMAT;
      11: params.DeviceName := DEFAULT_DEVICE_NAME;
      12: params.DownloadAttempts := DEFAULT_DOWNLOAD_ATTEMPTS;
      13: params.DownloadTimeout := DEFAULT_DOWNLOAD_TIMEOUT;
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
       1: params.Host := OptionsGrid.cells[2, i];
       2: params.Port := PortEdit.value;
       3: params.User := OptionsGrid.cells[2, i];
       4: params.Password := PasswordEdit.Text;
       5: params.Topic := OptionsGrid.cells[2, i];
       6: params.MqttTimeout := MqttTimeoutEdit.value;
       7: params.directory := OptionsGrid.cells[2, i];
       8: params.extension := OptionsGrid.cells[2, i];
       9: params.dateformat := DateFormatEdit.ItemIndex;
      10: if RadioButton1.Checked then
            params.filenameformat := 0
          else
            params.filenameformat := 1;
      11: if RadioButton3.Checked then
            params.devicename := 0
          else
            params.devicename := 1;
      12: params.DownloadAttempts := DownloadAttemptsEdit.value;
      13: params.DownloadTimeout := DownloadTimeoutEdit.value;
     end;
  end;
  close;
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

