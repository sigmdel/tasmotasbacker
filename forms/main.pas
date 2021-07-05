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
{$DEFINE DEBUG_HTTP_REQUEST}
{$DEFINE DEBUG_BACKUP}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, EditBtn, Grids;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Back2Button: TButton;
    ImageList1: TImageList;
    Label18: TLabel;
    Label19: TLabel;
    PasswordEdit: TEditButton;
    ResetButton: TButton;
    CheckBox2: TCheckBox;
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
    Page5: TPage;
    OptionsGrid: TStringGrid;
    ConnectAttemptsEdit: TSpinEdit;
    TimeoutEdit: TSpinEdit;
    TopicEdit: TEdit;
    HostEdit: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Page4: TPage;
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
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    PortEdit: TSpinEdit;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Back2ButtonClick(Sender: TObject);
    procedure PasswordEditButtonClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
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
    procedure Page2BeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure Page4BeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure Page5BeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure QuitButtonClick(Sender: TObject);
    procedure GridHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure GridResize(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    currentCol: integer;
    resizinggrid: boolean;
    updategrid: TObject;
    newdev: boolean;
    function GetExtension: string;
    procedure GetDevicesFromMqttBroker;
    procedure UpdateCheckCount;
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  fileinfo, options, ssockets, fphttpclient, mqttclass, mosquitto;

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
     assign(StdOut,  'tasmotasbacker.log');
     rewrite(StdOut);
     {$ENDIF}
     writeln(StdOut, 'time':19, 'diff':8,'   ', 'message');
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


(*
TSocketErrorType = (
    seHostNotFound,          0
    seCreationFailed,        1
    seBindFailed,            2
    seListenFailed,          3
    seConnectFailed,         4
    seConnectTimeOut,        5
    seAcceptFailed,          6
    seAcceptWouldBlock,      7
    seIOTimeOut);            8


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
  log(Format('HttpRequest(url: %s, maxtries: %d, backoff: %d', [aURL, maxtries, backoff]));
  eclass := '';
  emsg := '';
  {$ENDIF}
  rawdata := '';
  code := -1;
  for i := 0 to maxtries-1 do begin
    sleep(i*backoff);
    with TFPHTTPClient.create(nil) do try
      {$IFDEF DEBUG_HTTP_REQUEST}
      gotE := false;
      eclass := '';
      emsg := '';
      {$ENDIF}
      try
        KeepConnection := False;
        ConnectTimeOut := backoff; //(i+1)*backoff;
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
    finally
      result := code = 200;
      Free;
    end;
  end;
  {$IFDEF DEBUG_HTTP_SCAN}
  log(Format('  http request returns after %d tries with code = %d, data = %s', [i+1, code, copy(rawdata, 1, 64)]));
  {$ENDIF}
end;
*)

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
  log(Format('HttpRequest(url: %s, maxtries: %d, backoff: %d', [aURL, maxtries, backoff]));
  {$ENDIF}
  rawdata := '';
  code := -1;
  with TFPHTTPClient.create(nil) do try
    KeepConnection := False;
    ConnectTimeOut := backoff; //(i+1)*backoff;
    for i := 0 to maxtries-1 do begin
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

procedure TMainForm.ResetButtonClick(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to OptionsGrid.Rowcount-1 do begin
    if OptionsGrid.cells[0, i] = '0' then
       continue;
    case i of
       1: params.host := DEFAULT_HOST;
       2: params.port := DEFAULT_PORT;
       3: params.user := DEFAULT_USER;
       4: params.password := DEFAULT_PASSWORD;
       5: params.topic := DEFAULT_TOPIC;
       6: params.directory := DEFAULT_BACK_DIRECTORY;
       7: params.extension := DEFAULT_EXTENSION;
       8: params.dateformat := DEFAUT_DATE_FORMAT;
       9: params.filenameformat := DEFAULT_FILENAME_FORMAT;
      10: params.devicename := DEFAULT_DEVICE_NAME;
      11: params.connectattempts := DEFAULT_ATTEMPTS;
      12: params.connecttimeout := DEFAULT_TIMEOUT;
     end;
  end;
  close;
end;

procedure TMainForm.CheckBox2Change(Sender: TObject);
var
  b: string;
  i: integer;
begin
  if CheckBox2.Checked then
    b := '1'
  else
    b := '0';
  for i := 0 to OptionsGrid.RowCount-1 do
    OptionsGrid.cells[0, i] := b;
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
  OptionsButton.setFocus;
  Notebook1.Invalidate;
  Notebook1.Update;
  Label19.caption := ExpandFilename(DirectoryEdit.Directory);
  application.ProcessMessages;
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

       if HttpRequest(url, code, html, ConnectAttemptsEdit.value,
         TimeoutEdit.value) and (html <> '') then begin
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
  HostEdit.Text := params.host;
  PortEdit.value := params.port;
  UserEdit.Text := params.user;
  PasswordEdit.Text := params.password;
  TopicEdit.Text := params.topic;
  DirectoryEdit.directory := params.directory;
  ExtensionEdit.text := params.extension;
  DateFormatEdit.ItemIndex := params.dateformat;
  DateEdit.DateOrder := TDateOrder(DateFormatEdit.ItemIndex + 1);
  if params.FilenameFormat = 0 then
    RadioButton1.Checked := true
  else
    RadioButton2.Checked := true;
  if params.DeviceName = 0 then
    RadioButton3.Checked := true
  else
    RadioButton4.Checked := true;
  TimeoutEdit.Value := params.connecttimeout;
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
    MqttClient := TThisMQTTConnection.Create('mqttClient', MqttConfig, MOSQ_LOG);
    try
      MqttClient.AutoReconnect := true;
      MqttClient.OnMessage := @MqttClient.MessageHandler;
      MqttClient.Connect;
      MqttClient.Subscribe('stat/+/STATUS5', 0);
      newdev := true;
      for i := 0 to sl.count-1 do begin
        MqttClient.Publish('cmnd/' + sl[i] + '/status', '5', 0, false);
        delay(5);
      end;
    except
      // what ??
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

procedure TMainForm.Page2BeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin
  updategrid := DeviceGrid;
end;

procedure TMainForm.Page4BeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin
  updategrid := ResGrid;
end;

procedure TMainForm.Page5BeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin
  updategrid := OptionsGrid;
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

procedure TMainForm.Next1ButtonClick(Sender: TObject);
begin
  Notebook1.PageIndex := 1;
  CheckBox1.SetFocus;
  Notebook1.invalidate;
  Delay(2);
  GetDevicesFromMqttBroker;
end;

procedure TMainForm.Next2ButtonClick(Sender: TObject);
begin
  DateEdit.Date := date;
  Notebook1.PageIndex := 2;
  DirectoryEdit.SetFocus;
end;

procedure TMainForm.OptionsButtonClick(Sender: TObject);

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
    cells[1, 1] := 'MQTT Host';
    cells[2, 1] := HostEdit.Text;
    setCb(1, params.host);

    cells[1, 2] := 'MQTT Port';
    cells[2, 2] := PortEdit.Text;
    if PortEdit.value = params.port then
      cells[0, 2] := '0'
    else
      cells[0, 2] := '1';

    cells[1, 3] := 'MQTT User';
    cells[2, 3] := UserEdit.Text;
    setCb(3, params.user);

    cells[1, 4] := 'MQTT Password';
    cells[2, 4] := PasswordEdit.Text;
    setCb(4, params.password);

    cells[1, 5] := 'MQTT Topic';
    cells[2, 5] := TopicEdit.Text;
    setCb(5, params.topic);

    cells[1, 6] := 'Backup Directory';
    cells[2, 6] := DirectoryEdit.Directory;
    setCb(6, params.directory);

    cells[1, 7] := 'Backup Extension';
    cells[2, 7] := ExtensionEdit.Text;
    setCb(7, params.extension);

    cells[1, 8] := 'Date Format';
    cells[2, 8] := DateformatEdit.Text;
    if DateformatEdit.ItemIndex = params.dateformat then
      cells[0, 8] := '0'
    else
      cells[0, 8] := '1';

    cells[1, 9] := 'Filename Format';
    if radioButton1.Checked then
      cells[2, 9] := ChangeFileExt(radiobutton1.caption, '')
    else
      cells[2, 9] := ChangeFileExt(radiobutton2.caption, '');
    if (radioButton1.checked and (params.filenameformat = 0))
    or (radioButton2.checked and (params.filenameformat = 1)) then
      cells[0, 9] := '0'
    else
      cells[0, 9] := '1';

    cells[1, 10] := 'Device Name';
    if radioButton3.Checked then
      cells[2, 10] := 'Topic'
    else
      cells[2, 10] := 'Hostname';
    if (radioButton3.checked and (params.devicename = 0))
    or (radioButton4.checked and (params.devicename = 1)) then
      cells[0, 10] := '0'
    else
      cells[0, 10] := '1';

    cells[1, 11] := 'Connect Attempts';
    cells[2, 11] := ConnectAttemptsEdit.Text;
    if ConnectAttemptsEdit.value = params.connectattempts then
      cells[0, 11] := '0'
    else
      cells[0, 11] := '1';

  cells[1, 12] := 'Connect Timepout';
  cells[2, 12] := TimeoutEdit.Text;
  if TimeOutEdit.value = params.connecttimeout then
    cells[0, 12] := '0'
  else
    cells[0, 12] := '1';

  end;
  Notebook1.PageIndex := 4;
  CheckBox2.SetFocus;
end;

procedure TMainForm.QuitButtonClick(Sender: TObject);
begin
  close;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to OptionsGrid.Rowcount-1 do begin
    if OptionsGrid.cells[0, i] = '0' then
       continue;
    case i of
       1: params.host := OptionsGrid.cells[2, i];
       2: params.port := PortEdit.value;
       3: params.user := OptionsGrid.cells[2, i];
       4: params.password := OptionsGrid.cells[2, i];
       5: params.topic := OptionsGrid.cells[2, i];
       6: params.directory := OptionsGrid.cells[2, i];
       7: params.extension := OptionsGrid.cells[2, i];
       8: params.dateformat := DateFormatEdit.ItemIndex;
       9: if RadioButton1.Checked then
            params.filenameformat := 0
          else
            params.filenameformat := 1;
      10: if RadioButton3.Checked then
            params.devicename := 0
          else
            params.devicename := 1;
      11: params.connectattempts := ConnectAttemptsEdit.value;
      12: params.connecttimeout := TimeoutEdit.value;
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

