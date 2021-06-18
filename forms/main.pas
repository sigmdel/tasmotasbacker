unit main;

(*
  Uses mosquitto-p a Free Pascal conversions of the libmosquitto header file
  mosquitto.h and mqttclass.pas by Károly Balogh (chainq)
  @ https://github.com/chainq/mosquitto-p

  Requirements: The Eclipse mosquitto libraries must be installed on the
  system. See the README file.

  There is no need to install the mosquitto broker assuming access to
  an MQTT broker is available on the network.

  See project source because the ctypes unit must be loaded at the start of
  the program. Furthermore, the cthreads unit must be loaded first in
  Linux systems. See the uses clause in the project source:

    uses
      {$IFDEF UNIX}{$IFDEF UseCThreads}
      cthreads,
      {$ENDIF}{$ENDIF}
      ctypes, // needed by mosquitto
      ...

  The  UseCThreads was defined by adding -dUseCThreads
  Personalised Options of Compiler Options in Project Options
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, CheckLst, EditBtn, Grids;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    DateFormatEdit: TComboBox;
    Next1Button: TButton;
    Next2Button: TButton;
    OptionsButton: TButton;
    BackupButton: TButton;
    QuitButton: TButton;
    BackButton: TButton;
    SaveButton: TButton;
    CheckBox1: TCheckBox;
    DeviceList: TCheckListBox;
    DateEdit: TDateEdit;
    DirectoryEdit: TDirectoryEdit;
    Label14: TLabel;
    Page5: TPage;
    OptionsGrid: TStringGrid;
    TopicEdit: TEdit;
    HostEdit: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Page4: TPage;
    ResGrid: TStringGrid;
    UserEdit: TEdit;
    PasswordEdit: TEdit;
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
    procedure DateFormatEditChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Next1ButtonClick(Sender: TObject);
    procedure Next2ButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure BackupButtonClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure DeviceListClickCheck(Sender: TObject);
    procedure ExtensionEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OptionsGridClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure GridHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure GridResize(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    newdev: boolean;
    function GetExtension: string;
    procedure GetDevices;
    procedure UpdateCheckCount;
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  fileinfo, options, fphttpclient, mqttclass, mosquitto;

{$R *.lfm}

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
    FThisName: string;
    procedure UpdateGUI;
    procedure MessageHandler(const payload: Pmosquitto_message);
  end;

var
  MqttClient: TThisMQTTConnection = nil;
  MqttConfig: TMQTTConfig;

{ TThisMQTTConnection }
procedure TThisMQTTConnection.UpdateGUI;
begin
   with MainForm.DeviceList do begin
     Items.Add('%s, %s', [FThisIP, FThisName]);
     Checked[Items.Count-1] := true;
     MainForm.newdev := true;
     {
     SelStart := Lines.Text.Length-1;
     SelLength := 1;
     }
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
      FThisName := copy(top, 1, p-1);
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
  end;
end;

procedure TMainForm.DateFormatEditChange(Sender: TObject);
begin
   DateEdit.DateOrder := TDateOrder(DateFormatEdit.ItemIndex + 1);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  MqttClient.free;
end;

procedure TMainForm.BackButtonClick(Sender: TObject);
begin
  Notebook1.PageIndex := 1;
end;

procedure TMainForm.BackupButtonClick(Sender: TObject);
var
  i, p: integer;
  ip, device, s1, s2: string;
  httpclient: TFPHTTPClient;
  url, html: String;
  aRow: integer;
  resstring: string;
begin
  Notebook1.PageIndex := 3;
  Notebook1.Invalidate;
  Notebook1.Update;
  application.ProcessMessages;

  for i := 0 to DeviceList.Count-1 do begin
     device := DeviceList.Items[i];
     p := pos(',', device);
     if p < 1 then
       continue;
     ip := copy(device, 1,p-1);
     delete(device, 1, p);  // remove ip
     device := trim(device);

     aRow := ResGrid.RowCount;
     ResGrid.RowCount := aRow + 1;
     ResGrid.Cells[0, aRow] := ip;
     ResGrid.Cells[1, aRow] := device;
     if not DeviceList.Checked[i] then
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
       ForceDirectories(DirectoryEdit.Directory);
       url := 'http://' + ip + '/dl';
       html := '';
       write('reading ', url);
       httpclient := TFPHttpClient.Create(Nil);
       try
         try
           html := httpclient.Get(url);
           if html <> '' then begin
             SaveStringToFile(html, device);
             writeln(' saved to ', device);
             resstring := 'saved';
           end
           else begin
             writeln('get error');
             resstring := 'http get error';
           end;
         except
           On E: Exception do begin
             writeln(Format('Get error %s', [E.message]));
             resstring := E.message;
           end;
         end;
       finally
         httpclient.free;
       end;
     end;
     ResGrid.Cells[2, aRow] := resstring;
     ResGrid.invalidate;
     ResGrid.Update;
     application.ProcessMessages;
  end;
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
var
  b: boolean;
  i: integer;
begin
  b := CheckBox1.Checked;
  for i := 0 to DeviceList.Count-1 do
    DeviceList.Checked[i] := b;
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
end;

procedure TMainForm.GetDevices;
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
  MqttClient := TThisMQTTConnection.Create('mqttClient', MqttConfig, MOSQ_LOG);
  try
    MqttClient.AutoReconnect := true;
    MqttClient.OnMessage := @MqttClient.MessageHandler;
    MqttClient.Connect;
    MqttClient.Subscribe('stat/+/STATUS5', 0);
    newdev := true;
    MqttClient.Publish('cmnd/' + TopicEdit.Text + '/status', '5', 0, false);
  finally

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

procedure TMainForm.GridHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  GridResize(sender);
end;

procedure TMainForm.GridResize(Sender: TObject);
begin
  with Sender as TStringGrid do
    colwidths[2] := clientwidth - colwidths[0] - colwidths[1] - 3;
end;

procedure TMainForm.OptionsGridClick(Sender: TObject);
begin
  if (OptionsGrid.col = 0) and (OptionsGrid.row > 0) then begin
    if OptionsGrid.Cells[0, OptionsGrid.Row] = '0' then
       OptionsGrid.Cells[0, OptionsGrid.Row] := '1'
    else
       OptionsGrid.Cells[0, OptionsGrid.Row] := '0';
  end;
end;

procedure TMainForm.Next1ButtonClick(Sender: TObject);
begin
  GetDevices;
  Notebook1.PageIndex := 1;
end;

procedure TMainForm.Next2ButtonClick(Sender: TObject);
begin
  DateEdit.Date := date;
  Notebook1.PageIndex := 2;
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
  end;
  Notebook1.PageIndex := 4;
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
     end;
  end;
  close;
end;

procedure TMainForm.UpdateCheckCount;
var
  i, checkcount: integer;
begin
  checkcount := 0;
  for i := 0 to DeviceList.Count-1 do
    if DeviceList.Checked[i] then inc(checkcount);
  label11.Caption := Format('%d devices checked / %d total', [checkcount, DeviceList.Count]);
end;

end.
