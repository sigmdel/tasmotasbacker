unit iplistedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ComCtrls,
  StdCtrls, ExtCtrls;

type

  { TIpListEditorForm }

  TIpListEditorForm = class(TForm)
    AcceptButton: TButton;
    actAdd: TAction;
    actClear: TAction;
    actDel: TAction;
    actEdit: TAction;
    ActionList1: TActionList;
    CancelButton: TButton;
    ImageList1: TImageList;
    IpListBox: TListBox;
    Panel1: TPanel;
    tbAdd: TToolButton;
    tbClear: TToolButton;
    tbDelete: TToolButton;
    tbEdit: TToolButton;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    procedure actAddExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IpListBoxClick(Sender: TObject);
    procedure IpListBoxDblClick(Sender: TObject);
  private
    lastIp: string;
    procedure AddIp(const ips: string);
    procedure ListChange;
  public

  end;

var
  IpListEditorForm: TIpListEditorForm;

function EditIPList(ipl: TStrings): boolean;

implementation

{$R *.lfm}

uses
  ipv4, ipedit;

{ TIpListEditorForm }

procedure TIpListEditorForm.ListChange;
begin
  actDel.Enabled := (IpListBox.ItemIndex >= 0) and (IpListBox.ItemIndex < IpListBox.Count);
  actEdit.Enabled := actDel.Enabled;
  actClear.Enabled := IpListBox.Count > 0;
end;

procedure TIpListEditorForm.actClearExecute(Sender: TObject);
begin
  if IpListBox.Count > 0 then begin
    IpListBox.Items.Clear;
    ListChange;
  end;
end;

procedure TIpListEditorForm.actDelExecute(Sender: TObject);
var
  OldIndex : integer;
begin
  with IpListBox do begin
    OldIndex := itemindex;
    if (oldIndex >= 0)
      and (oldIndex < Count)
      //and ConfirmAction(Format(cbsDeleteQuery, [FBroker.SubTopics[pred(row)].Topic])) then begin
      then begin
        Items.Delete(oldIndex);
        {
        if OldIndex >= RowCount then
          Row := RowCount-1
        else
          Row := OldIndex;
        }
        ListChange;
    end;
  end;
end;


procedure TIpListEditorForm.AddIp(const ips: string);
var
  i: integer;
  ip: TIPv4;

  procedure insertHere(n: integer);
  begin
    IpListBox.Items.InsertObject(n, ips, TObject(intptr(ip.value)));
    IpListBox.ItemIndex := n;
  end;

begin
  ip.FromString(ips);
  for i := 0 to IpListBox.Count-1 do
    if ip.value < longword(ptrint(IpListBox.Items.Objects[i])) then begin
       insertHere(i);
       exit;
    end
    else if ip.value = longword(ptrint(IpListBox.Items.Objects[i]))  then begin
      IpListBox.ItemIndex := i;
      exit;
    end;
  insertHere(IpListBox.Count);
end;

procedure TIpListEditorForm.actEditExecute(Sender: TObject);
begin
  with IpListBox do begin
    if (ItemIndex >= 0) and (ItemIndex < Count) then begin
      with TIpEditForm.Create(self) do try
        AddressEdit.Text := Items[ItemIndex];
        if (ShowModal = mrOk) then begin
          if AddressEdit.Text <> Items[ItemIndex] then begin
            Items.Delete(ItemIndex);
            AddIp(AddressEdit.Text);
          end;
          lastip := AddressEdit.Text;
        end;
      finally
        free;
        ListChange;
      end;
    end;
  end;
end;

procedure TIpListEditorForm.FormShow(Sender: TObject);
begin
  lastIp := '';
  ListChange;
end;

procedure TIpListEditorForm.IpListBoxClick(Sender: TObject);
begin
  ListChange;
end;

procedure TIpListEditorForm.IpListBoxDblClick(Sender: TObject);
begin
  actEditExecute(nil);
end;

procedure TIpListEditorForm.actAddExecute(Sender: TObject);
begin
  with TIpEditForm.Create(self) do try
    AddressEdit.Text := lastip;
    if (ShowModal = mrOk) then begin
      if (ShowModal = mrOk) then begin
        AddIp(AddressEdit.Text);
        lastip := AddressEdit.Text;
      end;
    end;
  finally
    ListChange;
    free;
  end;
end;

function EditIPList(ipl: TStrings): boolean;
var
  i: integer;
begin
  IpListEditorForm.IpListBox.Clear;
  for i :=  0 to ipl.count-1 do
    try
      IpListEditorForm.AddIp(ipl[i]);
    except
      // eat exception
    end;
  result := IpListEditorForm.ShowModal = mrOk;
  if result then
    ipl.assign(IpListEditorForm.IpListBox.Items);
end;

end.

