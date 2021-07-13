unit ipedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TIpEditForm }

  TIpEditForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    AddressEdit: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

uses ipv4;

{ TIpEditForm }

procedure TIpEditForm.FormShow(Sender: TObject);
begin
  with constraints do begin
    MinHeight := height;
    MaxHeight := MinHeight;
    minWidth := width;
  end;
end;

procedure TIpEditForm.Button1Click(Sender: TObject);
begin
  if ValidIPv4(AddressEdit.text) then
    modalResult := mrOk
  else begin
    AddressEdit.SetFocus;
    modalResult := mrCancel;
  end;
end;

procedure TIpEditForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = $D then
    Button1Click(nil);
end;

end.

