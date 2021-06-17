unit html_get;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    LabeledEdit1: TLabeledEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    html: String;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses fphttpclient;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  httpclient: TFPHTTPClient;
  url: String;
  i, j: integer;
  s: string;
  t: string;
begin
  memo1.Text := '';
  html := '';
  httpclient := TFPHttpClient.Create(Nil);
  url := 'http://' +  LabeledEdit1.Caption + '/dl';
  memo1.Lines.add('HTTP request: ' + url);
  try
    html := httpclient.Get(url);
  finally
    httpclient.Free;
  end;
  memo1.Lines.add('File size: ' + inttostr(length(html)));
  i := 1;
  while i <= length(html) do begin
    s := '';
    t := '';
    for j := 0 to 15 do begin
       if (i <= length(html)) then begin
         if (j = 0) then
           s := format('%.4X  ', [i-1]);
         s := s + format('%.2X ', [ord(html[i])]);
         if (32 <= ord(html[i])) and (ord(html[i]) < 127) then
           t := t + ' ' + html[i]
         else
           t := t + ' .';
         inc(i);
       end;
    end;
    memo1.lines.add(s + ' ' + t);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  memo1.text := '';
  html := '';
end;

//Save a string as a file
procedure SaveString(const InString, OutFilePath: string);
var
  F: TextFile;
begin
  AssignFile(F, OutFilePath);
  try
    ReWrite(F);
    Write(F, InString);
  finally
    CloseFile(F);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SaveString(html, Edit1.Text);
  memo1.text := 'Configuration saved to ' + Edit1.Text;
  html := '';
end;


end.

