unit main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, http;

type

  { TForm1 }

  TForm1 = class(TForm)
    button1: TBitBtn;
    button2: TBitBtn;
    Button3: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  TD: TTCPHttpDaemon;

implementation

uses log;

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  TD := TTCPHttpDaemon.create;

  button1.enabled := false;
  button2.enabled := true;
  label3.visible := true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TD.Terminate;

  button1.enabled := true;
  button2.enabled := false;
  label3.visible := false;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  application.createform(tfrmLog, frmLog);
  frmLog.show;
end;

end.
