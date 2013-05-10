unit log;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmLog }

  TfrmLog = class(TForm)
    memLog: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmLog: TfrmLog;

implementation

{$R *.lfm}

{ TfrmLog }

procedure TfrmLog.FormDestroy(Sender: TObject);
begin

end;

procedure TfrmLog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  frmLog.free;
  frmlog := nil;

  CloseAction := CAFree;
end;

end.

