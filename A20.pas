(*
说明：全志A20的地址映射封装类。
单例，请勿在程序中使用。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.10.10
*)

unit A20;

{$mode objfpc}{$H+}

interface

uses Unix, BaseUnix, SysUtils;

type
  TGOURP1_REG = ^LongWord;
  TGOURP2_REG = array [0..1] of ^LongWord;
  TGOURP4_REG = array [0..3] of ^LongWord;
  TGOURP5_REG = array [0..4] of ^LongWord;
  TGOURP6_REG = array [0..5] of ^LongWord;
  TGOURP16_REG = array [0..15] of ^LongWord;

type
  TA20 = class
  private
    class var FInstance: TA20;
    class function GetInstance: TA20; static;
  public
    class procedure Release;
    class property Instance: TA20 read GetInstance;

  private
    FhMEM: Integer;
    constructor Create;
    destructor Destroy; override;
  public
    function GetMMap(BaseAddr: LongWord): PLongWord;
    function BaseOffset(BaseAddr: LongWord): LongWord;
    procedure FreeMMap(MMapAddr: PLongWord);
  end;

implementation

const
  PAGE_SIZE = 4096;
  BLOCK_SIZE = 4096;

class function TA20.GetInstance: TA20;
begin
  if FInstance = nil then FInstance:= TA20.Create;
  Result:= FInstance;
end;

class procedure TA20.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TA20.Create;
begin
  inherited Create;

  FhMEM:= fpopen('/dev/mem', O_RdWr);
  if (FhMEM < 0) then Exit;
end;

destructor TA20.Destroy;
begin
  fpclose(FhMEM);

  inherited Destroy;
end;

function TA20.GetMMap(BaseAddr: LongWord): PLongWord;
var MEM: LongWord;
begin
  MEM:= (BaseAddr and $FFFFF000) div PAGE_SIZE;
  Result:= fpMmap(nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED, FhMem, MEM);
end;

function TA20.BaseOffset(BaseAddr: LongWord): LongWord;
begin
  Result:= (BaseAddr and $00000FFF);
end;

procedure TA20.FreeMMap(MMapAddr: PLongWord);
begin
  if (MMapAddr <> nil) then fpMUnmap(MMapAddr, PAGE_SIZE);
end;

finalization
  TA20.Instance.Release;

end.

