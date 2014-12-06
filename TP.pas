(*
说明：全志A20的TP底层操作封装类。分为两个部分：
1.TP类，可对寄存器直接操作，单例；
2.TTemperature类，对具体的PWM通道实现了一些功能的简化。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.12.06
*)

unit TP;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, A20;

type
  TTP = class
  private
    class var FInstance: TTP;
    class function GetInstance: TTP; static;
  public
    class procedure Release;
    class property Instance: TTP read GetInstance;

  private
    FTP_BASE: ^LongWord;
    FTP_CTRL: TGROUP4_REG;
    FTP_INT_FIFOC: TGROUP1_REG;
    FTP_INT_FIFOS: TGROUP1_REG;
    FTP_TPR: TGROUP1_REG;
    FTP_CDAT: TGROUP1_REG;
    FTEMP_DATA: TGROUP1_REG;
    FTP_DATA: TGROUP1_REG;
    FTP_IO_CONFIG: TGROUP1_REG;
    FTP_PORT_DATA: TGROUP1_REG;

    constructor Create;
    destructor Destroy; override;
  public
    property TP_CTRL: TGROUP4_REG read FTP_CTRL;
    property TP_INT_FIFOC: TGROUP1_REG read FTP_INT_FIFOC;
    property TP_INT_FIFOS: TGROUP1_REG read FTP_INT_FIFOS;
    property TP_TPR: TGROUP1_REG read FTP_TPR;
    property TP_CDAT: TGROUP1_REG read FTP_CDAT;
    property TEMP_DATA: TGROUP1_REG read FTEMP_DATA;
    property TP_DATA: TGROUP1_REG read FTP_DATA;
    property TP_IO_CONFIG: TGROUP1_REG read FTP_IO_CONFIG;
    property TP_PORT_DATA: TGROUP1_REG read FTP_PORT_DATA;
  end;

  TTemperature = class
  private
    class var FInstance: TTemperature;
    class function GetInstance: TTemperature; static;
  public
    class procedure Release;
    class property Instance: TTemperature read GetInstance;

  private
    FINTBit: Integer;
    constructor Create;
    destructor Destroy; override;
  public
    function Temperature: Double;
  end;

implementation

const
  TP_BASE = $01C25000;

(*TP***************************************************************************)
class function TTP.GetInstance: TTP;
begin
  if FInstance = nil then FInstance:= TTP.Create;
  Result:= FInstance;
end;

class procedure TTP.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TTP.Create;
var Base: LongWord; I: Byte;
begin
  inherited Create;

  FTP_BASE:= TA20.Instance.GetMMap(TP_BASE);
  Base:= LongWord(FTP_BASE) + TA20.Instance.BaseOffset(TP_BASE);

  for I:= 0 to 3 do FTP_CTRL[I]:= Pointer(Base + $04 * I);
  FTP_INT_FIFOC:= Pointer(Base + $10);
  FTP_INT_FIFOS:= Pointer(Base + $14);
  FTP_TPR:= Pointer(Base + $18);
  FTP_CDAT:= Pointer(Base + $1C);
  FTEMP_DATA:= Pointer(Base + $20);
  FTP_DATA:= Pointer(Base + $24);
  FTP_IO_CONFIG:= Pointer(Base + $28);
  FTP_PORT_DATA:= Pointer(Base + $2C);
end;

destructor TTP.Destroy;
begin
  TA20.Instance.FreeMMap(FTP_BASE);

  inherited Destroy;
end;

(*Temperature******************************************************************)
class function TTemperature.GetInstance: TTemperature;
begin
  if FInstance = nil then FInstance:= TTemperature.Create;
  Result:= FInstance;
end;

class procedure TTemperature.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TTemperature.Create;
begin
  inherited Create;

  with TTP.Instance do
  begin
    FTP_CTRL[0]^:= FTP_CTRL[0]^ and not (($3 shl 20) or ($FFFF shl 0));
    FTP_CTRL[0]^:= FTP_CTRL[0]^ or (($1 shl 22) or ($0 shl 20) or ($F shl 16) or ($3F shl 0));
    FTP_CTRL[1]^:= FTP_CTRL[1]^ or (($1 shl 7){ or ($1 shl 4)});

    FTP_TPR^:= FTP_TPR^ and ($FFFF shl 0);
    FTP_TPR^:= FTP_TPR^ or ($FFF shl 0);
  end;

  FINTBit:= ($1 shl 18);
end;

destructor TTemperature.Destroy;
begin
  inherited Destroy;
end;

function TTemperature.Temperature: Double;
begin
  with TTP.Instance do
  begin
    FTP_INT_FIFOS^:= FTP_INT_FIFOS^ or FINTBit;

    FTP_INT_FIFOC^:= FTP_INT_FIFOC^ or FINTBit;
    FTP_TPR^:= FTP_TPR^ or ($1 shl 16);

    while (FTP_INT_FIFOS^ and FINTBit) <> FINTBit do ;
    Result:= FTEMP_DATA^ * 85 / 4096;

    FTP_INT_FIFOC^:= FTP_INT_FIFOC^ and not FINTBit;
    FTP_TPR^:= FTP_TPR^ and not ($1 shl 16);
  end;
end;


finalization
  TTP.Instance.Release;

end.
