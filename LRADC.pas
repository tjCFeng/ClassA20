(*
说明：全志A20的LRADC底层操作封装类。分为两个部分：
1.TLRADCGROUP类，可对寄存器直接操作，单例；
2.TLRADC类，对具体的LRADC通道实现了一些功能的简化。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.10.10
*)

unit LRADC;

{$mode objfpc}{$H+}

interface

uses Unix, BaseUnix, SysUtils, A20;

type
  TChannel = (LRADC_0, LRADC_1);
  TLRADCINT = (ADCDATA, KEYDOWN, HOLD, ALRDYHOLD, KEYUP);
  TLRADCINTs = set of TLRADCINT;

  TLRADCGROUP = class
  private
    class var FInstance: TLRADCGROUP;
    class function GetInstance: TLRADCGROUP; static;
  public
    class procedure Release;
    class property Instance: TLRADCGROUP read GetInstance;

  private
    FLRADC_BASE: ^LongWord;
    FLRADC_CTRL: TGOURP1_REG;
    FLRADC_INTC: TGOURP1_REG;
    FLRADC_INTS: TGOURP1_REG;
    FLRADC_DATA: TGOURP2_REG;

    constructor Create;
    destructor Destroy; override;
  public
    procedure Start;
    procedure Stop;
    procedure ClearAllPending;
  public
    property LRADC_CTRL: TGOURP1_REG read FLRADC_CTRL;
    property LRADC_INTC: TGOURP1_REG read FLRADC_INTC;
    property LRADC_INTS: TGOURP1_REG read FLRADC_INTS;
    property LRADC_DATA: TGOURP2_REG read FLRADC_DATA;
  end;

  TLRADC = class
  private
    FChannel: TChannel;
    FINTs: TLRADCINTs;

    FLRADC_DATA: ^LongWord;
  private
    procedure SetINTs(INTs: TLRADCINTs);
    function GetDATA: Byte;
  public
    constructor Create(Channel: TChannel);

    function GetStatus(INT: TLRADCINT): Boolean;
    procedure ClearPendings(INTs: TLRADCINTs);
  public
    property Channel: TChannel read FChannel;
    property INTs: TLRADCINTs read FINTs write SetINTs default [];
    property Data: Byte read GetDATA;
  end;

implementation

const
  LRADC_BASE = $01C22800;

(*LRADC Group******************************************************************)
class function TLRADCGROUP.GetInstance: TLRADCGROUP;
begin
  if FInstance = nil then FInstance:= TLRADCGROUP.Create;
  Result:= FInstance;
end;

class procedure TLRADCGROUP.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TLRADCGROUP.Create;
var Base: LongWord;
begin
  inherited Create;

  FLRADC_BASE:= TA20.Instance.GetMMap(LRADC_BASE);
  Base:= LongWord(FLRADC_BASE) + TA20.Instance.BaseOffset(LRADC_BASE);

  FLRADC_CTRL:= Pointer(Base + $00);
  FLRADC_INTC:= Pointer(Base + $04);
  FLRADC_INTS:= Pointer(Base + $08);
  FLRADC_DATA[0]:= Pointer(Base + $0C);
  FLRADC_DATA[1]:= Pointer(Base + $10);
end;

destructor TLRADCGROUP.Destroy;
begin
  TA20.Instance.FreeMMap(FLRADC_BASE);

  inherited Destroy;
end;

procedure TLRADCGROUP.Start;
begin
  FLRADC_CTRL^:= FLRADC_CTRL^ or ($1 shl 0);
end;

procedure TLRADCGROUP.Stop;
begin
  FLRADC_CTRL^:= FLRADC_CTRL^ and  not ($1 shl 0);
end;

procedure TLRADCGROUP.ClearAllPending;
begin
  FLRADC_INTS^:= $00001F1F;
end;

(*LRADC Channel****************************************************************)
constructor TLRADC.Create(Channel: TChannel);
begin
  inherited Create;

  FChannel:= Channel;
  FLRADC_DATA:= TLRADCGROUP.Instance.LRADC_DATA[Ord(FChannel)];
end;

function TLRADC.GetDATA: Byte;
begin
  Result:= Byte(FLRADC_DATA^);
end;

function TLRADC.GetStatus(INT: TLRADCINT): Boolean;
var INTBIT: LongWord;
begin
  INTBIT:= Ord(FChannel) * 8 + Ord(INT);
  Result:= (TLRADCGROUP.Instance.LRADC_INTS^ and INTBIT) = INTBIT;
end;

procedure TLRADC.SetINTs(INTs: TLRADCINTs);
  procedure SetINT(IndexINT: Byte; ED: Boolean);
  begin
    with TLRADCGROUP.Instance do
    if ED then
      LRADC_INTC^:= LRADC_INTC^ or ($1 shl (Ord(FChannel) * 8) + IndexINT)
    else
      LRADC_INTC^:= LRADC_INTC^ and  not ($1 shl (Ord(FChannel) * 8) + IndexINT);
  end;
begin
  FINTs:= INTs;
  SetINT(Ord(ADCDATA), ADCDATA in INTs);
  SetINT(Ord(KEYDOWN), KEYDOWN in INTs);
  SetINT(Ord(HOLD), HOLD in INTs);
  SetINT(Ord(ALRDYHOLD), ALRDYHOLD in INTs);
  SetINT(Ord(KEYUP), KEYUP in INTs);
end;

procedure TLRADC.ClearPendings(INTs: TLRADCINTs);
  procedure ClearPending(IndexINT: Byte);
  begin
    with TLRADCGROUP.Instance do
    LRADC_INTS^:= LRADC_INTS^ or ($1 shl Ord(FChannel) * 8 + IndexINT);
  end;
begin
  if (ADCDATA in INTs) then ClearPending(Ord(DATA));
  if (KEYDOWN in INTs) then ClearPending(Ord(KEYDOWN));
  if (HOLD in INTs) then ClearPending(Ord(HOLD));
  if (ALRDYHOLD in INTs) then ClearPending(Ord(ALRDYHOLD));
  if (KEYUP in INTs) then ClearPending(Ord(KEYUP));
end;

finalization
  TLRADCGROUP.Instance.Release;

end.

