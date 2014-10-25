(*
说明：全志A20的RTC底层操作封装类。单例。
TRTC类，可对寄存器直接操作，并实现了一些功能的简化。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.10.15
*)

unit RTC;

{$mode objfpc}{$H+}

interface

uses Unix, BaseUnix, SysUtils, A20;

type
  TWeek = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

  TYMDHNSW = packed record //not TDateTime;
    Year: Byte;
    Month: Byte;
    Day: Byte;
    Hour: Byte;
    Minute: Byte;
    Second: Byte;
    Week: TWeek;
  end;

  TRTC = class
  private
    class var FInstance: TRTC;
    class function GetInstance: TRTC; static;
  public
    class procedure Release;
    class property Instance: TRTC read GetInstance;
	
  private
    FRTC_BASE: ^LongWord;

    procedure SetDT(Value: TYMDHNSW);
    function GetDT: TYMDHNSW;
  protected
    FLOSC_CTRL: ^LongWord;
    FRTC_DATE: ^LongWord;
    FRTC_TIME: ^LongWord;
    //Alarm Reg ...
  public
    constructor Create;
    destructor Destroy; override;
  public
    property DateTime: TYMDHNSW read GetDT write SetDT;
  end;
  
implementation

const
  RTC_BASE = $01C20D00;

class function TRTC.GetInstance: TRTC;
begin
  if FInstance = nil then FInstance:= TRTC.Create;
  Result:= FInstance;
end;

class procedure TRTC.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TRTC.Create;
var Base: LongWord;
begin
  inherited Create;

  FRTC_BASE:= TA20.Instance.GetMMap(RTC_BASE);
  Base:= LongWord(FRTC_BASE) + TA20.Instance.BaseOffset(RTC_BASE);

  FLOSC_CTRL:= Pointer(Base + $00);
  FRTC_DATE:= Pointer(Base + $04);
  FRTC_TIME:= Pointer(Base + $08);

  FLOSC_CTRL^:= FLOSC_CTRL^ and not ($1 shl 14);
  FLOSC_CTRL^:= FLOSC_CTRL^ or ($1 shl 15) or $16AA0000;
  FLOSC_CTRL^:= FLOSC_CTRL^ or $8; //($3 shl 2) or ($1 shl 0);
end;

destructor TRTC.Destroy;
begin
  TA20.Instance.FreeMMap(FRTC_BASE);

  inherited Destroy;
end;

procedure TRTC.SetDT(Value: TYMDHNSW);
var Leap: Byte; YMDFlag, HMSFlag: LongWord;
begin
  with Value do
  begin
    if not (Year in [0..100]) then raise Exception.Create('Year must in [0..100]');
    if not (Month in [1..12]) then raise Exception.Create('Month Error!');
    if not (Day in [1..31]) then raise Exception.Create('Day Error!');
    if not (Hour in [0..23]) then raise Exception.Create('Hour Error!');
    if not (Minute in [0..59]) then raise Exception.Create('Minute Error!');
    if not (Second in [0..59]) then raise Exception.Create('Second Error!');
  end;
  if IsLeapYear(Value.Year) then Leap:= 1 else Leap:= 0;
  
  YMDFlag:= ($1 shl 7);
  HMSFlag:= ($1 shl 8);
  
  with Value do
  begin
    FRTC_DATE^:= 0;
    while (FLOSC_CTRL^ and YMDFlag) <> 0 do Sleep(100);
    FRTC_DATE^:= FRTC_DATE^ or (Leap shl 24) or (Year shl 16) or (Month shl 8) or (Day shl 0);
    while (FLOSC_CTRL^ and YMDFlag) <> 0 do Sleep(100);
  
    FRTC_TIME^:= 0;
    while (FLOSC_CTRL^ and HMSFlag) <> 0 do Sleep(100);
    FRTC_TIME^:= FRTC_TIME^ or (Ord(Week) shl 29) or (Hour shl 16) or (Minute shl 8) or (Second shl 0);
  end;
end;

function TRTC.GetDT: TYMDHNSW;
var YMD, HMS: LongWord;
begin
  YMD:= FRTC_DATE^;
  HMS:= FRTC_TIME^;
  
  with Result do
  begin
    Year:= (YMD shr 16) and $FF;
    Month:= (YMD shr 8) and $F;
    Day:= YMD and $1F;
	
    Hour:= (HMS shr 16) and $1F;
    Minute:= (HMS shr 8) and $3F;
    Second:= HMS and $3F;

    case (HMS shr 29) and $7 of
    0: Week:= Monday;
    1: Week:= Tuesday;
    2: Week:= Wednesday;
    3: Week:= Thursday;
    4: Week:= Friday;
    5: Week:= Saturday;
    6: Week:= Sunday;
    end;
  end;
end;

finalization
  TRTC.Instance.Release;
  
end.
