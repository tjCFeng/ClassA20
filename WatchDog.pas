(*
说明：全志A20的WatchDog底层操作封装类。单例。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.12.06
*)

unit WatchDog;

{$mode objfpc}{$H+}

interface

uses SysUtils, A20, Clock;

type
  TInterval = (S0_5, S1, S2, S3, S4, S5, S6, S8, S10, S12, S14, S16);

  TWDOG = class
  private
    class var FInstance: TWDOG;
    class function GetInstance: TWDOG; static;
  public
    class procedure Release;
    class property Instance: TWDOG read GetInstance;

  private
    FWDOG_BASE: ^LongWord;

    constructor Create;
    destructor Destroy; override;

    procedure SetInterval(Value: TInterval);
    function GetInterval: TInterval;

    procedure SetForceRestart(Value: Boolean);
    function GetForceRestart: Boolean;
  protected
    FWDOG_CTRL: TGROUP1_REG;
    FWDOG_MODE: TGROUP1_REG;
  public
    procedure Start;
    procedure Stop;
    procedure Reset;
  public
    property Interval: TInterval read GetInterval write SetInterval;
    property ForceRestart: Boolean read GetForceRestart write SetForceRestart;
    property WDOG_CTRL: TGROUP1_REG read FWDOG_CTRL;
    property WDOG_MODE: TGROUP1_REG read FWDOG_MODE;
  end;

implementation

const
  WDOG_BASE = $01C20C90;

class function TWDOG.GetInstance: TWDOG;
begin
  if FInstance = nil then FInstance:= TWDOG.Create;
  Result:= FInstance;
end;

class procedure TWDOG.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TWDOG.Create;
var Base: LongWord;
begin
  inherited Create;

  FWDOG_BASE:= TA20.Instance.GetMMap(WDOG_BASE);
  Base:= LongWord(FWDOG_BASE) + TA20.Instance.BaseOffset(WDOG_BASE);

  FWDOG_CTRL:= Pointer(Base + $00);
  FWDOG_MODE:= Pointer(Base + $04);
end;

destructor TWDOG.Destroy;
begin
  TA20.Instance.FreeMMap(FWDOG_BASE);

  inherited Destroy;
end;

procedure TWDOG.SetInterval(Value: TInterval);
begin
  FWDOG_MODE^:= FWDOG_MODE^ and not ($F shl 3);
  FWDOG_MODE^:= FWDOG_MODE^ or (Ord(Value) shl 3);
end;

function TWDOG.GetInterval: TInterval;
var Value: Byte;
begin
  Value:= (FWDOG_MODE^ and ($F shl 3)) shr 3;
  case Value of
  0: Result:= S0_5;
  1: Result:= S1;
  2:Result:= S2;
  3: Result:= S3;
  4: Result:= S4;
  5: Result:= S5;
  6: Result:= S6;
  7: Result:= S8;
  8: Result:= S10;
  9: Result:= S12;
  10: Result:= S14;
  11: Result:= S16;
  end;
end;

procedure TWDOG.SetForceRestart(Value: Boolean);
begin
  case Value of
  False: FWDOG_MODE^:= FWDOG_MODE^ and not ($1 shl 1);
  True: FWDOG_MODE^:= FWDOG_MODE^ or ($1 shl 1);
  end;
end;

function TWDOG.GetForceRestart: Boolean;
begin
  Result:= (FWDOG_MODE^ and ($1 shl 1)) > 0;
end;

procedure TWDOG.Start;
begin
  FWDOG_MODE^:= FWDOG_MODE^ or ($1 shl 0);
end;

procedure TWDOG.Stop;
begin
  FWDOG_MODE^:= FWDOG_MODE^ and not ($1 shl 0);
end;

procedure TWDOG.Reset;
begin
  FWDOG_CTRL^:= 1;
end;


finalization
  TWDOG.Instance.Release;

end.
