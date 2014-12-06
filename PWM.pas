(*
说明：全志A20的PWM底层操作封装类。分为两个部分：
1.TPWMGROUP类，可对寄存器直接操作，单例；
2.TPWM类，对具体的PWM通道实现了一些功能的简化。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.12.06
*)

unit PWM;

{$mode objfpc}{$H+}

interface

uses SysUtils, A20, GPIO;

type
  TChannel = (PWM_0, PWM_1);
  TPrescal =
    (P120, P180, P240, P360, P480, P720, P840, P960,
     P12K, P24K, P36K, P48K, P72K, P84K, P96K, P108K);

  TPWMGROUP = class
  private
    class var FInstance: TPWMGROUP;
    class function GetInstance: TPWMGROUP; static;
  public
    class procedure Release;
    class property Instance: TPWMGROUP read GetInstance;

  private
    FPWM_BASE: ^LongWord;
    FPWM_CTRL: TGROUP1_REG;
    FPWM_PERIOD: TGROUP2_REG;

    constructor Create;
    destructor Destroy; override;
  public
    property PWM_CTRL: TGROUP1_REG read FPWM_CTRL;
    property PWM_PERIOD: TGROUP2_REG read FPWM_PERIOD;
  end;

  TPWM = class(TGPIO)
  private
    FChannel: TChannel;
    FCycle: Word;
    FDuty: Word;
    FPWM_PERIOD: ^LongWord;
  private
    procedure SetPrescale(Value: TPrescal);
    procedure SetCycle(Value: Word);
    procedure SetDuty(Value: Word);
  public
    constructor Create(Channel: TChannel);
    procedure Start;
    procedure Stop;
  public
    property Channel: TChannel read FChannel;
    property Prescale: TPrescal write SetPrescale;
    property Cycle: Word write SetCycle;
    property Duty: Word write SetDuty;
  end;

implementation

const
  PWM_BASE = $01C20C00;

(*PWM Group********************************************************************)
class function TPWMGROUP.GetInstance: TPWMGROUP;
begin
  if FInstance = nil then FInstance:= TPWMGROUP.Create;
  Result:= FInstance;
end;

class procedure TPWMGROUP.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TPWMGROUP.Create;
var Base: LongWord;
begin
  inherited Create;

  FPWM_BASE:= TA20.Instance.GetMMap(PWM_BASE);
  Base:= LongWord(FPWM_BASE) + TA20.Instance.BaseOffset(PWM_BASE);

  FPWM_CTRL:= Pointer(Base + $200);
  FPWM_PERIOD[0]:= Pointer(Base + $204);
  FPWM_PERIOD[1]:= Pointer(Base + $208);

  //FPWM_CTRL^:= FPWM_CTRL^ or (($1 shl 24) + ($1 shl 9)); //OSC24MHz
end;

destructor TPWMGROUP.Destroy;
begin
  FPWM_CTRL^:= 0;
  FPWM_PERIOD[0]^:= 0;
  FPWM_PERIOD[1]^:= 0;
  TA20.Instance.FreeMMap(FPWM_BASE);

  inherited Destroy;
end;

(*PWM Channel******************************************************************)
constructor TPWM.Create(Channel: TChannel);
begin
  FChannel:= Channel;

  case FChannel of
  PWM_0: inherited Create(PB, 2);
  PWM_1: inherited Create(PI, 3);
  end;
  SetFun(Fun2);

  FPWM_PERIOD:= TPWMGROUP.Instance.PWM_PERIOD[Ord(FChannel)];
  FPWM_PERIOD^:= 0;

  with TPWMGROUP.Instance do
  PWM_CTRL^:= PWM_CTRL^ or ($1 shl (Ord(FChannel) * 15 + 6)); //Special Clock
end;

procedure TPWM.SetPrescale(Value: TPrescal);
begin
  with TPWMGROUP.Instance do
  begin
    PWM_CTRL^:= PWM_CTRL^ and not ($F shl (Ord(FChannel) * 15));
    PWM_CTRL^:= PWM_CTRL^ or (Ord(Value) shl (Ord(FChannel) * 15));
  end;
end;

procedure TPWM.SetCycle(Value: Word);
begin
  FCycle:= Value;
  FPWM_PERIOD^:= FPWM_PERIOD^ and $0000FFFF;
  FPWM_PERIOD^:= FPWM_PERIOD^ or (Value shl 16);
end;

procedure TPWM.SetDuty(Value: Word);
var D: LongWord;
begin
  if (FCycle > 0) and (Value >= FCycle) then FDuty:= FCycle - 1;
  FPWM_PERIOD^:= FPWM_PERIOD^ and $FFFF0000;
  FPWM_PERIOD^:= FPWM_PERIOD^ or Value;
end;

procedure TPWM.Start;
begin
  with TPWMGROUP.Instance do
  FPWM_CTRL^:= FPWM_CTRL^ or ($1 shl (Ord(FChannel) * 15 + 4));
end;

procedure TPWM.Stop;
begin
  with TPWMGROUP.Instance do
  FPWM_CTRL^:= FPWM_CTRL^ and not ($1 shl (Ord(FChannel) * 15 + 4));
end;


finalization
  TPWMGROUP.Instance.Release;

end.
