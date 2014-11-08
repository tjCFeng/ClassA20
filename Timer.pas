(*
说明：全志A20的Timer底层操作封装类。分为两个部分：
1.TTimerGROUP类，可对寄存器直接操作，单例；
2.TTimer类，对具体的Timer实现了一些功能的简化。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.10.13
*)

unit Timer;

{$mode objfpc}{$H+}

interface

uses Unix, BaseUnix, SysUtils, A20;

type
  TChannel = (Timer_0, Timer_1, Timer_2, Timer_3, Timer_4, Timer_5);
  TMode = (Loop, Single);
  TCLK = (OSC32K, OSC24M, PLL6_6);
  TPrescal = (Div1, Div2, Div4, Div8, Div16, Div32, Div64, Div128);

  TTimerGroup = class
  private
    class var FInstance: TTimerGroup;
    class function GetInstance: TTimerGroup; static;
  public
    class procedure Release;
    class property Instance: TTimerGroup read GetInstance;

  private
    FTMR_BASE: ^LongWord;

    constructor Create;
    destructor Destroy; override;
  protected
    FTMR_IRQ_CTRL: TGOURP1_REG;
    FTMR_IRQ_STA: TGOURP1_REG;
    FTMR_CTRL: TGOURP6_REG;
    FTMR_INTV_VALUE: TGOURP6_REG;
    FTMR_CURR_VALUE: TGOURP6_REG;
  public
    property TMR_IRQ_CTRL: TGOURP1_REG read FTMR_IRQ_CTRL;
    property TMR_IRQ_STA: TGOURP1_REG read FTMR_IRQ_STA;
    property TMR_CTRL: TGOURP6_REG read FTMR_CTRL;
    property TMR_INTV_VALUE: TGOURP6_REG read FTMR_INTV_VALUE;
    property TMR_CURR_VALUE: TGOURP6_REG read FTMR_CURR_VALUE;
  end;
  
  TTimer = class
  private
    FChannel: TChannel;
    FCHBit: LongWord;
    FMode: TMode;
    FAutoReload: Boolean;
    FCLK: TCLK;
    FPrescal: TPrescal;
    FCNT: LongWord;
    FCUR: LongWord;

    FTIM_IRQ_CTL: ^LongWord;
    FTIM_IRQ_STA: ^LongWord;
    FTIM_CTRL: ^LongWord;
    FTIM_INTV: ^LongWord;
    FTIM_CURR: ^LongWord;
  protected
    procedure SetMode(Value: TMode);
    procedure SetAutoReload(Value: Boolean);
    procedure SetCLK(Value: TCLK);
    procedure SetPrescal(Value: TPrescal);
    procedure SetCNT(Value: LongWord);
    procedure SetCUR(Value: LongWord);
    procedure SetINT(INT: Boolean);
    function GetINT: Boolean;
  public
    constructor Create(Channel: TChannel);
    destructor Destroy; override;
    procedure ClearPending;
    procedure Start;
    procedure Stop;
  public
    property Channel: TChannel read FChannel;
    property Mode: TMode read FMode write SetMode default Single;
    property AutoReload: Boolean read FAutoReload write SetAutoReload default True;
    property CLK: TCLK read FCLK write SetCLK default OSC24M;
    property Prescal: TPrescal read FPrescal write SetPrescal default Div1;
    property CNT: LongWord read FCNT write SetCNT default 0;
    property CUR: LongWord read FCUR write SetCUR default 0;
    property INT: Boolean read GetINT write SetINT default False;
  end;
  
implementation

const
  TMR_BASE = $01C20C00;

(*Timer Group******************************************************************)
class function TTimerGroup.GetInstance: TTimerGroup;
begin
  if FInstance = nil then FInstance:= TTimerGroup.Create;
  Result:= FInstance;
end;

class procedure TTimerGroup.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TTimerGroup.Create;
var Base: LongWord; I: Byte;
begin
  inherited Create;

  FTMR_BASE:= TA20.Instance.GetMMap(TMR_BASE);
  Base:= LongWord(FTMR_BASE) + TA20.Instance.BaseOffset(TMR_BASE);

  FTMR_IRQ_CTRL:= Pointer(Base + $00);
  FTMR_IRQ_STA:= Pointer(Base + $04);
  
  for I:= 0 to 5 do
  begin
    FTMR_CTRL[I]:= Pointer(Base + (I + 1) * $10);
    FTMR_INTV_VALUE[I]:= Pointer(Base + (I + 1) * $10 + $04);
    FTMR_CURR_VALUE[I]:= Pointer(Base + (I + 1) * $10 + $08);
  end;
  FTMR_CURR_VALUE[3]:= nil;
end;

destructor TTimerGroup.Destroy;
begin
  TA20.Instance.FreeMMap(FTMR_BASE);

  inherited Destroy;
end;

(*TTimer Channel*********************************************************************)
constructor TTimer.Create(Channel: TChannel);
begin
  inherited Create;

  FChannel:= Channel;
  FCHBit:= ($1 shl Ord(FChannel));

  with TTimerGROUP.Instance do
  begin
    FTIM_IRQ_CTL:= TMR_IRQ_CTRL;
    FTIM_IRQ_STA:= TMR_IRQ_STA;
    FTIM_CTRL:= TMR_CTRL[Ord(FChannel)];
    FTIM_INTV:= TMR_INTV_VALUE[Ord(FChannel)];
    FTIM_CURR:= TMR_CURR_VALUE[Ord(FChannel)];
  end;

  FMode:= Single;
  SetMode(Single);
  FAutoReload:= True;
  SetAutoReload(True);
  FCLK:= OSC24M;
  SetCLK(OSC24M);
  FPrescal:= Div1;
  SetPrescal(Div1);
  FCNT:= 0;
  FCUR:= 0;
  SetINT(False);
end;

destructor TTimer.Destroy;
begin
  Stop;
end;

procedure TTimer.SetMode(Value: TMode);
begin
  FMode:= Value;
  case FMode of
  Loop: FTIM_CTRL^:= FTIM_CTRL^ and not ($1 shl 7);
  Single: FTIM_CTRL^:= FTIM_CTRL^ or ($1 shl 7);
  end;
end;

procedure TTimer.SetAutoReload(Value: Boolean);
begin
  FAutoReload:= Value;
  
  case FAutoReload of
  True: FTIM_CTRL^:= FTIM_CTRL^ and not ($1 shl 1);
  False: FTIM_CTRL^:= FTIM_CTRL^ or ($1 shl 1);
  end;  
end;

procedure TTimer.SetCLK(Value: TCLK);
begin
  FCLK:= Value;
  
  FTIM_CTRL^:= FTIM_CTRL^ and not ($3 shl 2);
  FTIM_CTRL^:= FTIM_CTRL^ or (Ord(FCLK) shl 2);
end;

procedure TTimer.SetPrescal(Value: TPrescal);
begin
  FPrescal:= Value;
  
  FTIM_CTRL^:= FTIM_CTRL^ and not ($3 shl 4);
  FTIM_CTRL^:= FTIM_CTRL^ or (Ord(FPrescal) shl 4)
end;

procedure TTimer.SetCNT(Value: LongWord);
begin
  FCNT:= Value;
  FTIM_INTV^:= FCNT;
end;

procedure TTimer.SetCUR(Value: LongWord);
begin
  FCUR:= Value;
  FTIM_CURR^:= FCUR;
end;

procedure TTimer.SetINT(INT: Boolean);
begin
  case INT of
  False: FTIM_IRQ_CTL^:= FTIM_IRQ_CTL^ and not FCHBit;
  True: FTIM_IRQ_CTL^:= FTIM_IRQ_CTL^ or FCHBit;
  end;
end;

function TTimer.GetINT: Boolean;
begin
  Result:= (FTIM_IRQ_STA^ and FCHBit) = FCHBit;
end;

procedure TTimer.ClearPending;
begin
  FTIM_IRQ_STA^:= FTIM_IRQ_STA^ or FCHBit;
end;

procedure TTimer.Start;
begin
  FTIM_CTRL^:= FTIM_CTRL^ or $1;
end;

procedure TTimer.Stop;
begin
  FTIM_CTRL^:= FTIM_CTRL^ and not $1;
end;


finalization
  TTimerGROUP.Instance.Release;
  
end.
=======
(*
说明：全志A20的Timer底层操作封装类。分为两个部分：
1.TTimerGROUP类，可对寄存器直接操作，单例；
2.TTimer类，对具体的Timer实现了一些功能的简化。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.10.13
*)

unit Timer;

{$mode objfpc}{$H+}

interface

uses Unix, BaseUnix, SysUtils, A20;

type
  TChannel = (Timer_0, Timer_1, Timer_2, Timer_3, Timer_4, Timer_5);
  TMode = (Loop, Single);
  TCLK = (OSC32K, OSC24M, PLL6_6);
  TPrescal = (Div1, Div2, Div4, Div8, Div16, Div32, Div64, Div128);

  TTimerGroup = class
  private
    class var FInstance: TTimerGroup;
    class function GetInstance: TTimerGroup; static;
  public
    class procedure Release;
    class property Instance: TTimerGroup read GetInstance;

  private
    FTMR_BASE: ^LongWord;

    constructor Create;
    destructor Destroy; override;
  protected
    FTMR_IRQ_CTRL: TGOURP1_REG;
    FTMR_IRQ_STA: TGOURP1_REG;
    FTMR_CTRL: TGOURP6_REG;
    FTMR_INTV_VALUE: TGOURP6_REG;
    FTMR_CURR_VALUE: TGOURP6_REG;
  public
    property TMR_IRQ_CTRL: TGOURP1_REG read FTMR_IRQ_CTRL;
    property TMR_IRQ_STA: TGOURP1_REG read FTMR_IRQ_STA;
    property TMR_CTRL: TGOURP6_REG read FTMR_CTRL;
    property TMR_INTV_VALUE: TGOURP6_REG read FTMR_INTV_VALUE;
    property TMR_CURR_VALUE: TGOURP6_REG read FTMR_CURR_VALUE;
  end;
  
  TTimer = class
  private
    FChannel: TChannel;
    FCHBit: LongWord;
    FMode: TMode;
    FAutoReload: Boolean;
    FCLK: TCLK;
    FPrescal: TPrescal;
    FCNT: LongWord;
    FCUR: LongWord;

    FTIM_IRQ_CTL: ^LongWord;
    FTIM_IRQ_STA: ^LongWord;
    FTIM_CTRL: ^LongWord;
    FTIM_INTV: ^LongWord;
    FTIM_CURR: ^LongWord;
  protected
    procedure SetMode(Value: TMode);
    procedure SetAutoReload(Value: Boolean);
    procedure SetCLK(Value: TCLK);
    procedure SetPrescal(Value: TPrescal);
    procedure SetCNT(Value: LongWord);
    procedure SetCUR(Value: LongWord);
    procedure SetINT(INT: Boolean);
    function GetINT: Boolean;
  public
    constructor Create(Channel: TChannel);
    destructor Destroy; override;
    procedure ClearPending;
    procedure Start;
    procedure Stop;
  public
    property Channel: TChannel read FChannel;
    property Mode: TMode read FMode write SetMode default Single;
    property AutoReload: Boolean read FAutoReload write SetAutoReload default True;
    property CLK: TCLK read FCLK write SetCLK default OSC24M;
    property Prescal: TPrescal read FPrescal write SetPrescal default Div1;
    property CNT: LongWord read FCNT write SetCNT default 0;
    property CUR: LongWord read FCUR write SetCUR default 0;
    property INT: Boolean read GetINT write SetINT default False;
  end;
  
implementation

const
  TMR_BASE = $01C20C00;

(*Timer Group******************************************************************)
class function TTimerGroup.GetInstance: TTimerGroup;
begin
  if FInstance = nil then FInstance:= TTimerGroup.Create;
  Result:= FInstance;
end;

class procedure TTimerGroup.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TTimerGroup.Create;
var Base: LongWord; I: Byte;
begin
  inherited Create;

  FTMR_BASE:= TA20.Instance.GetMMap(TMR_BASE);
  Base:= LongWord(FTMR_BASE) + TA20.Instance.BaseOffset(TMR_BASE);

  FTMR_IRQ_CTRL:= Pointer(Base + $00);
  FTMR_IRQ_STA:= Pointer(Base + $04);
  
  for I:= 0 to 5 do
  begin
    FTMR_CTRL[I]:= Pointer(Base + (I + 1) * $10);
    FTMR_INTV_VALUE[I]:= Pointer(Base + (I + 1) * $10 + $04);
    FTMR_CURR_VALUE[I]:= Pointer(Base + (I + 1) * $10 + $08);
  end;
  FTMR_CURR_VALUE[3]:= nil;
end;

destructor TTimerGroup.Destroy;
begin
  TA20.Instance.FreeMMap(FTMR_BASE);

  inherited Destroy;
end;

(*TTimer Channel*********************************************************************)
constructor TTimer.Create(Channel: TChannel);
begin
  inherited Create;

  FChannel:= Channel;
  FCHBit:= ($1 shl Ord(FChannel));

  with TTimerGROUP.Instance do
  begin
    FTIM_IRQ_CTL:= TMR_IRQ_CTRL;
    FTIM_IRQ_STA:= TMR_IRQ_STA;
    FTIM_CTRL:= TMR_CTRL[Ord(FChannel)];
    FTIM_INTV:= TMR_INTV_VALUE[Ord(FChannel)];
    FTIM_CURR:= TMR_CURR_VALUE[Ord(FChannel)];
  end;

  FMode:= Single;
  SetMode(Single);
  FAutoReload:= True;
  SetAutoReload(True);
  FCLK:= OSC24M;
  SetCLK(OSC24M);
  FPrescal:= Div1;
  SetPrescal(Div1);
  FCNT:= 0;
  FCUR:= 0;
  SetINT(False);
end;

destructor TTimer.Destroy;
begin
  Stop;
end;

procedure TTimer.SetMode(Value: TMode);
begin
  FMode:= Value;
  case FMode of
  Loop: FTIM_CTRL^:= FTIM_CTRL^ and not ($1 shl 7);
  Single: FTIM_CTRL^:= FTIM_CTRL^ or ($1 shl 7);
  end;
end;

procedure TTimer.SetAutoReload(Value: Boolean);
begin
  FAutoReload:= Value;
  
  case FAutoReload of
  True: FTIM_CTRL^:= FTIM_CTRL^ and not ($1 shl 1);
  False: FTIM_CTRL^:= FTIM_CTRL^ or ($1 shl 1);
  end;  
end;

procedure TTimer.SetCLK(Value: TCLK);
begin
  FCLK:= Value;
  
  FTIM_CTRL^:= FTIM_CTRL^ and not ($3 shl 2);
  FTIM_CTRL^:= FTIM_CTRL^ or (Ord(FCLK) shl 2);
end;

procedure TTimer.SetPrescal(Value: TPrescal);
begin
  FPrescal:= Value;
  
  FTIM_CTRL^:= FTIM_CTRL^ and not ($3 shl 4);
  FTIM_CTRL^:= FTIM_CTRL^ or (Ord(FPrescal) shl 4)
end;

procedure TTimer.SetCNT(Value: LongWord);
begin
  FCNT:= Value;
  FTIM_INTV^:= FCNT;
end;

procedure TTimer.SetCUR(Value: LongWord);
begin
  FCUR:= Value;
  FTIM_CURR^:= FCUR;
end;

procedure TTimer.SetINT(INT: Boolean);
begin
  case INT of
  False: FTIM_IRQ_CTL^:= FTIM_IRQ_CTL^ and not FCHBit;
  True: FTIM_IRQ_CTL^:= FTIM_IRQ_CTL^ or FCHBit;
  end;
end;

function TTimer.GetINT: Boolean;
begin
  Result:= (FTIM_IRQ_STA^ and FCHBit) = FCHBit;
end;

procedure TTimer.ClearPending;
begin
  FTIM_IRQ_STA^:= FTIM_IRQ_STA^ or FCHBit;
end;

procedure TTimer.Start;
begin
  FTIM_CTRL^:= FTIM_CTRL^ or $1;
end;

procedure TTimer.Stop;
begin
  FTIM_CTRL^:= FTIM_CTRL^ and not $1;
end;


finalization
  TTimerGROUP.Instance.Release;
  
end.
