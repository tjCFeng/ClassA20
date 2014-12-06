(*
说明：全志A20的GPIO底层操作封装类。分为两个部分：
1.TGPIOGROUP类，可对寄存器直接操作，单例；
2.TGPIO类，对具体的Pin实现了一些功能的简化。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.12.06
*)

unit GPIO;

{$mode objfpc}{$H+}

interface

uses A20, Clock;

type
  TPort = (PA, PB, PC, PD, PE, PF, PG, PH, PI); //PS:DRAM
  TFun = (Fun0, Fun1, Fun2, Fun3, Fun4, Fun5, Fun6, Fun7); //Fun0:Input; Fun1:Output
  TDrv = (Level0, Level1, Level2, Level3);
  TPull = (PULL_OFF, PULL_UP, PULL_DOWN);

  TGPIOGROUP = class
  private
    FPort: TPort;
    FGPIO_BASE: ^LongWord;
  protected
    FGPIO_CFG: TGROUP4_REG;
    FGPIO_DAT: TGROUP1_REG;
    FGPIO_DRV: TGROUP2_REG;
    FGPIO_PUL: TGROUP2_REG;
  public
    constructor Create(Port: TPort);
    destructor Destroy; override;
  public
    property GPIO_CFG: TGROUP4_REG read FGPIO_CFG;
    property GPIO_DAT: TGROUP1_REG read FGPIO_DAT;
    property GPIO_DRV: TGROUP2_REG read FGPIO_DRV;
    property GPIO_PUL: TGROUP2_REG read FGPIO_PUL;
  end;

  TGPIO = class(TGPIOGROUP)
  private
    FPin: Byte;
    FPinBit: LongWord;

    FPIN_CFG: ^LongWord;
    FPIN_DAT: ^LongWord;
    FPIN_DRV: ^LongWord;
    FPIN_PUL: ^LongWord;
  protected
    procedure SetFun(Fun: TFun);
    procedure SetDrv(Drv: TDrv);
    procedure SetPull(Pull: TPull);
    procedure SetData(Level: Boolean);
    function GetData: Boolean;
  public
    constructor Create(Port: TPort; Pin: Byte);
    procedure Reverse;
  public
    property Fun: TFun write SetFun;
    property Drv: TDrv write SetDrv;
    property Pull: TPull write SetPull;
    property Data: Boolean read GetData write SetData;
  end;

implementation

const
  PIO_BASE = $01C20800;

(*GPIO Group*******************************************************************)
constructor TGPIOGROUP.Create(Port: TPort);
var Base: LongWord;
begin
  inherited Create;

  FPort:= Port;
  FGPIO_BASE:= TA20.Instance.GetMMap(PIO_BASE);
  Base:= LongWord(FGPIO_BASE) + TA20.Instance.BaseOffset(PIO_BASE) + (Ord(FPort) * $24);

  FGPIO_CFG[0]:= Pointer(Base + $00);
  FGPIO_CFG[1]:= Pointer(Base + $04);
  FGPIO_CFG[2]:= Pointer(Base + $08);
  FGPIO_CFG[3]:= Pointer(Base + $0C);
  FGPIO_DAT:= Pointer(Base + $10);
  FGPIO_DRV[0]:= Pointer(Base + $14);
  FGPIO_DRV[1]:= Pointer(Base + $18);
  FGPIO_PUL[0]:= Pointer(Base + $1C);
  FGPIO_PUL[1]:= Pointer(Base + $20);

  TCCU.Instance.CLK_SetGPIO(True);
end;

destructor TGPIOGROUP.Destroy;
begin
  TA20.Instance.FreeMMap(FGPIO_BASE);

  inherited Destroy;
end;

(*GPIO Pin*********************************************************************)
constructor TGPIO.Create(Port: TPort; Pin: Byte);
begin
  inherited Create(Port);

  FPort:= Port;
  FPin:= Pin;
  FPinBit:= ($1 shl FPin);

  FPIN_CFG:= FGPIO_CFG[FPin div 8];
  FPIN_DAT:= FGPIO_DAT;
  FPIN_DRV:= FGPIO_DRV[FPin div 16];
  FPIN_PUL:= FGPIO_PUL[FPin div 16];
end;

procedure TGPIO.SetFun(Fun: TFun);
var PinN: Byte;
begin
  PinN:= (FPin mod 8) * 4;
  FPIN_CFG^:= FPIN_CFG^ and not ($7 shl PinN);
  FPIN_CFG^:= FPIN_CFG^ or (Ord(Fun) shl PinN);
end;

procedure TGPIO.SetDrv(Drv: TDrv);
var PinN: Byte;
begin
  PinN:= (FPin mod 16) * 2;
  FPIN_DRV^:= FPIN_DRV^ and not ($3 shl PinN);
  FPIN_DRV^:= FPIN_DRV^ or (Ord(Drv) shl PinN);
end;

procedure TGPIO.SetPull(Pull: TPull);
var PinN: Byte;
begin
  PinN:= (FPin mod 16) * 2;
  FPIN_PUL^:= FPIN_PUL^ and not ($3 shl PinN);
  FPIN_PUL^:= FPIN_PUL^ or (Ord(Pull) shl PinN);
end;

procedure TGPIO.SetData(Level: Boolean);
begin
  case Level of
  False: FPIN_DAT^:= FPIN_DAT^ and not FPinBit;
  True: FPIN_DAT^:= FPIN_DAT^ or FPinBit;
  end;
end;

function TGPIO.GetData: Boolean;
begin
  Result:= ((FPIN_DAT^ and FPinBit) = FPinBit);
end;

procedure TGPIO.Reverse;
begin
  FPIN_DAT^:= FPIN_DAT^ xor FPinBit;
end;

end.
