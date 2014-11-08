(*
说明：全志A20的General Purpose底层操作封装类。单例。
当RTC电池电压大于1.0V时可以保存数据
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.10.15
*)

unit GP;

{$mode objfpc}{$H+}

interface

uses Unix, BaseUnix, SysUtils, A20;

type
  TChannel = 
    (GP_0, GP_1, GP_2,  GP_3,  GP_4,  GP_5,  GP_6,  GP_7,
     GP_8, GP_9, GP_10, GP_11, GP_12, GP_13, GP_14, GP_15);

  TGP = class
  private
    class var FInstance: TGP;
    class function GetInstance: TGP; static;
  public
    class procedure Release;
    class property Instance: TGP read GetInstance;
	
  private
    FGP_BASE: ^LongWord;

    constructor Create;
    destructor Destroy; override;
  protected
    FTMR_GP: TGOURP16_REG;
  public
    procedure SetGP(Channel: TChannel; Value: LongWord);
    function GetGP(Channel: TChannel): LongWord;
  public
    property TMR_GP: TGOURP16_REG read FTMR_GP;
  end;
  
implementation

const
  GP_BASE = $01C20D20;

class function TGP.GetInstance: TGP;
begin
  if FInstance = nil then FInstance:= TGP.Create;
  Result:= FInstance;
end;

class procedure TGP.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TGP.Create;
var Base: LongWord; I: Byte;
begin
  inherited Create;

  FGP_BASE:= TA20.Instance.GetMMap(GP_BASE);
  Base:= LongWord(FGP_BASE) + TA20.Instance.BaseOffset(GP_BASE);

  for I:= 0 to 15 do FTMR_GP[I]:= Pointer(Base + I * $04);
end;

destructor TGP.Destroy;
begin
  TA20.Instance.FreeMMap(FGP_BASE);

  inherited Destroy;
end;

procedure TGP.SetGP(Channel: TChannel; Value: LongWord);
begin
  FTMR_GP[Ord(Channel)]^:= Value;
end;

function TGP.GetGP(Channel: TChannel): LongWord;
begin
  Result:= FTMR_GP[Ord(Channel)]^;
end;

finalization
  TGP.Instance.Release;
  
end.
=======
(*
说明：全志A20的General Purpose底层操作封装类。单例。
当RTC电池电压大于1.0V时可以保存数据
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.10.15
*)

unit GP;

{$mode objfpc}{$H+}

interface

uses Unix, BaseUnix, SysUtils, A20;

type
  TChannel = 
    (GP_0, GP_1, GP_2,  GP_3,  GP_4,  GP_5,  GP_6,  GP_7,
     GP_8, GP_9, GP_10, GP_11, GP_12, GP_13, GP_14, GP_15);

  TGP = class
  private
    class var FInstance: TGP;
    class function GetInstance: TGP; static;
  public
    class procedure Release;
    class property Instance: TGP read GetInstance;
	
  private
    FGP_BASE: ^LongWord;

    constructor Create;
    destructor Destroy; override;
  protected
    FTMR_GP: TGOURP16_REG;
  public
    procedure SetGP(Channel: TChannel; Value: LongWord);
    function GetGP(Channel: TChannel): LongWord;
  public
    property TMR_GP: TGOURP16_REG read FTMR_GP;
  end;
  
implementation

const
  GP_BASE = $01C20D20;

class function TGP.GetInstance: TGP;
begin
  if FInstance = nil then FInstance:= TGP.Create;
  Result:= FInstance;
end;

class procedure TGP.Release;
begin
  FreeAndNil(FInstance);
end;

constructor TGP.Create;
var Base: LongWord; I: Byte;
begin
  inherited Create;

  FGP_BASE:= TA20.Instance.GetMMap(GP_BASE);
  Base:= LongWord(FGP_BASE) + TA20.Instance.BaseOffset(GP_BASE);

  for I:= 0 to 15 do FTMR_GP[I]:= Pointer(Base + I * $04);
end;

destructor TGP.Destroy;
begin
  TA20.Instance.FreeMMap(FGP_BASE);

  inherited Destroy;
end;

procedure TGP.SetGP(Channel: TChannel; Value: LongWord);
begin
  FTMR_GP[Ord(Channel)]^:= Value;
end;

function TGP.GetGP(Channel: TChannel): LongWord;
begin
  Result:= FTMR_GP[Ord(Channel)]^;
end;

finalization
  TGP.Instance.Release;
  
end.
