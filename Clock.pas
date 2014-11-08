(*
说明：全志A20的System Clock底层操作封装类。单例。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.11.08
*)

unit Clock;

{$mode objfpc}{$H+}

interface

uses Unix, BaseUnix, SysUtils, A20;

type
  TCCU = class
  private
    class var FInstance: TCCU;
    class function GetInstance: TCCU; static;
  public
    class procedure Release;
    class property Instance: TCCU read GetInstance;
	
  private
    FCCU_BASE: ^LongWord;
	
    FPLL1_CFG: ^LongWord;
    FPLL1_TUN: ^LongWord;
    FPLL2_CFG: ^LongWord;
    FPLL2_TUN: ^LongWord;
    FPLL3_CFG: ^LongWord;
    FPLL4_CFG: ^LongWord;
    FPLL5_CFG: ^LongWord;
    FPLL5_TUN: ^LongWord;
    FPLL6_CFG: ^LongWord;
    FPLL6_TUN: ^LongWord;
    FPLL7_CFG: ^LongWord;
    FPLL1_TUN2: ^LongWord;
    FPLL5_TUN2: ^LongWord;
    FPLL8_CFG: ^LongWord;

    FOSC24M_CFG: ^LongWord;
    FCPU_AHB_APB0_CFG: ^LongWord;
    FAPB1_CLK_DIV: ^LongWord;
    FAHB_GATING0: ^LongWord;
    FAHB_GATING1: ^LongWord;
    FAPB0_GATING: ^LongWord;
    FAPB1_GATING: ^LongWord;

    FNAND_SCLK_CFG: ^LongWord;
    FMS_SCLK_CFG: ^LongWord;

    FSD0_CLK: ^LongWord;
    FSD1_CLK: ^LongWord;
    FSD2_CLK: ^LongWord;
    FSD3_CLK: ^LongWord;

    FTS_CLK: ^LongWord;
    FSS_CLK: ^LongWord;

    FSPI0_CLK: ^LongWord;
    FSPI1_CLK: ^LongWord;
    FSPI2_CLK: ^LongWord;

    FIR0_CLK: ^LongWord;
    FIR1_CLK: ^LongWord;

    FIIS0_CLK: ^LongWord;
    FAC97_CLK: ^LongWord;
    FSPDIF_CLK: ^LongWord;

    FKEYPAD_CLK: ^LongWord;
    FSATA_CLK: ^LongWord;
    FUSB_CLK: ^LongWord;
    FSPI3_CLK: ^LongWord;
    FIIS1_CLK: ^LongWord;
    FIIS2_CLK: ^LongWord;
    FDRAM_CLK: ^LongWord;

    FBE0_SCLK_CFG: ^LongWord;
    FBE1_SCLK_CFG: ^LongWord;
    FFE0_CLK: ^LongWord;
    FFE1_CLK: ^LongWord;

    FMP_CLK: ^LongWord;
    FLCD0_CH0_CLK: ^LongWord;
    FLCD1_CH0_CLK: ^LongWord;
    FCSI_SCLK: ^LongWord;
    FTVD_CLK: ^LongWord;
    FLCD0_CH1_CLK: ^LongWord;
    FLCD1_CH1_CLK: ^LongWord;
    FCSI0_CLK: ^LongWord;
    FCSI1_CLK: ^LongWord;

    FVE_CLK: ^LongWord;
    FAUDIO_CODEC_CLK: ^LongWord;
    FAVS_CLK: ^LongWord;
    FACE_CLK: ^LongWord;
    FLVDS_CLK: ^LongWord;
    FHDMI_CLK: ^LongWord;
    FMALI400_CLK: ^LongWord;
    FMBUS_SCLK_CFG: ^LongWord;
    FGMAC_CLK: ^LongWord;
    FHDMI1_RST: ^LongWord;
    FHDMI1_CTRL: ^LongWord;
    FHDMI1_SLOW_CLK: ^LongWord;
    FHDMI1_REPEAT_CLK: ^LongWord;

    FCLK_OUTA: ^LongWord;
    FCLK_OUTB: ^LongWord;

    constructor Create;
    destructor Destroy; override;
  public
    function Get_CorePLL: LongWord;
    function Get_AXI: LongWord;
    function Get_AHB: LongWord;
    function Get_APB0: LongWord;
    function Get_APB1: LongWord;
  public
    procedure CLK_SetGPIO(ED: Boolean);
    function CLK_GetGPIO: Boolean;
	
    procedure CLK_SetTWI(IndexTWI: Byte; ED: Boolean);
    function CLK_GetTWI(IndexTWI: Byte): Boolean;

    procedure CLK_SetSPI(IndexSPI: Byte; ED: Boolean);
    function CLK_GetSPI(IndexSPI: Byte): Boolean;
  end;

implementation

const
  CCU_BASE = $01C20000;
  
class function TCCU.GetInstance: TCCU;
begin
  if FInstance = nil then FInstance:= TCCU.Create;
  Result:= FInstance;
end;

class procedure TCCU.Release;
begin
  FreeAndNil(FInstance);
end;


constructor TCCU.Create;
var Base: LongWord;
begin
  inherited Create;
  
  FCCU_BASE:= TA20.Instance.GetMMap(CCU_BASE);
  Base:= LongWord(FCCU_BASE) + TA20.Instance.BaseOffset(CCU_BASE);

  FPLL1_CFG:= Pointer(Base + $0000);
  FPLL1_TUN:= Pointer(Base + $0004);
  FPLL2_CFG:= Pointer(Base + $0008);
  FPLL2_TUN:= Pointer(Base + $000C);
  FPLL3_CFG:= Pointer(Base + $0010);
  FPLL4_CFG:= Pointer(Base + $0018);
  FPLL5_CFG:= Pointer(Base + $0020);
  FPLL5_TUN:= Pointer(Base + $0024);
  FPLL6_CFG:= Pointer(Base + $0028);
  FPLL6_TUN:= Pointer(Base + $002C);
  FPLL7_CFG:= Pointer(Base + $0030);
  FPLL1_TUN2:= Pointer(Base + $0038);
  FPLL5_TUN2:= Pointer(Base + $003C);
  FPLL8_CFG:= Pointer(Base + $0040);

  FOSC24M_CFG:= Pointer(Base + $0050);
  FCPU_AHB_APB0_CFG:= Pointer(Base + $0054);
  FAPB1_CLK_DIV:= Pointer(Base + $0058);
  FAHB_GATING0:= Pointer(Base + $0060);
  FAHB_GATING1:= Pointer(Base + $0064);
  FAPB0_GATING:= Pointer(Base + $0068);
  FAPB1_GATING:= Pointer(Base + $006C);

  FNAND_SCLK_CFG:= Pointer(Base + $0080);
  FMS_SCLK_CFG:= Pointer(Base + $0084);

  FSD0_CLK:= Pointer(Base + $0088);
  FSD1_CLK:= Pointer(Base + $008C);
  FSD2_CLK:= Pointer(Base + $0090);
  FSD3_CLK:= Pointer(Base + $0094);

  FTS_CLK:= Pointer(Base + $0098);
  FSS_CLK:= Pointer(Base + $009C);

  FSPI0_CLK:= Pointer(Base + $00A0);
  FSPI1_CLK:= Pointer(Base + $00A4);
  FSPI2_CLK:= Pointer(Base + $00A8);

  FIR0_CLK:= Pointer(Base + $00B0);
  FIR1_CLK:= Pointer(Base + $00B4);

  FIIS0_CLK:= Pointer(Base + $00B8);
  FAC97_CLK:= Pointer(Base + $00BC);
  FSPDIF_CLK:= Pointer(Base + $00C0);

  FKEYPAD_CLK:= Pointer(Base + $00C4);
  FSATA_CLK:= Pointer(Base + $00C8);
  FUSB_CLK:= Pointer(Base + $00CC);
  FSPI3_CLK:= Pointer(Base + $00D4);
  FIIS1_CLK:= Pointer(Base + $00D8);
  FIIS2_CLK:= Pointer(Base + $00DC);
  FDRAM_CLK:= Pointer(Base + $0100);

  FBE0_SCLK_CFG:= Pointer(Base + $0104);
  FBE1_SCLK_CFG:= Pointer(Base + $0108);
  FFE0_CLK:= Pointer(Base + $010C);
  FFE1_CLK:= Pointer(Base + $0110);

  FMP_CLK:= Pointer(Base + $0114);
  FLCD0_CH0_CLK:= Pointer(Base + $0118);
  FLCD1_CH0_CLK:= Pointer(Base + $011C);
  FCSI_SCLK:= Pointer(Base + $0120);
  FTVD_CLK:= Pointer(Base + $0128);
  FLCD0_CH1_CLK:= Pointer(Base + $012C);
  FLCD1_CH1_CLK:= Pointer(Base + $0130);
  FCSI0_CLK:= Pointer(Base + $0134);
  FCSI1_CLK:= Pointer(Base + $0138);

  FVE_CLK:= Pointer(Base + $013C);
  FAUDIO_CODEC_CLK:= Pointer(Base + $0140);
  FAVS_CLK:= Pointer(Base + $0144);
  FACE_CLK:= Pointer(Base + $0148);
  FLVDS_CLK:= Pointer(Base + $014C);
  FHDMI_CLK:= Pointer(Base + $0150);
  FMALI400_CLK:= Pointer(Base + $0154);
  FMBUS_SCLK_CFG:= Pointer(Base + $015C);
  FGMAC_CLK:= Pointer(Base + $0164);
  FHDMI1_RST:= Pointer(Base + $0170);
  FHDMI1_CTRL:= Pointer(Base + $0174);
  FHDMI1_SLOW_CLK:= Pointer(Base + $0178);
  FHDMI1_REPEAT_CLK:= Pointer(Base + $017C);

  FCLK_OUTA:= Pointer(Base + $01F0);
  FCLK_OUTB:= Pointer(Base + $01F4);

  FOSC24M_CFG^:= FOSC24M_CFG^ or (($1 shl 16) + ($1 shl 15) + ($1 shl 1) + $1);
end;

destructor TCCU.Destroy;
begin
  TA20.Instance.FreeMMap(FCCU_BASE);
  inherited Destroy;
end;

(******************************************************************************)
function TCCU.Get_CorePLL: LongWord;
var P, M, N, K: LongWord;
begin
  Result:= FPLL1_CFG^;

  P:= 1 shl ((Result shr 16) and $03);
  N:= (Result shr 8) and $1F;
  M:= ((Result shr 0) and $03) + 1;
  K:= ((Result shr 4) and $03) + 1;

  Result:= 24000000 * N * K div P div M;
end;

function TCCU.Get_AXI: LongWord;
var CLKSRC, Factor: LongWord;
begin
  Result:= FCPU_AHB_APB0_CFG^;

  CLKSRC:= (Result shr 16) and $03;
  Factor:= ((Result shr 0) and $03) + 1;

  case CLKSRC of
  0: Result:= 32000;
  1: Result:= 24000000;
  2: Result:= Get_CorePLL;
  else
    Result:= 0;
  end;
end;

function TCCU.Get_AHB: LongWord;
var Factor: LongWord;
begin
  Factor:= (FCPU_AHB_APB0_CFG^ shr 4) and $03;
  Result:= Get_AXI shr Factor;
end;

function TCCU.Get_APB0: LongWord;
var Factor: LongWord;
begin
  Factor:= (FCPU_AHB_APB0_CFG^ shr 8) and $03;

  if (Factor > 0) then
    Result:= Get_AHB shr Factor
  else
    Result:= Get_AHB shr 1;
end;

function TCCU.Get_APB1: LongWord;
var Factor: LongWord;
begin
  Result:= 0;

  Factor:= (FAPB1_CLK_DIV^ shr 24) and $03;
  if (Factor = 0) then Result:= 24000000;
end;


(******************************************************************************)
procedure TCCU.CLK_SetGPIO(ED: Boolean);
begin
  case ED of
  False: FAPB0_GATING^:= FAPB0_GATING^ and not ($1 shl 5);
  True: FAPB0_GATING^:= FAPB0_GATING^ or ($1 shl 5);
  end;
end;

function TCCU.CLK_GetGPIO: Boolean;
begin
  Result:= (FAPB0_GATING^ and ($1 shl 5) = 1); 
end;
	
procedure TCCU.CLK_SetTWI(IndexTWI: Byte; ED: Boolean);
var Bit: Byte;
begin
  case IndexTWI of
  0..3: Bit:= IndexTWI;
  4: Bit:= 15;
  end;

  case ED of
  False: FAPB1_GATING^:= FAPB1_GATING^ and not ($1 shl Bit);
  True: FAPB1_GATING^:= FAPB1_GATING^ or ($1 shl Bit);
  end;  
end;

function TCCU.CLK_GetTWI(IndexTWI: Byte): Boolean;
begin
  case IndexTWI of
  0..3: Result:= (FAPB1_GATING^ and ($1 shl IndexTWI) = 1);
  4: Result:= (FAPB1_GATING^ and ($1 shl 15) = 1);
  end;
end;

procedure TCCU.CLK_SetSPI(IndexSPI: Byte; ED: Boolean);
begin
  case ED of
  False: FAHB_GATING0^:= FAHB_GATING0^ and not ($1 shl (IndexSPI + 20));
  True: FAHB_GATING0^:= FAHB_GATING0^ or ($1 shl (IndexSPI + 20));
  end;
end;

function TCCU.CLK_GetSPI(IndexSPI: Byte): Boolean;
begin
  Result:= (FAHB_GATING0^ and ($1 shl (IndexSPI + 20)) = 1);
end;

finalization
  TCCU.Instance.Release;

end.
=======
(*
说明：全志A20的System Clock底层操作封装类。单例。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.10.17
*)

unit Clock;

{$mode objfpc}{$H+}

interface

uses Unix, BaseUnix, SysUtils, A20;

type
  TCCU = class
  private
    class var FInstance: TCCU;
    class function GetInstance: TCCU; static;
  public
    class procedure Release;
    class property Instance: TCCU read GetInstance;
	
  private
    FCCU_BASE: ^LongWord;
	
    FPLL1_CFG: ^LongWord;
    FPLL1_TUN: ^LongWord;
    FPLL2_CFG: ^LongWord;
    FPLL2_TUN: ^LongWord;
    FPLL3_CFG: ^LongWord;
    FPLL4_CFG: ^LongWord;
    FPLL5_CFG: ^LongWord;
    FPLL5_TUN: ^LongWord;
    FPLL6_CFG: ^LongWord;
    FPLL6_TUN: ^LongWord;
    FPLL7_CFG: ^LongWord;
    FPLL1_TUN2: ^LongWord;
    FPLL5_TUN2: ^LongWord;
    FPLL8_CFG: ^LongWord;

    FOSC24M_CFG: ^LongWord;
    FCPU_AHB_APB0_CFG: ^LongWord;
    FAPB1_CLK_DIV: ^LongWord;
    FAHB_GATING0: ^LongWord;
    FAHB_GATING1: ^LongWord;
    FAPB0_GATING: ^LongWord;
    FAPB1_GATING: ^LongWord;

    FNAND_SCLK_CFG: ^LongWord;
    FMS_SCLK_CFG: ^LongWord;

    FSD0_CLK: ^LongWord;
    FSD1_CLK: ^LongWord;
    FSD2_CLK: ^LongWord;
    FSD3_CLK: ^LongWord;

    FTS_CLK: ^LongWord;
    FSS_CLK: ^LongWord;

    FSPI0_CLK: ^LongWord;
    FSPI1_CLK: ^LongWord;
    FSPI2_CLK: ^LongWord;

    FIR0_CLK: ^LongWord;
    FIR1_CLK: ^LongWord;

    FIIS0_CLK: ^LongWord;
    FAC97_CLK: ^LongWord;
    FSPDIF_CLK: ^LongWord;

    FKEYPAD_CLK: ^LongWord;
    FSATA_CLK: ^LongWord;
    FUSB_CLK: ^LongWord;
    FSPI3_CLK: ^LongWord;
    FIIS1_CLK: ^LongWord;
    FIIS2_CLK: ^LongWord;
    FDRAM_CLK: ^LongWord;

    FBE0_SCLK_CFG: ^LongWord;
    FBE1_SCLK_CFG: ^LongWord;
    FFE0_CLK: ^LongWord;
    FFE1_CLK: ^LongWord;

    FMP_CLK: ^LongWord;
    FLCD0_CH0_CLK: ^LongWord;
    FLCD1_CH0_CLK: ^LongWord;
    FCSI_SCLK: ^LongWord;
    FTVD_CLK: ^LongWord;
    FLCD0_CH1_CLK: ^LongWord;
    FLCD1_CH1_CLK: ^LongWord;
    FCSI0_CLK: ^LongWord;
    FCSI1_CLK: ^LongWord;

    FVE_CLK: ^LongWord;
    FAUDIO_CODEC_CLK: ^LongWord;
    FAVS_CLK: ^LongWord;
    FACE_CLK: ^LongWord;
    FLVDS_CLK: ^LongWord;
    FHDMI_CLK: ^LongWord;
    FMALI400_CLK: ^LongWord;
    FMBUS_SCLK_CFG: ^LongWord;
    FGMAC_CLK: ^LongWord;
    FHDMI1_RST: ^LongWord;
    FHDMI1_CTRL: ^LongWord;
    FHDMI1_SLOW_CLK: ^LongWord;
    FHDMI1_REPEAT_CLK: ^LongWord;

    FCLK_OUTA: ^LongWord;
    FCLK_OUTB: ^LongWord;

    constructor Create;
    destructor Destroy; override;
  public
    function Get_CorePLL: LongWord;
    function Get_AXI: LongWord;
    function Get_AHB: LongWord;
    function Get_APB0: LongWord;
    function Get_APB1: LongWord;
  public
    procedure CLK_SetGPIO(ED: Boolean);
    function CLK_GetGPIO: Boolean;
	
    procedure CLK_SetTWI(IndexTWI: Byte; ED: Boolean);
    function CLK_GetTWI(IndexTWI: Byte): Boolean;

    procedure CLK_SetSPI(IndexSPI: Byte; ED: Boolean);
    function CLK_GetSPI(IndexSPI: Byte): Boolean;
  end;

implementation

const
  CCU_BASE = $01C20000;
  
class function TCCU.GetInstance: TCCU;
begin
  if FInstance = nil then FInstance:= TCCU.Create;
  Result:= FInstance;
end;

class procedure TCCU.Release;
begin
  FreeAndNil(FInstance);
end;


constructor TCCU.Create;
var Base: LongWord;
begin
  inherited Create;
  
  FCCU_BASE:= TA20.Instance.GetMMap(CCU_BASE);
  Base:= LongWord(FCCU_BASE) + TA20.Instance.BaseOffset(CCU_BASE);

  FPLL1_CFG:= Pointer(Base + $0000);
  FPLL1_TUN:= Pointer(Base + $0004);
  FPLL2_CFG:= Pointer(Base + $0008);
  FPLL2_TUN:= Pointer(Base + $000C);
  FPLL3_CFG:= Pointer(Base + $0010);
  FPLL4_CFG:= Pointer(Base + $0018);
  FPLL5_CFG:= Pointer(Base + $0020);
  FPLL5_TUN:= Pointer(Base + $0024);
  FPLL6_CFG:= Pointer(Base + $0028);
  FPLL6_TUN:= Pointer(Base + $002C);
  FPLL7_CFG:= Pointer(Base + $0030);
  FPLL1_TUN2:= Pointer(Base + $0038);
  FPLL5_TUN2:= Pointer(Base + $003C);
  FPLL8_CFG:= Pointer(Base + $0040);

  FOSC24M_CFG:= Pointer(Base + $0050);
  FCPU_AHB_APB0_CFG:= Pointer(Base + $0054);
  FAPB1_CLK_DIV:= Pointer(Base + $0058);
  FAHB_GATING0:= Pointer(Base + $0060);
  FAHB_GATING1:= Pointer(Base + $0064);
  FAPB0_GATING:= Pointer(Base + $0068);
  FAPB1_GATING:= Pointer(Base + $006C);

  FNAND_SCLK_CFG:= Pointer(Base + $0080);
  FMS_SCLK_CFG:= Pointer(Base + $0084);

  FSD0_CLK:= Pointer(Base + $0088);
  FSD1_CLK:= Pointer(Base + $008C);
  FSD2_CLK:= Pointer(Base + $0090);
  FSD3_CLK:= Pointer(Base + $0094);

  FTS_CLK:= Pointer(Base + $0098);
  FSS_CLK:= Pointer(Base + $009C);

  FSPI0_CLK:= Pointer(Base + $00A0);
  FSPI1_CLK:= Pointer(Base + $00A4);
  FSPI2_CLK:= Pointer(Base + $00A8);

  FIR0_CLK:= Pointer(Base + $00B0);
  FIR1_CLK:= Pointer(Base + $00B4);

  FIIS0_CLK:= Pointer(Base + $00B8);
  FAC97_CLK:= Pointer(Base + $00BC);
  FSPDIF_CLK:= Pointer(Base + $00C0);

  FKEYPAD_CLK:= Pointer(Base + $00C4);
  FSATA_CLK:= Pointer(Base + $00C8);
  FUSB_CLK:= Pointer(Base + $00CC);
  FSPI3_CLK:= Pointer(Base + $00D4);
  FIIS1_CLK:= Pointer(Base + $00D8);
  FIIS2_CLK:= Pointer(Base + $00DC);
  FDRAM_CLK:= Pointer(Base + $0100);

  FBE0_SCLK_CFG:= Pointer(Base + $0104);
  FBE1_SCLK_CFG:= Pointer(Base + $0108);
  FFE0_CLK:= Pointer(Base + $010C);
  FFE1_CLK:= Pointer(Base + $0110);

  FMP_CLK:= Pointer(Base + $0114);
  FLCD0_CH0_CLK:= Pointer(Base + $0118);
  FLCD1_CH0_CLK:= Pointer(Base + $011C);
  FCSI_SCLK:= Pointer(Base + $0120);
  FTVD_CLK:= Pointer(Base + $0128);
  FLCD0_CH1_CLK:= Pointer(Base + $012C);
  FLCD1_CH1_CLK:= Pointer(Base + $0130);
  FCSI0_CLK:= Pointer(Base + $0134);
  FCSI1_CLK:= Pointer(Base + $0138);

  FVE_CLK:= Pointer(Base + $013C);
  FAUDIO_CODEC_CLK:= Pointer(Base + $0140);
  FAVS_CLK:= Pointer(Base + $0144);
  FACE_CLK:= Pointer(Base + $0148);
  FLVDS_CLK:= Pointer(Base + $014C);
  FHDMI_CLK:= Pointer(Base + $0150);
  FMALI400_CLK:= Pointer(Base + $0154);
  FMBUS_SCLK_CFG:= Pointer(Base + $015C);
  FGMAC_CLK:= Pointer(Base + $0164);
  FHDMI1_RST:= Pointer(Base + $0170);
  FHDMI1_CTRL:= Pointer(Base + $0174);
  FHDMI1_SLOW_CLK:= Pointer(Base + $0178);
  FHDMI1_REPEAT_CLK:= Pointer(Base + $017C);

  FCLK_OUTA:= Pointer(Base + $01F0);
  FCLK_OUTB:= Pointer(Base + $01F4);

  FOSC24M_CFG^:= FOSC24M_CFG^ or (($1 shl 16) + ($1 shl 15) + ($1 shl 1) + $1);
end;

destructor TCCU.Destroy;
begin
  TA20.Instance.FreeMMap(FCCU_BASE);
  inherited Destroy;
end;

(******************************************************************************)
function TCCU.Get_CorePLL: LongWord;
var P, M, N, K: LongWord;
begin
  Result:= FPLL1_CFG^;

  P:= 1 shl ((Result shr 16) and $03);
  N:= (Result shr 8) and $1F;
  M:= ((Result shr 0) and $03) + 1;
  K:= ((Result shr 4) and $03) + 1;

  Result:= 24000000 * N * K div P div M;
end;

function TCCU.Get_AXI: LongWord;
var CLKSRC, Factor: LongWord;
begin
  Result:= FCPU_AHB_APB0_CFG^;

  CLKSRC:= (Result shr 16) and $03;
  Factor:= ((Result shr 0) and $03) + 1;

  case CLKSRC of
  0: Result:= 32000;
  1: Result:= 24000000;
  2: Result:= Get_CorePLL;
  else
    Result:= 0;
  end;
end;

function TCCU.Get_AHB: LongWord;
var Factor: LongWord;
begin
  Factor:= (FCPU_AHB_APB0_CFG^ shr 4) and $03;
  Result:= Get_AXI shr Factor;
end;

function TCCU.Get_APB0: LongWord;
var Factor: LongWord;
begin
  Factor:= (FCPU_AHB_APB0_CFG^ shr 8) and $03;

  if (Factor > 0) then
    Result:= Get_AHB shr Factor
  else
    Result:= Get_AHB shr 1;
end;

function TCCU.Get_APB1: LongWord;
var Factor: LongWord;
begin
  Result:= 0;

  Factor:= (FAPB1_CLK_DIV^ shr 24) and $03;
  if (Factor = 0) then Result:= 24000000;
end;


(******************************************************************************)
procedure TCCU.CLK_SetGPIO(ED: Boolean);
begin
  case ED of
  False: FAPB0_GATING^:= FAPB0_GATING^ and not ($1 shl 5);
  True: FAPB0_GATING^:= FAPB0_GATING^ or ($1 shl 5);
  end;
end;

function TCCU.CLK_GetGPIO: Boolean;
begin
  Result:= (FAPB0_GATING^ and ($1 shl 5) = 1); 
end;
	
procedure TCCU.CLK_SetTWI(IndexTWI: Byte; ED: Boolean);
var Bit: Byte;
begin
  case IndexTWI of
  0..3: Bit:= IndexTWI;
  4: Bit:= 15;
  end;

  case ED of
  False: FAPB1_GATING^:= FAPB1_GATING^ and not ($1 shl Bit);
  True: FAPB1_GATING^:= FAPB1_GATING^ or ($1 shl Bit);
  end;  
end;

function TCCU.CLK_GetTWI(IndexTWI: Byte): Boolean;
begin
  case IndexTWI of
  0..3: Result:= (FAPB1_GATING^ and ($1 shl IndexTWI) = 1);
  4: Result:= (FAPB1_GATING^ and ($1 shl 15) = 1);
  end;
end;

procedure TCCU.CLK_SetSPI(IndexSPI: Byte; ED: Boolean);
begin
  case ED of
  False: FAHB_GATING0^:= FAHB_GATING0^ and not ($1 shl (IndexSPI + 20));
  True: FAHB_GATING0^:= FAHB_GATING0^ or ($1 shl (IndexSPI + 20));
  end;
end;

function TCCU.CLK_GetSPI(IndexSPI: Byte): Boolean;
begin
  Result:= (FAHB_GATING0^ and ($1 shl (IndexSPI + 20)) = 1);
end;

finalization
  TCCU.Instance.Release;

end.
