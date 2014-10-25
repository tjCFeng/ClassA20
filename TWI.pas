(*
说明：全志A20的TWI底层操作封装类。
作者：tjCFeng
邮箱：tjCFeng@163.com
更新日期：2014.10.17
*)

unit TWI;

{$mode objfpc}{$H+}

interface

uses Unix, BaseUnix, SysUtils, A20, Clock, GPIO;

type
  TChannel = (TWI_0, TWI_1, TWI_2, TWI_3, TWI_4);
  TI2CSpeed = (I2C100K, I2C400K);

  TTWI = class
  private
    FChannel: TChannel;
    FSpeed: TI2CSpeed;
    FBus: Boolean;
    FAutoACK: Boolean;

    FSCK: TGPIO;
    FSDA: TGPIO;

    FTimeOut: LongWord;
    FTWI_BASE: ^LongWord;
  protected
    FTWI_ADDR: ^LongWord;  //TWI Slave address
    FTWI_XADDR: ^LongWord; //TWI Extended slave address
    FTWI_DATA: ^LongWord;  //TWI Data byte
    FTWI_CNTR: ^LongWord;  //TWI Control register
    FTWI_STAT: ^LongWord;  //TWI Status register
    FTWI_CCR: ^LongWord;   //TWI Clock control register
    FTWI_SRST: ^LongWord;  //TWI Software reset
    FTWI_EFR: ^LongWord;   //TWI Enhance Feature register
    FTWI_LCR: ^LongWord;   //TWI Line Control register
  private
    procedure SetSpeed(Value: TI2CSpeed);
    procedure SetBus(Value: Boolean);
    procedure SetAutoACK(Value: Boolean);
    function GetState(State: Byte): Boolean;
    
    function SendReset: Boolean;
    function SendStart: Boolean;
    function SendReStart: Boolean;
    function SendStop: Boolean;
    function SetByte(Dat: Byte): Boolean;
    function GetByte(out Dat: Byte): Boolean;
  public
    constructor Create(Channel: TChannel);
    destructor Destroy; override;

    function Write(Addr: Byte; Reg: Byte; Dat: Byte): Boolean;
    function Read(Addr: Byte; Reg: Byte; out Dat: Byte): Boolean;
  public
    property Speed: TI2CSpeed read FSpeed write SetSpeed;
    property Bus: Boolean read FBus write SetBus;
    property AutoACK: Boolean read FAutoACK write SetAutoACK;
  end;

implementation

const
  TWI_BASE = $01C2AC00;
  TWI_TIMEOUT = $FFFFFF;

const
  TWI_CNTR_ACK    = $04; //($1 shl 2);
  TWI_CNTR_INT    = $08; //($1 shl 3);
  TWI_CNTR_STP    = $10; //($1 shl 4);
  TWI_CNTR_STA    = $20; //($1 shl 5);
  TWI_CNTR_BUSEN  = $40; //($1 shl 6);
  TWI_CNTR_INTEN  = $80; //($1 shl 7);

const
  TWI_SRST_RESET = $1; //($1 shl 0);

const
  TWI_CCR_M = ($F shl 3);
  TWI_CCR_N = ($7 shl 0);

const
  TWI_STAT_BUS_ERR            = $00; // BUS ERROR
  (* Master mode use only *)
  TWI_STAT_TX_STA             = $08; // START condition transmitted
  TWI_STAT_TX_RESTA           = $10; // Repeated START condition transmitted
  TWI_STAT_TX_AW_ACK          = $18; // Address+Write bit transmitted, ACK received
  TWI_STAT_TX_AW_NAK          = $20; // Address+Write bit transmitted, ACK not received
  TWI_STAT_TXD_ACK            = $28; // data byte transmitted in master mode,ack received
  TWI_STAT_TXD_NAK            = $30; // data byte transmitted in master mode ,ack not received
  TWI_STAT_ARBLOST            = $38; // arbitration lost in address or data byte
  TWI_STAT_TX_AR_ACK          = $40; // Address+Read bit transmitted, ACK received
  TWI_STAT_TX_AR_NAK          = $48; // Address+Read bit transmitted, ACK not received
  TWI_STAT_RXD_ACK            = $50; // data byte received in master mode ,ack transmitted
  TWI_STAT_RXD_NAK            = $58; // date byte received in master mode,not ack transmitted
  (* Slave mode use only *)
  TWI_STAT_RXWS_ACK           = $60; // Slave address+Write bit received, ACK transmitted
  TWI_STAT_ARBLOST_RXWS_ACK   = $68;
  TWI_STAT_RXGCAS_ACK         = $70; // General Call address received, ACK transmitted
  TWI_STAT_ARBLOST_RXGCAS_ACK = $78;
  TWI_STAT_RXDS_ACK           = $80;
  TWI_STAT_RXDS_NAK           = $88;
  TWI_STAT_RXDGCAS_ACK        = $90;
  TWI_STAT_RXDGCAS_NAK        = $98;
  TWI_STAT_RXSTPS_RXRESTAS    = $A0;
  TWI_STAT_RXRS_ACK           = $A8;

  TWI_STAT_ARBLOST_SLAR_ACK   = $B0;

  (* 10bit Address, second part of address *)
  TWI_STAT_TX_SAW_ACK         = $D0; // Second Address byte+Write bit transmitted,ACK received
  TWI_STAT_TX_SAW_NAK         = $D8; // Second Address byte+Write bit transmitted,ACK not received

  TWI_STAT_IDLE               = $F8; // No relevant status infomation,INT_FLAG = 0

const
  TWI_LCR_SDA_EN    = $01; //($1 shl 0);
  TWI_LCR_SDA_CTL   = $02; //($1 shl 1);
  TWI_LCR_SCL_EN    = $04; //($1 shl 2);
  TWI_LCR_SCL_CTL   = $08; //($1 shl 3);
  TWI_LCR_SDA_STATE = $10; //($1 shl 4);
  TWI_LCR_SCL_STATE = $20; //($1 shl 5);

const
  TWI_LCR_DEFAULT = $3A;

constructor TTWI.Create(Channel: TChannel);
var Base: LongWord;
begin
  inherited Create;

  FChannel:= Channel;

  FTWI_BASE:= TA20.Instance.GetMMap(TWI_BASE + Ord(FChannel) * $400);
  Base:= LongWord(FTWI_BASE) + TA20.Instance.BaseOffset(TWI_BASE + Ord(FChannel) * $400);

  FTWI_ADDR:= Pointer(Base + $0000);
  FTWI_XADDR:= Pointer(Base + $0004);
  FTWI_DATA:= Pointer(Base + $0008);
  FTWI_CNTR:= Pointer(Base + $000C);
  FTWI_STAT:= Pointer(Base + $0010);
  FTWI_CCR:= Pointer(Base + $0014);
  FTWI_SRST:= Pointer(Base + $0018);
  FTWI_EFR:= Pointer(Base + $001C);
  FTWI_LCR:= Pointer(Base + $0020);
  
  case FChannel of
  TWI_0: begin
           FSCK:= TGPIO.Create(PB, 0);
           FSCK.Fun:= Fun2;
           FSDA:= TGPIO.Create(PB, 1);
           FSDA.Fun:= Fun2;
         end;
  TWI_1: begin
           FSCK:= TGPIO.Create(PB, 18);
           FSCK.Fun:= Fun2;
           FSDA:= TGPIO.Create(PB, 19);
           FSDA.Fun:= Fun2;
         end;
  TWI_2: begin
           FSCK:= TGPIO.Create(PB, 20);
           FSCK.Fun:= Fun2;
           FSDA:= TGPIO.Create(PB, 21);
           FSDA.Fun:= Fun2;
         end;
  TWI_3: begin
           FSCK:= TGPIO.Create(PI, 0);
           FSCK.Fun:= Fun3;
           FSDA:= TGPIO.Create(PI, 1);
           FSDA.Fun:= Fun3;
         end;
  TWI_4: begin
           FSCK:= TGPIO.Create(PI, 2);
           FSCK.Fun:= Fun3;
           FSDA:= TGPIO.Create(PI, 3);
           FSDA.Fun:= Fun3;
         end;
  end;

  TCCU.Instance.CLK_SetTWI(Ord(FChannel), True);
  SetSpeed(I2C100K);
  SetBus(True);
  SetAutoACK(True);
  FTWI_CNTR^:= FTWI_CNTR^ or ($1 shl 7);

  SendReset;
end;

destructor TTWI.Destroy;
begin
  SendReset;
  FSCK.Free;
  FSDA.Free;
  TA20.Instance.FreeMMap(FTWI_BASE);

  inherited Destroy;
end;

procedure TTWI.SetSpeed(Value: TI2CSpeed);
begin
  FSpeed:= Value;

  FTWI_CCR^:= 0;
  case FSpeed of
  I2C100K: FTWI_CCR^:= FTWI_CCR^ or (3 shl 3) or 3;
  I2C400K: FTWI_CCR^:= FTWI_CCR^ or (5 shl 3) or 0;
  end;

  SendReset;
end;

function TTWI.SetByte(Dat: Byte): Boolean;
begin
  FTimeOut:= TWI_TIMEOUT;
  FTWI_DATA^:= Dat;
  FTWI_CNTR^:= FTWI_CNTR^ and $F7; //Clear INT
  while ((FTWI_CNTR^ and TWI_CNTR_INT) = 0) and (FTimeOut > 0) do Dec(FTimeOut);
  Result:= (FTimeOut > 0);
end;

function TTWI.GetByte(out Dat: Byte): Boolean;
begin
  FTimeOut:= TWI_TIMEOUT;
  FTWI_CNTR^:= FTWI_CNTR^ and $F7; //Clear INT
  while ((FTWI_CNTR^ and TWI_CNTR_INT) = 0) and (FTimeOut > 0) do Dec(FTimeOut);
  Result:= (FTimeOut > 0);

  Dat:= FTWI_DATA^;
end;

procedure TTWI.SetBus(Value: Boolean);
begin
  FBus:= Value;
  case FBus of
  False: FTWI_CNTR^:= FTWI_CNTR^ and not TWI_CNTR_BUSEN;
  True: FTWI_CNTR^:= FTWI_CNTR^ or TWI_CNTR_BUSEN;
  end;
end;

procedure TTWI.SetAutoACK(Value: Boolean);
begin
  FAutoACK:= Value;
  case FAutoACK of
  False: FTWI_CNTR^:= FTWI_CNTR^ and not TWI_CNTR_ACK;
  True: FTWI_CNTR^:= FTWI_CNTR^ or TWI_CNTR_ACK;
  end;
end;

function TTWI.GetState(State: Byte): Boolean;
begin
  Result:= FTWI_STAT^ = State;
end;

function TTWI.SendReset: Boolean;
begin
  FTimeOut:= TWI_TIMEOUT;
  FTWI_SRST^:= FTWI_SRST^ or TWI_SRST_RESET;
  while ((FTWI_SRST^ and TWI_SRST_RESET) = TWI_SRST_RESET) and (FTimeOut > 0) do Dec(FTimeOut);
  Result:= (FTimeOut > 0);
end;

function TTWI.SendStart: Boolean;
begin
  SendReset;
  FTWI_CNTR^:= (FTWI_CNTR^ and $C0) or TWI_CNTR_STA;

  FTimeOut:= TWI_TIMEOUT;
  while ((FTWI_CNTR^ and TWI_CNTR_INT) = 0) and (FTimeOut > 0) do Dec(FTimeOut);
  if (FTimeOut = 0) then Exit(False);

  Result:= (FTWI_STAT^ = TWI_STAT_TX_STA);
end;

function TTWI.SendReStart: Boolean;
begin
  FTWI_CNTR^:= (FTWI_CNTR^ and $C0) or TWI_CNTR_STA;

  FTimeOut:= TWI_TIMEOUT;
  while ((FTWI_CNTR^ and TWI_CNTR_INT) = 0) and (FTimeOut > 0) do Dec(FTimeOut);
  if (FTimeOut = 0) then Exit(False);

  Result:= (FTWI_STAT^ = TWI_STAT_TX_RESTA);
end;

function TTWI.SendStop: Boolean;
begin
  FTWI_CNTR^:= (FTWI_CNTR^ and $C0) or TWI_CNTR_STP;

  FTimeOut:= TWI_TIMEOUT;
  while ((FTWI_CNTR^ and TWI_CNTR_STP) = TWI_CNTR_STP) and (FTimeOut > 0) do Dec(FTimeOut);
  Result:= (FTimeOut > 0);
end;

//INT_EN  BUS_EN  START  STOP  INT_FLAG  ACK  NOT  NOT
//7       6       5      4     3         2    1    0
function TTWI.Write(Addr: Byte; Reg: Byte; Dat: Byte): Boolean;
begin
  try
    FTWI_EFR^:= 0;
    FTimeOut:= TWI_TIMEOUT;
    while (not GetState(TWI_STAT_IDLE)) and (FTimeOut > 0) do Dec(FTimeOut);
    if (FTimeOut = 0) then Exit(False);

    if not SendStart then Exit(False);

    if not SetByte((Addr shl 1) or 0) then Exit(False);
    if not GetState(TWI_STAT_TX_AW_ACK) then Exit(False);

    if not SetByte(Reg) then Exit(False);
    if not GetState(TWI_STAT_TXD_ACK) then Exit(False);

    if not SetByte(Dat) then Exit(False);
    if not GetState(TWI_STAT_TXD_ACK) then Exit(False);

    Result:= True;
  finally
    SendStop;
  end;
end;

function TTWI.Read(Addr: Byte; Reg: Byte; out Dat: Byte): Boolean;
begin
  try
    FTWI_EFR^:= 0;
    FTimeOut:= TWI_TIMEOUT;
    while (not GetState(TWI_STAT_IDLE)) and (FTimeOut > 0) do Dec(FTimeOut);
    if (FTimeOut = 0) then Exit(False);

    if not SendStart then Exit(False);

    if not SetByte((Addr shl 1) or 0) then Exit(False);
    if not GetState(TWI_STAT_TX_AW_ACK) then Exit(False);

    if not SetByte(Reg) then Exit(False);
    if not GetState(TWI_STAT_TXD_ACK) then Exit(False);

    if not SendReStart then Exit(False);

    if not SetByte((Addr shl 1) or 1) then Exit(False);
    if not GetState(TWI_STAT_TX_AR_ACK) then Exit(False);

    Result:= GetByte(Dat);
    //if not GetState($58) then Exit(False);
  finally
    SendStop;
  end;
end;

end.

