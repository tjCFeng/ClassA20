全志A20处理器底层操作的一些封装，采用free pascal，封装类。
使用方法：在工程中添加该类所在的文件夹路径即可。

一般每一个外设功能分为两部分封装，一种是直接对某一Pin或通道的操作类，实现对常用功能的封装；另一种是对该类型外设的寄存器操作类，用来对前者没有封装到的功能进行设置。

目前实现了对GPIO、LRADC、PWM的封装，后续功能陆续完成，并且正在尝试增加对中断的响应功能。

该类可用在控制台、有界面或无界面程序中均可，并且根据编译器的不同，也可用在不同的操作系统，只需很少的代码改动，如引用不同的单元等。

全志其他系列的处理器也可使用该类，如A10等，只需要根据差异的部分进行改动，或者继承重新实现。

作者：tjCFeng
邮箱：tjCFeng@163.com

例子：
1.TGPIOGROUP

[code]
uses GPIO;

var PHG: TGPIOGROUP;
begin
  PHG:= TGPIOGROUP.Create(PH); //创建
  PHG.GPIO_DAT^:= PHG.GPIO_DAT^ or ($1 shl 24); //设置寄存器的值
  PHG.Free; //释放
end;
[/code]

2.TGPIO

[code]
uses GPIO;

begin
  with TGPIO.Create(PH, 24) do
  begin
    Fun:= Fun1; //设置PH24为输出
    Data:= True; //设置高电平
    Sleep(1000);
    Reverse; //反转电平
    Free; //释放
  end;
end;
[/code]

或

[code]
var PH24: TGPIO;
begin
  PH24:= TGPIO.Create(PH, 24);
  PH24.Fun:= Fun1;
  PH24.Reverse;
  PH24.Free;
end;
[/code]

3.LRADC

[code]
uses LRADC;

var ADC0: TLRADC; Data: Byte;
begin
  ADC0:= TLRADC.Create(LRADC_0); //创建LRADC通道0
  TLRADCGROUP.Instance.ClearAllPending; //清除所有未决中断，单例中的功能
  ADC0.INTs:= [ADCDATA, KEYDOWN, KEYUP]; //设置需要响应的中断类型
  TLRADCGROUP.Instance.Start; //开启LRADC，单例中的功能
  Data:= ADC0.Data; //获取LRADC通道0的值0~64
  TLRADCGROUP.Instance.Stop; //停止LRADC，单例中的功能
  ADC0.Free; //释放
end;
[/code]

4.PWM

[code]
uses PWM;

var PWM1: TPWM;
begin
  PWM1:= TPWM.Create(PWM_1); //创建PWM通道1
  with PWM1 do
  begin
    Prescale:= P960; //设置预分频
    Cycle:= 6000; //设置周期计数
    Duty:= 1000;  //设置占空比计数
    Start; //开始PWM输出
    Sleep(3000);
    Stop; //停止PWM输出
    Free; //释放
  end;
end;
[/code]

5.Timer

[code]
uses Timer;

var Timer0: TTimer;
begin
  Timer0:= TTimer.Create(Timer_0);
  with Timer0 do
  begin
    Prescal:= Div4;
    CNT:= 6000000;
    CUR:= 0;
    Start;
    while not Timer0.INT do ;
    //执行到这里是1秒
    Stop;
    Free;
  end;
end;
[/code]

6.RTC

[code]
uses RTC;

var DT: TYMDHNSW;
begin
  with DT do
  begin
    Year:= 14;
    Month:= 10;
    Day:= 20;

    Hour:= 9;
    Minute:= 30;
    Second:= 0;

    Week:= Monday;
  end;
  TRTC.Instance.DateTime:= DT;

  FillChar(DT, SizeOf(TYMDHNSW), 0);
  DT:= TRTC.Instance.DateTime;
end;
[/code]

7.General Purpose

[code]
uses GP;

var Data: LongWord;
begin
  TGP.Instance.TMR_GP[0]^:= 123456789;
  Data:= TGP.Instance.TMR_GP[10]^;
end;
[/code]

8.TWI
[code]
uses TWI;

var TWI0: TTWI; Data: Byte;
begin
  TWI0:= TTWI.Create(TWI_0);
  TWI0.Write($34, $35, $83);
  TWI0.Read($34, $35, Data);
  TWI0.Free;
end;
[/code]

9.WatchDog
[code]
uses WatchDog;
begin
  TWDOG.Instance.Interval:= S16; //16秒间隔
  TWDOG.Instance.ForceRestart:= True; //未喂狗重启
  TWDOG.Instance.Start;
  TWDOG.Instance.Reset; //喂狗
  TWDOG.InstanceStop;
end;
[/code]

10.Temperature
[code]
uses TP;

var T: Double;
begin
  T:= TTemperature.Instance.Temperature;
end;
[/code]

历史版本：
2014.12.06 v0.8 增加Temperature封装类，修正所有的TGOURP类型为TGROUP，抱歉，手误拼写错误
2014.10.21 v0.7 增加WatchDog封装类
2014.10.20 v0.6 增加GP封装类
2014.10.18 v0.5 增加TWI封装类，修正部分bug
2014.10.16 v0.3 增加General Purpose封装类
2014.10.15 v0.3 增加RTC封装类，修正部分变量位定义的bug
2014.10.14 v0.2 增加Timer封装类
2014.10.03 v0.1 完成GPIO、LRADC、PWM的封装类
