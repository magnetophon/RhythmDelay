
declare name 		"RhythmDelay";
declare version 	"1.0";
declare author 		"Bart Brouns";
declare license 	"GNU 3.0";
declare copyright 	"(c) Bart Brouns 2014";
declare description 	"tap a rhythm on the Tap button, an the delay will follow";
declare reference 	"inspired by the D-Two - Multitap Rhythm Delay by TC Electronic";
//-----------------------------------------------
// imports
//-----------------------------------------------

import ("filter.lib");


//-----------------------------------------------
// contants
//-----------------------------------------------
DelMax = 10*44100;  	//maximum delay time in samples
TapMax = 8;		//maximum number of taps
NrChan = 2;		//number of channels
NoTapMax = 3*44100;  	//maximum delay time for each tap. After this time we reset the nr of taps

//-----------------------------------------------
// the GUI
//-----------------------------------------------

ResetBtn	=  button("[0]Reset"):startPulse;
Tap		= button("[1]Tap"):startPulse;

//-----------------------------------------------
// the DSP
//-----------------------------------------------

SH(trig,x) = (*(1 - trig) + x * trig) ~_; //sample and hold "x" when "trig" is high

Reset = (SH(ResetBtn|Tap,ResetBtn) | JustStarted | (tooLong & Tap))//reset the currentTap counter. We stay "in reset mode" untill the first normal tap afterwards.
  with {
  JustStarted = (SH(ResetBtn|Tap,1)*-1)+1; // a bit of a hack to make sure we start the program in reset mode.
  tooLong = (timer(Tap xor 1) > NoTapMax); // also reset when there have been no taps for NoTapMax samples.
  };

startPulse= _ <: _, mem: - : >(0); //one sample pulse at start

stopPulse=(_==0):startPulse; //one sample pulse at stop

timer(pulse) = (pulse,(+(1)~((min(DelMax)):(startPulse(pulse)==1,_,0:select2))<:SH(stopPulse(pulse)),_)):select2:max(0):min(DelMax); //how many samples is pulse high?

countUpReset(count, trig, reset)	= \(c). ((trig, c, min(count, c+1)):select2)~_*(reset==0); //for each "trig" count one step up until a maximum of "count". reset sets to 0

currentTap = countUpReset(TapMax, Tap, Reset); //how many taps did we do?

tapIsHigh(N) = SH((Reset | startPulse(currentTap == N)),Reset)*((Reset*-1)+1); //the length of the Nth tap is how long "tapIsHigh(N) " is high

MonoRhythmDelay = _<:par(i, TapMax, (_@(timer(tapIsHigh(i+2)))) * ((currentTap > i+1):smooth(0.999)) ):>_; //make TapMax parallel delaylines but only let each hear when  we have a tap with that number.

RhythmDelay = par(i, NrChan, MonoRhythmDelay); //the multichannel version

process = RhythmDelay;