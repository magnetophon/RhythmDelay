
declare name 		"RhythmDelay";
declare version 	"0.1";
declare author 		"Bart Brouns";
declare license 	"GNU 3.0";
declare copyright 	"(c) Bart Brouns 2014";
//declare credits		"";

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

//-----------------------------------------------
// the GUI
//-----------------------------------------------

ResetBtn =  button("[0]Reset"):startPulse;
Tap = button("[1]Tap"):startPulse;

//-----------------------------------------------
// the DSP
//-----------------------------------------------

SH(trig,x) = (*(1 - trig) + x * trig) ~_;

Reset = SH(ResetBtn|Tap,ResetBtn);

startPulse= _ <: _, mem: - : >(0); //one sample high pulse at start

stopPulse=(_==0):startPulse; //one sample high pulse at stop

timer(pulse) = (pulse,(+(1)~((%(DelMax)):(startPulse(pulse)==1,_,0:select2))<:SH(stopPulse(pulse)),_)):select2:max(0):min(DelMax); //how many samples is pulse high?

countUpReset(count, trig, reset)	= \(c). ((trig, c, min(count, c+1)):select2)~_*(reset==0); //for each "trig" count one step up until "count". reset sets to 0

currentTap = countUpReset(TapMax, Tap, Reset); //how many taps did we do?

tapIsHigh(N) = SH((Reset | startPulse(currentTap == N)),Reset)*((Reset*-1)+1); //the length of the Nth tap is how long "tapIsHigh(N) " is one

MonoRhythmDelay(taps) = _<:par(i, taps, (_@(timer(tapIsHigh(i+2)))) * ((currentTap > i+1):smooth(0.999)) ):>_; //make TapMax parallel delaylines but only let each hear when  we have a tap with that number.

RhythmDelay(taps,chans) = par(i, chans, MonoRhythmDelay(taps));

process = RhythmDelay(TapMax,NrChan);