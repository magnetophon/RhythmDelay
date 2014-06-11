
declare name 		"RhythmDelay";
declare version 	"1.0";
declare author 		"Bart Brouns";
declare license 	"GNU 3.0";
declare copyright 	"(c) Bart Brouns 2014";
declare description 	"tap a rhythm on the tap button, an the delay will follow";
declare reference 	"inspired by the D-Two - Multitap Rhythm Delay by TC Electronic";
//-----------------------------------------------
// imports
//-----------------------------------------------

import ("filter.lib");


//-----------------------------------------------
// contants
//-----------------------------------------------
DelMax = 10*44100;  		//maximum delay time in samples
tapMax = 8;			//maximum number of taps
NrChan = 2;			//number of channels
noTapsMaxTime = 3*44100;  	//maximum delay time for each tap. After this time we reset the nr of taps

//-----------------------------------------------
// the GUI
//-----------------------------------------------

mainGroup(x)  = (vgroup("[0]RhythmDelay", x)); // To recieve OSC pitch and other messages
  tapGroup(x)  = mainGroup(vgroup("[0tap", x));
    resetBtn	=  tapGroup(button("[0]reset"):startPulse);
    tap		=  tapGroup(button("[1]tap"):startPulse);
    morphGroup(x)  = tapGroup(hgroup("[3]morph", x));
      morphSliders = morphGroup(par(i, tapMax, (vslider("[%i]A/B %nr",	((i/tapMax)*-1)+1, 0, 1, 0.01):smooth(0.999)) with { nr = i+1;}))
      ;

  //smoothing is done in morph, 
  ABgroup(x)  = mainGroup(vgroup("[1]effects", x));
    Agroup(x)	= ABgroup((hgroup("[0]A", x)));
      Avolume	= Agroup(vslider("[0]volume",	-144, -144, 0, 0.1)):db2linear;
      AlpFc	= Agroup(vslider("[1]lp freq",	1, 0, 1, 0.001):pow(2)*20000+20);
      AlpQ	= Agroup(vslider("[2]lp Q",	1, 0.5, 7, 0.001));
    Bgroup(x)	= ABgroup((hgroup("[0]B", x)));
      Bvolume	= Bgroup(vslider("[0]volume",	-144, -144, 0, 0.1)):db2linear;
      BlpFc	= Bgroup(vslider("[1]lp freq",	1, 0, 1, 0.001):pow(2)*20000+20);
      BlpQ	= Bgroup(vslider("[2]lp Q",	1, 0.5, 7, 0.001));
//-----------------------------------------------
// the morpher
//-----------------------------------------------
//we have two sets of controls for the insert FX, one for delay-time = 0 and one for dela-time = DelMax
// the actual settings for each tap are a linear interpolation between the two.

//todo adsr for interpolation AND/OR
//todo or make the two sets just be A and B, and have a slider for each tap that controlls the interpolation
// use interpolate(i) 
//todo make a knob for each control that controlls the offset between L and R (stereo only)
// use sdelay(N, it, dt) for smoot LR delay offset?
//todo AND/OR make A/B be midi velocity and offset be note-number

//reversed so that A will be up and B will be down
morph(A,B,tap) = B,A: interpolate(morphSliders:selector(tap,tapMax)):smooth(0.999);
//morphParams(i) = 

//-----------------------------------------------
// calculate the delay times
//-----------------------------------------------

SH(trig,x) = (*(1 - trig) + x * trig) ~_; //sample and hold "x" when "trig" is high

Reset = (SH(resetBtn|tap,resetBtn) | JustStarted | (tooLong & tap))//reset the currenttap counter. We stay "in reset mode" untill the first normal tap afterwards.
  with {
  JustStarted = (SH(resetBtn|tap,1)*-1)+1; // a bit of a hack to make sure we start the program in reset mode.
  tooLong = (timer(tap xor 1) > noTapsMaxTime); // also reset when there have been no taps for noTapsMaxTime samples.
  };

startPulse= _ <: _, mem: - : >(0); //one sample pulse at start

stopPulse=(_==0):startPulse; //one sample pulse at stop

timer(pulse) = (pulse,(+(1)~((min(DelMax)):(startPulse(pulse)==1,_,0:select2))<:SH(stopPulse(pulse)),_)):select2:max(0):min(DelMax); //how many samples is pulse high?

countUpReset(count, trig, reset)	= \(c). ((trig, c, min(count, c+1)):select2)~_*(reset==0); //for each "trig" count one step up until a maximum of "count". reset sets to 0

currenttap = countUpReset(tapMax+1, tap, Reset); //how many taps did we do?

tapIsHigh(N) = SH((Reset | startPulse(currenttap == N)),Reset)*((Reset*-1)+1); //the length of the Nth tap is how long "tapIsHigh(N) " is high

//-----------------------------------------------
// insert FX
//-----------------------------------------------

insertFX(tap) = 
resonlp(fc,Q,volume)
with {
volume	= morph(Avolume,Bvolume,tap);
fc 	= morph(AlpFc,BlpFc,tap);
Q 	= morph(AlpQ,BlpQ,tap);
};

//-----------------------------------------------
// putting it all together
//-----------------------------------------------

MonoRhythmDelay = _<:par(tap, tapMax, ((_@time(tap):insertFX(tap)) * (((currenttap > tap+1) & (time(tap)<DelMax)):smooth(0.999)) )):>_
with { 
time(nr) = (timer(tapIsHigh(nr+2)));
};
//make tapMax parallel delaylines but only let each hear when  we have a tap with that number.and if the delaytime is smaller than max.

RhythmDelay = par(i, NrChan, MonoRhythmDelay); //the multichannel version

process = RhythmDelay;