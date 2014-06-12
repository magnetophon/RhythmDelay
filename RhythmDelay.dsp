
declare name 		"RhythmDelay";
declare version 	"1.1";
declare author 		"Bart Brouns";
declare license 	"GNU 3.0";
declare copyright 	"(c) Bart Brouns 2014";
declare description 	"tap a rhythm on the tap button, an the delay will follow";
declare reference 	"inspired by the D-Two - Multitap Rhythm Delay by TC Electronic";
//-----------------------------------------------
// imports
//-----------------------------------------------

import ("filter.lib");
zita_rev_fdn(f1,f2,t60dc,t60m,fsmax) = component("effect.lib").zita_rev_fdn(f1,f2,t60dc,t60m,fsmax);

//-----------------------------------------------
// contants
//-----------------------------------------------
fsmax		= 44100;
NrChan		= 2;			//number of channels
tapNrMax		= 4;			//maximum number of taps
TapTimeMax	= 3*fsmax;  		//maximum delay time for each tap. After this time we reset the nr of taps
DelMax		= TapTimeMax*tapNrMax;  	//maximum delay time in samples
//-----------------------------------------------
// the GUI
//-----------------------------------------------

mainGroup(x) 		= (vgroup("[0]RhythmDelay[tooltip: RhythmDelay by magnetophon]", x)); // To recieve OSC pitch and other messages
  tapGroup(x)  		= mainGroup(vgroup("[0tap", x));
    tap			= tapGroup(button("[0]tap[tooltip: tap a rhythm]"):startPulse);
    resetBtn		= tapGroup(button("[1]reset[tooltip: reset the rhythm]"):startPulse);
    morphGroup(x)	= tapGroup(hgroup("[2]morph[tooltip: morph between settings A and B]", x));
      morphSliders 	= morphGroup(par(i, tapNrMax, (vslider("[%i]A/B tap %nr[tooltip: on which point between A and B is tap %nr]",	((i/(tapNrMax-1))*-1)+1, 0, 1, 0.01)) with { nr = i+1;}));
      feedbackMorph	= morphGroup(vslider("[%tapNrMax]A/B feedback[tooltip: on which point between A and B is feedback tap]",	0, 0, 1, 0.01));

  //smoothing is done in morph, 
  ABgroup(x)		= mainGroup(vgroup("[1]insert effects[tooltip: independant insert effects on each tap]", x));
    Agroup(x)		= ABgroup((hgroup("[0]A[tooltip: settings A]", x)));
    Bgroup(x)		= ABgroup((hgroup("[1]B[tooltip: settings B]", x)));
      FXparams = environment {
	level		= vslider("[0]level [unit:dB][tooltip: the volume level]",0, -144, 0, 0.1):db2linear;
	LPgroup(x)	= hgroup("[1]low pass[unit:Hz][tooltip: resonant low-pass filter]", x);
	  lpFc		= LPgroup(vslider("[0]freq[tooltip: lp-filter cutoff frequency]", 0.49*fsmax, 20, 0.49*fsmax, 1));
	  lpQ		= LPgroup(vslider("[1]Q[tooltip: lp-filter resonance]",	1, 0.5, 7, 0.1));
	reverbGroup(x)	= hgroup("[2]reverb[tooltip: zita-rev1 by Fons Adriaensen]", x);
	  f1		= reverbGroup(vslider("[0] LF X [unit:Hz] 
		  [tooltip: Crossover frequency (Hz) separating low and middle frequencies]",
		  200, 50, 1000, 1));
	  t60dc		= reverbGroup(vslider("[1] Low RT60 [unit:s] 
		  [tooltip: T60 = time (in seconds) to decay 60dB in low-frequency band]",
		  3, 0.1, 8, 0.1));
	  t60m		= reverbGroup(vslider("[2] Mid RT60 [unit:s] 
		  [tooltip: T60 = time (in seconds) to decay 60dB in middle band]",
		  2, 0.1, 8, 0.1));
	  f2		= reverbGroup(vslider("[3] HF Damping [unit:Hz] 
		  [tooltip: Frequency (Hz) at which the high-frequency T60 is half the middle-band's T60]",
		  6000, 1500, 0.49*fsmax, 1));
	  drywet 		= reverbGroup(vslider("[4] Dry/Wet Mix 
		  [tooltip: -1 = dry, 1 = wet]",
		  0, -1.0, 1.0, 0.01));
      };

//-----------------------------------------------
// the morpher
//-----------------------------------------------
// Each tap has the following insert-effects:
// a low-pass filter
// a reverb

// There are two sets of controls for the insert FX: A and B.
// The actual settings for each tap are a linear interpolation between the two.
// Each delay-tap has a slider controlling the interpolation.

// todo make a knob for each control that controlls the offset between L and R (stereo only)
// todo use sdelay(N, it, dt) for smoot LR delay offset?
// todo AND/OR make A/B be midi velocity and offset be note-number
// todo make another morph slider + effects set, for feedback from time(currenttap)

// reversed so that A will be up and B will be down
morph(A,B,tap) = B,A: interpolate(morphSliders:selector(tap,tapNrMax)):smooth(0.999);

//-----------------------------------------------
// calculate the delay times
//-----------------------------------------------

SH(trig,x) = (*(1 - trig) + x * trig) ~_; //sample and hold "x" when "trig" is high

Reset = (SH(resetBtn|tap,resetBtn) | JustStarted | (tooLong & tap))//reset the currenttap counter. We stay "in reset mode" untill the first normal tap afterwards.
  with {
  JustStarted = (SH(resetBtn|tap,1)*-1)+1; // a bit of a hack to make sure we start the program in reset mode.
  tooLong = (timer(tap xor 1) > TapTimeMax); // also reset when there have been no taps for TapTimeMax samples.
  };

startPulse= _ <: _, mem: - : >(0); //one sample pulse at start

stopPulse=(_==0):startPulse; //one sample pulse at stop

timer(pulse) = (pulse,(+(1)~((min(DelMax)):(startPulse(pulse)==1,_,0:select2))<:SH(stopPulse(pulse)),_)):select2:max(0):min(DelMax); //how many samples is pulse high?

countUpReset(count, trig, reset)	= \(c). ((trig, c, min(count, c+1)):select2)~_*(reset==0); //for each "trig" count one step up until a maximum of "count". reset sets to 0

currenttap = countUpReset(tapNrMax+1, tap, Reset); //how many taps did we do?

tapIsHigh(N) = SH((Reset | startPulse(currenttap == N)),Reset)*((Reset*-1)+1); //the length of the Nth tap is how long "tapIsHigh(N) " is high

//-----------------------------------------------
// insert FX
//-----------------------------------------------

insertFX(tap) = 
//_*level
resonlp(fc,Q,level)
:reverb(f1,f2,t60dc,t60m,drywet)
with {
level	= morph(Agroup(FXparams.level),Bgroup(FXparams.level),tap);
fc 	= morph(Agroup(FXparams.lpFc),Bgroup(FXparams.lpFc),tap);
Q 	= morph(Agroup(FXparams.lpQ),Bgroup(FXparams.lpQ),tap);
f1	= morph(Agroup(FXparams.f1),Bgroup(FXparams.f1),tap);
f2	= morph(Agroup(FXparams.f2),Bgroup(FXparams.f2),tap);
t60dc	= morph(Agroup(FXparams.t60dc),Bgroup(FXparams.t60dc),tap):max(0.1):min(8);
t60m	= morph(Agroup(FXparams.t60m),Bgroup(FXparams.t60m),tap):max(0.1):min(8);
drywet	= morph(Agroup(FXparams.drywet),Bgroup(FXparams.drywet),tap);
};


reverb(f1,f2,t60dc,t60m,drywet,x) = x:zita_distrib(N): zita_rev_fdn(f1,f2,t60dc,t60m,fsmax) : output(N): dry_wet(x)
with {
  N = 8;
  zita_distrib(N) = _<:_,_*-1<:  fanflip(N) with {
    fanflip(4) = _,_,*(-1),*(-1);
    fanflip(N) = fanflip(N/2),fanflip(N/2);
  };
  output(N) = outmix(N) :> *(t1);
  t1 = 0.37; // zita-rev1 linearly ramps from 0 to t1 over one buffer
  outmix(4) = !,butterfly(2),!; // probably the result of some experimenting!
  outmix(N) = outmix(N/2),par(i,N/2,!);
  dry_wet(x) = *(wet) + dry*x with {
    wet = 0.5*(drywet+1.0);
    dry = 1.0-wet;
  };
};
//BU:
//MonoRhythmDelay(x) = ((_,x:+@time(currenttap))~_<:par(tap, tapNrMax, ((_@time(tap):insertFX(tap)) * (((currenttap > tap+1) & (time(tap)<DelMax)):smooth(0.999)) ))):>_

//-----------------------------------------------
// putting it all together
//-----------------------------------------------
//MonoRhythmDelay(x) = ((_,x:+@time(currenttap))~_<:par(tap, tapNrMax, ((_@time(tap):insertFX(tap)) * (((currenttap > tap+1) & (time(tap)<DelMax)):smooth(0.999)) ))):>_
//MonoRhythmDelay = (+~_@time(currenttap)),(FBtime:vbargraph("time", 0, DelMax)),(FBhigh:vbargraph("FBhigh", 0, 2))

//MonoRhythmDelay(x) = par(tap, tapNrMax,time(tap))

//MonoRhythmDelay(x) = (_,x:+)~_@10
//\(c). 
//feedbackMorph
//MonoRhythmDelay = (+<:(((_@time(currenttap))~_),par(tap, tapNrMax, ((_@time(tap):insertFX(tap)) * (((currenttap > tap+1) & (time(tap)<DelMax)):smooth(0.999)) )))):>_


MonoRhythmDelay(x) = (_,x:+)~(_@FBtime*0.1):(_<:par(tap, tapNrMax, ((_@time(tap):insertFX(tap)) * (((currenttap > tap+1) & (time(tap)<DelMax)):smooth(0.999)) ))):>_
with { 
time(nr) = timer(tapIsHigh(nr+2));
TapIsOK(nr) =((currenttap > nr+1) & (time(nr)<DelMax));
maximum(1) = _;
maximum(2) = max;
maximum(N) = maximum(int(N/2)),maximum(int((N+1)/2)):max;
FBtime = par(tap, tapNrMax,(time(tap)*TapIsOK(tap))):maximum(tapNrMax):vbargraph("del", 0, DelMax);
};
//make tapNrMax parallel delaylines but only let each hear when  we have a tap with that number.and if the delaytime is smaller than max.

RhythmDelay = par(i, NrChan, MonoRhythmDelay); //the multichannel version





process =MonoRhythmDelay;
//mcount = 10;
//trig= tap;
//reset = Reset;
//process(c) = countUpReset(mcount, trig, reset);