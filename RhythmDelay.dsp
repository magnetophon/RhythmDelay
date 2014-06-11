
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
zita_rev_fdn(f1,f2,t60dc,t60m,fsmax) = component("effect.lib").zita_rev_fdn(f1,f2,t60dc,t60m,fsmax);

//-----------------------------------------------
// contants
//-----------------------------------------------
fsmax		= 44100;
DelMax		= 10*fsmax;  	//maximum delay time in samples
tapMax		= 4;		//maximum number of taps
NrChan		= 2;		//number of channels
noTapsMaxTime	= 3*fsmax;  	//maximum delay time for each tap. After this time we reset the nr of taps

//-----------------------------------------------
// the GUI
//-----------------------------------------------

mainGroup(x) 		= (vgroup("[0]RhythmDelay", x)); // To recieve OSC pitch and other messages
  tapGroup(x)  		= mainGroup(vgroup("[0tap", x));
    resetBtn		= tapGroup(button("[0]reset"):startPulse);
    tap			= tapGroup(button("[1]tap"):startPulse);
    morphGroup(x)	= tapGroup(hgroup("[3]morph", x));
      morphSliders 	= morphGroup(par(i, tapMax, (vslider("[%i]A/B %nr",	((i/tapMax)*-1)+1, 0, 1, 0.01):smooth(0.999)) with { nr = i+1;}))
      ;

  //smoothing is done in morph, 
  ABgroup(x)		= mainGroup(vgroup("[1]effects", x));
    Agroup(x)		= ABgroup((hgroup("[0]A", x)));
    Bgroup(x)		= ABgroup((hgroup("[1]B", x)));
      FXparams = environment {
	volume		= vslider("[0]volume",	-144, -144, 0, 0.1):db2linear;
	LPgroup(x)	= hgroup("[1]low pass", x);
	  lpFc		= LPgroup(vslider("[0]freq",	1, 0, 1, 0.001):pow(2)*20000+20);
	  lpQ		= LPgroup(vslider("[1]Q",	1, 0.5, 7, 0.1));
	reverbGroup(x)	= hgroup("[2]reverb", x);
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
// we have two sets of controls for the insert FX: A and B
// the actual settings for each tap are a linear interpolation between the two.
// each dekay-tap has a slider for each tap that controlls the interpolation

// todo make a knob for each control that controlls the offset between L and R (stereo only)
// todo use sdelay(N, it, dt) for smoot LR delay offset?
// todo AND/OR make A/B be midi velocity and offset be note-number

// reversed so that A will be up and B will be down
morph(A,B,tap) = B,A: interpolate(morphSliders:selector(tap,tapMax)):smooth(0.999);

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
//_*volume
resonlp(fc,Q,volume)
:reverb(f1,f2,t60dc,t60m,drywet)
with {
volume	= morph(Agroup(FXparams.volume),Bgroup(FXparams.volume),tap);
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

//-----------------------------------------------
// putting it all together
//-----------------------------------------------

MonoRhythmDelay = _<:par(tap, tapMax, ((_@time(tap):insertFX(tap)) * (((currenttap > tap+1) & (time(tap)<DelMax)):smooth(0.999)) )):>_
with { 
time(nr) = (timer(tapIsHigh(nr+2)));
};
//make tapMax parallel delaylines but only let each hear when  we have a tap with that number.and if the delaytime is smaller than max.

RhythmDelay = par(i, NrChan, MonoRhythmDelay); //the multichannel version

//process = insertFX(1);
process = RhythmDelay;