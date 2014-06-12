
declare name 		"RhythmDelay";
declare version 	"1.2";
declare author 		"Bart Brouns";
declare license 	"GNU 3.0";
declare copyright 	"(c) Bart Brouns 2014";
declare description 	"tap a rhythm on the tap button, an the delay will follow";
declare reference 	"inspired by the D-Two - Multitap Rhythm Delay by TC Electronic";
//-----------------------------------------------
// imports
//-----------------------------------------------

import ("effect.lib");

//-----------------------------------------------
// contants
//-----------------------------------------------
sampleRate		= 44100;
nrChans		= 2;			//number of channels
nrTaps		= 4;			//maximum number of taps
maxTapTime	= 3*sampleRate;  		//maximum delay time for each tap. After this time we reset the nr of taps
maxDelTime	= maxTapTime*nrTaps;  	//maximum delay time in samples
panTime		= 0.04;			//maximum panner delay time in seconds
//maximum panner delay time in samples,expressed as a power of 2
//maxPanSamples	= (_<:((_ < (sampleRate*panTime)),_*1,_*2:select2))~max(2:pow(10));
maxPanSamples	= 8192; //has to be a number.not an expression. It equals 42 ms at 192kHz
maxPanDelay	= panTime*sampleRate:min(maxPanSamples); //maximum panner delay time in samples
//-----------------------------------------------
// the GUI
//-----------------------------------------------

mainGroup(x) 		= (hgroup("[0]RhythmDelay[tooltip: RhythmDelay by magnetophon]", x));
  tapGroup(x)  		= mainGroup(vgroup("[0tap", x));
    tap			= tapGroup(button("[0]tap[tooltip: tap a rhythm]"):startPulse);
    resetBtn		= tapGroup(button("[1]reset[tooltip: reset the rhythm]"):startPulse);
    morphGroup(x)	= tapGroup(hgroup("[2]morph[tooltip: morph between settings A and B]", x));
      morphSliders 	= morphGroup(par(i, nrTaps, ABpanGroup(
	(hslider("[0]pan[tooltip: move tap %nr left to right] [style:knob]",	0, -1, 1, 0.01)
	,(vslider("[1]A/B[tooltip: on which point between A and B is tap %nr]",	((i/(nrTaps-1))*-1)+1, 0, 1, 0.01)))
      )with { 
	nr = i+1;
	ABpanGroup(x)	= vgroup("[%i]tap %nr[tooltip: the settings for tap %nr]", x);
      }):interleave(nrChans,nrTaps));
    feedbackGroup(x)	= tapGroup(hgroup("[%nr]feedback[tooltip: morph between settings A and B]", x))with {nr = 1+nrTaps;};
      feedback		= feedbackGroup(vslider("[0]amount[tooltip: the amount of feedback]",0.5, 0, 1, 0.01));
      feedbackMorph	= feedbackGroup(vslider("[1]A/B[tooltip: on which point between A and B is the feedback tap]",	0, 0, 1, 0.01))with {nr = 2+nrTaps;};

  //smoothing is done in morph, 
  ABgroup(x)		= mainGroup(vgroup("[1]insert effects[tooltip: independant insert effects on each tap]", x));
    Agroup(x)		= ABgroup((hgroup("[0]A[tooltip: settings A]", x)));
    Bgroup(x)		= ABgroup((hgroup("[1]B[tooltip: settings B]", x)));
      FXparams = environment {
	level		= vslider("[0]level [unit:dB][tooltip: the volume level]",0, -144, 0, 0.1):db2linear;
	HPgroup(x)	= hgroup("[1]high pass[unit:Hz][tooltip: resonant high-pass filter]", x);
	  hpFc		= HPgroup(vslider("[0]freq[tooltip: hp-filter cutoff frequency]", 20, 20, 0.49*sampleRate, 1));
	  hpQ		= HPgroup(vslider("[1]Q[tooltip: hp-filter resonance]",	1, 0.5, 7, 0.1));
	LPgroup(x)	= hgroup("[2]low pass[unit:Hz][tooltip: resonant low-pass filter]", x);
	  lpFc		= LPgroup(vslider("[0]freq[tooltip: lp-filter cutoff frequency]", 0.49*sampleRate, 20, 0.49*sampleRate, 1));
	  lpQ		= LPgroup(vslider("[1]Q[tooltip: lp-filter resonance]",	1, 0.5, 7, 0.1));
	reverbGroup(x)	= hgroup("[3]reverb[tooltip: zita-rev1 by Fons Adriaensen]", x);
	  f1		= reverbGroup(vslider("[0] lf X [unit:Hz] 
		  [tooltip: Crossover frequency (Hz) separating low and middle frequencies]",
		  200, 50, 1000, 1));
	  t60dc		= reverbGroup(vslider("[1] low RT60 [unit:s] 
		  [tooltip: T60 = time (in seconds) to decay 60dB in low-frequency band]",
		  3, 0.1, 8, 0.1));
	  t60m		= reverbGroup(vslider("[2] mid RT60 [unit:s] 
		  [tooltip: T60 = time (in seconds) to decay 60dB in middle band]",
		  2, 0.1, 8, 0.1));
	  f2		= reverbGroup(vslider("[3] hf damping [unit:Hz] 
		  [tooltip: Frequency (Hz) at which the high-frequency T60 is half the middle-band's T60]",
		  6000, 1500, 0.49*sampleRate, 1));
	  drywet 		= reverbGroup(vslider("[4] dry/wet 
		  [tooltip: -1 = dry, 1 = wet]",
		  -1, -1.0, 1.0, 0.01));
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
tapInterpol(tap) = (morphSliders:selector(tap+nrTaps,nrTaps*2));
morph(A,B,i) = B,A: interpolate(i):smooth(0.999);

//-----------------------------------------------
// calculate the delay times
//-----------------------------------------------

SH(trig,x) = (*(1 - trig) + x * trig) ~_; //sample and hold "x" when "trig" is high

Reset = (SH(resetBtn|tap,resetBtn) | JustStarted | (tooLong & tap))//reset the currenttap counter. We stay "in reset mode" untill the first normal tap afterwards.
  with {
  JustStarted = (SH(resetBtn|tap,1)*-1)+1; // a bit of a hack to make sure we start the program in reset mode.
  tooLong = (timer(tap xor 1) > maxTapTime); // also reset when there have been no taps for maxTapTime samples.
  };

startPulse= _ <: _, mem: - : >(0); //one sample pulse at start

stopPulse=(_==0):startPulse; //one sample pulse at stop

timer(pulse) = (pulse,(+(1)~((min(maxDelTime)):(startPulse(pulse)==1,_,0:select2))<:SH(stopPulse(pulse)),_)):select2:max(0):min(maxDelTime); //how many samples is pulse high?

countUpReset(count, trig, reset)	= \(c). ((trig, c, min(count, c+1)):select2)~_*(reset==0); //for each "trig" count one step up until a maximum of "count". reset sets to 0

currenttap = countUpReset(nrTaps+1, tap, Reset); //how many taps did we do?

tapIsHigh(N) = SH((Reset | startPulse(currenttap == N)),Reset)*((Reset*-1)+1); //the length of the Nth tap is how long "tapIsHigh(N) " is high

//-----------------------------------------------
// insert FX
//-----------------------------------------------

insertFX(i) =
  par(chan,nrChans,(resonhp(hpFc,hpQ,1)  : resonlp(lpFc,lpQ,level)) )
  :reverb(f1,f2,t60dc,t60m,drywet)
with {
  level		= morph(Agroup(FXparams.level),Bgroup(FXparams.level),i);
  lpFc 		= morph(Agroup(FXparams.lpFc),Bgroup(FXparams.lpFc),i);
  lpQ 		= morph(Agroup(FXparams.lpQ),Bgroup(FXparams.lpQ),i);
  hpFc 		= morph(Agroup(FXparams.hpFc),Bgroup(FXparams.hpFc),i);
  hpQ 		= morph(Agroup(FXparams.hpQ),Bgroup(FXparams.hpQ),i);
  f1		= morph(Agroup(FXparams.f1),Bgroup(FXparams.f1),i);
  f2		= morph(Agroup(FXparams.f2),Bgroup(FXparams.f2),i);
  t60dc		= morph(Agroup(FXparams.t60dc),Bgroup(FXparams.t60dc),i):max(0.1):min(8);
  t60m		= morph(Agroup(FXparams.t60m),Bgroup(FXparams.t60m),i):max(0.1):min(8);
  drywet	= morph(Agroup(FXparams.drywet),Bgroup(FXparams.drywet),i);
};

panDelay(tap)	= sdelay(maxPanSamples, 1024, Ldt),sdelay(maxPanSamples, 1024, Rdt)
with {
  pan		= (morphSliders:selector(tap,nrTaps*2)):smooth(0.999);
  Rdt		= (pan:min(0))*maxPanDelay:*(-1);
  Ldt		= (pan:max(0))*maxPanDelay;
};

reverb(f1,f2,t60dc,t60m,drywet,x,y) = x,y:zita_distrib2(N): zita_rev_fdn(f1,f2,t60dc,t60m,sampleRate) : output2(N): dry_wet(x,y)
with {
  N = 8;
  /*zita_distrib(N) = _<:_,_*-1<:  fanflip(N) 
  with {
    fanflip(4) = _,_,*(-1),*(-1);
    fanflip(N) = fanflip(N/2),fanflip(N/2);
  };*/
  output2(N) = outmix(N) : *(t1),*(t1);
  t1 = 0.37; // zita-rev1 linearly ramps from 0 to t1 over one buffer
  outmix(4) = !,butterfly(2),!; // probably the result of some experimenting!
  outmix(N) = outmix(N/2),par(i,N/2,!);
  dry_wet(x,y) = *(wet) + dry*x, *(wet) + dry*y with {
    wet = 0.5*(drywet+1.0);
    dry = 1.0-wet;};
  /*dry_wet(x) = *(wet) + dry*x 
  with {
    wet = 0.5*(drywet+1.0);
    dry = 1.0-wet;
    };
   */ 
};

//-----------------------------------------------
// putting it all together
//-----------------------------------------------
//the multichannel version
//RhythmDelay = par(i, nrChans, MonoRhythmDelay);
//mono FB, no FX:
//feedbacker = (_,_*TapIsOK(tap):+)~(_@FBtime*feedback:max(-4):min(4):compressor_mono(100,-18,0,100));

/*
MonoRhythmDelay = feedbacker : (_<:par(tap, nrTaps, ((_@time(tap):insertFX(tapInterpol(tap))) * (TapIsOK(tap):smooth(0.999)) ))):>_
with { 
time(nr) = timer(tapIsHigh(nr+2));
TapIsOK(nr) =((currenttap > nr+1) & (time(nr)<maxDelTime));
maximum(1) = _;
maximum(2) = max;
maximum(N) = maximum(int(N/2)),maximum(int((N+1)/2)):max;
FBtime = par(tap, nrTaps,(time(tap)*TapIsOK(tap))):maximum(nrTaps);
feedbacker = (_,_*TapIsOK(tap):+)~(_@FBtime*feedback:insertFX(feedbackMorph):max(-4):min(4):compressor_mono(100,-18,0,100));
};
*/
//make nrTaps parallel delaylines but only let each hear when  we have a tap with that number.and if the delaytime is smaller than max.
RhythmDelay = feedbacker:(bus(nrChans) <: par(tap, nrTaps, par(chan,nrChans,(_@time(tap))):insertFX(tapInterpol(tap))):par(tap, nrTaps,panDelay(tap):par(chan,nrChans,(_ * (TapIsOK(tap):smooth(0.999)) ))):>bus(nrChans))
  with { 
  time(nr) = timer(tapIsHigh(nr+2));
  TapIsOK(nr) = ((currenttap > nr+1) & (time(nr)<maxDelTime));
  maximum(1) = _;
  maximum(2) = max;
  maximum(N) = maximum(int(N/2)),maximum(int((N+1)/2)):max;
  FBtime = par(tap, nrTaps,(time(tap)*TapIsOK(tap))):maximum(nrTaps);
  feedbacker = (interleave(nrChans,nrChans):par(chan,nrChans,(_,_:+)))~(insertFX(feedbackMorph):par(chan,nrChans,(_@FBtime*feedback:max(-4):min(4):compressor_mono(100,-18,0,100))));
  };

process = RhythmDelay;
//panDelay(3);
//RhythmDelay;

//bus(nrChans) <: par(tap, nrTaps, par(chan,nrChans,(_@mtime(tap))):insertFX(tapInterpol(tap))):>par(chan,nrChans,(_ * (TapIsOK(3):smooth(0.999)) )):>bus(nrChans);

//bus(nrChans) <: par(tap, nrTaps, par(chan,nrChans,(_@mtime(tap))):insertFX(tapInterpol(tap))):>par(chan,nrChans,(_ * (TapIsOK(3):smooth(0.999)) )):>bus(nrChans);

//bus(nrChans) <: par(tap, nrTaps, par(chan,nrChans,(_@mtime(tap))):insertFX(tapInterpol(tap)));
//RhythmDelay(nrChans);
//RhythmDelay(nrTaps);
//mcount = 10;
//trig= tap;
//reset = Reset;
//process(c) = countUpReset(mcount, trig, reset);