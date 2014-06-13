RhythmDelay
===========

Tap a rhythm into your delay!

You won't hear anything if you don't ;)

Each tap has the following insert-effects:
* resonant high pass filter
* resonant low-pass filter
* reverb
* stereo-width

There are two sets of controls for the insert FX: A and B; and each tap has a fader to morph between those settings.

Each tap also has a panner, independent from the A/B fader.


This panner is not turning up and down the level of left and right, but slightly delaying one of them.
It's an interesting sounding way to pan things, because in real life we get our directional information from both level and delay times.
This panning technique is normally not used a lot because it gives phasing issues when summed to mono, but that doesn't matter much here since it only affects the delay.


Finally there is a feedback-unit. It behaves as you would expect: turn up the amount and the pattern repeats.
It has its own insert-effects, minus the panner, so you can have each repeat cycle sound different.
Be careful with long reverb times on the feedback tap and high feedback amounts, or you'll get nasty squeals!


This effect is wet-only, stereo in and out.
It's quite CPU-heavy , as it contains four high quality stereo reverbs.
The sounds it produces are worth it though.

If you want more taps, just change nrTaps on line 20 from 4 to something else.
If you want a version without the cpu-hungry reverbs, comment out line 131; the one that says: 

:reverb(f1,f2,t60dc,t60m,drywet)

Have fun!

