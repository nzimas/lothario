// version 0.0.3
//
// Engine_Lothario extends the original Carter's Delay engine by adding
// randomised effects to each granular reflection.  The probability of an
// effect being applied and the random parameters are controlled via OSC
// messages from the Lua script.  See lothario.lua for details.

Engine_Lothario : CroneEngine {
    var oscs, kernel, b, timer, micBus, ptrBus, panBus, cutBus, resBus,
    micGrp, ptrGrp, recGrp, granGrp, panLFOs, cutoffLFOs, resonanceLFOs,
    rates, durs, delays, a, g, h, i;
    var fb;
    // probability that a grain will receive an effect (0.0 – 1.0)
    var fxProb;
    // range parameters for each effect
    var pitchMin, pitchMax;
    var ringMin, ringMax;
    var rateMin, rateMax;
    var bitMin, bitMax;

    *new { arg context, doneCallback;
        ^super.new(context, doneCallback);
    }

    alloc {
        var s = context.server;
        "alloc".postln;
        oscs = Dictionary.new();
        timer = LinkClock.new(2).latency_(s.latency).quantum_(0);
        // allocate 512 beats worth of buffer for recording
        b = Buffer.alloc(s, s.sampleRate * (timer.beatDur*512), 1);
        micBus = Bus.audio(s, 1);
        ptrBus = Bus.audio(s, 1);
        SynthDef(\mic, {
            arg in = 0, out = 0, amp = 1;
            var sig;
            sig = SoundIn.ar(in) * amp;
            Out.ar(out, sig);
        }).add;
        SynthDef(\ptr, {
            arg out = 0, buf = 0, rate = 1;
            var sig;
            sig = Phasor.ar(0, BufRateScale.kr(buf)*rate, 0, BufFrames.kr(buf));
            Out.ar(out, sig);
        }).add;
        SynthDef(\rec, {
            arg ptrIn = 0, micIn = 0, buf = 0, preLevel = 0;
            var ptr, sig;
            ptr = In.ar(ptrIn, 1);
            sig = In.ar(micIn, 1);
            sig = sig + (BufRd.ar(1, buf, ptr) * preLevel);
            BufWr.ar(sig, buf, ptr);
        }).add;
        // downmixing feedbacking saturating filtering patchcord
        SynthDef(\fbPatchMix, {
            arg in=0, out=0, amp=0, balance=0, hpFreq=12,
                noiseLevel=0.0, sineLevel=0, sineHz=55;
            var input = InFeedback.ar(in, 2);
            var output;
            output = Balance2.ar(input[0], input[1], balance);
            output = output + (PinkNoise.ar * noiseLevel);
            output = output + (SinOsc.ar(sineHz) * sineLevel);
            output = HPF.ar(output, hpFreq);
            output = output.softclip;
            Out.ar(out, output * amp);
        }).add;
        // grain synth with randomised effects per grain
        SynthDef(\gran, {
            arg amp = 0.5, buf = 0, out = 0,
                atk = 1, rel = 1, gate = 1,
                sync = 1, dens = 40,
                baseDur = 0.05, durRand = 1,
                rate = 1, rateRand = 1,
                pan = 0, panRand = 0,
                grainEnv = (-1), ptrBus = 0, ptrSampleDelay = 20000,
                ptrRandSamples = 5000, minPtrDelay = 1000,
                cutoff = 12000, resonance = 1,
                fxProb = 0.0,
                // range parameters: min and max for each effect
                pitchMin = 0.5, pitchMax = 2.0,
                ringMin = 100.0, ringMax = 2000.0,
                rateMin = 1000.0, rateMax = 22050.0,
                bitMin = 4.0, bitMax = 16.0;
            var env, densCtrl, durCtrl, rateCtrl, panCtrl;
            var ptr, ptrRand, totalDelay, maxGrainDur;
            var sigRaw, sigProcessed;
            var grainTrig;
            var randomProba, applyFx;
            var effectChoice;
            var pitchRatio, ringFreq, sampleRateRed, bitDepth;
            var pitched, ringed, sampleReduced, bitReduced, fxSig;
            // amplitude of the grain signal for gating
            var ampSig;
            // envelope
            env = EnvGen.kr(Env.asr(atk,1,rel), gate, doneAction: 2);
            // control-rate grain trigger for random values
            grainTrig = Select.kr(sync, [Dust.kr(dens), Impulse.kr(dens)]);
            // density control for audio grain creation
            densCtrl = Select.ar(sync, [Dust.ar(dens), Impulse.ar(dens)]);
            // randomised grain parameters
            durCtrl = baseDur * LFNoise1.ar(100).exprange(1/durRand, durRand);
            rateCtrl = rate.lag3(0.5) * LFNoise1.ar(100).exprange(1/rateRand, rateRand);
            panCtrl = pan + LFNoise1.kr(100).bipolar(panRand);
            ptrRand = LFNoise1.ar(100).bipolar(ptrRandSamples);
            totalDelay = max(ptrSampleDelay - ptrRand, minPtrDelay);
            ptr = In.ar(ptrBus, 1);
            ptr = ptr - totalDelay;
            ptr = ptr / BufFrames.kr(buf);
            maxGrainDur = (totalDelay / rateCtrl) / SampleRate.ir;
            durCtrl = min(durCtrl, maxGrainDur);
            // raw grain signal (stereo)
            sigRaw = GrainBuf.ar(
                2,
                densCtrl,
                durCtrl,
                buf,
                rateCtrl,
                ptr,
                4,
                panCtrl,
                grainEnv
            );
            // random values per grain
            randomProba = TRand.kr(0.0, 1.0, grainTrig);
            effectChoice = TRand.kr(0.0, 3.9999, grainTrig).floor;
            // random values drawn within user defined ranges
            pitchRatio     = TRand.kr(pitchMin, pitchMax, grainTrig);
            ringFreq       = TRand.kr(ringMin, ringMax, grainTrig);
            sampleRateRed  = TRand.kr(rateMin, rateMax, grainTrig);
            bitDepth       = TRand.kr(bitMin, bitMax, grainTrig);
            // build effect variants
            pitched       = PitchShift.ar(sigRaw, 0.1, pitchRatio, 0.0, 0.0);
            ringed        = sigRaw * SinOsc.ar(ringFreq, 0);
            sampleReduced = Decimator.ar(sigRaw, rate: sampleRateRed, bits: 24);
            bitReduced    = Decimator.ar(sigRaw, rate: 44100, bits: bitDepth);
            fxSig = Select.ar(effectChoice, [pitched, ringed, sampleReduced, bitReduced]);
            // compute amplitude of the current grain signal (mixed to mono)
            ampSig = Amplitude.kr((sigRaw[0] + sigRaw[1]) * 0.5, 0.01, 0.01);
            // decide whether to apply an effect: random probability AND input amplitude above threshold
            applyFx = ((randomProba <= fxProb) & (ampSig > 0.0001)).asInteger;
            // choose between dry and effected signal
            sigProcessed = Select.ar(applyFx, [sigRaw, fxSig]);
            // filter and amplitude
            sigProcessed = MoogFF.ar(
                sigProcessed * env * amp,
                freq: cutoff,
                gain: resonance
            );
            Out.ar(out, sigProcessed);
            Group.tail
        }).add;
        s.sync;
        // create groups and synths as in the original engine
        micGrp = Group.new;
        ptrGrp = Group.after(micGrp);
        recGrp = Group.after(ptrGrp);
        granGrp = Group.after(recGrp);
        a = Synth(\mic, [\in, 0, \out, micBus, \amp, 0.5], micGrp);
        h = Synth(\ptr, [\buf, b, \out, ptrBus], ptrGrp);
        i = Synth(\rec, [\ptrIn, ptrBus, \micIn, micBus, \buf, b], recGrp);
        fb = Synth(\fbPatchMix, [\in, 0, \out, micBus], micGrp, addAction: \addToHead);
        panLFOs = Array.fill(16, {0});
        cutoffLFOs = Array.fill(16, {0});
        resonanceLFOs = Array.fill(16, {0});
        16.do({ arg ii;
            panLFOs.put(ii, Ndef(ii.asSymbol, {
                LFTri.kr(timer.beatDur/rrand(1,64)).range(-1,1);
            }));
            cutoffLFOs.put(ii, Ndef((ii+16).asSymbol, {
                LFTri.kr(timer.beatDur/rrand(1,64)).range(500,15000);
            }));
            resonanceLFOs.put(ii, Ndef((ii+32).asSymbol, {
                LFTri.kr(timer.beatDur/rrand(1,64)).range(0,2);
            }));
        });
        rates = [1/4,1/2,1,3/2,2].scramble;
        durs = 16.collect({ arg idx; timer.beatDur*(idx+1) }).scramble;
        delays = 16.collect({ arg idx; s.sampleRate*(timer.beatDur*(idx+1))*16 }).scramble;
        // initialise probability and effect ranges
        fxProb = 0.0;
        pitchMin = 0.5; pitchMax = 2.0;
        ringMin  = 100.0; ringMax  = 2000.0;
        rateMin  = 1000.0; rateMax  = 22050.0;
        bitMin   = 4.0; bitMax   = 16.0;
        // create voices with initial range values
        g = 16.collect({ arg n;
            Synth(\gran, [
                \amp, 0,
                \buf, b,
                \out, 0,
                \atk, 1,
                \rel, 1,
                \gate, 1,
                \sync, 1,
                \dens, 1/(durs[n]*rates[n%5]),
                \baseDur, durs[n],
                \durRand, 1,
                \rate, rates[n%5],
                \rateRand, 1,
                \pan, panLFOs[n],
                \panRand, 0,
                \grainEnv, -1,
                \ptrBus, ptrBus,
                \ptrSampleDelay, delays[n],
                \ptrRandSamples, s.sampleRate*(timer.beatDur*((n%8)+1))*2,
                \minPtrDelay, delays[n],
                \cutoff, cutoffLFOs[n],
                \resonance, resonanceLFOs[n],
                \fxProb, fxProb,
                \pitchMin, pitchMin, \pitchMax, pitchMax,
                \ringMin, ringMin, \ringMax, ringMax,
                \rateMin, rateMin, \rateMax, rateMax,
                \bitMin, bitMin, \bitMax, bitMax
            ], granGrp);
        });
        // OSC receiver for control messages
        oscs.put("receiver",
            OSCFunc.new({ |msg, time, addr, recvPort|
                // delay input level (index 2)
                if(msg[1] == 2, {
                    a.set(\amp, msg[2]);
                });
                // delay output on/off (index 3)
                if(msg[1] == 3, {
                    if(msg[2] == 1, {
                        16.do({ arg j; g[j].set(\amp, 0); });
                    }, {
                        16.do({ arg j; g[j].set(\amp, msg[2]); });
                    });
                });
                // preserve level (index 4)
                if(msg[1] == 4, {
                    i.set(\preLevel, msg[2]);
                });
                // feedback level (index 5)
                if(msg[1] == 5, {
                    fb.set(\amp, msg[2]);
                });
                // feedback balance (index 6)
                if(msg[1] == 6, {
                    fb.set(\balance, msg[2]);
                });
                // feedback highpass frequency (index 7)
                if(msg[1] == 7, {
                    fb.set(\hpFreq, msg[2]);
                });
                // feedback noise level (index 8)
                if(msg[1] == 8, {
                    fb.set(\noiseLevel, msg[2]);
                });
                // feedback sine level (index 9)
                if(msg[1] == 9, {
                    fb.set(\sineLevel, msg[2]);
                });
                // feedback sine frequency (index 10)
                if(msg[1] == 10, {
                    fb.set(\sineHz, msg[2]);
                });
                // set random effect probability (index 11)
                if(msg[1] == 11, {
                    fxProb = msg[2];
                    16.do({ arg j; g[j].set(\fxProb, fxProb); });
                });
                // set pitch ratio minimum (index 12)
                if(msg[1] == 12, {
                    pitchMin = msg[2];
                    16.do({ arg j; g[j].set(\pitchMin, pitchMin); });
                });
                // set pitch ratio maximum (index 13)
                if(msg[1] == 13, {
                    pitchMax = msg[2];
                    16.do({ arg j; g[j].set(\pitchMax, pitchMax); });
                });
                // set ring modulation frequency minimum (index 14)
                if(msg[1] == 14, {
                    ringMin = msg[2];
                    16.do({ arg j; g[j].set(\ringMin, ringMin); });
                });
                // set ring modulation frequency maximum (index 15)
                if(msg[1] == 15, {
                    ringMax = msg[2];
                    16.do({ arg j; g[j].set(\ringMax, ringMax); });
                });
                // set sample rate reduction minimum (index 16)
                if(msg[1] == 16, {
                    rateMin = msg[2];
                    16.do({ arg j; g[j].set(\rateMin, rateMin); });
                });
                // set sample rate reduction maximum (index 17)
                if(msg[1] == 17, {
                    rateMax = msg[2];
                    16.do({ arg j; g[j].set(\rateMax, rateMax); });
                });
                // set bit depth minimum (index 18)
                if(msg[1] == 18, {
                    bitMin = msg[2];
                    16.do({ arg j; g[j].set(\bitMin, bitMin); });
                });
                // set bit depth maximum (index 19)
                if(msg[1] == 19, {
                    bitMax = msg[2];
                    16.do({ arg j; g[j].set(\bitMax, bitMax); });
                });
            }, "/receiver");
        );
    }
    free {
        b.free;
        timer.free;
        micBus.free;
        ptrBus.free;
        micGrp.free;
        ptrGrp.free;
        recGrp.free;
        granGrp.free;
        16.do({ arg k;
            cutoffLFOs[k].free;
            resonanceLFOs[k].free;
            panLFOs[k].free;
        });
    }
}
