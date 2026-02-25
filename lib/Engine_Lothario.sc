Engine_Lothario : CroneEngine {
    var oscs, b, timer, micBus, ptrBus;
    var micGrp, ptrGrp, recGrp, granGrp;
    var panLFOs, cutoffLFOs, resonanceLFOs;
    var rates, durs, delays;
    var a, g, h, i, fb;
    var fxProb;
    var pitchMin, pitchMax;
    var ringMin, ringMax;
    var rateMin, rateMax;
    var bitMin, bitMax;

    *new { arg context, doneCallback;
        ^super.new(context, doneCallback);
    }

    alloc {
        var s = context.server;

        oscs = Dictionary.new;
        timer = LinkClock.new(2).latency_(s.latency).quantum_(0);

        b = Buffer.alloc(s, s.sampleRate * (timer.beatDur * 512), 1);

        micBus = Bus.audio(s, 1);
        ptrBus = Bus.audio(s, 1);

        SynthDef(\mic, {
            arg inA = 0, inB = 1, out = 0, amp = 1;
            var sig = SoundIn.ar([inA, inB]);
            sig = (sig[0] + sig[1]) * 0.5;
            Out.ar(out, sig * amp);
        }).add;

        SynthDef(\ptr, {
            arg out = 0, buf = 0, rate = 1;
            var sig = Phasor.ar(0, BufRateScale.kr(buf) * rate, 0, BufFrames.kr(buf));
            Out.ar(out, sig);
        }).add;

        SynthDef(\rec, {
            arg ptrIn = 0, micIn = 0, buf = 0, preLevel = 0;
            var ptr = In.ar(ptrIn, 1);
            var sig = In.ar(micIn, 1);
            sig = sig + (BufRd.ar(1, buf, ptr) * preLevel);
            BufWr.ar(sig, buf, ptr);
        }).add;

        SynthDef(\fbPatchMix, {
            arg in = 0, out = 0, amp = 0, balance = 0, hpFreq = 12,
                noiseLevel = 0.0, sineLevel = 0, sineHz = 55;
            var input = InFeedback.ar(in, 2);
            var stereo = Balance2.ar(input[0], input[1], balance);
            stereo = stereo + (PinkNoise.ar * noiseLevel);
            stereo = stereo + (SinOsc.ar(sineHz) * sineLevel);
            stereo = HPF.ar(stereo, hpFreq);
            stereo = stereo.softclip;
            Out.ar(out, ((stereo[0] + stereo[1]) * 0.5) * amp);
        }).add;

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
            var ampSig;
            var monoSig, chanIdx, mask, outSig;

            env = EnvGen.kr(Env.asr(atk, 1, rel), gate, doneAction: 2);

            grainTrig = Select.kr(sync, [Dust.kr(dens), Impulse.kr(dens)]);
            densCtrl  = Select.ar(sync, [Dust.ar(dens), Impulse.ar(dens)]);
            durCtrl   = baseDur * LFNoise1.ar(100).exprange(1 / durRand, durRand);
            rateCtrl  = rate.lag3(0.5) * LFNoise1.ar(100).exprange(1 / rateRand, rateRand);
            panCtrl   = pan + LFNoise1.kr(100).bipolar(panRand);

            ptrRand = LFNoise1.ar(100).bipolar(ptrRandSamples);
            totalDelay = max(ptrSampleDelay - ptrRand, minPtrDelay);
            ptr = In.ar(ptrBus, 1);
            ptr = ptr - totalDelay;
            ptr = ptr / BufFrames.kr(buf);

            maxGrainDur = (totalDelay / rateCtrl) / SampleRate.ir;
            durCtrl = min(durCtrl, maxGrainDur);

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

            randomProba  = TRand.kr(0.0, 1.0, grainTrig);
            effectChoice = TRand.kr(0.0, 3.9999, grainTrig).floor;
            pitchRatio   = TRand.kr(pitchMin, pitchMax, grainTrig);
            ringFreq     = TRand.kr(ringMin, ringMax, grainTrig);
            sampleRateRed= TRand.kr(rateMin, rateMax, grainTrig);
            bitDepth     = TRand.kr(bitMin, bitMax, grainTrig);

            pitched       = PitchShift.ar(sigRaw, 0.1, pitchRatio, 0.0, 0.0);
            ringed        = sigRaw * SinOsc.ar(ringFreq, 0);
            sampleReduced = Decimator.ar(sigRaw, rate: sampleRateRed, bits: 24);
            bitReduced    = Decimator.ar(sigRaw, rate: 44100, bits: bitDepth);
            fxSig         = Select.ar(effectChoice, [pitched, ringed, sampleReduced, bitReduced]);

            ampSig  = Amplitude.kr((sigRaw[0] + sigRaw[1]) * 0.5, 0.01, 0.01);
            applyFx = ((randomProba <= fxProb) & (ampSig > 0.0001)).asInteger;

            sigProcessed = Select.ar(applyFx, [sigRaw, fxSig]);
            sigProcessed = MoogFF.ar(sigProcessed * env * amp, freq: cutoff, gain: resonance);

            monoSig = (sigProcessed[0] + sigProcessed[1]) * 0.5;

            chanIdx = TIRand.kr(0, 3, grainTrig);
            mask = Select.kr(chanIdx, [
                [1, 0, 0, 0],
                [0, 1, 0, 0],
                [0, 0, 1, 0],
                [0, 0, 0, 1]
            ]);
            mask = Lag.kr(mask, 0.005);

            outSig = monoSig * mask;
            Out.ar(out, outSig);
        }).add;

        s.sync;

        micGrp  = Group.new;
        ptrGrp  = Group.after(micGrp);
        recGrp  = Group.after(ptrGrp);
        granGrp = Group.after(recGrp);

        a = Synth(\mic, [\inA, 0, \inB, 1, \out, micBus, \amp, 0.5], micGrp);
        h = Synth(\ptr, [\buf, b, \out, ptrBus], ptrGrp);
        i = Synth(\rec, [\ptrIn, ptrBus, \micIn, micBus, \buf, b], recGrp);
        fb = Synth(\fbPatchMix, [\in, 0, \out, micBus], micGrp, addAction: \addToHead);

        panLFOs = Array.fill(16, { 0 });
        cutoffLFOs = Array.fill(16, { 0 });
        resonanceLFOs = Array.fill(16, { 0 });

        16.do({ arg ii;
            panLFOs.put(ii, Ndef(ii.asSymbol, { LFTri.kr(timer.beatDur / rrand(1, 64)).range(-1, 1) }));
            cutoffLFOs.put(ii, Ndef((ii + 16).asSymbol, { LFTri.kr(timer.beatDur / rrand(1, 64)).range(500, 15000) }));
            resonanceLFOs.put(ii, Ndef((ii + 32).asSymbol, { LFTri.kr(timer.beatDur / rrand(1, 64)).range(0, 2) }));
        });

        rates  = [1/4, 1/2, 1, 3/2, 2].scramble;
        durs   = 16.collect({ arg idx; timer.beatDur * (idx + 1) }).scramble;
        delays = 16.collect({ arg idx; s.sampleRate * (timer.beatDur * (idx + 1)) * 16 }).scramble;

        fxProb = 0.0;
        pitchMin = 0.5; pitchMax = 2.0;
        ringMin = 100.0; ringMax = 2000.0;
        rateMin = 1000.0; rateMax = 22050.0;
        bitMin = 4.0; bitMax = 16.0;

        g = 16.collect({ arg n;
            Synth(\gran, [
                \amp, 0,
                \buf, b,
                \out, 0,
                \atk, 1,
                \rel, 1,
                \gate, 1,
                \sync, 1,
                \dens, 1 / (durs[n] * rates[n % 5]),
                \baseDur, durs[n],
                \durRand, 1,
                \rate, rates[n % 5],
                \rateRand, 1,
                \pan, panLFOs[n],
                \panRand, 0,
                \grainEnv, -1,
                \ptrBus, ptrBus,
                \ptrSampleDelay, delays[n],
                \ptrRandSamples, s.sampleRate * (timer.beatDur * ((n % 8) + 1)) * 2,
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

        oscs.put("receiver",
            OSCFunc.new({ |msg, time, addr, recvPort|
                if(msg[1] == 2, { a.set(\amp, msg[2]); });

                if(msg[1] == 3, {
                    if(msg[2] == 1, {
                        16.do({ arg j; g[j].set(\amp, 0); });
                    }, {
                        16.do({ arg j; g[j].set(\amp, msg[2]); });
                    });
                });

                if(msg[1] == 4, { i.set(\preLevel, msg[2]); });

                if(msg[1] == 5, { fb.set(\amp, msg[2]); });
                if(msg[1] == 6, { fb.set(\balance, msg[2]); });
                if(msg[1] == 7, { fb.set(\hpFreq, msg[2]); });
                if(msg[1] == 8, { fb.set(\noiseLevel, msg[2]); });
                if(msg[1] == 9, { fb.set(\sineLevel, msg[2]); });
                if(msg[1] == 10, { fb.set(\sineHz, msg[2]); });

                if(msg[1] == 11, {
                    fxProb = msg[2];
                    16.do({ arg j; g[j].set(\fxProb, fxProb); });
                });

                if(msg[1] == 12, {
                    pitchMin = msg[2];
                    16.do({ arg j; g[j].set(\pitchMin, pitchMin); });
                });

                if(msg[1] == 13, {
                    pitchMax = msg[2];
                    16.do({ arg j; g[j].set(\pitchMax, pitchMax); });
                });

                if(msg[1] == 14, {
                    ringMin = msg[2];
                    16.do({ arg j; g[j].set(\ringMin, ringMin); });
                });

                if(msg[1] == 15, {
                    ringMax = msg[2];
                    16.do({ arg j; g[j].set(\ringMax, ringMax); });
                });

                if(msg[1] == 16, {
                    rateMin = msg[2];
                    16.do({ arg j; g[j].set(\rateMin, rateMin); });
                });

                if(msg[1] == 17, {
                    rateMax = msg[2];
                    16.do({ arg j; g[j].set(\rateMax, rateMax); });
                });

                if(msg[1] == 18, {
                    bitMin = msg[2];
                    16.do({ arg j; g[j].set(\bitMin, bitMin); });
                });

                if(msg[1] == 19, {
                    bitMax = msg[2];
                    16.do({ arg j; g[j].set(\bitMax, bitMax); });
                });
            }, "/receiver");
        );
    }

    free {
        if(b.notNil, { b.free; });
        if(timer.notNil, { timer.free; });
        if(micBus.notNil, { micBus.free; });
        if(ptrBus.notNil, { ptrBus.free; });

        if(micGrp.notNil, { micGrp.free; });
        if(ptrGrp.notNil, { ptrGrp.free; });
        if(recGrp.notNil, { recGrp.free; });
        if(granGrp.notNil, { granGrp.free; });

        16.do({ arg k;
            if(cutoffLFOs.notNil and: { cutoffLFOs[k].notNil }, { cutoffLFOs[k].free; });
            if(resonanceLFOs.notNil and: { resonanceLFOs[k].notNil }, { resonanceLFOs[k].free; });
            if(panLFOs.notNil and: { panLFOs[k].notNil }, { panLFOs[k].free; });
        });
    }
}
