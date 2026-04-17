-- Lothario
--
-- Feed the machine.
-- Let its inner beast reign supreme.

musicutil = require 'musicutil'

-- use our custom engine
engine.name = 'Lothario'

local K2_LONG_PRESS_TIME = 0.75
local BUFFER_PURGE_TIME = 30
local K3_SCAN_HOLD_TIME = 0.2

local k2_down = false
local k2_long_press_clock = nil
local k2_long_press_triggered = false
local k3_down = false
local k3_hold_clock = nil
local k3_hold_triggered = false
local purge_active = false
local purge_end_time = nil
local purge_notice_until = nil
local speaker_scan_active = false
local speaker_scan_clock = nil
local current_scan_speaker = 1
local scan_step_index = 0
local scan_ping_pong_direction = 1
local scan_drunk_direction = 1
local scan_shuffle_bag = {}

------------------------------
-- init
------------------------------
function init()
  inpass = true
  delin = true
  delout = true
  add_parameters()
  redrawtimer = metro.init(function()
    redraw()
  end, 1/15, -1)
  redrawtimer:start()
end

local function db_to_amp(db)
  if db == nil or db <= -60 then
    return 0
  end
  return math.pow(10, db / 20)
end

local function current_monitor_amp()
  if not inpass then
    return 0
  end
  return db_to_amp(params:get('input_passthrough'))
end

local function refresh_monitor_routing()
  if speaker_scan_active then
    params:set('monitor_level', -inf)
    osc.send({'localhost', 57120}, '/receiver', {25, current_monitor_amp()})
  else
    osc.send({'localhost', 57120}, '/receiver', {25, 0})
    if inpass then
      params:set('monitor_level', remember)
    else
      params:set('monitor_level', -inf)
    end
  end
end

local function toggle_input_passthrough()
  if inpass then
    inpass = false
  else
    inpass = true
  end
  refresh_monitor_routing()
end

local function toggle_delay_input()
  if delin then
    delin = false
    params:set('delay_input_onoff', 1)
  else
    delin = true
    params:set('delay_input_onoff', 2)
  end
end

local function toggle_delay_output()
  if delout then
    delout = false
    params:set('delay_output_onoff', 1)
  else
    delout = true
    params:set('delay_output_onoff', 2)
  end
end

local function next_shuffle_speaker()
  if #scan_shuffle_bag == 0 then
    scan_shuffle_bag = {1, 2, 3, 4}
    for i = #scan_shuffle_bag, 2, -1 do
      local j = math.random(i)
      scan_shuffle_bag[i], scan_shuffle_bag[j] = scan_shuffle_bag[j], scan_shuffle_bag[i]
    end
  end

  return table.remove(scan_shuffle_bag, 1)
end

local function step_scan_speaker(initial)
  local scan_type = params:string('speaker_scan_type')

  if scan_type == 'random' then
    current_scan_speaker = math.random(4)
  elseif scan_type == 'circular' then
    scan_step_index = (scan_step_index % 4) + 1
    current_scan_speaker = scan_step_index
  elseif scan_type == 'ping-pong' then
    if initial then
      current_scan_speaker = 1
      scan_ping_pong_direction = 1
    else
      current_scan_speaker = current_scan_speaker + scan_ping_pong_direction
      if current_scan_speaker >= 4 then
        current_scan_speaker = 4
        scan_ping_pong_direction = -1
      elseif current_scan_speaker <= 1 then
        current_scan_speaker = 1
        scan_ping_pong_direction = 1
      end
    end
  elseif scan_type == 'shuffle' then
    current_scan_speaker = next_shuffle_speaker()
  else
    if initial then
      current_scan_speaker = math.random(4)
      scan_drunk_direction = (math.random(2) == 1) and -1 or 1
    else
      if math.random() < 0.35 then
        scan_drunk_direction = -scan_drunk_direction
      end
      current_scan_speaker = current_scan_speaker + scan_drunk_direction
      if current_scan_speaker >= 4 then
        current_scan_speaker = 4
        scan_drunk_direction = -1
      elseif current_scan_speaker <= 1 then
        current_scan_speaker = 1
        scan_drunk_direction = 1
      end
    end
  end

  osc.send({'localhost', 57120}, '/receiver', {24, current_scan_speaker})
end

local function stop_speaker_scan()
  if not speaker_scan_active then
    return
  end

  speaker_scan_active = false
  if speaker_scan_clock ~= nil then
    clock.cancel(speaker_scan_clock)
    speaker_scan_clock = nil
  end

  osc.send({'localhost', 57120}, '/receiver', {23, 0})
  osc.send({'localhost', 57120}, '/receiver', {24, 1})
  refresh_monitor_routing()
end

local function start_speaker_scan()
  if speaker_scan_active or purge_active then
    return
  end

  speaker_scan_active = true
  scan_step_index = 0
  scan_ping_pong_direction = 1
  scan_drunk_direction = (math.random(2) == 1) and -1 or 1
  scan_shuffle_bag = {}
  osc.send({'localhost', 57120}, '/receiver', {23, 1})
  refresh_monitor_routing()

  speaker_scan_clock = clock.run(function()
    step_scan_speaker(true)
    while speaker_scan_active do
      clock.sleep(params:get('speaker_scan_rate') / 1000)
      if speaker_scan_active then
        step_scan_speaker(false)
      end
    end
  end)
end

local function begin_buffer_purge()
  if purge_active then
    return
  end

  stop_speaker_scan()
  purge_active = true
  purge_end_time = util.time() + BUFFER_PURGE_TIME
  purge_notice_until = nil
  osc.send({'localhost', 57120}, '/receiver', {21, BUFFER_PURGE_TIME})

  clock.run(function()
    clock.sleep(BUFFER_PURGE_TIME)
    purge_active = false
    purge_end_time = nil
    purge_notice_until = util.time() + 2
  end)
end

--------------------------
-- encoders and keys
--------------------------
function enc(n, d)
  if n == 1 then
    params:delta('input_passthrough', d)
  elseif n == 2 then
    params:delta('delay_input', d)
  else
    params:delta('input_passthrough', d)
    params:delta('delay_input', d)
  end
end

function key(n, z)
  if n == 2 then
    if purge_active then
      return
    end
    if z == 1 then
      k2_down = true
      k2_long_press_triggered = false
      if k2_long_press_clock ~= nil then
        clock.cancel(k2_long_press_clock)
      end
      k2_long_press_clock = clock.run(function()
        clock.sleep(K2_LONG_PRESS_TIME)
        if k2_down and not purge_active then
          k2_long_press_triggered = true
          begin_buffer_purge()
        end
      end)
    else
      k2_down = false
      if k2_long_press_clock ~= nil then
        clock.cancel(k2_long_press_clock)
        k2_long_press_clock = nil
      end
      if not k2_long_press_triggered then
        toggle_delay_input()
      end
    end
    return
  end

  if n == 3 then
    if purge_active then
      return
    end
    if z == 1 then
      k3_down = true
      k3_hold_triggered = false
      if k3_hold_clock ~= nil then
        clock.cancel(k3_hold_clock)
      end
      k3_hold_clock = clock.run(function()
        clock.sleep(K3_SCAN_HOLD_TIME)
        if k3_down and not purge_active then
          k3_hold_triggered = true
          start_speaker_scan()
        end
      end)
    else
      k3_down = false
      if k3_hold_clock ~= nil then
        clock.cancel(k3_hold_clock)
        k3_hold_clock = nil
      end
      if k3_hold_triggered then
        stop_speaker_scan()
      else
        toggle_delay_output()
      end
    end
    return
  end

  if z == 1 and n == 1 then
      toggle_input_passthrough()
  end
end

--------------------------
-- init params
--------------------------
function add_parameters()
  pre_init_monitor_level = params:get('monitor_level')
  onoff = {'off', 'on'}
  remember = pre_init_monitor_level
  params:add_option('input_passthrough_onoff', 'input passthru', onoff, 2)
  params:set_action('input_passthrough_onoff', function(value)
    if value == 1 then
      inpass = false
    else
      inpass = true
    end
    refresh_monitor_routing()
  end)
  params:add_option('delay_input_onoff', 'delay input', onoff, 2)
  params:set_action('delay_input_onoff', function(value)
    osc.send({'localhost', 57120}, '/receiver', {2, params:get('delay_input') * (value - 1)})
    if value == 1 then
      delin = false
    else
      delin = true
    end
  end)
  params:add_option('delay_output_onoff', 'delay output', onoff, 2)
  params:set_action('delay_output_onoff', function(value)
    osc.send({'localhost', 57120}, '/receiver', {3, value})
    if value == 1 then
      delout = false
    else
      delout = true
    end
  end)
  params:add_group('delay levels', 5)
  params:add_control('input_passthrough', 'input passthru level', controlspec.DB)
  params:set_action('input_passthrough', function(value)
    remember = value
    if not speaker_scan_active and inpass then
      params:set('monitor_level', value)
    else
      refresh_monitor_routing()
    end
  end)
  params:set('input_passthrough', 0)
  params:add_control('delay_input', 'delay input level', controlspec.AMP)
  params:set_action('delay_input', function(value)
    osc.send({'localhost', 57120}, '/receiver', {2, value})
  end)
  params:set('delay_input', 0.5)
  params:add_control('pre_level', 'preserve level', controlspec.AMP)
  params:set_action('pre_level', function(value)
    osc.send({'localhost', 57120}, '/receiver', {4, value})
  end)
  params:add_control('grain_decay_time', 'grain decay time', controlspec.new(10, 600, 'lin', 1, 120, 's'))
  params:set_action('grain_decay_time', function(value)
    osc.send({'localhost', 57120}, '/receiver', {20, value})
  end)
  params:add_option('outputs', 'outputs', {'all', '1-2', '3-4'}, 1)
  params:set_action('outputs', function(value)
    osc.send({'localhost', 57120}, '/receiver', {22, value})
  end)
  params:add_group('speaker scan', 2)
  params:add_control('speaker_scan_rate', 'speaker scan rate', controlspec.new(50, 2000, 'lin', 50, 250, 'ms'))
  params:add_option('speaker_scan_type', 'speaker scan type', {'random', 'circular', 'ping-pong', 'shuffle', 'drunk'}, 1)
  params:add_group('feedback', 6)
  params:add_control('fb_level', 'feedback level', controlspec.AMP)
  params:set_action('fb_level', function(value)
    osc.send({'localhost', 57120}, '/receiver', {5, value})
  end)
  params:add_control('fb_balance', 'feedback balance', controlspec.new(-1, 1, 'lin', 0, 0, ''))
  params:set_action('fb_balance', function(value)
    osc.send({'localhost', 57120}, '/receiver', {6, value})
  end)
  params:add_control('fb_hp', 'feedback highpass hz', controlspec.new(0, 220, 'lin', 0, 0, ''))
  params:set_action('fb_hp', function(value)
    osc.send({'localhost', 57120}, '/receiver', {7, value})
  end)
  params:add_control('fb_noise_level', 'feedback noise level', controlspec.AMP)
  params:set_action('fb_noise_level', function(value)
    osc.send({'localhost', 57120}, '/receiver', {8, value})
  end)
  params:add_control('fb_sine_level', 'feedback sine level', controlspec.AMP)
  params:set_action('fb_sine_level', function(value)
    osc.send({'localhost', 57120}, '/receiver', {9, value})
  end)
  params:add_control('fb_sine_midi', 'feedback sine note', controlspec.new(0, 90, 'lin', 0, 30, ''))
  params:set_action('fb_sine_midi', function(value)
    local hz = musicutil.note_num_to_freq(value)
    osc.send({'localhost', 57120}, '/receiver', {10, hz})
  end)
  -- FX probability parameter
  params:add_control('fx_probability', 'fx probability (%)', controlspec.new(0, 100, 'lin', 1, 50, '%'))
  params:set_action('fx_probability', function(value)
    -- convert 0–100 to 0.0–1.0 and send to engine
    osc.send({'localhost', 57120}, '/receiver', {11, value / 100})
  end)
  params:set('fx_probability', 50)
  params:set('grain_decay_time', 120)
  params:set('outputs', 1)
  params:set('speaker_scan_rate', 250)
  params:set('speaker_scan_type', 1)

  -- effect range configuration
  -- group them together for clarity
  params:add_group('fx ranges', 8)
  -- Pitch shift range
  params:add_control('pitch_min', 'pitch ratio min', controlspec.new(0.0, 4.0, 'lin', 0.01, 0.5, ''))
  params:set_action('pitch_min', function(value)
    osc.send({'localhost', 57120}, '/receiver', {12, value})
  end)
  params:add_control('pitch_max', 'pitch ratio max', controlspec.new(0.0, 4.0, 'lin', 0.01, 2.0, ''))
  params:set_action('pitch_max', function(value)
    osc.send({'localhost', 57120}, '/receiver', {13, value})
  end)
  -- Ring modulation frequency range (Hz)
  params:add_control('ring_min', 'ring freq min (Hz)', controlspec.new(20.0, 20000.0, 'exp', 0.0, 100.0, 'Hz'))
  params:set_action('ring_min', function(value)
    osc.send({'localhost', 57120}, '/receiver', {14, value})
  end)
  params:add_control('ring_max', 'ring freq max (Hz)', controlspec.new(20.0, 20000.0, 'exp', 0.0, 2000.0, 'Hz'))
  params:set_action('ring_max', function(value)
    osc.send({'localhost', 57120}, '/receiver', {15, value})
  end)
  -- Sample rate reduction range (Hz)
  params:add_control('rate_min', 'sample rate min (Hz)', controlspec.new(100.0, 44100.0, 'exp', 0.0, 1000.0, 'Hz'))
  params:set_action('rate_min', function(value)
    osc.send({'localhost', 57120}, '/receiver', {16, value})
  end)
  params:add_control('rate_max', 'sample rate max (Hz)', controlspec.new(100.0, 44100.0, 'exp', 0.0, 22050.0, 'Hz'))
  params:set_action('rate_max', function(value)
    osc.send({'localhost', 57120}, '/receiver', {17, value})
  end)
  -- Bit depth range
  params:add_control('bit_min', 'bit depth min', controlspec.new(2.0, 24.0, 'lin', 1.0, 4.0, 'bits'))
  params:set_action('bit_min', function(value)
    osc.send({'localhost', 57120}, '/receiver', {18, value})
  end)
  params:add_control('bit_max', 'bit depth max', controlspec.new(2.0, 24.0, 'lin', 1.0, 16.0, 'bits'))
  params:set_action('bit_max', function(value)
    osc.send({'localhost', 57120}, '/receiver', {19, value})
  end)
  params:bang()
end

--------------------------
-- redraw
--------------------------
function redraw()
  screen.clear()
  screen.font_size(8)
  screen.font_face(1)
  screen.move(1, 15)
  if inpass then
    screen.level(15)
  else
    screen.level(5)
  end
  screen.text('input passthru')
  screen.move(75, 15)
  screen.text(math.floor(params:get('monitor_level') * 100) / 100)
  screen.move(36, 35)
  if delin then
    screen.level(15)
  else
    screen.level(5)
  end
  screen.text('delay input')
  screen.move(93, 35)
  screen.text(params:get('delay_input'))
  screen.move(61, 55)
  if delout then
    screen.level(15)
  else
    screen.level(5)
  end
  screen.text('delay output')
  screen.move(1, 63)
  screen.level(15)
  if purge_active and purge_end_time ~= nil then
    screen.text('clearing')
    screen.move(75, 63)
    screen.text(math.ceil(math.max(0, purge_end_time - util.time())) .. 's')
  elseif speaker_scan_active then
    screen.text('scan ' .. params:string('speaker_scan_type'))
    screen.move(100, 63)
    screen.text(current_scan_speaker)
  elseif purge_notice_until ~= nil and util.time() < purge_notice_until then
    screen.text('buffer ready')
  else
    screen.text('fx prob')
    screen.move(75, 63)
    screen.text(params:get('fx_probability'))
  end
  screen.update()
end

function cleanup()
  stop_speaker_scan()
  params:set('monitor_level', pre_init_monitor_level)
end
