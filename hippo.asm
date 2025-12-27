;=============================================================================
; HIPPO CHARGE - An Excitebike-style game for the NES
; A hippo collects dollars and jumps for joy!
;=============================================================================

.segment "HEADER"
;=============================================================================
; iNES Header (16 bytes)
;=============================================================================
.byte "NES", $1A      ; iNES magic number
.byte $01             ; PRG-ROM size: 1 x 16KB = 16KB
.byte $01             ; CHR-ROM size: 1 x 8KB = 8KB  
.byte $00             ; Flags 6: Mapper 0 (NROM), horizontal mirroring
.byte $00             ; Flags 7: Mapper 0 continued
.byte $00             ; PRG-RAM size (0 = 8KB for compatibility)
.byte $00             ; TV system: NTSC
.byte $00             ; Flags 10: unused
.byte $00,$00,$00,$00,$00  ; Padding to 16 bytes

;=============================================================================
; NES Hardware Registers
;=============================================================================

; PPU Registers
PPUCTRL   = $2000     ; PPU control register
PPUMASK   = $2001     ; PPU mask register (rendering control)
PPUSTATUS = $2002     ; PPU status register
OAMADDR   = $2003     ; OAM address port
OAMDATA   = $2004     ; OAM data port
PPUSCROLL = $2005     ; PPU scroll position
PPUADDR   = $2006     ; PPU address port
PPUDATA   = $2007     ; PPU data port

; APU and I/O Registers
OAMDMA    = $4014     ; OAM DMA register (high byte of RAM address)
JOYPAD1   = $4016     ; Joypad 1 data port
JOYPAD2   = $4017     ; Joypad 2 data port

; APU Sound Registers
APU_PULSE1_CTRL = $4000   ; Pulse 1 duty, volume, envelope
APU_PULSE1_SWEEP = $4001  ; Pulse 1 sweep control
APU_PULSE1_LO = $4002     ; Pulse 1 timer low
APU_PULSE1_HI = $4003     ; Pulse 1 timer high, length counter
APU_PULSE2_CTRL = $4004   ; Pulse 2 duty, volume, envelope
APU_PULSE2_SWEEP = $4005  ; Pulse 2 sweep control
APU_PULSE2_LO = $4006     ; Pulse 2 timer low
APU_PULSE2_HI = $4007     ; Pulse 2 timer high, length counter
APU_TRI_CTRL = $4008      ; Triangle linear counter
APU_TRI_LO = $400A        ; Triangle timer low
APU_TRI_HI = $400B        ; Triangle timer high, length counter
APU_NOISE_CTRL = $400C    ; Noise volume/envelope
APU_NOISE_LO = $400E      ; Noise period and mode
APU_NOISE_HI = $400F      ; Noise length counter
APU_STATUS = $4015        ; APU status (enable channels)

;=============================================================================
; Game Constants
;=============================================================================

; Screen dimensions
SCREEN_WIDTH  = 256
SCREEN_HEIGHT = 240

; Hippo settings
HIPPO_START_X = 40        ; Starting X position
HIPPO_START_Y = 168       ; Starting Y position (lowest lane)
HIPPO_GROUND  = 128       ; Ground Y position
HIPPO_SPEED   = 2         ; Horizontal scroll speed
JUMP_VELOCITY = 6         ; Initial jump velocity
GRAVITY       = 1         ; Gravity acceleration

; Lane Y positions (like Excitebike lanes)
; Hippo is 24 pixels tall, so bottom of hippo = lane Y + 24
; Track area roughly Y=120-200, keep hippo away from HUD at top
LANE1_Y = 120             ; Bottom at 144 (top lane)
LANE2_Y = 136             ; Bottom at 160  
LANE3_Y = 152             ; Bottom at 176
LANE4_Y = 168             ; Bottom at 192 (lowest lane, on track)

; Controller button masks
BTN_A      = %10000000
BTN_B      = %01000000
BTN_SELECT = %00100000
BTN_START  = %00010000
BTN_UP     = %00001000
BTN_DOWN   = %00000100
BTN_LEFT   = %00000010
BTN_RIGHT  = %00000001

;=============================================================================
; Zero Page Variables (fast access)
;=============================================================================
.segment "ZEROPAGE"

; Frame counter
frame_counter:  .res 1

; Controller state
buttons:        .res 1    ; Current button state
buttons_old:    .res 1    ; Previous frame button state
buttons_new:    .res 1    ; Newly pressed buttons

; Hippo state
hippo_x:        .res 1    ; Hippo X position (screen coords)
hippo_y:        .res 1    ; Hippo Y position
hippo_vel_y:    .res 1    ; Hippo vertical velocity (signed)
hippo_lane:     .res 1    ; Current lane (0-3)
hippo_state:    .res 1    ; 0=running, 1=jumping, 2=in water
hippo_anim:     .res 1    ; Animation frame counter
hippo_speed:    .res 1    ; Current speed (can overheat)
hippo_heat:     .res 1    ; Overheat meter (0-255)
hippo_land_y:   .res 1    ; Y position to land at after jump

; Scrolling
scroll_x:       .res 1    ; Scroll X position (low byte)
scroll_x_hi:    .res 1    ; Scroll X position (high byte for distance)

; Temp variables for calculations
temp1:          .res 1
temp2:          .res 1
ptr_lo:         .res 1    ; General purpose pointer
ptr_hi:         .res 1

; Game state
game_state:     .res 1    ; 0=title, 1=playing, 2=game over
score_lo:       .res 1    ; Score low byte
score_hi:       .res 1    ; Score high byte
hiscore_lo:     .res 1    ; High score low byte
hiscore_hi:     .res 1    ; High score high byte
timer_seconds:  .res 2    ; Countdown timer (16-bit for 100+ seconds)
timer_frames:   .res 1    ; Frame counter for timer (0-59)
last_lane:      .res 1    ; Last lane a dollar spawned in (to avoid repetition)
rng_seed:       .res 1    ; Simple RNG seed
spawn_timer:    .res 1    ; Frames until next dollar spawn

; Sound effect state
sfx_timer:      .res 1    ; Frames remaining for current sound effect
sfx_type:       .res 1    ; 0=none, 1=collect, 2=jump

; Music state - Melody (Pulse 2)
music_ptr:      .res 2    ; Pointer to current position in melody data
music_timer:    .res 1    ; Frames until next melody note
music_playing:  .res 1    ; 0=stopped, 1=playing
music_loop:     .res 1    ; 0=play once, 1=loop

; Music state - Bass (Triangle)
bass_ptr:       .res 2    ; Pointer to current position in bass data
bass_timer:     .res 1    ; Frames until next bass note

; Music state - Drums (Noise)
drum_ptr:       .res 2    ; Pointer to current position in drum data
drum_timer:     .res 1    ; Frames until next drum hit

;=============================================================================
; OAM Buffer (Sprite RAM - must be at $0200)
;=============================================================================
; Define OAM buffer at fixed address $0200
oam_buffer = $0200

;=============================================================================
; CODE Segment
;=============================================================================
.segment "CODE"

;-----------------------------------------------------------------------------
; RESET Handler - Called when NES powers on or resets
;-----------------------------------------------------------------------------
reset:
    sei                   ; Disable IRQs
    cld                   ; Disable decimal mode (not used on NES)
    ldx #$40
    stx $4017             ; Disable APU frame IRQ
    ldx #$FF
    txs                   ; Set up stack pointer at $01FF
    inx                   ; X = 0
    stx PPUCTRL           ; Disable NMI
    stx PPUMASK           ; Disable rendering
    stx $4010             ; Disable DMC IRQs

    ; Wait for first VBlank
@vblank_wait1:
    bit PPUSTATUS
    bpl @vblank_wait1

    ; Clear all RAM to zero
    lda #$00
    ldx #$00
@clear_ram:
    sta $0000, x
    sta $0100, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    inx
    bne @clear_ram

    ; Initialize OAM buffer - hide all sprites (Y = $FF)
    lda #$FF
    ldx #$00
@clear_oam:
    sta oam_buffer, x
    inx
    bne @clear_oam

    ; Initialize APU - enable pulse 1, pulse 2, triangle, and noise
    lda #$0F              ; Enable pulse 1, pulse 2, triangle, noise
    sta APU_STATUS
    lda #$00
    sta sfx_timer         ; No sound playing initially
    sta music_playing     ; Music not playing yet

    ; Wait for second VBlank (PPU is ready after this)
@vblank_wait2:
    bit PPUSTATUS
    bpl @vblank_wait2

    ; Initialize game state to title screen
    lda #$00
    sta game_state

    ; Load palettes
    jsr load_palettes

    ; Load title screen
    jsr load_title_screen
    
    ; Start title jingle
    jsr start_title_jingle

    ; Enable NMI and set pattern tables
    lda #%10010000        ; NMI on, sprites from pattern table 0, BG from pattern table 1
    sta PPUCTRL
    lda #%00011110        ; Enable sprites and background
    sta PPUMASK

    ; Fall through to main loop

;-----------------------------------------------------------------------------
; Main Game Loop
;-----------------------------------------------------------------------------
main_loop:
    ; Wait for NMI to complete (frame sync)
    lda frame_counter
@wait_nmi:
    cmp frame_counter
    beq @wait_nmi         ; Wait until NMI increments frame_counter

    ; Read controller input
    jsr read_controller

    ; Update game based on state
    lda game_state
    cmp #$00
    beq @title_screen
    cmp #$01
    beq @playing
    jmp @game_over

@title_screen:
    ; Update title jingle
    jsr update_music
    ; Check for START button to begin
    lda buttons_new
    and #BTN_START
    beq @loop_end
    ; Disable rendering while we change background
    lda #$00
    sta PPUMASK
    ; Load game background
    jsr load_background
    ; Initialize game
    jsr init_game
    ; Start background music
    jsr start_music
    ; Set game state to playing
    lda #$01
    sta game_state
    ; Re-enable rendering
    lda #%00011110
    sta PPUMASK
    jmp @loop_end

@playing:
    jsr update_timer
    jsr update_hippo
    jsr update_scroll
    jsr update_obstacles
    jsr check_collisions
    jsr update_sound
    jsr update_music
    jsr update_sprites
    jmp @loop_end

@game_over:
    ; Draw game over screen using sprites
    jsr draw_game_over
    ; Update game over jingle
    jsr update_music
    
    ; Check for START to restart
    lda buttons_new
    and #BTN_START
    beq @loop_end
    ; Restart game
    lda #$00
    sta PPUMASK           ; Disable rendering
    jsr load_background   ; Reload game background
    jsr init_game         ; Reinitialize game
    jsr start_music       ; Restart background music
    lda #$01
    sta game_state        ; Set to playing
    lda #%00011110
    sta PPUMASK           ; Re-enable rendering

@loop_end:
    jmp main_loop

;-----------------------------------------------------------------------------
; NMI Handler - Called every VBlank (60 times per second)
;-----------------------------------------------------------------------------
nmi:
    pha                   ; Save registers
    txa
    pha
    tya
    pha

    ; Increment frame counter
    inc frame_counter

    ; Sprite DMA transfer (copy OAM buffer to PPU)
    lda #$00
    sta OAMADDR           ; Set OAM address to 0
    lda #$02              ; High byte of $0200 (oam_buffer)
    sta OAMDMA            ; Trigger DMA transfer (takes 513 cycles)

    ; Set scroll position
    lda PPUSTATUS         ; Reset PPU latch
    lda scroll_x
    sta PPUSCROLL         ; Set X scroll
    lda #$00
    sta PPUSCROLL         ; Set Y scroll (no vertical scrolling)

    ; Restore registers
    pla
    tay
    pla
    tax
    pla
    rti                   ; Return from interrupt

;-----------------------------------------------------------------------------
; IRQ Handler - Not used
;-----------------------------------------------------------------------------
irq:
    rti

;-----------------------------------------------------------------------------
; Read Controller - Read joypad 1 button states
;-----------------------------------------------------------------------------
read_controller:
    ; Save old button state for edge detection
    lda buttons
    sta buttons_old

    ; Strobe the controller to latch button states
    lda #$01
    sta JOYPAD1
    lda #$00
    sta JOYPAD1

    ; Read 8 buttons (A, B, Select, Start, Up, Down, Left, Right)
    ldx #$08
    lda #$00
@read_loop:
    pha                   ; Save accumulator
    lda JOYPAD1           ; Read button (bit 0)
    and #$01              ; Mask to bit 0
    cmp #$01              ; Set carry if button pressed
    pla                   ; Restore accumulator
    rol a                 ; Rotate carry into accumulator
    dex
    bne @read_loop

    sta buttons           ; Store final button state

    ; Calculate newly pressed buttons (buttons AND NOT buttons_old)
    lda buttons_old
    eor #$FF              ; Invert old buttons
    and buttons           ; AND with current buttons
    sta buttons_new       ; These are newly pressed this frame

    rts

;-----------------------------------------------------------------------------
; Initialize Game - Set up starting game state
;-----------------------------------------------------------------------------
init_game:
    ; Set hippo starting position
    lda #HIPPO_START_X
    sta hippo_x
    lda #HIPPO_START_Y
    sta hippo_y

    ; Set hippo initial state
    lda #$00
    sta hippo_vel_y       ; No vertical velocity
    sta hippo_state       ; State = running
    sta hippo_anim        ; Animation frame 0
    sta hippo_heat        ; No overheat
    sta hippo_lane        ; Start in lane 0

    lda #$02
    sta hippo_speed       ; Starting speed

    ; Reset scroll
    lda #$00
    sta scroll_x
    sta scroll_x_hi

    ; Reset score
    sta score_lo
    sta score_hi

    ; Initialize timer to 30 seconds
    lda #30
    sta timer_seconds
    lda #$00
    sta timer_seconds+1   ; High byte = 0
    sta timer_frames      ; Frame counter = 0
    
    ; Initialize spawning variables
    lda #$FF              ; Start with invalid lane so first spawn is random
    sta last_lane
    lda frame_counter     ; Use current frame as initial RNG seed
    sta rng_seed
    lda #30               ; Spawn first dollar after ~0.5 seconds
    sta spawn_timer

    rts

;-----------------------------------------------------------------------------
; Update Timer - Countdown timer, 60 fps
;-----------------------------------------------------------------------------
update_timer:
    ; Increment frame counter
    inc timer_frames
    lda timer_frames
    cmp #60               ; 60 frames = 1 second
    bcc @timer_done       ; Not a full second yet
    
    ; Reset frame counter
    lda #$00
    sta timer_frames
    
    ; Decrement seconds
    lda timer_seconds
    bne @decrement_lo     ; Low byte not zero
    lda timer_seconds+1
    beq @time_up          ; Both bytes zero = time's up!
    dec timer_seconds+1   ; Borrow from high byte
    lda #$FF              ; Set low byte to 255
    sta timer_seconds
    jmp @timer_done
    
@decrement_lo:
    dec timer_seconds
    jmp @timer_done
    
@time_up:
    ; Time's up! Game over
    ; Check if current score > high score
    lda score_hi
    cmp hiscore_hi
    bcc @no_new_hiscore   ; score_hi < hiscore_hi, no new high score
    bne @new_hiscore      ; score_hi > hiscore_hi, new high score
    ; High bytes equal, check low bytes
    lda score_lo
    cmp hiscore_lo
    bcc @no_new_hiscore   ; score_lo < hiscore_lo
    beq @no_new_hiscore   ; score_lo == hiscore_lo (not greater)
@new_hiscore:
    ; New high score!
    lda score_lo
    sta hiscore_lo
    lda score_hi
    sta hiscore_hi
@no_new_hiscore:
    lda #$02
    sta game_state
    jsr stop_music        ; Stop background music
    jsr start_gameover_jingle  ; Play game over jingle
    
@timer_done:
    rts

;-----------------------------------------------------------------------------
; Update Hippo - Handle movement, jumping, and physics
;-----------------------------------------------------------------------------
update_hippo:
    ; Increment animation counter
    inc hippo_anim

    ; --- Handle Lane Changes (Up/Down) ---
    lda buttons_new
    and #BTN_UP
    beq @check_down
    ; Move up a lane
    lda hippo_lane
    beq @check_down       ; Already at top lane
    dec hippo_lane
    jmp @lane_done

@check_down:
    lda buttons_new
    and #BTN_DOWN
    beq @lane_done
    ; Move down a lane
    lda hippo_lane
    cmp #$03
    beq @lane_done        ; Already at bottom lane
    inc hippo_lane

@lane_done:
    ; Calculate target Y based on lane
    lda hippo_lane
    asl a                 ; Multiply by 2
    asl a                 ; Multiply by 4
    asl a                 ; Multiply by 8
    asl a                 ; Multiply by 16
    asl a                 ; Multiply by 32 (lane spacing)
    clc
    adc #LANE1_Y          ; Add base lane Y
    sta temp1             ; Target Y position

    ; --- Handle Jumping (A button) ---
    lda hippo_state
    cmp #$01              ; Already jumping?
    beq @apply_gravity

    lda buttons_new
    and #BTN_A
    beq @no_jump
    ; Start jump
    lda #$01
    sta hippo_state       ; Set jumping state
    lda hippo_y
    sta hippo_land_y      ; Remember where to land
    lda #<(-JUMP_VELOCITY)
    sta hippo_vel_y       ; Negative velocity = going up
    jsr play_jump_sound   ; Play jump sound effect
    jmp @apply_gravity

@no_jump:
    ; Check if we're in a jump (e.g. from collecting a dollar)
    lda hippo_state
    cmp #$01
    beq @apply_gravity    ; Already jumping, apply gravity instead
    
    ; Not jumping - smoothly move to target lane Y
    lda hippo_y
    cmp temp1
    beq @check_speed      ; Already at target
    bcc @move_down_y
    ; Move up toward target
    dec hippo_y
    dec hippo_y
    jmp @check_speed
@move_down_y:
    inc hippo_y
    inc hippo_y
    jmp @check_speed

@apply_gravity:
    ; Apply velocity to Y position
    lda hippo_y
    clc
    adc hippo_vel_y
    sta hippo_y

    ; Apply gravity to velocity
    lda hippo_vel_y
    clc
    adc #GRAVITY
    sta hippo_vel_y

    ; Check if landed (compare to saved land position)
    lda hippo_y
    cmp hippo_land_y      ; Compare to where we should land
    bcc @check_speed      ; Still in air (above landing spot)
    ; Landed
    lda hippo_land_y
    sta hippo_y           ; Snap to landing position
    lda #$00
    sta hippo_vel_y       ; Stop vertical movement
    sta hippo_state       ; Back to running state

@check_speed:
    ; --- Handle Speed (B button = turbo) ---
    lda buttons
    and #BTN_B
    beq @no_turbo
    ; Turbo - increase speed and heat
    lda hippo_speed
    cmp #$05              ; Max speed
    beq @add_heat
    inc hippo_speed
@add_heat:
    lda hippo_heat
    cmp #$F0              ; Near max heat?
    bcs @overheat
    clc
    adc #$04              ; Add heat (faster accumulation)
    sta hippo_heat
    jmp @update_done      ; Skip cooldown when turboing

@overheat:
    ; Overheated! Slow down
    lda #$01
    sta hippo_speed
    jmp @update_done

@no_turbo:
    ; Slow down gradually
    lda hippo_speed
    cmp #$02
    beq @cool_down
    bcc @speed_min
    dec hippo_speed
    jmp @cool_down
@speed_min:
    lda #$02
    sta hippo_speed

@cool_down:
    ; Cool down heat (only when not turboing)
    lda hippo_heat
    beq @update_done
    dec hippo_heat

@update_done:
    rts

;-----------------------------------------------------------------------------
; Update Scroll - Scroll background based on speed
;-----------------------------------------------------------------------------
update_scroll:
    ; Add speed to scroll position
    lda scroll_x
    clc
    adc hippo_speed
    sta scroll_x
    bcc @no_carry
    inc scroll_x_hi       ; Increment high byte on overflow
@no_carry:

@scroll_done:
    rts

;-----------------------------------------------------------------------------
; Update Dollars - Move dollars and spawn new ones
;-----------------------------------------------------------------------------
; Dollar data in BSS
.segment "BSS"
NUM_DOLLARS = 8
dollar_x:     .res NUM_DOLLARS    ; X positions
dollar_y:     .res NUM_DOLLARS    ; Y positions  
dollar_active: .res NUM_DOLLARS   ; 0=inactive, 1=active

.segment "CODE"

update_obstacles:
    ldx #$00              ; Dollar index

@dollar_loop:
    ; Check if dollar is active
    lda dollar_active, x
    beq @next_dollar      ; Skip if inactive

    ; Move dollar left based on hippo speed
    lda dollar_x, x
    sec
    sbc hippo_speed
    sta dollar_x, x
    bcs @check_offscreen
    ; Wrapped around - deactivate
    lda #$00
    sta dollar_active, x
    jmp @next_dollar

@check_offscreen:
    cmp #$08              ; Off left side of screen?
    bcs @next_dollar
    ; Deactivate dollar
    lda #$00
    sta dollar_active, x

@next_dollar:
    inx
    cpx #NUM_DOLLARS
    bne @dollar_loop

    ; --- Spawn new dollars ---
    ; Use a timer-based spawn system for consistent pacing
    dec spawn_timer
    bne @spawn_done       ; Not time yet
    
    ; Reset spawn timer with some randomness (60-120 frames = 1-2 seconds)
    ; Base: 60 frames, plus 0-63 random frames
    lda rng_seed
    and #$3F              ; 0-63
    clc
    adc #60               ; 60-123 frames between spawns
    sta spawn_timer

    ; Find an empty dollar slot
    ldx #$00
@find_slot:
    lda dollar_active, x
    beq @spawn_dollar
    inx
    cpx #NUM_DOLLARS
    bne @find_slot
    jmp @spawn_done       ; No empty slots

@spawn_dollar:
    ; Set dollar X to right side of screen
    lda #$F8
    sta dollar_x, x

    ; Better RNG for lane selection
    ; Use a combination of frame_counter, scroll_x, and previous seed
    lda rng_seed
    asl a
    asl a
    eor frame_counter
    adc scroll_x
    eor scroll_x_hi
    sta rng_seed
    
    ; Get lane 0-3, but avoid same lane as last time
    and #$03
    cmp last_lane         ; Same as last lane?
    bne @lane_ok
    ; Same lane - shift to next lane
    clc
    adc #$01
    and #$03              ; Wrap to 0-3
@lane_ok:
    sta last_lane         ; Remember this lane
    tay
    lda lane_y_table, y
    sta dollar_y, x

    ; Activate dollar
    lda #$01
    sta dollar_active, x

@spawn_done:
    rts

; Lane Y position lookup table
lane_y_table:
    .byte LANE1_Y, LANE2_Y, LANE3_Y, LANE4_Y

; Energy meter threshold lookup table (avoids overflow)
; Block X is filled if hippo_heat < threshold[X]
meter_thresholds:
    .byte 30, 60, 90, 120, 150, 180, 210, 241

; "ENERGY" label - tile numbers for each letter (sprite pattern table 0)
; E=$20, N=$21, R=$22, G=$23, Y=$24
energy_label:
    .byte $20, $21, $20, $22, $23, $24  ; E, N, E, R, G, Y

; "TIME" label - tile numbers for each letter
; T=$2F, I=$30, M=$31, E=$20
time_label:
    .byte $2F, $30, $31, $20  ; T, I, M, E

; "SCORE" label - tile numbers for each letter
; S=$32, C=$33, O=$34, R=$22, E=$20
score_label:
    .byte $32, $33, $34, $22, $20  ; S, C, O, R, E

; Game over screen text labels
; A=$35, H=$36, K=$37, F=$38, P=$39, L=$3A, W=$3B, U=$3C, .=$3D, X=$3E

; "THANKS FOR PLAYING" (18 chars)
thanks_label:
    .byte $2F, $36, $35, $21, $37, $32  ; T, H, A, N, K, S
thanks_len = 6

for_label:
    .byte $38, $34, $22  ; F, O, R
for_len = 3

playing_label:
    .byte $39, $3A, $35, $24, $30, $21, $23  ; P, L, A, Y, I, N, G
playing_len = 7

; "CHECK OUT" (9 chars)
check_label:
    .byte $33, $36, $20, $33, $37  ; C, H, E, C, K
check_len = 5

out_label:
    .byte $34, $3C, $2F  ; O, U, T
out_len = 3

; "RESUPPLY.FI" (11 chars)
resupply_label:
    .byte $22, $20, $32, $3C, $39, $39, $3A, $24, $3D, $38, $30  ; R, E, S, U, P, P, L, Y, ., F, I
resupply_len = 11

; "PRESS START" (11 chars)
press_label:
    .byte $39, $22, $20, $32, $32  ; P, R, E, S, S
press_len = 5

start_label:
    .byte $32, $2F, $35, $22, $2F  ; S, T, A, R, T
start_len = 5

; "TO REPLAY" (9 chars)
to_label:
    .byte $2F, $34  ; T, O
to_len = 2

replay_label:
    .byte $22, $20, $39, $3A, $35, $24  ; R, E, P, L, A, Y
replay_len = 6

; Number tiles start at $25: 0=$25, 1=$26, 2=$27, etc.

;-----------------------------------------------------------------------------
; Check Collisions - Detect hippo collecting dollars
;-----------------------------------------------------------------------------
check_collisions:
    ldx #$00              ; Dollar index

@collision_loop:
    ; Check if dollar is active
    lda dollar_active, x
    beq @next_collision   ; Skip if inactive

    ; Check X overlap (hippo is at hippo_x, 48 pixels wide)
    lda dollar_x, x
    sec
    sbc hippo_x
    bcc @next_collision   ; Dollar is behind hippo
    cmp #48               ; Within 48 pixels? (hippo width)
    bcs @next_collision   ; Too far ahead

    ; Check Y overlap (same lane check - must be within 8 pixels)
    ; Lanes are 16 pixels apart, so 8 pixel tolerance = same lane only
    lda dollar_y, x
    sec
    sbc hippo_y
    bcc @check_negative_y
    cmp #8                ; Within 8 pixels vertically? (same lane)
    bcs @next_collision   ; Different lane
    jmp @collision_detected

@check_negative_y:
    ; Result was negative, check if close enough
    eor #$FF              ; Negate
    clc
    adc #$01
    cmp #8                ; Within 8 pixels?
    bcs @next_collision

@collision_detected:
    ; Collected a dollar! Deactivate it
    lda #$00
    sta dollar_active, x

    ; Play collect sound
    jsr play_collect_sound

    ; Add to score
    lda score_lo
    clc
    adc #$01              ; Add 1 to score per dollar
    sta score_lo
    bcc @add_energy
    inc score_hi

@add_energy:
    ; Add energy (reduce heat by 30, one meter block)
    lda hippo_heat
    sec
    sbc #30               ; Reduce heat by 30
    bcs @store_heat       ; No underflow
    lda #$00              ; Clamp to 0
@store_heat:
    sta hippo_heat

    ; Add 1 second to timer
    inc timer_seconds
    bne @do_jump          ; No overflow
    inc timer_seconds+1   ; Increment high byte

@do_jump:
    ; Launch hippo into the air (like a ramp)!
    lda hippo_state
    cmp #$01              ; Already jumping?
    beq @next_collision   ; Don't double-jump

    lda #$01
    sta hippo_state       ; Set jumping
    lda hippo_y
    sta hippo_land_y      ; Remember current Y as landing spot
    lda #<(-7)            ; Jump from collecting dollar!
    sta hippo_vel_y
    jmp @next_collision

@next_collision:
    inx
    cpx #NUM_DOLLARS
    beq @collision_done   ; If done, exit
    jmp @collision_loop   ; Otherwise continue loop

@collision_done:
    rts

;-----------------------------------------------------------------------------
; Sound Effect Routines
;-----------------------------------------------------------------------------

; Play the collect dollar sound - a cheerful two-note sparkle
play_collect_sound:
    ; Set up pulse 1 for a nice bell-like tone
    ; Duty 50% for smooth tone, constant volume, volume = 10
    lda #%10101010        ; Duty 50% (10), disable length counter (1), constant volume (0), volume 10
    sta APU_PULSE1_CTRL
    
    ; Disable sweep
    lda #$00
    sta APU_PULSE1_SWEEP
    
    ; First note: G5 (~784 Hz)
    ; Timer = 1789773 / (16 * 784) - 1 = 142 = $8E
    lda #$8E              ; Timer low
    sta APU_PULSE1_LO
    lda #$08              ; Timer high (0) + length counter load
    sta APU_PULSE1_HI
    
    ; Mark this as collect sound, set timer for first note (4 frames)
    lda #1
    sta sfx_type
    lda #4
    sta sfx_timer
    rts

; Play the jump sound - a quick rising "boing" effect
play_jump_sound:
    ; Set up pulse 1 for a bouncy jump sound
    ; Duty 25%, constant volume, volume = 10
    lda #%01001010        ; Duty 25% (01), disable length counter (0), constant volume (0), volume 10
    sta APU_PULSE1_CTRL
    
    ; Enable upward sweep for rising pitch effect
    ; Sweep enabled, period=2, negate (pitch rises), shift=3
    lda #%10101011        ; Enable (1), period 2 (010), negate (1), shift 3 (011)
    sta APU_PULSE1_SWEEP
    
    ; Set starting frequency for a lower tone (~330 Hz, E4)
    ; Timer = 1789773 / (16 * 330) - 1 = 338 = $152
    lda #$52              ; Timer low
    sta APU_PULSE1_LO
    lda #$09              ; Timer high ($01) + length counter load
    sta APU_PULSE1_HI
    
    ; Set sound timer for duration (8 frames)
    lda #2
    sta sfx_type
    lda #8
    sta sfx_timer
    rts

; Update sound effects (called each frame)
update_sound:
    lda sfx_timer
    beq @sound_done       ; No sound playing
    
    dec sfx_timer
    bne @sound_done       ; Still playing current phase
    
    ; Timer hit zero - check if we need second note (collect sound)
    lda sfx_type
    cmp #1
    bne @silence_sound    ; Not collect sound, just silence
    
    ; Play second note of collect arpeggio: C6 (~1047 Hz)
    ; Timer = 1789773 / (16 * 1047) - 1 = 106 = $6A
    lda #$6A              ; Timer low
    sta APU_PULSE1_LO
    lda #$08              ; Timer high + length counter load
    sta APU_PULSE1_HI
    
    ; Set timer for second note, clear type so we silence after
    lda #5
    sta sfx_timer
    lda #0
    sta sfx_type
    rts
    
@silence_sound:
    ; Sound finished - silence the channel
    lda #%00110000        ; Duty 00, constant volume, volume = 0 (silent)
    sta APU_PULSE1_CTRL
    lda #0
    sta sfx_type
    
@sound_done:
    rts

;-----------------------------------------------------------------------------
; Music Engine - 3-Channel: Melody (Pulse 2), Bass (Triangle), Drums (Noise)
;-----------------------------------------------------------------------------

; Start playing the jungle theme (loops) - all 3 channels
start_music:
    ; Initialize melody (Pulse 2)
    lda #<jungle_melody
    sta music_ptr
    lda #>jungle_melody
    sta music_ptr+1
    lda #1
    sta music_timer       ; Start immediately
    sta music_playing
    sta music_loop        ; Enable looping
    
    ; Set up pulse 2 for melody - 50% duty, volume 2
    lda #%10110010        ; Duty 50% (10), halt (1), const vol (1), vol=2
    sta APU_PULSE2_CTRL
    lda #$00
    sta APU_PULSE2_SWEEP  ; No sweep
    
    ; Initialize bass (Triangle)
    lda #<jungle_bass
    sta bass_ptr
    lda #>jungle_bass
    sta bass_ptr+1
    lda #1
    sta bass_timer
    
    ; Set up triangle - linear counter for sustain
    lda #%11111111        ; Halt flag + max linear counter (sustain)
    sta APU_TRI_CTRL
    
    ; Initialize drums (Noise)
    lda #<jungle_drums
    sta drum_ptr
    lda #>jungle_drums
    sta drum_ptr+1
    lda #1
    sta drum_timer
    
    ; Set up noise channel
    lda #%00111100        ; Halt (0), constant vol (0), vol=12
    sta APU_NOISE_CTRL
    rts

; Start title screen jingle (plays once) - melody only
start_title_jingle:
    lda #<title_jingle
    sta music_ptr
    lda #>title_jingle
    sta music_ptr+1
    lda #1
    sta music_timer
    lda #1
    sta music_playing
    lda #0
    sta music_loop        ; Don't loop
    
    ; Silence bass and drums for jingles
    lda #$00
    sta bass_timer
    sta drum_timer
    lda #%10000000        ; Silence triangle
    sta APU_TRI_CTRL
    lda #%00110000        ; Silence noise
    sta APU_NOISE_CTRL
    
    ; Set up pulse 2 - brighter sound for title, volume 5
    lda #%01110101        ; Duty 25% (01), halt (1), const vol (1), vol=5
    sta APU_PULSE2_CTRL
    lda #$00
    sta APU_PULSE2_SWEEP
    rts

; Start game over jingle (plays once) - melody only
start_gameover_jingle:
    lda #<gameover_jingle
    sta music_ptr
    lda #>gameover_jingle
    sta music_ptr+1
    lda #1
    sta music_timer
    lda #1
    sta music_playing
    lda #0
    sta music_loop        ; Don't loop
    
    ; Silence bass and drums for jingles
    lda #$00
    sta bass_timer
    sta drum_timer
    lda #%10000000        ; Silence triangle
    sta APU_TRI_CTRL
    lda #%00110000        ; Silence noise
    sta APU_NOISE_CTRL
    
    ; Set up pulse 2 - sadder tone, volume 4
    lda #%10110100        ; Duty 50% (10), halt (1), const vol (1), vol=4
    sta APU_PULSE2_CTRL
    lda #$00
    sta APU_PULSE2_SWEEP
    rts

; Stop music - silence all channels
stop_music:
    lda #$00
    sta music_playing
    sta bass_timer
    sta drum_timer
    lda #%00110000        ; Silence pulse 2
    sta APU_PULSE2_CTRL
    lda #%10000000        ; Silence triangle
    sta APU_TRI_CTRL
    lda #%00110000        ; Silence noise
    sta APU_NOISE_CTRL
    rts

; Update music (called each frame) - updates all 3 channels
update_music:
    lda music_playing
    beq @all_done         ; Music not playing
    
    ; === Update Melody (Pulse 2) ===
    jsr update_melody
    
    ; === Update Bass (Triangle) ===
    jsr update_bass
    
    ; === Update Drums (Noise) ===
    jsr update_drums
    
@all_done:
    rts

;--- Update Melody Channel (Pulse 2) ---
update_melody:
    dec music_timer
    bne @melody_done      ; Not time for next note yet
    
    ; Time for next note - read from music data
    ldy #0
    lda (music_ptr), y    ; Get note duration
    beq @loop_melody      ; 0 = end of song, loop
    sta music_timer       ; Set duration
    
    iny
    lda (music_ptr), y    ; Get note pitch (timer low)
    cmp #$FF              ; $FF = rest
    beq @melody_rest
    sta APU_PULSE2_LO
    
    iny  
    lda (music_ptr), y    ; Get timer high
    sta APU_PULSE2_HI
    
    ; Re-enable sound (in case we were resting)
    lda #%10110010        ; Duty 50% (10), halt (1), const vol (1), vol=2
    sta APU_PULSE2_CTRL
    jmp @advance_melody
    
@melody_rest:
    ; Silence for rest
    lda #%00110000
    sta APU_PULSE2_CTRL
    
@advance_melody:
    ; Advance pointer by 3 bytes (duration, pitch_lo, pitch_hi)
    lda music_ptr
    clc
    adc #3
    sta music_ptr
    bcc @melody_done
    inc music_ptr+1
    
@melody_done:
    rts
    
@loop_melody:
    ; Check if we should loop
    lda music_loop
    beq @end_jingle       ; Don't loop - just stop
    
    ; Reset all channels to beginning
    lda #<jungle_melody
    sta music_ptr
    lda #>jungle_melody
    sta music_ptr+1
    lda #<jungle_bass
    sta bass_ptr
    lda #>jungle_bass
    sta bass_ptr+1
    lda #<jungle_drums
    sta drum_ptr
    lda #>jungle_drums
    sta drum_ptr+1
    lda #1
    sta music_timer
    sta bass_timer
    sta drum_timer
    rts
    
@end_jingle:
    ; Jingle finished - stop playing
    lda #0
    sta music_playing
    lda #%00110000        ; Silence pulse 2
    sta APU_PULSE2_CTRL
    rts

;--- Update Bass Channel (Triangle) ---
update_bass:
    lda bass_timer
    beq @bass_done        ; Bass not active
    
    dec bass_timer
    bne @bass_done        ; Not time for next note yet
    
    ; Time for next bass note
    ldy #0
    lda (bass_ptr), y     ; Get note duration
    beq @bass_end         ; 0 = end of bass pattern
    sta bass_timer        ; Set duration
    
    iny
    lda (bass_ptr), y     ; Get note pitch (timer low)
    cmp #$FF              ; $FF = rest
    beq @bass_rest
    sta APU_TRI_LO
    
    iny  
    lda (bass_ptr), y     ; Get timer high
    sta APU_TRI_HI
    
    ; Enable triangle
    lda #%11111111        ; Max linear counter (sustain)
    sta APU_TRI_CTRL
    jmp @advance_bass
    
@bass_rest:
    ; Silence triangle for rest
    lda #%10000000        ; Halt, counter = 0
    sta APU_TRI_CTRL
    
@advance_bass:
    ; Advance pointer by 3 bytes
    lda bass_ptr
    clc
    adc #3
    sta bass_ptr
    bcc @bass_done
    inc bass_ptr+1
    
@bass_done:
    rts
    
@bass_end:
    ; Bass pattern ended - will be reset when melody loops
    lda #0
    sta bass_timer
    lda #%10000000
    sta APU_TRI_CTRL
    rts

;--- Update Drums Channel (Noise) ---
update_drums:
    lda drum_timer
    beq @drum_done        ; Drums not active
    
    dec drum_timer
    bne @drum_done        ; Not time for next hit yet
    
    ; Time for next drum hit
    ldy #0
    lda (drum_ptr), y     ; Get duration
    beq @drum_end         ; 0 = end of drum pattern
    sta drum_timer        ; Set duration
    
    iny
    lda (drum_ptr), y     ; Get drum type/volume
    cmp #$FF              ; $FF = rest
    beq @drum_rest
    sta APU_NOISE_CTRL    ; Volume and envelope
    
    iny  
    lda (drum_ptr), y     ; Get noise period (pitch)
    sta APU_NOISE_LO
    
    ; Trigger the noise
    lda #%11111000        ; Length counter load
    sta APU_NOISE_HI
    jmp @advance_drum
    
@drum_rest:
    ; Silence drums for rest
    lda #%00110000
    sta APU_NOISE_CTRL
    
@advance_drum:
    ; Advance pointer by 3 bytes
    lda drum_ptr
    clc
    adc #3
    sta drum_ptr
    bcc @drum_done
    inc drum_ptr+1
    
@drum_done:
    rts
    
@drum_end:
    ; Drum pattern ended - will be reset when melody loops
    lda #0
    sta drum_timer
    lda #%00110000
    sta APU_NOISE_CTRL
    rts

;-----------------------------------------------------------------------------
; Jungle Action Melody Data - Extended Version with Progression
; Format: duration (frames), timer_lo, timer_hi (0 = end)
; Timer values: lower = higher pitch
;-----------------------------------------------------------------------------
; Note timer values (approximate):
; C3=855/$357  D3=762/$2FA  E3=679/$2A7  F3=640/$280  G3=570/$23A
; A3=508/$1FC  B3=453/$1C5  
; C4=428/$1AC  D4=381/$17D  E4=339/$153  F4=320/$140  G4=285/$11D
; A4=254/$0FE  B4=226/$0E2  C5=214/$0D6  D5=190/$0BE  E5=170/$0AA
; F5=160/$0A0  G5=142/$08E  A5=127/$07F  B5=113/$071  C6=107/$06B
; D6=95/$05F   E6=85/$055

jungle_melody:
    ;=========================================================================
    ; SECTION A - Intro Theme (E minor, driving rhythm)
    ; Establishes the main motif
    ;=========================================================================
    ; Measure 1-2: Main hook
    .byte 8,   $53, $09    ; E4 - quick
    .byte 8,   $53, $09    ; E4
    .byte 8,   $1D, $09    ; G4
    .byte 8,   $53, $09    ; E4
    .byte 12,  $FE, $08    ; A4 - slightly longer
    .byte 8,   $1D, $09    ; G4
    .byte 16,  $53, $09    ; E4 - hold
    .byte 8,   $FF, $00    ; Rest
    
    ; Measure 3-4: Repeat with variation
    .byte 8,   $53, $09    ; E4
    .byte 8,   $1D, $09    ; G4
    .byte 8,   $53, $09    ; E4
    .byte 8,   $1D, $09    ; G4
    .byte 12,  $E2, $08    ; B4 - go higher this time
    .byte 8,   $FE, $08    ; A4
    .byte 16,  $1D, $09    ; G4 - hold
    .byte 8,   $FF, $00    ; Rest
    
    ;=========================================================================
    ; SECTION A' - Variation with climb
    ;=========================================================================
    ; Measure 5-6: Building energy
    .byte 8,   $1D, $09    ; G4
    .byte 8,   $1D, $09    ; G4  
    .byte 8,   $FE, $08    ; A4
    .byte 8,   $1D, $09    ; G4
    .byte 12,  $E2, $08    ; B4
    .byte 8,   $FE, $08    ; A4
    .byte 16,  $1D, $09    ; G4 - hold
    .byte 8,   $FF, $00    ; Rest
    
    ; Measure 7-8: Peak of A section
    .byte 6,   $E2, $08    ; B4 - faster rhythm
    .byte 6,   $E2, $08    ; B4
    .byte 6,   $D6, $08    ; C5
    .byte 6,   $E2, $08    ; B4
    .byte 10,  $FE, $08    ; A4
    .byte 10,  $1D, $09    ; G4
    .byte 20,  $53, $09    ; E4 - long hold
    .byte 12,  $FF, $00    ; Rest - breath
    
    ;=========================================================================
    ; SECTION B - Development (G major feel, brighter)
    ; Contrasting section with new melodic material
    ;=========================================================================
    ; Measure 9-10: New theme - ascending
    .byte 10,  $1D, $09    ; G4
    .byte 10,  $FE, $08    ; A4
    .byte 10,  $E2, $08    ; B4
    .byte 14,  $D6, $08    ; C5 - hold
    .byte 8,   $FF, $00    ; Rest
    .byte 10,  $E2, $08    ; B4
    .byte 10,  $D6, $08    ; C5
    .byte 14,  $BE, $08    ; D5 - higher!
    .byte 8,   $FF, $00    ; Rest
    
    ; Measure 11-12: Descending answer
    .byte 10,  $BE, $08    ; D5
    .byte 10,  $D6, $08    ; C5
    .byte 10,  $E2, $08    ; B4
    .byte 14,  $FE, $08    ; A4
    .byte 8,   $FF, $00    ; Rest
    .byte 10,  $FE, $08    ; A4
    .byte 10,  $1D, $09    ; G4
    .byte 14,  $53, $09    ; E4
    .byte 12,  $FF, $00    ; Rest
    
    ; Measure 13-14: Playful syncopation
    .byte 6,   $1D, $09    ; G4
    .byte 10,  $FF, $00    ; Rest  
    .byte 6,   $1D, $09    ; G4
    .byte 6,   $E2, $08    ; B4
    .byte 12,  $D6, $08    ; C5
    .byte 6,   $FF, $00    ; Rest
    .byte 6,   $E2, $08    ; B4
    .byte 16,  $1D, $09    ; G4
    .byte 8,   $FF, $00    ; Rest
    
    ; Measure 15-16: Syncopation continues
    .byte 6,   $FE, $08    ; A4
    .byte 10,  $FF, $00    ; Rest
    .byte 6,   $FE, $08    ; A4
    .byte 6,   $D6, $08    ; C5
    .byte 12,  $BE, $08    ; D5
    .byte 6,   $FF, $00    ; Rest
    .byte 6,   $D6, $08    ; C5
    .byte 16,  $FE, $08    ; A4
    .byte 12,  $FF, $00    ; Rest - breath
    
    ;=========================================================================
    ; SECTION C - Climax (Higher register, intense)
    ; Peak energy section
    ;=========================================================================
    ; Measure 17-18: Soaring melody
    .byte 8,   $D6, $08    ; C5
    .byte 8,   $BE, $08    ; D5
    .byte 12,  $AA, $08    ; E5 - highest yet!
    .byte 8,   $BE, $08    ; D5
    .byte 8,   $D6, $08    ; C5
    .byte 12,  $BE, $08    ; D5
    .byte 20,  $AA, $08    ; E5 - hold
    .byte 8,   $FF, $00    ; Rest
    
    ; Measure 19-20: Continue high energy
    .byte 8,   $AA, $08    ; E5
    .byte 8,   $AA, $08    ; E5
    .byte 6,   $8E, $08    ; G5 - peak!
    .byte 6,   $AA, $08    ; E5
    .byte 12,  $BE, $08    ; D5
    .byte 8,   $D6, $08    ; C5
    .byte 8,   $E2, $08    ; B4
    .byte 16,  $D6, $08    ; C5 - hold
    .byte 8,   $FF, $00    ; Rest
    
    ; Measure 21-22: Triumphant phrase
    .byte 6,   $8E, $08    ; G5 - quick
    .byte 6,   $8E, $08    ; G5
    .byte 6,   $AA, $08    ; E5
    .byte 6,   $8E, $08    ; G5
    .byte 14,  $AA, $08    ; E5
    .byte 8,   $BE, $08    ; D5
    .byte 20,  $D6, $08    ; C5
    .byte 10,  $FF, $00    ; Rest
    
    ; Measure 23-24: Descending run back down
    .byte 6,   $AA, $08    ; E5
    .byte 6,   $BE, $08    ; D5
    .byte 6,   $D6, $08    ; C5
    .byte 6,   $E2, $08    ; B4
    .byte 6,   $FE, $08    ; A4
    .byte 6,   $1D, $09    ; G4
    .byte 8,   $53, $09    ; E4
    .byte 20,  $1D, $09    ; G4 - landing
    .byte 12,  $FF, $00    ; Rest - breath
    
    ;=========================================================================
    ; SECTION D - Bridge (Calmer, builds tension)
    ; Lower register, mysterious feel before final return
    ;=========================================================================
    ; Measure 25-26: Mysterious low melody
    .byte 14,  $53, $09    ; E4 - slower, deliberate
    .byte 14,  $1D, $09    ; G4
    .byte 14,  $53, $09    ; E4
    .byte 20,  $7D, $09    ; D4 - low!
    .byte 10,  $FF, $00    ; Rest
    
    ; Measure 27-28: Tension building
    .byte 14,  $53, $09    ; E4
    .byte 14,  $1D, $09    ; G4
    .byte 14,  $FE, $08    ; A4
    .byte 20,  $E2, $08    ; B4 - rising
    .byte 10,  $FF, $00    ; Rest
    
    ; Measure 29-30: More tension
    .byte 10,  $1D, $09    ; G4
    .byte 10,  $FE, $08    ; A4
    .byte 10,  $E2, $08    ; B4
    .byte 10,  $D6, $08    ; C5
    .byte 10,  $E2, $08    ; B4
    .byte 10,  $D6, $08    ; C5
    .byte 14,  $BE, $08    ; D5 - preparing for return
    .byte 12,  $FF, $00    ; Rest
    
    ;=========================================================================
    ; SECTION A'' - Return of Main Theme (Embellished)
    ; Familiar melody returns with ornaments
    ;=========================================================================
    ; Measure 31-32: Main hook with embellishment
    .byte 6,   $53, $09    ; E4 - quicker
    .byte 6,   $1D, $09    ; G4 - grace note
    .byte 6,   $53, $09    ; E4
    .byte 8,   $1D, $09    ; G4
    .byte 8,   $53, $09    ; E4
    .byte 12,  $FE, $08    ; A4
    .byte 8,   $1D, $09    ; G4
    .byte 16,  $53, $09    ; E4 - hold
    .byte 6,   $FF, $00    ; Rest
    
    ; Measure 33-34: Energetic variation
    .byte 6,   $53, $09    ; E4
    .byte 6,   $53, $09    ; E4
    .byte 6,   $1D, $09    ; G4
    .byte 6,   $1D, $09    ; G4
    .byte 6,   $FE, $08    ; A4
    .byte 6,   $FE, $08    ; A4
    .byte 12,  $E2, $08    ; B4 - peak
    .byte 8,   $FE, $08    ; A4
    .byte 16,  $1D, $09    ; G4 - hold
    .byte 8,   $FF, $00    ; Rest
    
    ;=========================================================================
    ; SECTION E - Finale (Grand ending before loop)
    ; Big finish with fanfare-like quality
    ;=========================================================================
    ; Measure 35-36: Building to finale
    .byte 8,   $E2, $08    ; B4
    .byte 8,   $D6, $08    ; C5
    .byte 8,   $BE, $08    ; D5
    .byte 12,  $AA, $08    ; E5
    .byte 8,   $FF, $00    ; Rest
    .byte 8,   $AA, $08    ; E5
    .byte 8,   $BE, $08    ; D5
    .byte 12,  $D6, $08    ; C5
    .byte 8,   $FF, $00    ; Rest
    
    ; Measure 37-38: Fanfare sequence
    .byte 6,   $D6, $08    ; C5
    .byte 6,   $AA, $08    ; E5
    .byte 6,   $8E, $08    ; G5
    .byte 14,  $AA, $08    ; E5 - hold
    .byte 6,   $D6, $08    ; C5
    .byte 6,   $AA, $08    ; E5
    .byte 6,   $8E, $08    ; G5
    .byte 14,  $AA, $08    ; E5 - hold
    .byte 8,   $FF, $00    ; Rest
    
    ; Measure 39-40: Final descent
    .byte 10,  $8E, $08    ; G5 - high
    .byte 10,  $AA, $08    ; E5
    .byte 10,  $D6, $08    ; C5
    .byte 10,  $E2, $08    ; B4
    .byte 10,  $FE, $08    ; A4
    .byte 10,  $1D, $09    ; G4
    .byte 24,  $53, $09    ; E4 - long final note
    .byte 16,  $FF, $00    ; Rest before loop
    
    ; End marker - triggers loop
    .byte 0, 0, 0

;-----------------------------------------------------------------------------
; Title Screen Jingle - Welcoming, adventurous fanfare
;-----------------------------------------------------------------------------
title_jingle:
    ; Uplifting C major arpeggio fanfare
    .byte 10,  $D6, $08    ; C5
    .byte 10,  $AA, $08    ; E5
    .byte 10,  $8E, $08    ; G5
    .byte 20,  $6B, $08    ; C6 - hold high note
    .byte 8,   $FF, $00    ; Rest
    .byte 8,   $8E, $08    ; G5
    .byte 8,   $6B, $08    ; C6
    .byte 25,  $AA, $08    ; E5 - resolve
    .byte 0, 0, 0          ; End

;-----------------------------------------------------------------------------
; Game Over Jingle - Descending, melancholy
;-----------------------------------------------------------------------------
gameover_jingle:
    ; Descending minor phrase
    .byte 15,  $8E, $08    ; G5
    .byte 15,  $AA, $08    ; E5 (but feels minor in context)
    .byte 15,  $D6, $08    ; C5
    .byte 10,  $FF, $00    ; Rest
    .byte 20,  $FE, $08    ; A4
    .byte 30,  $53, $09    ; E4 - low sad ending
    .byte 0, 0, 0          ; End

;-----------------------------------------------------------------------------
; Jungle Bass Line (Triangle Channel) - Driving bass rhythm
; Format: duration (frames), timer_lo, timer_hi (0 = end)
; Triangle plays one octave lower than pulse for deep bass
;-----------------------------------------------------------------------------
; Bass note timer values (low octave for proper bass sound):
; E2=1358/$54E  F2=1281/$501  G2=1140/$474  A2=1016/$3F8  B2=905/$389
; C2=1712/$6B0  D2=1524/$5F4
; Keep bass in E2-B2 range for best sound quality

jungle_bass:
    ;=========================================================================
    ; SECTION A - Intro (matches melody sections A + A')
    ; Driving root notes, E minor foundation
    ;=========================================================================
    ; Measures 1-4: E minor groove
    .byte 16,  $4E, $05    ; E2 - root, half measure
    .byte 16,  $4E, $05    ; E2
    .byte 16,  $74, $04    ; G2
    .byte 16,  $4E, $05    ; E2
    .byte 16,  $4E, $05    ; E2
    .byte 16,  $74, $04    ; G2
    .byte 16,  $F8, $03    ; A2
    .byte 16,  $74, $04    ; G2
    
    ; Measures 5-8: Building with movement
    .byte 16,  $74, $04    ; G2
    .byte 16,  $74, $04    ; G2
    .byte 16,  $F8, $03    ; A2
    .byte 16,  $74, $04    ; G2
    .byte 12,  $89, $03    ; B2
    .byte 12,  $F8, $03    ; A2
    .byte 24,  $74, $04    ; G2
    .byte 12,  $FF, $00    ; Rest
    
    ;=========================================================================
    ; SECTION B - Development (brighter, G major feel)
    ;=========================================================================
    ; Measures 9-12: G-based movement (stay in low octave)
    .byte 20,  $74, $04    ; G2
    .byte 20,  $F8, $03    ; A2
    .byte 20,  $89, $03    ; B2
    .byte 20,  $74, $04    ; G2
    .byte 20,  $89, $03    ; B2
    .byte 20,  $74, $04    ; G2
    .byte 20,  $F8, $03    ; A2
    .byte 20,  $74, $04    ; G2
    
    ; Measures 13-16: Syncopated bass
    .byte 12,  $74, $04    ; G2 - quick
    .byte 12,  $FF, $00    ; Rest
    .byte 12,  $74, $04    ; G2
    .byte 12,  $89, $03    ; B2
    .byte 24,  $74, $04    ; G2 - hold
    .byte 12,  $89, $03    ; B2
    .byte 24,  $74, $04    ; G2
    .byte 12,  $FF, $00    ; Rest
    .byte 12,  $F8, $03    ; A2
    .byte 12,  $FF, $00    ; Rest
    .byte 12,  $F8, $03    ; A2
    .byte 12,  $74, $04    ; G2
    .byte 24,  $F8, $03    ; A2
    .byte 12,  $74, $04    ; G2
    .byte 24,  $F8, $03    ; A2
    .byte 12,  $FF, $00    ; Rest
    
    ;=========================================================================
    ; SECTION C - Climax (driving, energetic - still low octave)
    ;=========================================================================
    ; Measures 17-20: High energy bass (E2-B2 range)
    .byte 16,  $74, $04    ; G2
    .byte 16,  $F8, $03    ; A2
    .byte 16,  $4E, $05    ; E2 - back to root
    .byte 16,  $F8, $03    ; A2
    .byte 16,  $4E, $05    ; E2
    .byte 16,  $4E, $05    ; E2
    .byte 16,  $74, $04    ; G2
    .byte 16,  $4E, $05    ; E2
    
    ; Measures 21-24: Driving pattern
    .byte 16,  $74, $04    ; G2
    .byte 16,  $4E, $05    ; E2
    .byte 16,  $74, $04    ; G2
    .byte 16,  $89, $03    ; B2
    .byte 16,  $F8, $03    ; A2
    .byte 16,  $74, $04    ; G2
    .byte 24,  $4E, $05    ; E2 - landing on root
    .byte 16,  $FF, $00    ; Rest
    
    ;=========================================================================
    ; SECTION D - Bridge (mysterious, low)
    ;=========================================================================
    ; Measures 25-30: Low sustained notes
    .byte 32,  $4E, $05    ; E2 - long
    .byte 32,  $74, $04    ; G2
    .byte 32,  $4E, $05    ; E2
    .byte 32,  $F4, $05    ; D2 - low
    .byte 32,  $4E, $05    ; E2
    .byte 32,  $74, $04    ; G2
    .byte 24,  $F8, $03    ; A2
    .byte 24,  $89, $03    ; B2
    .byte 24,  $74, $04    ; G2
    .byte 24,  $F8, $03    ; A2 - rising tension
    
    ;=========================================================================
    ; SECTION A'' + E - Return and Finale
    ;=========================================================================
    ; Measures 31-36: Return to main groove
    .byte 14,  $4E, $05    ; E2
    .byte 14,  $74, $04    ; G2
    .byte 14,  $4E, $05    ; E2
    .byte 14,  $74, $04    ; G2
    .byte 14,  $F8, $03    ; A2
    .byte 14,  $74, $04    ; G2
    .byte 20,  $4E, $05    ; E2
    .byte 12,  $FF, $00    ; Rest
    
    ; Measures 37-40: Finale
    .byte 16,  $89, $03    ; B2
    .byte 16,  $74, $04    ; G2
    .byte 16,  $F8, $03    ; A2
    .byte 16,  $4E, $05    ; E2
    .byte 16,  $F8, $03    ; A2
    .byte 16,  $74, $04    ; G2
    .byte 16,  $89, $03    ; B2
    .byte 16,  $F8, $03    ; A2
    .byte 16,  $74, $04    ; G2
    .byte 32,  $4E, $05    ; E2 - final root
    .byte 16,  $FF, $00    ; Rest before loop
    
    ; End marker
    .byte 0, 0, 0

;-----------------------------------------------------------------------------
; Jungle Drum Pattern (Noise Channel) - Disabled
; The noise channel can sound harsh, so we'll let melody + bass carry the music
;-----------------------------------------------------------------------------

jungle_drums:
    ; Just silence - no drums
    .byte 255, $FF, $00    ; Very long rest
    .byte 255, $FF, $00    ; Very long rest
    .byte 255, $FF, $00    ; Very long rest
    .byte 255, $FF, $00    ; Very long rest
    .byte 255, $FF, $00    ; Very long rest
    .byte 255, $FF, $00    ; Very long rest
    .byte 255, $FF, $00    ; Very long rest
    .byte 255, $FF, $00    ; Very long rest
    
    ; End marker
    .byte 0, 0, 0
    ; End marker
    .byte 0, 0, 0

;-----------------------------------------------------------------------------
; Update Sprites - Write sprite data to OAM buffer
;-----------------------------------------------------------------------------
update_sprites:
    ; --- Draw Hippo (18 sprites in a 3x6 grid for 24x48 hippo) ---
    ; Grid: 6 columns (tiles 0-5), 3 rows (tiles 0-2 per column)
    ; Total: 18 tiles, tile $00-$11
    
    ; Row 0 (top row, y offset = 0)
    ; Sprite 0 - Row 0, Col 0 (back)
    lda hippo_y
    sta oam_buffer+0      ; Y position
    lda #$00              ; Tile $00
    sta oam_buffer+1
    lda #$00              ; Attributes (palette 0, no flip)
    sta oam_buffer+2
    lda hippo_x
    sta oam_buffer+3      ; X position

    ; Sprite 1 - Row 0, Col 1 (body back top)
    lda hippo_y
    sta oam_buffer+4
    lda #$01              ; Tile $01
    sta oam_buffer+5
    lda #$00
    sta oam_buffer+6
    lda hippo_x
    clc
    adc #$08
    sta oam_buffer+7

    ; Sprite 2 - Row 0, Col 2 (body mid top)
    lda hippo_y
    sta oam_buffer+8
    lda #$02              ; Tile $02
    sta oam_buffer+9
    lda #$00
    sta oam_buffer+10
    lda hippo_x
    clc
    adc #$10
    sta oam_buffer+11

    ; Sprite 3 - Row 0, Col 3 (body front top with ear)
    lda hippo_y
    sta oam_buffer+12
    lda #$03              ; Tile $03
    sta oam_buffer+13
    lda #$00
    sta oam_buffer+14
    lda hippo_x
    clc
    adc #$18
    sta oam_buffer+15

    ; Sprite 4 - Row 0, Col 4 (head top)
    lda hippo_y
    sta oam_buffer+16
    lda #$04              ; Tile $04
    sta oam_buffer+17
    lda #$00
    sta oam_buffer+18
    lda hippo_x
    clc
    adc #$20
    sta oam_buffer+19

    ; Sprite 5 - Row 0, Col 5 (snout top)
    lda hippo_y
    sta oam_buffer+20
    lda #$05              ; Tile $05
    sta oam_buffer+21
    lda #$00
    sta oam_buffer+22
    lda hippo_x
    clc
    adc #$28
    sta oam_buffer+23

    ; Row 1 (middle row, y offset = 8)
    ; Sprite 6 - Row 1, Col 0 (back middle)
    lda hippo_y
    clc
    adc #$08
    sta oam_buffer+24
    lda #$06              ; Tile $06
    sta oam_buffer+25
    lda #$00
    sta oam_buffer+26
    lda hippo_x
    sta oam_buffer+27

    ; Sprite 7 - Row 1, Col 1 (body)
    lda hippo_y
    clc
    adc #$08
    sta oam_buffer+28
    lda #$07              ; Tile $07
    sta oam_buffer+29
    lda #$00
    sta oam_buffer+30
    lda hippo_x
    clc
    adc #$08
    sta oam_buffer+31

    ; Sprite 8 - Row 1, Col 2 (belly)
    lda hippo_y
    clc
    adc #$08
    sta oam_buffer+32
    lda #$08              ; Tile $08
    sta oam_buffer+33
    lda #$00
    sta oam_buffer+34
    lda hippo_x
    clc
    adc #$10
    sta oam_buffer+35

    ; Sprite 9 - Row 1, Col 3 (belly/neck)
    lda hippo_y
    clc
    adc #$08
    sta oam_buffer+36
    lda #$09              ; Tile $09
    sta oam_buffer+37
    lda #$00
    sta oam_buffer+38
    lda hippo_x
    clc
    adc #$18
    sta oam_buffer+39

    ; Sprite 10 - Row 1, Col 4 (face)
    lda hippo_y
    clc
    adc #$08
    sta oam_buffer+40
    lda #$0A              ; Tile $0A
    sta oam_buffer+41
    lda #$00
    sta oam_buffer+42
    lda hippo_x
    clc
    adc #$20
    sta oam_buffer+43

    ; Sprite 11 - Row 1, Col 5 (snout middle)
    lda hippo_y
    clc
    adc #$08
    sta oam_buffer+44
    lda #$0B              ; Tile $0B
    sta oam_buffer+45
    lda #$00
    sta oam_buffer+46
    lda hippo_x
    clc
    adc #$28
    sta oam_buffer+47

    ; Row 2 (bottom row, y offset = 16)
    ; Sprite 12 - Row 2, Col 0 (back leg)
    lda hippo_y
    clc
    adc #$10
    sta oam_buffer+48
    lda #$0C              ; Tile $0C
    sta oam_buffer+49
    lda #$00
    sta oam_buffer+50
    lda hippo_x
    sta oam_buffer+51

    ; Sprite 13 - Row 2, Col 1 (back leg/belly)
    lda hippo_y
    clc
    adc #$10
    sta oam_buffer+52
    lda #$0D              ; Tile $0D
    sta oam_buffer+53
    lda #$00
    sta oam_buffer+54
    lda hippo_x
    clc
    adc #$08
    sta oam_buffer+55

    ; Sprite 14 - Row 2, Col 2 (belly bottom)
    lda hippo_y
    clc
    adc #$10
    sta oam_buffer+56
    lda #$0E              ; Tile $0E
    sta oam_buffer+57
    lda #$00
    sta oam_buffer+58
    lda hippo_x
    clc
    adc #$10
    sta oam_buffer+59

    ; Sprite 15 - Row 2, Col 3 (front leg back)
    lda hippo_y
    clc
    adc #$10
    sta oam_buffer+60
    lda #$0F              ; Tile $0F
    sta oam_buffer+61
    lda #$00
    sta oam_buffer+62
    lda hippo_x
    clc
    adc #$18
    sta oam_buffer+63

    ; Sprite 16 - Row 2, Col 4 (front leg)
    lda hippo_y
    clc
    adc #$10
    sta oam_buffer+64
    lda #$10              ; Tile $10
    sta oam_buffer+65
    lda #$00
    sta oam_buffer+66
    lda hippo_x
    clc
    adc #$20
    sta oam_buffer+67

    ; Sprite 17 - Row 2, Col 5 (snout bottom)
    lda hippo_y
    clc
    adc #$10
    sta oam_buffer+68
    lda #$11              ; Tile $11
    sta oam_buffer+69
    lda #$00
    sta oam_buffer+70
    lda hippo_x
    clc
    adc #$28
    sta oam_buffer+71

    ; --- Draw Dollars ---
    lda #72               ; Start at OAM offset 72 (after hippo's 18 sprites)
    sta temp2             ; OAM index

    ldx #$00              ; Dollar index
@draw_dollar_loop:
    lda dollar_active, x
    beq @skip_dollar      ; Skip inactive dollars

    ; Get OAM index
    ldy temp2

    ; Y position - align dollar with middle of hippo (hippo is 24px tall)
    ; Dollar should appear at hippo_y + 8 to be vertically centered
    lda dollar_y, x
    clc
    adc #$08              ; Offset to align with hippo center
    sta oam_buffer, y
    iny

    ; Tile number - use tile $18 for dollar sign
    lda #$18
    sta oam_buffer, y
    iny

    ; Attributes (palette 1 for yellow/gold color)
    lda #$01
    sta oam_buffer, y
    iny

    ; X position
    lda dollar_x, x
    sta oam_buffer, y
    iny

    ; Update OAM index
    sty temp2

@skip_dollar:
    inx
    cpx #NUM_DOLLARS
    bne @draw_dollar_loop

    ; Load OAM index (temp2 has correct value even if no dollars drawn)
    ldy temp2

    ; --- Draw Timer (TIME label + 3 digits) ---
    ; Draw "TIME" label at X=8, Y=8
    ldx #$00
@draw_time_label:
    lda #$08              ; Y = 8
    sta oam_buffer, y
    iny
    lda time_label, x
    sta oam_buffer, y
    iny
    lda #$00              ; Palette 0 (white)
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$08              ; X position starting at 8
    sta oam_buffer, y
    iny
    inx
    cpx #$04              ; 4 letters
    bne @draw_time_label
    
    ; Save OAM index, prepare for division
    sty temp2
    
    ; Convert timer_seconds to 3 digits
    lda timer_seconds
    sta temp1             ; Working value
    
    ; Cap at 255 for display if high byte is set
    lda timer_seconds+1
    beq @do_div
    lda #$FF
    sta temp1
    
@do_div:
    ; Hundreds digit: divide by 100
    ldx #$00
@div100_loop:
    lda temp1
    cmp #100
    bcc @hundreds_done
    sec
    sbc #100
    sta temp1
    inx
    jmp @div100_loop
@hundreds_done:
    stx ptr_lo            ; Save hundreds
    
    ; Tens digit
    ldx #$00
@div10_loop:
    lda temp1
    cmp #10
    bcc @tens_done
    sec
    sbc #10
    sta temp1
    inx
    jmp @div10_loop
@tens_done:
    stx ptr_hi            ; Save tens (temp1 has ones)
    
    ; Draw 3 digits
    ldy temp2
    
    ; Hundreds
    lda #$10              ; Y = 16
    sta oam_buffer, y
    iny
    lda ptr_lo
    clc
    adc #$25
    sta oam_buffer, y
    iny
    lda #$00              ; Palette 0 (white)
    sta oam_buffer, y
    iny
    lda #$08              ; X = 8
    sta oam_buffer, y
    iny
    
    ; Tens
    lda #$10              ; Y = 16
    sta oam_buffer, y
    iny
    lda ptr_hi
    clc
    adc #$25
    sta oam_buffer, y
    iny
    lda #$00              ; Palette 0 (white)
    sta oam_buffer, y
    iny
    lda #$10              ; X = 16
    sta oam_buffer, y
    iny
    
    ; Ones
    lda #$10              ; Y = 16
    sta oam_buffer, y
    iny
    lda temp1
    clc
    adc #$25
    sta oam_buffer, y
    iny
    lda #$00              ; Palette 0 (white)
    sta oam_buffer, y
    iny
    lda #$18              ; X = 24
    sta oam_buffer, y
    iny

    ; Save OAM index after timer digits
    sty temp2

    ; --- Draw "SCORE" label and high score ---
    ; Draw "SCORE" label at X=8, Y=32
    ldx #$00
@draw_score_label:
    lda #$20              ; Y = 32
    sta oam_buffer, y
    iny
    lda score_label, x
    sta oam_buffer, y
    iny
    lda #$00              ; Palette 0 (white)
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$08              ; X position starting at 8
    sta oam_buffer, y
    iny
    inx
    cpx #$05              ; 5 letters
    bne @draw_score_label
    
    ; Save OAM index
    sty temp2
    
    ; Convert current score to 3 digits (use score_lo, 0-255)
    lda score_lo
    sta temp1
    
    ; Hundreds digit
    ldx #$00
@hi_div100:
    lda temp1
    cmp #100
    bcc @hi_hundreds_done
    sec
    sbc #100
    sta temp1
    inx
    jmp @hi_div100
@hi_hundreds_done:
    stx ptr_lo
    
    ; Tens digit
    ldx #$00
@hi_div10:
    lda temp1
    cmp #10
    bcc @hi_tens_done
    sec
    sbc #10
    sta temp1
    inx
    jmp @hi_div10
@hi_tens_done:
    stx ptr_hi
    
    ; Draw 3 high score digits
    ldy temp2
    
    ; Hundreds
    lda #$28              ; Y = 40
    sta oam_buffer, y
    iny
    lda ptr_lo
    clc
    adc #$25
    sta oam_buffer, y
    iny
    lda #$00              ; Palette 0 (white)
    sta oam_buffer, y
    iny
    lda #$08              ; X = 8
    sta oam_buffer, y
    iny
    
    ; Tens
    lda #$28              ; Y = 40
    sta oam_buffer, y
    iny
    lda ptr_hi
    clc
    adc #$25
    sta oam_buffer, y
    iny
    lda #$00              ; Palette 0 (white)
    sta oam_buffer, y
    iny
    lda #$10              ; X = 16
    sta oam_buffer, y
    iny
    
    ; Ones
    lda #$28              ; Y = 40
    sta oam_buffer, y
    iny
    lda temp1
    clc
    adc #$25
    sta oam_buffer, y
    iny
    lda #$00              ; Palette 0 (white)
    sta oam_buffer, y
    iny
    lda #$18              ; X = 24
    sta oam_buffer, y
    iny

    ; Save OAM index
    sty temp2

    ; --- Draw "ENERGY" label above meter ---
    ldx #$00              ; Letter counter
    

@draw_energy_label:
    ; Y position (move below everything else)
    lda #$38              ; Y = 56
    sta oam_buffer, y
    iny

    ; Tile - use font tiles (letters start at $21 in pattern table 0)
    lda energy_label, x
    sta oam_buffer, y
    iny

    ; Attributes (palette 2 for green color, same as meter)
    lda #$02
    sta oam_buffer, y
    iny

    ; X position (start at X=176, each letter 8 pixels apart)
    txa
    asl a                 ; *2
    asl a                 ; *4
    asl a                 ; *8
    clc
    adc #176              ; Start X position
    sta oam_buffer, y
    iny

    inx
    cpx #$06              ; 6 letters in "ENERGY"
    bne @draw_energy_label

    ; --- Draw Energy Meter (8 blocks in sky) ---
    ldx #$00              ; Meter block counter (0-7)
    
@draw_meter_loop:
    ; Y position (move below label)
    lda #$40              ; Y = 64
    sta oam_buffer, y
    iny

    ; Determine if this block is filled or empty
    ; Compare hippo_heat to threshold directly (no temp variable)
    lda hippo_heat
    cmp meter_thresholds, x  ; heat < threshold?
    bcs @meter_empty      ; No, block is empty

    ; Block is filled
    lda #$1E              ; Filled tile
    jmp @meter_tile_done
@meter_empty:
    lda #$1F              ; Empty tile
@meter_tile_done:
    sta oam_buffer, y
    iny

    ; Attributes (palette 2 for meter color)
    lda #$02
    sta oam_buffer, y
    iny

    ; X position (start at X=168, each block 8 pixels apart)
    txa
    asl a                 ; *2
    asl a                 ; *4
    asl a                 ; *8
    clc
    adc #168              ; Start X position (right side of screen)
    sta oam_buffer, y
    iny

    inx
    cpx #$08              ; 8 blocks total
    bne @draw_meter_loop
    
    ; Update OAM index
    sty temp2

    ; Hide remaining sprites (set Y to $FF)
    ldy temp2
@hide_sprites:
    cpy #$00              ; Wrapped around = done
    beq @sprites_done
    lda #$FF
    sta oam_buffer, y
    iny
    iny
    iny
    iny                   ; Next sprite (4 bytes)
    jmp @hide_sprites

@sprites_done:
    rts

;-----------------------------------------------------------------------------
; Draw Game Over Screen - Display game over message using sprites
;-----------------------------------------------------------------------------
draw_game_over:
    ; Clear all sprites first
    ldy #$00
@clear_oam:
    lda #$FF
    sta oam_buffer, y
    iny
    bne @clear_oam
    
    ldy #$00              ; OAM index
    
    ; --- Line 1: "SCORE" at Y=32 ---
    ldx #$00
@go_score:
    lda #$20              ; Y = 32
    sta oam_buffer, y
    iny
    lda score_label, x
    sta oam_buffer, y
    iny
    lda #$00              ; Palette 0 (white)
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$70              ; X = 112 (centered)
    sta oam_buffer, y
    iny
    inx
    cpx #$05
    bne @go_score
    
    ; --- Line 2: Score digits at Y=44 ---
    sty temp2
    lda score_lo
    sta temp1
    
    ; Hundreds
    ldx #$00
@go_div100:
    lda temp1
    cmp #100
    bcc @go_h_done
    sec
    sbc #100
    sta temp1
    inx
    jmp @go_div100
@go_h_done:
    stx ptr_lo
    
    ; Tens
    ldx #$00
@go_div10:
    lda temp1
    cmp #10
    bcc @go_t_done
    sec
    sbc #10
    sta temp1
    inx
    jmp @go_div10
@go_t_done:
    stx ptr_hi
    
    ldy temp2
    ; Hundreds digit
    lda #$2C              ; Y = 44
    sta oam_buffer, y
    iny
    lda ptr_lo
    clc
    adc #$25
    sta oam_buffer, y
    iny
    lda #$00
    sta oam_buffer, y
    iny
    lda #$70              ; X = 112
    sta oam_buffer, y
    iny
    ; Tens digit
    lda #$2C
    sta oam_buffer, y
    iny
    lda ptr_hi
    clc
    adc #$25
    sta oam_buffer, y
    iny
    lda #$00
    sta oam_buffer, y
    iny
    lda #$78              ; X = 120
    sta oam_buffer, y
    iny
    ; Ones digit
    lda #$2C
    sta oam_buffer, y
    iny
    lda temp1
    clc
    adc #$25
    sta oam_buffer, y
    iny
    lda #$00
    sta oam_buffer, y
    iny
    lda #$80              ; X = 128
    sta oam_buffer, y
    iny
    
    ; --- Line 3: "THANKS" at Y=64 (6 sprites - OK) ---
    ldx #$00
@go_thanks:
    lda #$40              ; Y = 64
    sta oam_buffer, y
    iny
    lda thanks_label, x
    sta oam_buffer, y
    iny
    lda #$00
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$58              ; X = 88 (centered for 6 chars)
    sta oam_buffer, y
    iny
    inx
    cpx #thanks_len
    bne @go_thanks
    
    ; --- Line 4: "FOR" at Y=76 (3 sprites - OK) ---
    ldx #$00
@go_for:
    lda #$4C              ; Y = 76
    sta oam_buffer, y
    iny
    lda for_label, x
    sta oam_buffer, y
    iny
    lda #$00
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$74              ; X = 116 (centered for 3 chars)
    sta oam_buffer, y
    iny
    inx
    cpx #for_len
    bne @go_for
    
    ; --- Line 5: "PLAYING" at Y=88 (7 sprites - OK) ---
    ldx #$00
@go_playing:
    lda #$58              ; Y = 88
    sta oam_buffer, y
    iny
    lda playing_label, x
    sta oam_buffer, y
    iny
    lda #$00
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$54              ; X = 84 (centered for 7 chars)
    sta oam_buffer, y
    iny
    inx
    cpx #playing_len
    bne @go_playing
    
    ; --- Line 6: "CHECK" at Y=108 (5 sprites - OK) ---
    ldx #$00
@go_check:
    lda #$6C              ; Y = 108
    sta oam_buffer, y
    iny
    lda check_label, x
    sta oam_buffer, y
    iny
    lda #$00
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$5C              ; X = 92 (centered for 5 chars)
    sta oam_buffer, y
    iny
    inx
    cpx #check_len
    bne @go_check
    
    ; --- Line 7: "OUT" at Y=120 (3 sprites - OK) ---
    ldx #$00
@go_out:
    lda #$78              ; Y = 120
    sta oam_buffer, y
    iny
    lda out_label, x
    sta oam_buffer, y
    iny
    lda #$00
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$74              ; X = 116 (centered for 3 chars)
    sta oam_buffer, y
    iny
    inx
    cpx #out_len
    bne @go_out
    
    ; --- Line 8: "RESUPPLY" (first 8 chars) at Y=140 ---
    ldx #$00
@go_resupply:
    lda #$8C              ; Y = 140
    sta oam_buffer, y
    iny
    lda resupply_label, x
    sta oam_buffer, y
    iny
    lda #$01              ; Palette 1 (gold - highlight the URL)
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$44              ; X = 68
    sta oam_buffer, y
    iny
    inx
    cpx #$08              ; First 8 chars only
    bne @go_resupply
    
    ; --- Line 9: ".FI" (last 3 chars) at Y=152 ---
    ldx #$08              ; Start at char 8
@go_resupply2:
    lda #$98              ; Y = 152
    sta oam_buffer, y
    iny
    lda resupply_label, x
    sta oam_buffer, y
    iny
    lda #$01              ; Palette 1 (gold)
    sta oam_buffer, y
    iny
    txa
    sec
    sbc #$08              ; Adjust X position (0, 1, 2)
    asl a
    asl a
    asl a
    clc
    adc #$74              ; X = 116 (centered for 3 chars)
    sta oam_buffer, y
    iny
    inx
    cpx #resupply_len
    bne @go_resupply2
    
    ; --- Line 10: "PRESS" at Y=172 (5 sprites - OK) ---
    ldx #$00
@go_press:
    lda #$AC              ; Y = 172
    sta oam_buffer, y
    iny
    lda press_label, x
    sta oam_buffer, y
    iny
    lda #$00
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$5C              ; X = 92 (centered)
    sta oam_buffer, y
    iny
    inx
    cpx #press_len
    bne @go_press
    
    ; --- Line 11: "START" at Y=184 (5 sprites - OK) ---
    ldx #$00
@go_start:
    lda #$B8              ; Y = 184
    sta oam_buffer, y
    iny
    lda start_label, x
    sta oam_buffer, y
    iny
    lda #$00
    sta oam_buffer, y
    iny
    txa
    asl a
    asl a
    asl a
    clc
    adc #$5C              ; X = 92 (centered)
    sta oam_buffer, y
    iny
    inx
    cpx #start_len
    bne @go_start
    
    rts

;-----------------------------------------------------------------------------
; Load Palettes - Copy palette data to PPU
;-----------------------------------------------------------------------------
load_palettes:
    ; Set PPU address to palette RAM ($3F00)
    lda PPUSTATUS         ; Reset PPU latch
    lda #$3F
    sta PPUADDR
    lda #$00
    sta PPUADDR

    ; Copy 32 bytes of palette data
    ldx #$00
@palette_loop:
    lda palette_data, x
    sta PPUDATA
    inx
    cpx #32
    bne @palette_loop

    rts

;-----------------------------------------------------------------------------
; Load Title Screen - Display "HIPPO CHARGE" and "PRESS START"
;-----------------------------------------------------------------------------
load_title_screen:
    ; Clear entire nametable first
    lda PPUSTATUS
    lda #$20
    sta PPUADDR
    lda #$00
    sta PPUADDR

    ; Fill with empty tiles (960 tiles for nametable)
    lda #$00
    tax
    ldy #$04              ; 4 x 256 = 1024 (covers nametable + attributes)
@clear_nt:
    sta PPUDATA
    inx
    bne @clear_nt
    dey
    bne @clear_nt

    ; Draw "HIPPO CHARGE" at row 10, centered (column 10)
    ; Row 10 = $20 + (10 * 32) = $20 + $140 = $2140
    lda PPUSTATUS
    lda #$21
    sta PPUADDR
    lda #$4A              ; Row 10, column 10
    sta PPUADDR

    ; Write "HIPPO CHARGE" (tiles $20+ = letters)
    ldx #$00
@title_loop:
    lda title_text, x
    cmp #$FF              ; Check for end marker
    beq @title_done
    sta PPUDATA
    inx
    jmp @title_loop
@title_done:

    ; Draw "PRESS START" at row 16, centered (column 10)
    ; Row 16 = $20 + (16 * 32) = $20 + $200 = $2200
    lda PPUSTATUS
    lda #$22
    sta PPUADDR
    lda #$0A              ; Row 16, column 10
    sta PPUADDR

    ldx #$00
@press_loop:
    lda press_start_text, x
    cmp #$FF              ; Check for end marker
    beq @press_done
    sta PPUDATA
    inx
    jmp @press_loop
@press_done:

    ; Set attributes for title area (use palette 1 for text)
    lda PPUSTATUS
    lda #$23
    sta PPUADDR
    lda #$C0
    sta PPUADDR

    ; Fill all attributes with palette 0
    lda #$00
    ldx #$40
@attr_clear:
    sta PPUDATA
    dex
    bne @attr_clear

    ; Reset scroll position
    lda PPUSTATUS         ; Reset latch
    lda #$00
    sta PPUSCROLL
    sta PPUSCROLL

    rts

; Title screen text (uses tile numbers, $FF = end)
; Font: $20=space, A=$21, B=$22, C=$23, D=$24, E=$25, F=$26, G=$27, H=$28
;       I=$29, J=$2A, K=$2B, L=$2C, M=$2D, N=$2E, O=$2F, P=$30, Q=$31
;       R=$32, S=$33, T=$34, U=$35, V=$36, W=$37, X=$38, Y=$39, Z=$3A
title_text:
    ;      H     I     P     P     O     (sp)  C     H     A     R     G     E
    .byte $28,  $29,  $30,  $30,  $2F,  $20,  $23,  $28,  $21,  $32,  $27,  $25
    .byte $FF                       ; End marker

press_start_text:
    ;      P     R     E     S     S     (sp)  S     T     A     R     T
    .byte $30,  $32,  $25,  $33,  $33,  $20,  $33,  $34,  $21,  $32,  $34
    .byte $FF                       ; End marker

;-----------------------------------------------------------------------------
; Load Background - Draw the track lanes
;-----------------------------------------------------------------------------
load_background:
    ; Set PPU address to first nametable ($2000)
    lda PPUSTATUS
    lda #$20
    sta PPUADDR
    lda #$00
    sta PPUADDR

    ; Fill top 10 rows with sky (tile $00)
    ; 10 rows x 32 tiles = 320 tiles
    lda #$00              ; Sky tile
    ldx #$00              ; Counter low
    ldy #$00              ; Counter high (need 320 = $140)
@sky_loop:
    sta PPUDATA
    inx
    cpx #$40              ; 64 iterations
    bne @sky_loop
    ldx #$00
    iny
    cpy #$05              ; 5 x 64 = 320 tiles
    bne @sky_loop

    ; Fill next 4 rows with ground/grass (tile $03) - between sky and track
    ; 4 rows x 32 tiles = 128 tiles
    lda #$03              ; Ground tile
    ldx #$00
@grass_loop:
    sta PPUDATA
    inx
    cpx #$80              ; 128 tiles
    bne @grass_loop

    ; Draw 4 lanes of track (16 rows total, 4 rows per lane)
    ; Each row is 32 tiles
    ldy #$00              ; Row counter
@track_row_loop:
    ldx #$00              ; Column counter
@track_col_loop:
    lda #$01              ; Track tile (solid track, no dividers)
    sta PPUDATA
    inx
    cpx #$20              ; 32 columns
    bne @track_col_loop
    iny
    cpy #$10              ; 16 rows of track
    bne @track_row_loop

    ; Set up attribute table (at $23C0)
    lda PPUSTATUS
    lda #$23
    sta PPUADDR
    lda #$C0
    sta PPUADDR

    ; Fill attributes - layout: sky(10 rows), grass(4 rows), track(16 rows)
    ; Attribute bytes cover 4x4 tile areas (32 pixels / 4 rows)
    ; Rows 0-9 = sky (first 16 attr bytes cover rows 0-7, next 8 cover rows 8-11)
    ; Rows 10-13 = grass  
    ; Rows 14-29 = track
    ldx #$00
@attr_loop:
    cpx #$10              ; First 16 bytes = pure sky (rows 0-7)
    bcc @sky_attr
    cpx #$18              ; Next 8 bytes = sky/grass transition (rows 8-11)
    bcc @sky_attr
    lda #$55              ; Rest = track (palette 1)
    jmp @write_attr
@sky_attr:
    lda #$00              ; Sky uses palette 0
@write_attr:
    sta PPUDATA
    inx
    cpx #$40              ; 64 attribute bytes
    bne @attr_loop

    rts

;=============================================================================
; Read-Only Data
;=============================================================================
.segment "RODATA"

;-----------------------------------------------------------------------------
; Palette Data - 32 bytes (16 BG + 16 Sprite)
;-----------------------------------------------------------------------------
palette_data:
    ; Background palettes (4 palettes x 4 colors)
    .byte $21, $29, $1A, $0F    ; Palette 0: Sky (light blue, green, dark)
    .byte $21, $17, $27, $0F    ; Palette 1: Track (brown/tan)
    .byte $21, $1C, $2B, $0F    ; Palette 2: Water (blues)
    .byte $21, $19, $29, $0F    ; Palette 3: Ground (greens)

    ; Sprite palettes (4 palettes x 4 colors)
    ; Color 0 = transparent (use BG color), Color 1 = dark (eyes/nostrils), Color 2 = medium, Color 3 = body
    .byte $21, $0F, $00, $30    ; Palette 0: Hippo - (bg), black, gray, white body
    .byte $21, $07, $27, $38    ; Palette 1: Dollar - dark gold, gold, bright yellow
    .byte $21, $0F, $1A, $2A    ; Palette 2: Energy meter - black outline, dark green, bright green
    .byte $21, $28, $38, $18    ; Palette 3: Ramp (yellows)

;=============================================================================
; Interrupt Vectors
;=============================================================================
.segment "VECTORS"
    .word nmi             ; NMI vector - called every VBlank
    .word reset           ; Reset vector - called on power/reset
    .word irq             ; IRQ vector - not used

;=============================================================================
; CHR-ROM Graphics Data (8KB)
;=============================================================================
.segment "TILES"

; Pattern Table 0 - Sprites ($0000-$0FFF)
;-----------------------------------------------------------------------------
; NES CHR format: 8x8 pixels, 2 bit color
; Byte layout: 8 bytes plane 0, then 8 bytes plane 1
; Color = plane0_bit + (plane1_bit * 2)
;   00 = transparent, 01 = color 1, 10 = color 2, 11 = color 3 (solid)

; Col:  0     1     2     3     4     5
;     0-5   6-11 12-17 18-23 24-29 30-35
     
;L0:  ░░░░░░ ░░░░░░ ░░░░██ ░░░░██ ░░░░░░ ░░░░░░  <- ears
;L1:  ░░░░░░ ██████ ██████ ██████ ░░░░░░ ░░░░░░  <- top head  
;L2:  ░░░░██ ██████ ██████ ██████ ██░░░░ ░░░░░░
;L3:  ░█████ ██████ ████░░ ████XX ██████ ████░░  <- eye gap
;L4:  ██████ ██████ ████░░ ██████ █████X X██░░░  <- nostril gap
;L5:  ░█████ ██████ ████░░ ██████ ██████ ████░░
;L6:  ░█████ ██████ ████░░ ██████ ██████ ████░░
;L7:  ░█████ ██████ ████░░ ░░████ ██████ ████░░  <- bottom body
;L8:  ░████░ ░░███░ ░░████ ░░░░░░ ░░░░░░ ░░░░░░  <- legs
;L9:  ░████░ ░░███░ ░░████ ░░████ ░░░░░░ ░░░░░░
;L10: ░████░ ░░░░░░ ░░████ ░░████ ░░░░░░ ░░░░░░  <- legs

; ░ = transparent
; █ = solid white
; X = solid black

; Splitting into 6 columns (chars 0-5, 6-11, 12-17, 18-23, 24-29, 30-35)
; and 3 rows (lines 0-3, 4-7, 8-10 stretched to 8 scanlines each)

; Tile $00 - Row 0, Col 0 (back of body with OUTLINE)
; Body pixels use color 3 (plane0=1, plane1=1)
; Outline pixels use color 1 (plane0=1, plane1=0)
.byte %00000000  ; L0
.byte %00000001  ; L1 outline
.byte %00000111  ; L2 outline around ██
.byte %00000111  ; L2
.byte %11111111  ; L3 body + outline
.byte %11111111  ; L3
.byte %11111111  ; L3
.byte %11111111  ; L3
; Plane 1 (only body pixels, not outline)
.byte %00000000
.byte %00000000  ; outline = 0
.byte %00000011  ; body only
.byte %00000011
.byte %01111111  ; body (inner)
.byte %01111111
.byte %01111111
.byte %01111111

; Tile $01 - Row 0, Col 1 (body back top with OUTLINE)
.byte %00000000  ; L0
.byte %11111111  ; L1 outline
.byte %11111111  ; L1 body
.byte %11111111  ; L2
.byte %11111111  ; L2
.byte %11111111  ; L3
.byte %11111111  ; L3
.byte %11111111  ; L3
; Plane 1
.byte %00000000
.byte %00000000  ; top edge = outline only
.byte %11111111  ; body
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111

; Tile $02 - Row 0, Col 2 (LEFT EAR + body with OUTLINE)
.byte %00000111  ; L0: outline around ear
.byte %00000111  ; L0
.byte %11111111  ; L1
.byte %11111111  ; L1
.byte %11111111  ; L2
.byte %11111111  ; L2
.byte %11111111  ; L3: body + outline for eye gap
.byte %11111111  ; L3
; Plane 1 (body only, outline=0)
.byte %00000011  ; ear body
.byte %00000011
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111100  ; eye gap = no plane1
.byte %11111100

; Tile $03 - Row 0, Col 3 (RIGHT EAR + EYE with OUTLINE)
.byte %11100000  ; L0: outline around ear
.byte %11100000  ; L0
.byte %11111111  ; L1
.byte %11111111  ; L1
.byte %11111111  ; L2
.byte %11111111  ; L2
.byte %11111111  ; L3: body + eye
.byte %11111111  ; L3
; Plane 1
.byte %11000000  ; ear body
.byte %11000000
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111100  ; eye = color 1 (plane1=0)
.byte %11111100

; Tile $04 - Row 0, Col 4 (head front with OUTLINE)
.byte %00000000  ; L0
.byte %00000000  ; L0
.byte %00000000  ; L1
.byte %11100000  ; L1 outline
.byte %11100000  ; L2 outline around ██
.byte %11111111  ; L2/L3 transition
.byte %11111111  ; L3
.byte %11111111  ; L3
; Plane 1
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000  ; outline
.byte %11000000  ; body
.byte %11000000
.byte %11111111
.byte %11111111

; Tile $05 - Row 0, Col 5 (snout tip with OUTLINE)
.byte %00000000  ; L0
.byte %00000000  ; L0
.byte %00000000  ; L1
.byte %00000000  ; L1
.byte %00000000  ; L2
.byte %11111000  ; L2/L3 outline
.byte %11111000  ; L3 outline + body
.byte %11111000  ; L3
; Plane 1
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000  ; outline only
.byte %11110000  ; body
.byte %11110000

; Tile $06 - Row 1, Col 0 (body/legs with OUTLINE)
.byte %11111111  ; L4
.byte %11111111  ; L4
.byte %11111111  ; L5 outline
.byte %11111111  ; L5
.byte %11111111  ; L6
.byte %11111111  ; L6
.byte %11111111  ; L7
.byte %11111111  ; L7
; Plane 1
.byte %11111111  ; body top edge connects
.byte %11111111
.byte %01111111  ; body
.byte %01111111
.byte %01111111
.byte %01111111
.byte %01111111
.byte %01111111

; Tile $07 - Row 1, Col 1 (body with OUTLINE - solid body, no outline needed inside)
.byte %11111111  ; L4
.byte %11111111  ; L4
.byte %11111111  ; L5
.byte %11111111  ; L5
.byte %11111111  ; L6
.byte %11111111  ; L6
.byte %11111111  ; L7
.byte %11111111  ; L7
; Plane 1
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111

; Tile $08 - Row 1, Col 2 (body under eye with OUTLINE)
; Eye gap needs outline
.byte %11111111  ; L4 outline extends into gap
.byte %11111111  ; L4
.byte %11111111  ; L5
.byte %11111111  ; L5
.byte %11111111  ; L6
.byte %11111111  ; L6
.byte %11111111  ; L7
.byte %11111111  ; L7
; Plane 1 (body only, gap = outline)
.byte %11111100
.byte %11111100
.byte %11111100
.byte %11111100
.byte %11111100
.byte %11111100
.byte %11111100
.byte %11111100

; Tile $09 - Row 1, Col 3 (face/eye area with OUTLINE)
.byte %11111111  ; L4
.byte %11111111  ; L4
.byte %11111111  ; L5
.byte %11111111  ; L5
.byte %11111111  ; L6
.byte %11111111  ; L6
.byte %01111111  ; L7: outline for gap
.byte %01111111  ; L7
; Plane 1
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %00111111  ; body only (inner)
.byte %00111111

; Tile $0A - Row 1, Col 4 (snout with NOSTRIL + OUTLINE)
.byte %11111111  ; L4 body + nostril
.byte %11111111  ; L4
.byte %11111111  ; L5
.byte %11111111  ; L5
.byte %11111111  ; L6
.byte %11111111  ; L6
.byte %11111111  ; L7
.byte %11111111  ; L7
; Plane 1
.byte %11111100  ; nostril = color 1
.byte %11111100
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111

; Tile $0B - Row 1, Col 5 (snout tip with NOSTRIL + OUTLINE)
.byte %11111000  ; L4: outline around snout
.byte %11111000  ; L4
.byte %11111000  ; L5 outline
.byte %11111000  ; L5
.byte %11111000  ; L6
.byte %11111000  ; L6
.byte %11111000  ; L7
.byte %11111000  ; L7
; Plane 1 (body only)
.byte %01100000  ; nostril = color 1
.byte %01100000
.byte %11110000  ; body
.byte %11110000
.byte %11110000
.byte %11110000
.byte %11110000
.byte %11110000

; Tile $0C - Row 2, Col 0 (back leg, lines 8-10) WITH OUTLINE
; L8: ░████░ L9: ░████░ L10: ░████░
; Outline: left, right, bottom edges
; Plane 0 (outline + body)
.byte %11111110  ; L8: outline+body+outline
.byte %11111110  ; L8
.byte %11111110  ; L8
.byte %11111110  ; L9
.byte %11111110  ; L9
.byte %11111110  ; L9
.byte %11111110  ; L10: body+outline
.byte %11111110  ; bottom outline
; Plane 1 (body only)
.byte %01111100
.byte %01111100
.byte %01111100
.byte %01111100
.byte %01111100
.byte %01111100
.byte %01111100
.byte %00000000  ; bottom outline row

; Tile $0D - Row 2, Col 1 (gap + middle leg, lines 8-10) WITH OUTLINE
; L8: ░░███░ L9: ░░███░ L10: ░░░░░░
; Outline: around middle leg
; Plane 0 (outline + body)
.byte %01111110  ; L8: outline+body+outline
.byte %01111110  ; L8
.byte %01111110  ; L8
.byte %01111110  ; L9
.byte %01111110  ; L9: bottom of leg
.byte %00111100  ; bottom outline
.byte %00000000  ; L10: gap
.byte %00000000  ; L10
; Plane 1 (body only)
.byte %00111100
.byte %00111100
.byte %00111100
.byte %00111100
.byte %00111100
.byte %00000000  ; outline row
.byte %00000000
.byte %00000000

; Tile $0E - Row 2, Col 2 (front leg, lines 8-10) WITH OUTLINE
; L8: ░░████ L9: ░░████ L10: ░░████
; Outline: left edge, bottom edge
; Plane 0 (outline + body)
.byte %01111111  ; L8: outline+body
.byte %01111111  ; L8
.byte %01111111  ; L8
.byte %01111111  ; L9
.byte %01111111  ; L9
.byte %01111111  ; L9
.byte %01111111  ; L10: body
.byte %01111111  ; bottom outline
; Plane 1 (body only)
.byte %00111111
.byte %00111111
.byte %00111111
.byte %00111111
.byte %00111111
.byte %00111111
.byte %00111111
.byte %00000000  ; bottom outline row

; Tile $0F - Row 2, Col 3 (front leg, lines 8-10) WITH OUTLINE
; L8: ░░░░░░ L9: ░░████ L10: ░░████
; Outline: left, right, bottom
; Plane 0 (outline + body)
.byte %00000000  ; L8
.byte %00000000  ; L8
.byte %01111110  ; top outline of leg
.byte %01111111  ; L9: outline+body+outline (extends right)
.byte %01111111  ; L9
.byte %01111111  ; L9
.byte %01111111  ; L10
.byte %01111111  ; bottom outline
; Plane 1 (body only)
.byte %00000000
.byte %00000000
.byte %00000000  ; outline row
.byte %00111111
.byte %00111111
.byte %00111111
.byte %00111111
.byte %00000000  ; bottom outline row

; Tile $10 - Row 2, Col 4 (empty)
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
; Plane 1
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000

; Tile $11 - Row 2, Col 5 (empty)
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
; Plane 1
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000

; Tiles $12-$17: Reserved for animation frame 2
.res 96, $00

; Tile $18 - Dollar sign ($) collectible
; Dollar sign design: $ centered in 8x8
; Plane 0 (shape)
.byte %00011000  ; Row 0:   ██
.byte %00111110  ; Row 1:  █████
.byte %01101000  ; Row 2: ██ █
.byte %00111100  ; Row 3:  ████
.byte %00011110  ; Row 4:   ████
.byte %01001100  ; Row 5: █  ██
.byte %00111110  ; Row 6:  █████
.byte %00011000  ; Row 7:   ██
; Plane 1 (color - all same for solid)
.byte %00011000
.byte %00111110
.byte %01101000
.byte %00111100
.byte %00011110
.byte %01001100
.byte %00111110
.byte %00011000

; Tile $19 - Dollar sign alternate (unused but keep space)
.byte %00011000
.byte %00111110
.byte %01101000
.byte %00111100
.byte %00011110
.byte %01001100
.byte %00111110
.byte %00011000
.byte %00000000
.byte %00000000
.byte %00100100
.byte %01000010
.byte %10000001
.byte %01000010
.byte %00111100
.byte %00000000

; Tile $1A - Water splash (renumbered from $12)
.byte %00000000
.byte %01000010
.byte %10100101
.byte %01000010
.byte %10100101
.byte %01000010
.byte %00000000
.byte %11111111
.byte %00000000    ; Plane 1
.byte %00100100
.byte %01011010
.byte %10100101
.byte %01011010
.byte %10100101
.byte %00000000
.byte %11111111

; Tile $1B - Water (wavy pattern, renumbered from $13)
.byte %00000000
.byte %00000000
.byte %01100110
.byte %10011001
.byte %00000000
.byte %01100110
.byte %10011001
.byte %11111111
.byte %00000000
.byte %00000000
.byte %01100110
.byte %10011001
.byte %00000000
.byte %01100110
.byte %10011001
.byte %11111111

; Tile $1C - Ramp (renumbered from $14)
.byte %00000001
.byte %00000011
.byte %00000111
.byte %00001111
.byte %00011111
.byte %00111111
.byte %01111111
.byte %11111111
.byte %00000000    ; Plane 1
.byte %00000001
.byte %00000011
.byte %00000111
.byte %00001111
.byte %00011111
.byte %00111111
.byte %01111111

; Tile $1D - Ramp second part (renumbered from $15)
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %01111111
.byte %00111111
.byte %00011111
.byte %00001111
.byte %00000111
.byte %00000011
.byte %00000001
.byte %00000000

; Energy Meter Tiles
; Tile $1E - Filled meter block (energy remaining)
.byte %11111111  ; Solid block
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
; Plane 1
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111

; Tile $1F - Empty meter block (energy used)
.byte %11111111  ; Outline only
.byte %10000001
.byte %10000001
.byte %10000001
.byte %10000001
.byte %10000001
.byte %10000001
.byte %11111111
; Plane 1
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000

; Font tiles for "ENERGY" label (sprites use pattern table 0)
; Tile $20 - Letter E
.byte %11111110
.byte %11000000
.byte %11000000
.byte %11111100
.byte %11000000
.byte %11000000
.byte %11111110
.byte %00000000
; Plane 1
.byte %11111110
.byte %11000000
.byte %11000000
.byte %11111100
.byte %11000000
.byte %11000000
.byte %11111110
.byte %00000000

; Tile $21 - Letter N
.byte %11000110
.byte %11100110
.byte %11110110
.byte %11011110
.byte %11001110
.byte %11000110
.byte %11000110
.byte %00000000
; Plane 1
.byte %11000110
.byte %11100110
.byte %11110110
.byte %11011110
.byte %11001110
.byte %11000110
.byte %11000110
.byte %00000000

; Tile $22 - Letter R
.byte %11111100
.byte %11000110
.byte %11000110
.byte %11111100
.byte %11011000
.byte %11001100
.byte %11000110
.byte %00000000
; Plane 1
.byte %11111100
.byte %11000110
.byte %11000110
.byte %11111100
.byte %11011000
.byte %11001100
.byte %11000110
.byte %00000000

; Tile $23 - Letter G
.byte %01111100
.byte %11000110
.byte %11000000
.byte %11001110
.byte %11000110
.byte %11000110
.byte %01111100
.byte %00000000
; Plane 1
.byte %01111100
.byte %11000110
.byte %11000000
.byte %11001110
.byte %11000110
.byte %11000110
.byte %01111100
.byte %00000000

; Tile $24 - Letter Y
.byte %11000110
.byte %11000110
.byte %01101100
.byte %00111000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000
; Plane 1
.byte %11000110
.byte %11000110
.byte %01101100
.byte %00111000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000

; Number tiles for timer display ($25-$2E = 0-9)
; Tile $25 - Number 0
.byte %01111100
.byte %11000110
.byte %11001110
.byte %11011110
.byte %11110110
.byte %11100110
.byte %01111100
.byte %00000000
.byte %01111100
.byte %11000110
.byte %11001110
.byte %11011110
.byte %11110110
.byte %11100110
.byte %01111100
.byte %00000000

; Tile $26 - Number 1
.byte %00011000
.byte %00111000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %01111110
.byte %00000000
.byte %00011000
.byte %00111000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %01111110
.byte %00000000

; Tile $27 - Number 2
.byte %01111100
.byte %11000110
.byte %00001110
.byte %00111100
.byte %01110000
.byte %11000000
.byte %11111110
.byte %00000000
.byte %01111100
.byte %11000110
.byte %00001110
.byte %00111100
.byte %01110000
.byte %11000000
.byte %11111110
.byte %00000000

; Tile $28 - Number 3
.byte %01111100
.byte %11000110
.byte %00000110
.byte %00111100
.byte %00000110
.byte %11000110
.byte %01111100
.byte %00000000
.byte %01111100
.byte %11000110
.byte %00000110
.byte %00111100
.byte %00000110
.byte %11000110
.byte %01111100
.byte %00000000

; Tile $29 - Number 4
.byte %00001100
.byte %00011100
.byte %00111100
.byte %01101100
.byte %11111110
.byte %00001100
.byte %00001100
.byte %00000000
.byte %00001100
.byte %00011100
.byte %00111100
.byte %01101100
.byte %11111110
.byte %00001100
.byte %00001100
.byte %00000000

; Tile $2A - Number 5
.byte %11111110
.byte %11000000
.byte %11111100
.byte %00000110
.byte %00000110
.byte %11000110
.byte %01111100
.byte %00000000
.byte %11111110
.byte %11000000
.byte %11111100
.byte %00000110
.byte %00000110
.byte %11000110
.byte %01111100
.byte %00000000

; Tile $2B - Number 6
.byte %00111100
.byte %01100000
.byte %11000000
.byte %11111100
.byte %11000110
.byte %11000110
.byte %01111100
.byte %00000000
.byte %00111100
.byte %01100000
.byte %11000000
.byte %11111100
.byte %11000110
.byte %11000110
.byte %01111100
.byte %00000000

; Tile $2C - Number 7
.byte %11111110
.byte %00000110
.byte %00001100
.byte %00011000
.byte %00110000
.byte %00110000
.byte %00110000
.byte %00000000
.byte %11111110
.byte %00000110
.byte %00001100
.byte %00011000
.byte %00110000
.byte %00110000
.byte %00110000
.byte %00000000

; Tile $2D - Number 8
.byte %01111100
.byte %11000110
.byte %11000110
.byte %01111100
.byte %11000110
.byte %11000110
.byte %01111100
.byte %00000000
.byte %01111100
.byte %11000110
.byte %11000110
.byte %01111100
.byte %11000110
.byte %11000110
.byte %01111100
.byte %00000000

; Tile $2E - Number 9
.byte %01111100
.byte %11000110
.byte %11000110
.byte %01111110
.byte %00000110
.byte %00001100
.byte %01111000
.byte %00000000
.byte %01111100
.byte %11000110
.byte %11000110
.byte %01111110
.byte %00000110
.byte %00001100
.byte %01111000
.byte %00000000

; Tile $2F - Letter T
.byte %11111110
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000
.byte %11111110
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000

; Tile $30 - Letter I
.byte %01111110
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %01111110
.byte %00000000
.byte %01111110
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %01111110
.byte %00000000

; Tile $31 - Letter M
.byte %11000110
.byte %11101110
.byte %11111110
.byte %11010110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %00000000
.byte %11000110
.byte %11101110
.byte %11111110
.byte %11010110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %00000000

; Tile $32 - Letter S
.byte %01111100
.byte %11000110
.byte %11000000
.byte %01111100
.byte %00000110
.byte %11000110
.byte %01111100
.byte %00000000
.byte %01111100
.byte %11000110
.byte %11000000
.byte %01111100
.byte %00000110
.byte %11000110
.byte %01111100
.byte %00000000

; Tile $33 - Letter C
.byte %01111100
.byte %11000110
.byte %11000000
.byte %11000000
.byte %11000000
.byte %11000110
.byte %01111100
.byte %00000000
.byte %01111100
.byte %11000110
.byte %11000000
.byte %11000000
.byte %11000000
.byte %11000110
.byte %01111100
.byte %00000000

; Tile $34 - Letter O
.byte %01111100
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %01111100
.byte %00000000
.byte %01111100
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %01111100
.byte %00000000

; Tile $35 - Letter A
.byte %00111100
.byte %01100110
.byte %11000011
.byte %11111111
.byte %11000011
.byte %11000011
.byte %11000011
.byte %00000000
.byte %00111100
.byte %01100110
.byte %11000011
.byte %11111111
.byte %11000011
.byte %11000011
.byte %11000011
.byte %00000000

; Tile $36 - Letter H
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11111110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %00000000
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11111110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %00000000

; Tile $37 - Letter K
.byte %11000110
.byte %11001100
.byte %11011000
.byte %11110000
.byte %11011000
.byte %11001100
.byte %11000110
.byte %00000000
.byte %11000110
.byte %11001100
.byte %11011000
.byte %11110000
.byte %11011000
.byte %11001100
.byte %11000110
.byte %00000000

; Tile $38 - Letter F
.byte %11111110
.byte %11000000
.byte %11000000
.byte %11111100
.byte %11000000
.byte %11000000
.byte %11000000
.byte %00000000
.byte %11111110
.byte %11000000
.byte %11000000
.byte %11111100
.byte %11000000
.byte %11000000
.byte %11000000
.byte %00000000

; Tile $39 - Letter P
.byte %11111100
.byte %11000110
.byte %11000110
.byte %11111100
.byte %11000000
.byte %11000000
.byte %11000000
.byte %00000000
.byte %11111100
.byte %11000110
.byte %11000110
.byte %11111100
.byte %11000000
.byte %11000000
.byte %11000000
.byte %00000000

; Tile $3A - Letter L
.byte %11000000
.byte %11000000
.byte %11000000
.byte %11000000
.byte %11000000
.byte %11000000
.byte %11111110
.byte %00000000
.byte %11000000
.byte %11000000
.byte %11000000
.byte %11000000
.byte %11000000
.byte %11000000
.byte %11111110
.byte %00000000

; Tile $3B - Letter W
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11010110
.byte %11111110
.byte %11101110
.byte %11000110
.byte %00000000
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11010110
.byte %11111110
.byte %11101110
.byte %11000110
.byte %00000000

; Tile $3C - Letter U
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %01111100
.byte %00000000
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %11000110
.byte %01111100
.byte %00000000

; Tile $3D - Period (.)
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00011000
.byte %00011000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %00011000
.byte %00011000
.byte %00000000

; Tile $3E - Letter X (for extra)
.byte %11000110
.byte %01101100
.byte %00111000
.byte %00111000
.byte %00111000
.byte %01101100
.byte %11000110
.byte %00000000
.byte %11000110
.byte %01101100
.byte %00111000
.byte %00111000
.byte %00111000
.byte %01101100
.byte %11000110
.byte %00000000

; Continue with more empty tiles to fill pattern table 0 (4096 bytes total)
; Tiles $00-$3E = 63 tiles = 1008 bytes. Need 4096 - 1008 = 3088 more bytes
.res 3088, $00  ; Remaining sprite tiles

; Pattern Table 1 - Background ($1000-$1FFF)
;-----------------------------------------------------------------------------

; Tile $00 - Sky (empty/solid color)
.byte $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00

; Tile $01 - Track/ground pattern
.byte %11111111
.byte %00000000
.byte %11111111
.byte %00000000
.byte %11111111
.byte %00000000
.byte %11111111
.byte %00000000
.byte %00000000    ; Plane 1
.byte %11111111
.byte %00000000
.byte %11111111
.byte %00000000
.byte %11111111
.byte %00000000
.byte %11111111

; Tile $02 - Lane divider (vertical stripe)
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000    ; Plane 1
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000

; Tile $03 - Ground (solid dirt)
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %10101010    ; Plane 1 (texture)
.byte %01010101
.byte %10101010
.byte %01010101
.byte %10101010
.byte %01010101
.byte %10101010
.byte %01010101

; Tiles $04-$1F - Reserved (28 tiles x 16 bytes = 448 bytes)
.res 448, $00

; Tiles $20-$39 - Font tiles (A-Z and some symbols)
; Each tile is 16 bytes (8 bytes plane 0 + 8 bytes plane 1)

; Tile $20 - Space (empty)
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00

; Tile $21 - A
.byte %00111100
.byte %01100110
.byte %01100110
.byte %01111110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00000000
.byte %00111100
.byte %01100110
.byte %01100110
.byte %01111110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00000000

; Tile $22 - B
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %00000000
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %00000000

; Tile $23 - C
.byte %00111100
.byte %01100110
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100110
.byte %00111100
.byte %00000000
.byte %00111100
.byte %01100110
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100110
.byte %00111100
.byte %00000000

; Tile $24 - D
.byte %01111000
.byte %01101100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01101100
.byte %01111000
.byte %00000000
.byte %01111000
.byte %01101100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01101100
.byte %01111000
.byte %00000000

; Tile $25 - E
.byte %01111110
.byte %01100000
.byte %01100000
.byte %01111100
.byte %01100000
.byte %01100000
.byte %01111110
.byte %00000000
.byte %01111110
.byte %01100000
.byte %01100000
.byte %01111100
.byte %01100000
.byte %01100000
.byte %01111110
.byte %00000000

; Tile $26 - F
.byte %01111110
.byte %01100000
.byte %01100000
.byte %01111100
.byte %01100000
.byte %01100000
.byte %01100000
.byte %00000000
.byte %01111110
.byte %01100000
.byte %01100000
.byte %01111100
.byte %01100000
.byte %01100000
.byte %01100000
.byte %00000000

; Tile $27 - G
.byte %00111100
.byte %01100110
.byte %01100000
.byte %01101110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000
.byte %00111100
.byte %01100110
.byte %01100000
.byte %01101110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000

; Tile $28 - H
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01111110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00000000
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01111110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00000000

; Tile $29 - I
.byte %00111100
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00111100
.byte %00000000
.byte %00111100
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00111100
.byte %00000000

; Tile $2A - J
.byte %00011110
.byte %00001100
.byte %00001100
.byte %00001100
.byte %01101100
.byte %01101100
.byte %00111000
.byte %00000000
.byte %00011110
.byte %00001100
.byte %00001100
.byte %00001100
.byte %01101100
.byte %01101100
.byte %00111000
.byte %00000000

; Tile $2B - K
.byte %01100110
.byte %01101100
.byte %01111000
.byte %01110000
.byte %01111000
.byte %01101100
.byte %01100110
.byte %00000000
.byte %01100110
.byte %01101100
.byte %01111000
.byte %01110000
.byte %01111000
.byte %01101100
.byte %01100110
.byte %00000000

; Tile $2C - L
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01111110
.byte %00000000
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01111110
.byte %00000000

; Tile $2D - M
.byte %01100011
.byte %01110111
.byte %01111111
.byte %01101011
.byte %01100011
.byte %01100011
.byte %01100011
.byte %00000000
.byte %01100011
.byte %01110111
.byte %01111111
.byte %01101011
.byte %01100011
.byte %01100011
.byte %01100011
.byte %00000000

; Tile $2E - N
.byte %01100110
.byte %01110110
.byte %01111110
.byte %01111110
.byte %01101110
.byte %01100110
.byte %01100110
.byte %00000000
.byte %01100110
.byte %01110110
.byte %01111110
.byte %01111110
.byte %01101110
.byte %01100110
.byte %01100110
.byte %00000000

; Tile $2F - O
.byte %00111100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000
.byte %00111100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000

; Tile $30 - P
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %01100000
.byte %01100000
.byte %01100000
.byte %00000000
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %01100000
.byte %01100000
.byte %01100000
.byte %00000000

; Tile $31 - Q
.byte %00111100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01101010
.byte %01101100
.byte %00110110
.byte %00000000
.byte %00111100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01101010
.byte %01101100
.byte %00110110
.byte %00000000

; Tile $32 - R
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %01111000
.byte %01101100
.byte %01100110
.byte %00000000
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %01111000
.byte %01101100
.byte %01100110
.byte %00000000

; Tile $33 - S
.byte %00111100
.byte %01100110
.byte %01110000
.byte %00111100
.byte %00001110
.byte %01100110
.byte %00111100
.byte %00000000
.byte %00111100
.byte %01100110
.byte %01110000
.byte %00111100
.byte %00001110
.byte %01100110
.byte %00111100
.byte %00000000

; Tile $34 - T
.byte %01111110
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000
.byte %01111110
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000

; Tile $35 - U
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000

; Tile $36 - V
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00011000
.byte %00000000
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00011000
.byte %00000000

; Tile $37 - W
.byte %01100011
.byte %01100011
.byte %01100011
.byte %01101011
.byte %01111111
.byte %01110111
.byte %01100011
.byte %00000000
.byte %01100011
.byte %01100011
.byte %01100011
.byte %01101011
.byte %01111111
.byte %01110111
.byte %01100011
.byte %00000000

; Tile $38 - X
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00011000
.byte %00111100
.byte %01100110
.byte %01100110
.byte %00000000
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00011000
.byte %00111100
.byte %01100110
.byte %01100110
.byte %00000000

; Tile $39 - Y
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000

; Tile $3A - Z
.byte %01111110
.byte %00000110
.byte %00001100
.byte %00011000
.byte %00110000
.byte %01100000
.byte %01111110
.byte %00000000
.byte %01111110
.byte %00000110
.byte %00001100
.byte %00011000
.byte %00110000
.byte %01100000
.byte %01111110
.byte %00000000

; Fill remaining background tiles with empty
; We've used tiles $00-$03 (64 bytes) + $04-$1F (448 bytes) + $20-$3A (27 tiles x 16 = 432 bytes)
; Total so far: 64 + 448 + 432 = 944 bytes
; Pattern table 1 is 4096 bytes, so we need 4096 - 944 = 3152 bytes
.res 3152, $00    ; Rest of pattern table 1

