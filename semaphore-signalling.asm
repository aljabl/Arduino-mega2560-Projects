; semaphore-signalling.asm
;
; Author: Mike Zastre (2022-Oct-15)
; Author: Alexandra Blais (2024-Mar-11)


.include "m2560def.inc"
.cseg
.org 0



; # ------------------------------ INITIALIZATIONS ----------------------------- #
; Author: Alexandra Blais

	; setting up stack
	ldi r16, low(RAMEND)
	out SPL, r16
	ldi r17, high(RAMEND)
	out SPH, r17

	; setting up ports
	ldi r19, 0xff; set all pins to output
	sts DDRL, r19 ; assign DDRL to r19
	out DDRB, r19 ; assign DDRB to r19

	; defining registers
	.def PL = r24
	.def counter = r23
	.def PB = r21
	.def mask = r22

	; parameter offset for functions with pass-by-reference parameters
	.equ PARAM_OFFSET = 4
; # -------------------------- END OF INITIALIZATIONS -------------------------- #



; # ------------------------------ TESTING SECTION ----------------------------- #
; Author: Mike Zastre

	rjmp test_part_e
	; Test code

test_part_a:
	ldi r16, 0b00000001
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00000010
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00000100
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00001000
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00010000
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long


	ldi r16, 0b00101001
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00111000
	rcall set_leds
	rcall delay_short

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00100001
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00111111
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00111001
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00110000
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00001111
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds


	rjmp end

test_part_b:
	ldi r17, 0b00101010
	rcall slow_leds
	ldi r17, 0b00010101
	rcall slow_leds
	ldi r17, 0b00101010
	rcall slow_leds
	ldi r17, 0b00010101
	rcall slow_leds

	rcall delay_long
	rcall delay_long

	ldi r17, 0b00101010
	rcall fast_leds
	ldi r17, 0b00010101
	rcall fast_leds
	ldi r17, 0b00101010
	rcall fast_leds
	ldi r17, 0b00010101
	rcall fast_leds
	ldi r17, 0b00101010
	rcall fast_leds
	ldi r17, 0b00010101
	rcall fast_leds
	ldi r17, 0b00101010
	rcall fast_leds
	ldi r17, 0b00010101
	rcall fast_leds

	rjmp end

test_part_c:

	ldi r16, 0b11111000
	push r16
	rcall leds_with_speed
	pop r16

	ldi r16, 0b11011100
	push r16
	rcall leds_with_speed
	pop r16

	ldi r20, 0b00100000
test_part_c_loop:
	push r20
	rcall leds_with_speed
	pop r20
	lsr r20
	brne test_part_c_loop

	rjmp end


test_part_d:

	ldi r21, '-'
	push r21
	rcall encode_letter
	pop r21
	push r25
	rcall leds_with_speed
	pop r25

	rcall delay_long

	ldi r21, 'A'
	push r21
	rcall encode_letter
	pop r21
	push r25
	rcall leds_with_speed
	pop r25

	rcall delay_long


	ldi r21, 'M'
	push r21
	rcall encode_letter
	pop r21
	push r25
	rcall leds_with_speed
	pop r25

	rcall delay_long

	ldi r21, 'H'
	push r21
	rcall encode_letter
	pop r21
	push r25
	rcall leds_with_speed
	pop r25

	rcall delay_long


	rjmp end


test_part_e:
	ldi r25, HIGH(WORD02 << 1)
	ldi r24, LOW(WORD02 << 1)
	rcall display_message

	rcall delay_long 

		ldi r25, HIGH(WORD01 << 1)
	ldi r24, LOW(WORD01 << 1)
	rcall display_message

	rcall delay_long

		ldi r25, HIGH(WORD02 << 1)
	ldi r24, LOW(WORD02 << 1)
	rcall display_message

	rcall delay_long

		ldi r25, HIGH(WORD03 << 1)
	ldi r24, LOW(WORD03 << 1)
	rcall display_message

	rcall delay_long

		ldi r25, HIGH(WORD04 << 1)
	ldi r24, LOW(WORD04 << 1)
	rcall display_message
	rjmp end

end:
    rjmp end
; # -------------------------- END OF TESTING SECTION -------------------------- #



; # --------------------------------- FUNCTIONS -------------------------------- #

; # --------------------------------- set_leds --------------------------------- #
; Author: Alexandra Blais

set_leds:
; recieves r16 as a parameter (pass-by-value)
; take bits in r16 and assign them to corressponding port bits in PB and PL

	clr PB
	clr PL
	clr counter

	; turn lights off
	rcall lights_off

	; assign first 4 bits of r16 to corressponding bits for PORTL (PL)
	updatePL:
		lsr r16; rightmost bit is of r16 is stored in C flag
		rol PL; if carry is set, a 1 is recorded in PL. If not, a 0 is recorded
		lsl PL; account for spaces between bits
		inc counter
		cpi counter, 4; only 4 bits can be set in PL, so iterate 4 times
		brlo updatePL

	; assign last 2 bits of r16 to corressponding bits for PORTB (PB)
	updatePB:
	; r16 should be right-aligned already
	; repeat same procedure, but only for 2 iterations (and recording in register PB)
		lsr r16
		rol PB
		lsl PB
		inc counter
		cpi counter, 6
		brlo updatePB

	; r16 is now appropriately encoded in PL and PB

	; turn on PORTL lights
	sts PORTL, PL
	; turn on PORTB lights
	out PORTB, PB

	ret


; # --------------------------------- slow_leds -------------------------------- #
; Author: Alexandra Blais

slow_leds:
	; parameter is stored in r17
	mov r16, r17

	rcall set_leds
	rcall delay_long

	; turn lights off
	rcall lights_off

	ret


; # --------------------------------- fast_leds -------------------------------- #
; Author: Alexandra Blais

fast_leds:
	mov r16, r17

	rcall set_leds
	rcall delay_short

	; turn off lights
	rcall lights_off

	ret


; # ------------------------------ leds_with_speed ----------------------------- #
; Author: Alexandra Blais

leds_with_speed:
; passed a byte that's pushed onto the stack by caller
; if the two top bits of r16 are SET, LED pattern is on for 1 second
; if the top two bits of r16 are UNSET, LED pattern is on for .25 seconds

	; need to get parameter value from the stack
	; creating our own SP
	in YH, SPH
	in YL, SPL

	; move up 4 values in the stack to get the parameter value that was saved to the stack by the caller
	ldd r17, Y + PARAM_OFFSET; r17 now has the value that was pushed to the stack BEFORE the ret addr was pushed
	push r17; save value to stack
	andi r17, 0b11000000; check if top two bits are set
	cpi r17, 0; r17 = 0 means top two bits are not set -> call fast_leds
	pop r17; get value of r17 (encoded character) back so we can use it as a parameter for slow_leds/fast_leds
	breq bitUnset

	; bitSet
		rcall slow_leds
	rjmp skip

	bitUnset:
		rcall fast_leds

	skip:
	; short delay so the lights turn off in between letters (so we can distinguish between >= 2 of the same letters in a row)
	rcall delay_short
	ret


; # ------------------------------- encode_letter ------------------------------ #
; Author: Alexandra Blais

encode_letter:; treats '-' as a normal input
	; we need to retrieve parameter letter from stack
	; creating our own SP
	in YH, SPH
	in YL, SPL

	; move up 4 values in the stack to get the parameter value that was saved to the stack by the caller
	ldd r17, Y + PARAM_OFFSET; r17 now has the value that was pushed to the stack BEFORE the ret addr was pushed

	; push starting addr (SRAM) of alphabet onto stack. We will iterate through until we find a matching letter/character
	ldi r18, high(PATTERNS << 1)
	push r18
	ldi r18, low(PATTERNS << 1)
	push r18

	; save PATTERNS addr to ZH:ZL
	pop ZL
	pop ZH
	; now Z holds the starting addr of PATTERNS


	; iterate through characters until we get a match
	matchCharacter:
		lpm r18, Z+; load byte pointed to by Z into r18. Z increments by 1 each time
		cp r18, r17; compare current character to our parameter character to see if they match
		brne matchCharacter; iterate until you find a match

	; next 6 characters are our light cues
	clr r25; r16 will hold the light/duration encoding of our parameter character
	clr counter

	setLights:
		lpm r17, Z+; load character into r17
		cpi r17, 0x2e; 0x2e = '.'
		breq skip2
		inc r25; if character is a 'o' increment r16 
		skip2:
			lsl r25; in both cases, shift r16 (records a 0 for . condition)
		inc counter
		cpi counter, 6; we iterate a total of 6 times
		brne setLights
		lsr r25

	; last character is our duration cue
	setDuration:
		lpm r17, Z+; load current character into r17
		cpi r17, 1; 1 = slow
		brne ending
		ori r25, 0b11000000; set top 2 bits for slow flashes if last character is a 1

	ending:
	ret

; # ------------------------------ display_message ----------------------------- #
; Author: Alexandra Blais

display_message:
	; save message SRAM addr onto stack
	push r25
	push r24

	; iterate through characters in sequence
	characterInMessage:
		pop ZL
		pop ZH
		; Z now holds the starting address of the message

		lpm r19, Z+; load current character into r18
		cpi r19, 0; check if character is 0
		breq endOfMessage; messages end at 00, so branch out of loop

		; save addr and value of current character onto stack BEFORE we call encode_letter
		; encode_letter also uses Z, so its value will be changed when we return
		push ZH
		push ZL
		push r19; push current character onto stack

		; turn character into bit sequence
		rcall encode_letter

		; encoded letter is returned in r25, save this value in stack before calling leds_with_speed
		push r25
		rcall leds_with_speed

		; clear stack before next iteration
		pop r25
		pop r19

		rjmp characterInMessage

	endOfMessage:
		ret

; turns lights off
lights_off:
	clr r19
	sts PORTL, r19
	out PORTB, r19
	ret
; # ---------------------------------------------------------------------------- #



; # ------------------------------ DELAY FUNCTIONS ----------------------------- #
; Author: Mike Zastre

; about one second
delay_long:
	push r16

	ldi r16, 14
delay_long_loop:
	rcall delay
	dec r16
	brne delay_long_loop

	pop r16
	ret


; about 0.25 of a second
delay_short:
	push r16

	ldi r16, 4
delay_short_loop:
	rcall delay
	dec r16
	brne delay_short_loop

	pop r16
	ret

; When wanting about a 1/5th of a second delay, all other
; code must call this function
;
delay:
	rcall delay_busywait
	ret


; This function is ONLY called from "delay", and
; never directly from other code. Really this is
; nothing other than a specially-tuned triply-nested
; loop. It provides the delay it does by virtue of
; running on a mega2560 processor.
;
delay_busywait:
	push r16
	push r17
	push r18

	ldi r16, 0x08
delay_busywait_loop1:
	dec r16
	breq delay_busywait_exit

	ldi r17, 0xff
delay_busywait_loop2:
	dec r17
	breq delay_busywait_loop1

	ldi r18, 0xff
delay_busywait_loop3:
	dec r18
	breq delay_busywait_loop2
	rjmp delay_busywait_loop3

delay_busywait_exit:
	pop r18
	pop r17
	pop r16
	ret
; # -------------------------- END OF DELAY FUNCTIONS -------------------------- #



; # --------------------------------- VARIABLES -------------------------------- #
; Author: Mike Zastre
;.cseg
;.org 0x600

PATTERNS: 
	; LED pattern shown from left to right: "." means off, "o" means
    ; on, 1 means long/slow, while 2 means short/fast.
	.db "A", "..oo..", 1
	.db "B", ".o..o.", 2
	.db "C", "o.o...", 1
	.db "D", ".....o", 1
	.db "E", "oooooo", 1
	.db "F", ".oooo.", 2
	.db "G", "oo..oo", 2
	.db "H", "..oo..", 2
	.db "I", ".o..o.", 1
	.db "J", ".....o", 2
	.db "K", "....oo", 2
	.db "L", "o.o.o.", 1
	.db "M", "oooooo", 2
	.db "N", "oo....", 1
	.db "O", ".oooo.", 1
	.db "P", "o.oo.o", 1
	.db "Q", "o.oo.o", 2
	.db "R", "oo..oo", 1
	.db "S", "....oo", 1
	.db "T", "..oo..", 1
	.db "U", "o.....", 1
	.db "V", "o.o.o.", 2
	.db "W", "o.o...", 2
	.db "X", "oo....", 2
	.db "Y", "..oo..", 2
	.db "Z", "o.....", 2
	.db "-", "o...oo", 1   ; Just in case!

WORD00: .db "HELLOWORLD", 0, 0
WORD01: .db "THE", 0
WORD02: .db "QUICK", 0
WORD03: .db "BROWN", 0
WORD04: .db "FOX", 0
WORD05: .db "JUMPED", 0, 0
WORD06: .db "OVER", 0, 0
WORD07: .db "THE", 0
WORD08: .db "LAZY", 0, 0
WORD09: .db "DOG", 0
; # ----------------------------- END OF VARIABLES ----------------------------- #