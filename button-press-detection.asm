
; button-press-detection.asm

; Author: Mike Zastre (2022-Nov-05)
; Author: Alexandra Blais (2024-Mar-23)

; Code that permits the use of the “up” and “down” buttons to set a hexadecimal
; digit at the top-left of the LCD display. Pressing the "left" or "right" buttons
; will permit the “up”/”down” buttons to work in a different part of the display’s 
; top row.

# ------------------------------ INITIALIZATIONS ----------------------------- #
; Author: Mike Zastre

; This sections initializes the following: 

; (1) assembler direction setting up the interrupt-vector table
;
; (2) "includes" for the LCD display
;
; (3) some definitions of constants that may be used later in
;     the program
;
; (4) code for initial setup of the Analog-to-Digital Converter
;
; (5) code for setting up three timers (timers 1, 3, and 4).
;

.cseg
.org 0
	jmp reset

; Actual .org details for this an other interrupt vectors can be
; obtained from main ATmega2560 data sheet
;
.org 0x22
	jmp timer1

; This included for completeness. Because timer3 is used to
; drive updates of the LCD display, and because LCD routines
; *cannot* be called from within an interrupt handler, we
; will need to use a polling loop for timer3.
;
; .org 0x40
;	jmp timer3

.org 0x54
	jmp timer4

.include "m2560def.inc"
.include "lcd.asm"

.cseg
#define CLOCK 16.0e6
#define DELAY1 0.01
#define DELAY3 0.1
#define DELAY4 0.5

#define BUTTON_RIGHT_MASK 0b00000001	
#define BUTTON_UP_MASK    0b00000010
#define BUTTON_DOWN_MASK  0b00000100
#define BUTTON_LEFT_MASK  0b00001000

#define BUTTON_RIGHT_ADC  0x032
#define BUTTON_UP_ADC     0x0b0   ; was 0x0c3
#define BUTTON_DOWN_ADC   0x160   ; was 0x17c
#define BUTTON_LEFT_ADC   0x22b
#define BUTTON_SELECT_ADC 0x316

.equ PRESCALE_DIV=1024   ; w.r.t. clock, CS[2:0] = 0b101

; TIMER1 is a 16-bit timer. If the Output Compare value is
; larger than what can be stored in 16 bits, then either
; the PRESCALE needs to be larger, or the DELAY has to be
; shorter, or both.
.equ TOP1=int(0.5+(CLOCK/PRESCALE_DIV*DELAY1))
.if TOP1>65535
.error "TOP1 is out of range"
.endif

; TIMER3 is a 16-bit timer. If the Output Compare value is
; larger than what can be stored in 16 bits, then either
; the PRESCALE needs to be larger, or the DELAY has to be
; shorter, or both.
.equ TOP3=int(0.5+(CLOCK/PRESCALE_DIV*DELAY3))
.if TOP3>65535
.error "TOP3 is out of range"
.endif

; TIMER4 is a 16-bit timer. If the Output Compare value is
; larger than what can be stored in 16 bits, then either
; the PRESCALE needs to be larger, or the DELAY has to be
; shorter, or both.
.equ TOP4=int(0.5+(CLOCK/PRESCALE_DIV*DELAY4))
.if TOP4>65535
.error "TOP4 is out of range"
.endif

reset:
# -------------------------- END OF INITIALIZATIONS -------------------------- #



# ----------------------- PRE_INTERRUPT INITIALIZATIONS ---------------------- #
; Author: Alexandra Blais

.def boundary_low = r0
.def boundary_high = r1
.def data_low = r24
.def data_high = r25
.def temp_low = r22
.def temp_high = r23
.def column = r20
.def char_index = r21

# ---------------------------------------------------------------------------- #

; Set up stack.
ldi temp, low(RAMEND)
out SPL, temp
ldi temp, high(RAMEND)
out SPH, temp

# ---------------------------------------------------------------------------- #

; Initalize LCD display.
rcall lcd_init

# ---------------------------------------------------------------------------- #

; Initialize pointer for TOP_LINE_CONTENT. 
ldi YH, high(TOP_LINE_CONTENT)
ldi YL, low(TOP_LINE_CONTENT)

push YL 
push YH

	; Put spaces in every column.
	ldi temp, ' '
	ldi r17, 16
	loop1:
		st Y+, temp
		dec r17
		cpi r17, 0
		brne loop1

pop YH
pop YL

# ---------------------------------------------------------------------------- #

; Initialize pointer for CURRENT_CHARSET_INDEX.
push XL
push XH

	ldi XH, high(CURRENT_CHARSET_INDEX)
	ldi XL, low(CURRENT_CHARSET_INDEX)

	; Put -1 in every column.
	ldi temp, -1
	ldi r17, 0
	loop2:
		st X+, temp
		inc r17
		cpi r17, 16
		brne loop2

	pop XH
	pop XL

# ---------------------------------------------------------------------------- #

; Initalize CURRENT_CHAR_INDEX to 0.
	ldi r17, 0
	sts CURRENT_CHAR_INDEX, r17
# ------------------- END OF PRE-INTERRUPT INITIALIZATIONS ------------------- #



# --------------------------- TIMER INITIALIZATIONS -------------------------- #
; Author: Mike Zastre

	; initialize the ADC converter (which is needed
	; to read buttons on shield). Note that we'll
	; use the interrupt handler for timer 1 to
	; read the buttons (i.e., every 10 ms)
	;
	ldi temp, (1 << ADEN) | (1 << ADPS2) | (1 << ADPS1) | (1 << ADPS0)
	sts ADCSRA, temp
	ldi temp, (1 << REFS0)
	sts ADMUX, r16

	; Timer 1 is for sampling the buttons at 10 ms intervals.
	; We will use an interrupt handler for this timer.
	ldi r17, high(TOP1)
	ldi r16, low(TOP1)
	sts OCR1AH, r17
	sts OCR1AL, r16
	clr r16
	sts TCCR1A, r16
	ldi r16, (1 << WGM12) | (1 << CS12) | (1 << CS10)
	sts TCCR1B, r16
	ldi r16, (1 << OCIE1A)
	sts TIMSK1, r16

	; Timer 3 is for updating the LCD display. We are
	; *not* able to call LCD routines from within an 
	; interrupt handler, so this timer must be used
	; in a polling loop.
	ldi r17, high(TOP3)
	ldi r16, low(TOP3)
	sts OCR3AH, r17
	sts OCR3AL, r16
	clr r16
	sts TCCR3A, r16
	ldi r16, (1 << WGM32) | (1 << CS32) | (1 << CS30)
	sts TCCR3B, r16
	; Notice that the code for enabling the Timer 3
	; interrupt is missing at this point.

	; Timer 4 is for updating the contents to be displayed
	; on the top line of the LCD.
	ldi r17, high(TOP4)
	ldi r16, low(TOP4)
	sts OCR4AH, r17
	sts OCR4AL, r16
	clr r16
	sts TCCR4A, r16
	ldi r16, (1 << WGM42) | (1 << CS42) | (1 << CS40)
	sts TCCR4B, r16
	ldi r16, (1 << OCIE4A)
	sts TIMSK4, r16

	sei
# ----------------------- END OF TIMER INITIALIZATIONS ----------------------- #



start:
# ---------------------------------- TIMER 3 --------------------------------- #
; Author: Alexandra Blais
; For timer3: The polling loop will examine the values in the five memory areas 
; (listed in the two bullet points above) every 100 milliseconds. The code in 
; this loop will make the appropriate calls to lcd_gotoxy and lcd_putchar to 
; ensure the LCD display represents that state of the program.

timer3:
	; Check if TOP value reached.
	in temp, TIFR3
	sbrs temp, OCF3A
	rjmp timer3

	; Resetting bit.
	ldi temp, 1<<OCF3A
	out TIFR3, temp

	; Set row and column for display.
	ldi temp, 1; row
	ldi r17, 15; column
	rcall set_position

	; Determine if button was pressed.
	lds temp, BUTTON_IS_PRESSED
	cpi temp, 1
	breq pressed

	; If no button was pressed, display '-' and do nothing.
	notPressed:
		ldi temp, '-'
		call put_char
		rjmp end_timer3

	; If button was pressed, display '*' and identify the button. 
	pressed:
		ldi temp, '*'
		call put_char

		; Clear spaces on bottom row.
		call clear_spots


	display_bottom_row:
		; Get parameter value (button identifier).
		lds temp, LAST_BUTTON_PRESSED

		cpi temp, BUTTON_LEFT_MASK
		breq left_button_display

		cpi temp, BUTTON_DOWN_MASK
		breq down_button_display

		cpi temp, BUTTON_UP_MASK
		breq up_button_display

		cpi temp, BUTTON_RIGHT_MASK
		breq right_button_display


	left_button_display:
		; Set cursor.
		ldi temp, 1; row
		ldi r17, 0; column
		rcall set_position

		; Encode char.
		ldi temp, 'L'
		call put_char

		rjmp end_timer3
		
	down_button_display:
		; Set cursor to bottom row.
		ldi temp, 1; row
		ldi r17, 1; column
		rcall set_position

		; Encode char.
		ldi temp, 'D'
		call put_char

		rjmp display_top_line

	up_button_display:
		; Set cursor to bottom row.
		ldi temp, 1; row
		ldi r17, 2; column
		rcall set_position

		; Encode char.
		ldi temp, 'U'
		call put_char

		rjmp display_top_line

	right_button_display:
		; Set cursor to bottom row.
		ldi temp, 1; row
		ldi r17, 3; column
		rcall set_position

		; Encode char.
		ldi r16, 'R'
		call put_char
		
	end_timer3:
		rjmp start
# ------------------------------ END OF TIMER 3 ------------------------------ #



# ------------------------- TIMER 3 HELPER FUNCTIONS ------------------------- #
; Author: Alexandra Blais
	
put_char:
		push temp
		rcall lcd_putchar
		pop temp
		ret

# ---------------------------------------------------------------------------- #

set_position:
		push temp
		push r17
		rcall lcd_gotoxy
		pop r17
		pop temp
		ret

# ---------------------------------------------------------------------------- #

clear_spots:
	push temp
	push r17

	ldi temp, 1; row
	ldi r17, 0; column
	rcall set_position

	ldi r17, 4
	clear_spots_loop:
		ldi temp, ' '
		call put_char
		dec r17
		cpi r17, 0
		brne clear_spots_loop

	pop r17
	pop temp
	ret

# ---------------------------------------------------------------------------- #

display_top_line:
	push temp
	push r17
	push YH
	push YL

	; Set cursor to top row and current column.
	clr temp; row = 0
	lds column, CURRENT_CHAR_INDEX
	mov r17, column; column
	rcall set_position

	; Initalize and move TOP_LINE_CONTENT pointer to current column.
	call set_TOP_LINE_CONTENT_pointer

	; Y now points to the byte containing the correct char for our current column, now we display the char.
	ld temp, Y; load byte pointed to by Y into temp
	call put_char

	pop YL
	pop YH
	pop r17
	pop temp
	rjmp end_timer3

# ---------------------- END OF TIMER 3 HELPER FUNCTIONS --------------------- #



# ---------------------------------- TIMER 1 --------------------------------- #
; Author: Alexandra Blais
; The handler will use a polling loop to examine the ADC for button-presses every 10 milliseconds, 
; writing correct values into the BUTTON_IS_PRESSED and LAST_BUTTON_PRESSED 
; memory areas.

timer1:
	push temp
	in temp, SREG
	push temp

	rcall timer1_polling_routine

	; After conversion, get data (digital signal in binary) from button.
	lds data_low, ADCL
	lds data_high, ADCH
		
	; Compare data to boundary -> value > 900 means no button was pressed.
	cp data_low, boundary_low
	cpc data_high, boundary_high

	; If no button was pressed, do nothing.
	brsh end_timer1

		; Condition where button was pressed.
		ldi temp, 1
		sts BUTTON_IS_PRESSED, temp

		; x > 0x22c -> branch to end
		ldi temp_low, low(BUTTON_LEFT_ADC)
		ldi temp_high, high(BUTTON_LEFT_ADC)
		cp data_low, temp_low
		cpc data_high, temp_high
		brsh end_timer1

		; x > 0x161 -> branch to left
		ldi temp_low, low(BUTTON_DOWN_ADC)
		ldi temp_high, high(BUTTON_DOWN_ADC)
		cp data_low, temp_low
		cpc data_high, temp_high
		brsh left

		; x > 0xb1 -> branch to down
		ldi temp_low, low(BUTTON_UP_ADC)
		ldi temp_high, high (BUTTON_UP_ADC)
		cp data_low, temp_low
		cpc data_high, temp_high
		brsh down

		; x > 0x33 -> branch to up
		cpi data_low, BUTTON_RIGHT_ADC
		brsh up

		; else: branch to right
		rjmp right


		; When we have a match, update the last_button_pressed.
		left:
			ldi temp, BUTTON_LEFT_MASK
			sts LAST_BUTTON_PRESSED, temp
			rjmp end_timer1


		down:
			ldi temp, BUTTON_DOWN_MASK
			sts LAST_BUTTON_PRESSED, temp
			rjmp end_timer1


		up:
			ldi temp, BUTTON_UP_MASK
			sts LAST_BUTTON_PRESSED, temp
			rjmp end_timer1


		right:
			ldi temp, BUTTON_RIGHT_MASK
			sts LAST_BUTTON_PRESSED, temp


		end_timer1:
			; restore SREG
			pop temp
			out SREG, temp
			pop temp
	reti

# ------------------------------ END OF TIMER 1 ------------------------------ #



# -------------------------- TIMER 1 POLLING ROUTINE ------------------------- #
; Author: Alexandra Blais

timer1_polling_routine:
	; Initialize to 0 (so button-state resets back to noPress).
	clr temp
	sts BUTTON_IS_PRESSED, temp

	; Set boundaries: if ADC value is greater than 900, then no button is being pressed.
	ldi temp, low(901)
	mov boundary_low, temp
	ldi temp, high(901)
	mov boundary_high, temp

	; NOT SURE WHY THIS PART IS NEEDED OR WHAT IT DOES
	lds temp, ADCSRA
	ori temp, 0x40
	sts ADCSRA, temp

	; Check if conversion is complete.
	wait:
		; get value of ADC
		lds temp, ADCSRA
		; check if bit is set
		andi temp, 0x40
		; if not set, continue polling until it is
		brne wait
	ret
# ---------------------- END OF TIMER 1 POLLING ROUTINE ---------------------- #



# ---------------------------------- TIMER 4 --------------------------------- #
; Author: Alexandra Blais
; The handler will examine the values in BUTTON_IS_PRESSED and LAST_BUTTON_PRESSED 
; every 0.5 seconds in order read and write memory areas CURRENT_CHARSET_INDEX, 
; CURRENT_CHAR_INDEX, and possibly TOP_LINE_CONTENT.

timer4:
	push temp
	in temp, SREG
	push temp
	push column
	push char_index
	push r17

	; Test if button was pressed.
	lds temp, BUTTON_IS_PRESSED
	cpi temp, 0 
	breq end_timer4

	; Get column.
	lds column, CURRENT_CHAR_INDEX

	; Initalize and move CURRENT_CHARSET_INDEX pointer to current column.
	rcall set_CURRENT_CHARSET_INDEX_pointer
	
	; Store byte as CURRENT_CHARSET_INDEX's value.
	ld char_index, X

	; Initalize and move TOP_LINE_CONTENT pointer to current column.
	rcall set_TOP_LINE_CONTENT_pointer
		

	; Determine which button was pressed.
	lds temp, LAST_BUTTON_PRESSED

	cpi temp, BUTTON_LEFT_MASK
	breq decrease_column_index

	cpi temp, BUTTON_DOWN_MASK
	breq decrease_char_index

	cpi temp, BUTTON_UP_MASK
	breq increase_char_index

	cpi temp, BUTTON_RIGHT_MASK
	breq increase_column_index

	rjmp end_timer4

	increase_column_index:
		cpi column, 15; Make sure we're not at the last column of display (16 columns total, indexed 0-15).
		brsh end_timer4

		inc column; move right by 1
		sts CURRENT_CHAR_INDEX, column; Update CURRENT_CHAR_INDEX value.
		rjmp end_timer4


	decrease_column_index:
		cpi column, 0; Make sure we're not at the end of the display.
		breq end_timer4

		dec column; move left 1
		sts CURRENT_CHAR_INDEX, column; Update CURRENT_CHAR_INDEX value.
		rjmp end_timer4


	increase_char_index:
		; Increment char_index.
		inc char_index

		; Initalize and move AVAILABLE_CHARSET pointer to current column's char index.
		rcall set_AVAILABLE_CHARSET_pointer

		; Check if Z is pointing at end of char string.
		lpm temp, Z
		cpi temp, 0; null terminator
		breq reset_increase_char_index

		rjmp store


	decrease_char_index:
		; Before we decrement, make sure we're not at the beginning of the char string.
		cpi char_index, 1
		brlt reset_decrease_char_index
		
		; Decrement char_index.
		dec char_index

		; Initalize and move AVAILABLE_CHARSET pointer to current column's char index.
		rcall set_AVAILABLE_CHARSET_pointer

		rjmp store
			

	store:
		; Update value of CURRENT_CHARSET_INDEX's current byte.
		st X, char_index; char value is now stored at byte located at current column's addr in CURRENT_CHARSET_INDEX

		; Update TOP_LINE_CONTENT.
		lpm temp, Z; Z points to current char.
		st Y, temp; Store char in TOP_LINE_CONTENT's byte associated with current column

		rjmp end_timer4
			

	reset_increase_char_index:
		; Reset char_index back to 0.
		clr char_index
		; Update Z pointer to point to the beginning of the charset.
		rcall set_AVAILABLE_CHARSET_pointer
		; Store updated values.
		rjmp store


	reset_decrease_char_index:
	clr char_index
		;  Find length of charset.
		ldi char_index, -1

		length_of_charset_loop:
			inc char_index
			lpm temp, Z+
			cpi temp, 0; null terminator
		brne length_of_charset_loop

		; Update Z pointer to point to the end of the charset.
		rcall set_AVAILABLE_CHARSET_pointer

		rjmp decrease_char_index	

	end_timer4:
		pop r17
		pop char_index
		pop column
		pop temp
		out SREG, temp
		pop temp
		reti
# ------------------------------ END OF TIMER 4 ------------------------------ #



# ------------------------- TIMER 4 HELPER FUNCTIONS ------------------------- #
; Author: Alexandra Blais

set_CURRENT_CHARSET_INDEX_pointer:
	; Set X pointer to beginning of CURRENT_CHARSET_INDEX.
	ldi XH, high(CURRENT_CHARSET_INDEX)
	ldi XL, low(CURRENT_CHARSET_INDEX)

	; Update X pointer to byte (of CURRENT_CHARSET_INDEX) associated with current column.
	add XL, column
	clr temp 
	adc XH, temp
	; X now points to addr of byte (containing char index) associated with current column.
	ret

# ---------------------------------------------------------------------------- #

set_TOP_LINE_CONTENT_pointer:
	; Set Y pointer to beginning of TOP_LINE_CONTENT.
	ldi YH, high(TOP_LINE_CONTENT)
	ldi YL, low(TOP_LINE_CONTENT)
		
	; Update Y pointer to byte (of TOP_LINE_CONTENT) associated with current column.
	add YL, column
	clr temp
	adc YH, temp
	; Y now points to addr of byte (containing char index) associated with current column.
	ret

# ---------------------------------------------------------------------------- #

set_AVAILABLE_CHARSET_pointer:
	; Set Z pointer to beginning of AVAILABLE_CHARSET.
	ldi ZH, high(AVAILABLE_CHARSET<<1)
	ldi ZL, low(AVAILABLE_CHARSET<<1)

	; Update Z pointer to char-string value associated with current column.
	add ZL, char_index
	clr temp
	adc ZH, temp
	; Now Z points to char associated with our most recent char index value.
	ret
# ---------------------- END OF TIMER 4 HELPER FUNCTIONS --------------------- #



# --------------------------------- VARIABLES -------------------------------- #
; Author: Mike Zastre

.cseg
AVAILABLE_CHARSET: .db "0123456789abcdef_", 0


.dseg

BUTTON_IS_PRESSED: .byte 1			; updated by timer1 interrupt, used by LCD update loop
LAST_BUTTON_PRESSED: .byte 1        ; updated by timer1 interrupt, used by LCD update loop

TOP_LINE_CONTENT: .byte 16			; updated by timer4 interrupt, used by LCD update loop
CURRENT_CHARSET_INDEX: .byte 16		; updated by timer4 interrupt, used by LCD update loop
CURRENT_CHAR_INDEX: .byte 1			; updated by timer4 interrupt, used by LCD update loop

# ----------------------------- END OF VARIABLES ----------------------------- #