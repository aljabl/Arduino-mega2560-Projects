/** LED_functions.c
 *
 * @author Mike Zastre (2022-Nov-22)
 * @author Alexandra Blais (2024-Apr-5)
 *
 */


/** ----------------------------- INITIALIZATION ----------------------------- 
 * @author Mike Zastre */

#define __DELAY_BACKWARD_COMPATIBLE__ 1
#define F_CPU 16000000UL

#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>

#define DELAY1 0.000001
#define DELAY3 0.01

#define PRESCALE_DIV1 8
#define PRESCALE_DIV3 64
#define TOP1 ((int)(0.5 + (F_CPU/PRESCALE_DIV1*DELAY1))) 
#define TOP3 ((int)(0.5 + (F_CPU/PRESCALE_DIV3*DELAY3)))

#define PWM_PERIOD ((long int)500)

volatile long int count = 0;
volatile long int slow_count = 0;


ISR(TIMER1_COMPA_vect) {
	count++;
}

ISR(TIMER3_COMPA_vect) {
	slow_count += 5;
}


/** -------------------------------------------------------------------------- 
 * Function: led_state
 * --------------
 * @brief Turns an LED on or off and immediately returns. Works with PORTL LEDs (1 - 3) 
 *
 * @param LED The number of an LCD.
 * @param state A number indicating the state to which the LCD must be put (0 = off, values > 0 = on).
 * 
 * @author Alexandra Blais
 -------------------------------------------------------------------------- */
void led_state(uint8_t LED, uint8_t state) {
	/* LED = 0 is the default */
	int which_LED = 0b10000000;
		
	/* set the parameter LED */
	which_LED = (which_LED >> LED);
	which_LED = (which_LED >> LED);
	
	if (state == 1) {
		/* turn on the LED associated with current parameter LED, while keeping other LEDs (from past calls) on */
		PORTL |= which_LED;
	}
	else {
		/* turn off the LED associated with current parameter LED */ 
		PORTL &= ~(which_LED);
	} 
}


/** -------------------------------------------------------------------------- 
 * Function: SOS
 * --------------
 * @brief  Displays the Morse code for SOS. 
 * 
 * @author Alexandra Blais
 -------------------------------------------------------------------------- */
void SOS() {
    /** 
    An array of 8 bit values that indicate the LED pattern to be displayed.
    An array value indicates LEDS on or off by bits set or cleared (bit 0 is the
    state for LED #0, bit 1 is the state for LED #1, etc).
    @author Mike Zastre
    */
    uint8_t light[] = {
        0x1, 0, 0x1, 0, 0x1, 0,
        0xf, 0, 0xf, 0, 0xf, 0,
        0x1, 0, 0x1, 0, 0x1, 0,
        0x0
    };
    /**
    An array of 16-bit values representing the duration (ms) for an LED pattern.
    @author Mike Zastre
    */
    int duration[] = {
        100, 250, 100, 250, 100, 500,
        250, 250, 250, 250, 250, 500,
        100, 250, 100, 250, 100, 250,
        250
    };
    /** 
    The number of elements in light[] and the number of elements in duration[].
    @author Mike Zastre
    */
	int length = 19;
	
	for (int i = 0; i < 19; i++) {
		int current_LED = light[i];
		int current_duration = duration[i];
		
		if (current_LED == 0xf) {
			/* We have to turn on all the LEDs individually? */
			for (int j = 0; j < 4; j++) {
				led_state(j, 1);
			}
			
			_delay_ms(current_duration);
			/* turn LEDs back off  */
			for (int z = 0; z < 4; z++) {
				led_state(z, 0);
			} 
		}
		
		else if (current_LED == 1) {
			led_state(0, 1);
		
			_delay_ms(current_duration);
		
			/* turn LED off in between loops */
			led_state(0, 0); 
			
		}
		_delay_ms(current_duration);
	}
}


/** -------------------------------------------------------------------------- 
 * Function: display_LED
 * --------------
 * @brief  Helper function for glow(). Turns on specified LED at a user-specified 
 * brightness. We control the brightness by exploiting pulse-width modulation. 
 * Here, the LED is turned off or on depending on the threshold and count values. 
 * 
 * @param which_LED The LED to be turned on/off.
 * @param threshold The time (microseconds) for which the LED is on, relative to 
 * the 500 microsecond cycle. The duty cycle for the LED multiplied by the 
 * pulse-width modulation.
 * 
 * @author Alexandra Blais
 -------------------------------------------------------------------------- */
void display_LED(int which_LED, int threshold) {
    /* 
    count is a program-scope variable that is incremented every microsecond 
    by the timer 1 interrupt handler. 
    */
	// if count is below threshold value, turn light on (light stays on until count passes threshold)
	if (count <= threshold) {
		PORTL = which_LED;
	}
	// if count is above threshold value, turn light off (light stays off until count resets)
	else if (count < PWM_PERIOD) {
		PORTL &= ~(which_LED); // or just PORTL = 0
	}
	else {
		// count is greater than 500, so cycle restarts
		count = 0;
	}
}


/** -------------------------------------------------------------------------- 
 * Function: glow
 * --------------
 * @brief  Turns on specified LED at a user-specified brightness by calling 
 * display_LED in an infinite loop.
 * 
 * @param LED The LED to be turned on/off.
 * @param brightness The duty cycle for the LED expressed as a float value
 * between 0.0 (fully off) and 1.0 (fully on). 
 * 
 * @author Alexandra Blais
 -------------------------------------------------------------------------- */
void glow(uint8_t LED, float brightness) {
	int threshold = PWM_PERIOD * brightness;
	int which_LED = 0b10000000;
	
	/* set the parameter LED */
	which_LED = (which_LED >> LED);
	which_LED = (which_LED >> LED);
	
	/* infinite loop to modulate brightness. Each loop is 1ms */
	for (; ;){
		display_LED(which_LED, threshold);
	}
}


/** -------------------------------------------------------------------------- 
 * Function: pulse_glow
 * --------------
 * @brief  Pulses the specified LED. We do this by increasing and decreasing the
 * duty cycle (threshold), relative to the value of slow_count. Variable count 
 * is incremented every microsecond, and variable slow_count is incremented 
 * every 10 milliseconds. 
 * 
 * @param LED The LED to be turned on/off. 
 * 
 * @author Alexandra Blais
 -------------------------------------------------------------------------- */
void pulse_glow(uint8_t LED) {
	int threshold = 0;
	int which_LED = 0b10000000;
		
	/* set the parameter LED */
	which_LED = (which_LED >> LED);
	which_LED = (which_LED >> LED);
	
	for (;;) {
		// using sin to modulate the pulse! 
		threshold = (PWM_PERIOD * sin(slow_count/700)/2) + (PWM_PERIOD/2);
		display_LED(which_LED, threshold); 
	}
}


/** ---------------------------------- MAIN ---------------------------------- 
 * @author Mike Zastre */
int main() {
    /* Turn off global interrupts while setting up timers. */
	cli();

	/* Set up timer 1, i.e., an interrupt every 1 microsecond. */
	OCR1A = TOP1;
	TCCR1A = 0;
	TCCR1B = 0;
	TCCR1B |= (1 << WGM12);
    /* Next two lines provide a prescaler value of 8. */
	TCCR1B |= (1 << CS11);
	TCCR1B |= (1 << CS10);
	TIMSK1 |= (1 << OCIE1A);

	/* Set up timer 3, i.e., an interrupt every 10 milliseconds. */
	OCR3A = TOP3;
	TCCR3A = 0;
	TCCR3B = 0;
	TCCR3B |= (1 << WGM32);
    /* Next line provides a prescaler value of 64. */
	TCCR3B |= (1 << CS31);
	TIMSK3 |= (1 << OCIE3A);

	/* Turn on global interrupts */
	sei();

/* Test for part A. */   
	// led_state(0, 1);
	// _delay_ms(1000);
	// led_state(2, 1);
	// _delay_ms(1000);
	// led_state(1, 1);
	// _delay_ms(1000);
	// led_state(2, 0);
	// _delay_ms(1000);
	// led_state(0, 0);
	// _delay_ms(1000);
	// led_state(1, 0);
	// _delay_ms(1000); 

/* Test for part B. */ 
	//SOS();

/* Test for part C. */
	//glow(0, 1);
	
/* Test for part D.  */
	//pulse_glow(3);
}
