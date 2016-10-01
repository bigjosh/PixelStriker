// Change this to be at least as long as your pixel string (too long will work fine, just be a little slower)

//#define DEBUG

#define PIXELS_PER_METER 96

#define LENGTH_METERS 4

#define PIXELS PIXELS_PER_METER * LENGTH_METERS

#define G_METERS_PER_SECOND_PER_SECOND (9.8)

#define V_METERS_PER_SECOND 9     // Starting velocity to almost make it to the top 

// These values depend on which pins your 8 strings are connected to and what board you are using 
// More info on how to find these at http://www.arduino.cc/en/Reference/PortManipulation

// PORTD controls Digital Pins 0-7 on the Uno

// You'll need to look up the port/bit combination for other boards. 

// Note that you could also include the DigitalWriteFast header file to not need to to this lookup.

#define PIXEL_PORT  PORTD  // Port of the pin the pixels are connected to
#define PIXEL_DDR   DDRD   // Port of the pin the pixels are connected to

#define BELL_PIN 10         // Output Pin that the bell is attached to

static const uint8_t onBits=0b11111110;   // Bit pattern to write to port to turn on all pins connected to LED strips. 
                                          // If you do not want to use all 8 pins, you can mask off the ones you don't want
                                          // Note that these will still get 0 written to them when we send pixels
                                          // TODO: If we have time, we could even add a variable that will and/or into the bits before writing to the port to support any combination of bits/values                                  

// These are the timing constraints taken mostly from 
// imperically measuring the output from the Adafruit library strandtest program

// Note that some of these defined values are for refernce only - the actual timing is determinted by the hard code.

#define T1H  814    // Width of a 1 bit in ns - 13 cycles
#define T1L  438    // Width of a 1 bit in ns -  7 cycles

#define T0H  312    // Width of a 0 bit in ns -  5 cycles
#define T0L  936    // Width of a 0 bit in ns - 15 cycles 

// Phase #1 - Always 1  - 5 cycles
// Phase #2 - Data part - 8 cycles
// Phase #3 - Always 0  - 7 cycles

#define RES 50000   // Width of the low gap between bits to cause a frame to latch

// Here are some convience defines for using nanoseconds specs to generate actual CPU delays

#define NS_PER_SEC (1000000000L)          // Note that this has to be SIGNED since we want to be able to check for negative values of derivatives

#define CYCLES_PER_SEC (F_CPU)

#define NS_PER_CYCLE ( NS_PER_SEC / CYCLES_PER_SEC )

#define NS_TO_CYCLES(n) ( (n) / NS_PER_CYCLE )


// Sends a full 8 bits down all the pins, represening a single color of 1 pixel
// We walk though the 8 bits in colorbyte one at a time. If the bit is 1 then we send the 8 bits of row out. Otherwise we send 0. 
// We send onBits at the first phase of the signal generation. We could just send 0xff, but that mught enable pull-ups on pins that we are not using. 

/// Unforntunately we have to drop to ASM for this so we can interleave the computaions durring the delays, otherwise things get too slow.

// OnBits is the mask of which bits are connected to strips. We pass it on so that we
// do not turn on unused pins becuase this would enable the pullup. Also, hopefully passing this
// will cause the compiler to allocate a Register for it and avoid a reload every pass.

static inline void sendBitx8(  const uint8_t row , const uint8_t colorbyte , const uint8_t onBits ) {  
              
    asm volatile (


      "L_%=: \n\r"  
      
      "out %[port], %[onBits] \n\t"                 // (1 cycles) - send either T0H or the first part of T1H. Onbits is a mask of which bits have strings attached.

      // Next determine if we are going to be sending 1s or 0s based on the current bit in the color....
      
      "mov r0, %[bitwalker] \n\t"                   // (1 cycles) 
      "and r0, %[colorbyte] \n\t"                   // (1 cycles)  - is the current bit in the color byte set?
      "breq OFF_%= \n\t"                            // (1 cycles) - bit in color is 0, then send full zero row (takes 2 cycles if branch taken, count the extra 1 on the target line)

      // If we get here, then we want to send a 1 for every row that has an ON dot...
      "nop \n\t  "                                  // (1 cycles) 
      "out %[port], %[row]   \n\t"                  // (1 cycles) - set the output bits to [row] This is phase for T0H-T1H.
                                                    // ==========
                                                    // (5 cycles) - T0H (Phase #1)


      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t "                                   // (1 cycles) 

      "out %[port], __zero_reg__ \n\t"              // (1 cycles) - set the output bits to 0x00 based on the bit in colorbyte. This is phase for T0H-T1H
                                                    // ==========
                                                    // (8 cycles) - Phase #2
                                                    
      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag
                  
      "brcs DONE_%= \n\t"                          // (1 cycles) Exit if carry bit is set as a result of us walking all 8 bits. We assume that the process around us will tak long enough to cover the phase 3 delay

      "nop \n\t \n\t "                             // (1 cycles) - When added to the 5 cycles in S:, we gte the 7 cycles of T1L
            
      "jmp L_%= \n\t"                              // (3 cycles) 
                                                   // (1 cycles) - The OUT on the next pass of the loop
                                                   // ==========
                                                   // (7 cycles) - T1L
                                                   
                                                          
      "OFF_%=: \n\r"                                // (1 cycles)    Note that we land here becuase of breq, which takes takes 2 cycles

      "out %[port], __zero_reg__ \n\t"              // (1 cycles) - set the output bits to 0x00 based on the bit in colorbyte. This is phase for T0H-T1H
                                                    // ==========
                                                    // (5 cycles) - T0H

      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag
                  
      "brcs DONE_%= \n\t"                          // (1 cycles) Exit if carry bit is set as a result of us walking all 8 bits. We assume that the process around us will tak long enough to cover the phase 3 delay

      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles)             
      "nop \n\t nop \n\t "                          // (2 cycles)             
      "nop \n\t "                                   // (1 cycles)             
            
      "jmp L_%= \n\t"                               // (3 cycles) 
                                                    // (1 cycles) - The OUT on the next pass of the loop      
                                                    // ==========
                                                    //(15 cycles) - T0L 
      
            
      "DONE_%=: \n\t"

      // Don't need an explicit delay here since the overhead that follows will always be long enough
    
      ::
      [port]    "I" (_SFR_IO_ADDR(PIXEL_PORT)),
      [row]   "d" (row),
      [onBits]   "d" (onBits),
      [colorbyte]   "d" (colorbyte ),     // Phase 2 of the signal where the actual data bits show up.                
      [bitwalker] "r" (0x80)                      // Alocate a register to hold a bit that we will walk down though the color byte

    );
                                  
    // Note that the inter-bit gap can be as long as you want as long as it doesn't exceed the reset timeout (which is A long time)
    
} 


// Just wait long enough without sending any bots to cause the pixels to latch and display the last sent frame

void show() {
  delayMicroseconds( (RES / 1000UL) + 1);       // Round up since the delay must be _at_least_ this long (too short might not work, too long not a problem)
}


// Send 3 bytes of color data (R,G,B) for a signle pixel down all the connected stringsat the same time
// A 1 bit in "row" means send the color, a 0 bit means send black. 

static inline void sendRowRGB( uint8_t row ,  uint8_t r,  uint8_t g,  uint8_t b ) {

  sendBitx8( row , g , onBits);    // WS2812 takes colors in GRB order
  sendBitx8( row , r , onBits);    // WS2812 takes colors in GRB order
  sendBitx8( row , b , onBits);    // WS2812 takes colors in GRB order
  
}

// Turn off all pixels

static inline void clear() {

  cli();
  for( unsigned int i=0; i< PIXELS; i++ ) {

    sendRowRGB( 0 , 0 , 0 , 0 );
  }
  sei();
  show(); 
}

// Turn off all pixels

static inline void clearIntsOff() {

  for( unsigned int i=0; i< PIXELS; i++ ) {

    sendRowRGB( 0 , 0 , 0 , 0 );
  }
  show(); 
}

// This nice 5x7 font from here...
// http://sunge.awardspace.com/glcd-sd/node4.html

// Font details:
// 1) Each char is fixed 5x7 pixels. 
// 2) Each byte is one column.
// 3) Columns are left to right order, leftmost byte is leftmost column of pixels.
// 4) Each column is 8 bits high.
// 5) Bit #7 is top line of char, Bit #1 is bottom.
// 6) Bit #0 is always 0, becuase this pin is used as serial input and setting to 1 would enable the pull-up.

// defines ascii characters 0x20-0x7F (32-127)
// PROGMEM after variable name as per https://www.arduino.cc/en/Reference/PROGMEM

#define FONT_WIDTH 5      
#define INTERCHAR_SPACE 1
#define ASCII_OFFSET 0x20    // ASSCI code of 1st char in font array

const uint8_t Font5x7[] PROGMEM = {
0x00,0x00,0x00,0x00,0x00,//  
0x00,0x00,0xfa,0x00,0x00,// !
0x00,0xe0,0x00,0xe0,0x00,// "
0x28,0xfe,0x28,0xfe,0x28,// #
0x24,0x54,0xfe,0x54,0x48,// $
0xc4,0xc8,0x10,0x26,0x46,// %
0x6c,0x92,0xaa,0x44,0x0a,// &
0x00,0xa0,0xc0,0x00,0x00,// '
0x00,0x38,0x44,0x82,0x00,// (
0x00,0x82,0x44,0x38,0x00,// )
0x10,0x54,0x38,0x54,0x10,// *
0x10,0x10,0x7c,0x10,0x10,// +
0x00,0x0a,0x0c,0x00,0x00,// ,
0x10,0x10,0x10,0x10,0x10,// -
0x00,0x06,0x06,0x00,0x00,// .
0x04,0x08,0x10,0x20,0x40,// /
0x7c,0x8a,0x92,0xa2,0x7c,// 0
0x00,0x42,0xfe,0x02,0x00,// 1
0x42,0x86,0x8a,0x92,0x62,// 2
0x84,0x82,0xa2,0xd2,0x8c,// 3
0x18,0x28,0x48,0xfe,0x08,// 4
0xe4,0xa2,0xa2,0xa2,0x9c,// 5
0x3c,0x52,0x92,0x92,0x0c,// 6
0x80,0x8e,0x90,0xa0,0xc0,// 7
0x6c,0x92,0x92,0x92,0x6c,// 8
0x60,0x92,0x92,0x94,0x78,// 9
0x00,0x6c,0x6c,0x00,0x00,// :
0x00,0x6a,0x6c,0x00,0x00,// ;
0x00,0x10,0x28,0x44,0x82,// <
0x28,0x28,0x28,0x28,0x28,// =
0x82,0x44,0x28,0x10,0x00,// >
0x40,0x80,0x8a,0x90,0x60,// ?
0x4c,0x92,0x9e,0x82,0x7c,// @
0x7e,0x88,0x88,0x88,0x7e,// A
0xfe,0x92,0x92,0x92,0x6c,// B
0x7c,0x82,0x82,0x82,0x44,// C
0xfe,0x82,0x82,0x44,0x38,// D
0xfe,0x92,0x92,0x92,0x82,// E
0xfe,0x90,0x90,0x80,0x80,// F
0x7c,0x82,0x82,0x8a,0x4c,// G
0xfe,0x10,0x10,0x10,0xfe,// H
0x00,0x82,0xfe,0x82,0x00,// I
0x04,0x02,0x82,0xfc,0x80,// J
0xfe,0x10,0x28,0x44,0x82,// K
0xfe,0x02,0x02,0x02,0x02,// L
0xfe,0x40,0x20,0x40,0xfe,// M
0xfe,0x20,0x10,0x08,0xfe,// N
0x7c,0x82,0x82,0x82,0x7c,// O
0xfe,0x90,0x90,0x90,0x60,// P
0x7c,0x82,0x8a,0x84,0x7a,// Q
0xfe,0x90,0x98,0x94,0x62,// R
0x62,0x92,0x92,0x92,0x8c,// S
0x80,0x80,0xfe,0x80,0x80,// T
0xfc,0x02,0x02,0x02,0xfc,// U
0xf8,0x04,0x02,0x04,0xf8,// V
0xfe,0x04,0x18,0x04,0xfe,// W
0xc6,0x28,0x10,0x28,0xc6,// X
0xc0,0x20,0x1e,0x20,0xc0,// Y
0x86,0x8a,0x92,0xa2,0xc2,// Z
0x00,0x00,0xfe,0x82,0x82,// [
0x40,0x20,0x10,0x08,0x04,// (backslash)
0x82,0x82,0xfe,0x00,0x00,// ]
0x20,0x40,0x80,0x40,0x20,// ^
0x02,0x02,0x02,0x02,0x02,// _
0x00,0x80,0x40,0x20,0x00,// `
0x04,0x2a,0x2a,0x2a,0x1e,// a
0xfe,0x12,0x22,0x22,0x1c,// b
0x1c,0x22,0x22,0x22,0x04,// c
0x1c,0x22,0x22,0x12,0xfe,// d
0x1c,0x2a,0x2a,0x2a,0x18,// e
0x10,0x7e,0x90,0x80,0x40,// f
0x10,0x28,0x2a,0x2a,0x3c,// g
0xfe,0x10,0x20,0x20,0x1e,// h
0x00,0x22,0xbe,0x02,0x00,// i
0x04,0x02,0x22,0xbc,0x00,// j
0x00,0xfe,0x08,0x14,0x22,// k
0x00,0x82,0xfe,0x02,0x00,// l
0x3e,0x20,0x18,0x20,0x1e,// m
0x3e,0x10,0x20,0x20,0x1e,// n
0x1c,0x22,0x22,0x22,0x1c,// o
0x3e,0x28,0x28,0x28,0x10,// p
0x10,0x28,0x28,0x18,0x3e,// q
0x3e,0x10,0x20,0x20,0x10,// r
0x12,0x2a,0x2a,0x2a,0x04,// s
0x20,0xfc,0x22,0x02,0x04,// t
0x3c,0x02,0x02,0x04,0x3e,// u
0x38,0x04,0x02,0x04,0x38,// v
0x3c,0x02,0x0c,0x02,0x3c,// w
0x22,0x14,0x08,0x14,0x22,// x
0x30,0x0a,0x0a,0x0a,0x3c,// y
0x22,0x26,0x2a,0x32,0x22,// z
0x00,0x10,0x6c,0x82,0x00,// {
0x00,0x00,0xfe,0x00,0x00,// |
0x00,0x82,0x6c,0x10,0x00,// }
0x10,0x10,0x54,0x38,0x10,// ~
0x10,0x38,0x54,0x10,0x10,// 
};

// Send the pixels to form the specified char, not including interchar space
// skip is the number of pixels to skip at the begining to enable sub-char smooth scrolling

// TODO: Subtract the offset from the char before starting the send sequence to save time if nessisary
// TODO: Also could pad the begining of the font table to aovid the offset subtraction at the cost of 20*8 bytes of progmem
// TODO: Could pad all chars out to 8 bytes wide to turn the the multiply by FONT_WIDTH into a shift 

static inline void sendChar( uint8_t c ,  uint8_t skip , uint8_t r,  uint8_t g,  uint8_t b ) {

  const uint8_t *charbase = Font5x7 + (( c -' ')* FONT_WIDTH ) ; 

  uint8_t col=FONT_WIDTH; 

  while (skip--) {
      charbase++;
      col--;    
  }
  
  while (col--) {
      sendRowRGB( pgm_read_byte_near( charbase++ ) , r , g , b );
  }    

  // TODO: FLexible interchar spacing

  sendRowRGB( 0 , r , g , b );    // Interchar space
  
}


// Show the passed string. The last letter of the string will be in the rightmost pixels of the display.
// Skip is how many cols of the 1st char to skip for smooth scrolling

static inline void sendString( const char *s , uint8_t skip ,  const uint8_t r,  const uint8_t g,  const uint8_t b ) {

  unsigned int l=PIXELS/(FONT_WIDTH+INTERCHAR_SPACE); 

  sendChar( *s , skip ,  r , g , b );   // First char is special case becuase it can be stepped for smooth scrolling
  
  while ( *(++s) && l--) {

    sendChar( *s , 0,  r , g , b );

  }
}

// A nice arcade font from...
// http://jared.geek.nz/2014/jan/custom-fonts-for-microcontrollers

#define ALTFONT_WIDTH 8

const uint8_t altfont[] PROGMEM = {
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,//  
  0x06,0x06,0x30,0x30,0x60,0xc0,0xc0,0x00,// !
  0xe0,0xe0,0x00,0xe0,0xe0,0x00,0x00,0x00,// "
  0x28,0xfe,0xfe,0x28,0xfe,0xfe,0x28,0x00,// #
  0xf6,0xf6,0xd6,0xd6,0xd6,0xde,0xde,0x00,// $
  0xc6,0xce,0x1c,0x38,0x70,0xe6,0xc6,0x00,// %
  0xfe,0xfe,0xd6,0xc6,0x16,0x1e,0x1e,0x00,// &
  0xe0,0xe0,0x00,0x00,0x00,0x00,0x00,0x00,// '
  0xfe,0xfe,0x00,0x00,0x00,0x00,0x00,0x00,// (
  0x00,0xfe,0xfe,0x00,0x00,0x00,0x00,0x00,// )
  0x6c,0x10,0xfe,0xfe,0xfe,0x10,0x6c,0x00,// *
  0x10,0x10,0x7c,0x10,0x10,0x00,0x00,0x00,// +
  0x06,0x06,0x00,0x00,0x00,0x00,0x00,0x00,// ,
  0x10,0x10,0x10,0x10,0x10,0x00,0x00,0x00,// -
  0x06,0x06,0x00,0x00,0x00,0x00,0x00,0x00,// .
  0x0e,0x38,0xe0,0x00,0x00,0x00,0x00,0x00,// /
  0xfe,0xfe,0xc6,0xc6,0xc6,0xfe,0xfe,0x00,// 0
  0x06,0x66,0x66,0xfe,0xfe,0x06,0x06,0x00,// 1
  0xde,0xde,0xd6,0xd6,0xd6,0xf6,0xf6,0x00,// 2
  0xc6,0xc6,0xd6,0xd6,0xd6,0xfe,0xfe,0x00,// 3
  0xf8,0xf8,0x18,0x18,0x18,0x7e,0x7e,0x00,// 4
  0xf6,0xf6,0xd6,0xd6,0xd6,0xde,0xde,0x00,// 5
  0xfe,0xfe,0x36,0x36,0x36,0x3e,0x3e,0x00,// 6
  0xc2,0xc6,0xce,0xdc,0xf8,0xf0,0xe0,0x00,// 7
  0xfe,0xfe,0xd6,0xd6,0xd6,0xfe,0xfe,0x00,// 8
  0xf8,0xf8,0xd8,0xd8,0xd8,0xfe,0xfe,0x00,// 9
  0x36,0x36,0x00,0x00,0x00,0x00,0x00,0x00,// :
  0x36,0x36,0x00,0x00,0x00,0x00,0x00,0x00,// ;
  0x10,0x28,0x44,0x44,0x00,0x00,0x00,0x00,// <
  0x28,0x28,0x28,0x28,0x28,0x00,0x00,0x00,// =
  0x44,0x44,0x28,0x10,0x00,0x00,0x00,0x00,// >
  0xc0,0xc0,0xda,0xda,0xd0,0xf0,0xf0,0x00,// ?
  0xfe,0xfe,0xc6,0xf6,0xd6,0xf6,0xf6,0x00,// @
  0xfe,0xfe,0xd8,0xd8,0xd8,0xfe,0xfe,0x00,// A
  0xfe,0xfe,0xd6,0xd6,0xf6,0x7e,0x3e,0x00,// B
  0xfe,0xfe,0xc6,0xc6,0xc6,0xc6,0xc6,0x00,// C
  0xfe,0xfe,0xc6,0xc6,0xe6,0x7e,0x3e,0x00,// D
  0xfe,0xfe,0xd6,0xd6,0xd6,0xd6,0xd6,0x00,// E
  0xfe,0xfe,0xd0,0xd0,0xd0,0xc0,0xc0,0x00,// F
  0xfe,0xfe,0xc6,0xc6,0xd6,0xde,0xde,0x00,// G
  0xfe,0xfe,0x18,0x18,0x18,0xfe,0xfe,0x00,// H
  0xc6,0xc6,0xfe,0xfe,0xc6,0xc6,0xc6,0x00,// I
  0x06,0x06,0x06,0x06,0x06,0xfe,0xfc,0x00,// J
  0xfe,0xfe,0x18,0x18,0x78,0xfe,0x9e,0x00,// K
  0xfe,0xfe,0x06,0x06,0x06,0x06,0x06,0x00,// L
  0xfe,0xfe,0xc0,0x60,0xc0,0xfe,0xfe,0x00,// M
  0xfe,0xfe,0x70,0x38,0x1c,0xfe,0xfe,0x00,// N
  0xfe,0xfe,0xc6,0xc6,0xc6,0xfe,0xfe,0x00,// O
  0xfe,0xfe,0xd8,0xd8,0xd8,0xf8,0xf8,0x00,// P
  0xfe,0xfe,0xc6,0xce,0xce,0xfe,0xfe,0x00,// Q
  0xfe,0xfe,0xd8,0xdc,0xde,0xfe,0xfa,0x00,// R
  0xf6,0xf6,0xd6,0xd6,0xd6,0xde,0xde,0x00,// S
  0xc0,0xc0,0xfe,0xfe,0xc0,0xc0,0xc0,0x00,// T
  0xfe,0xfe,0x06,0x06,0x06,0xfe,0xfe,0x00,// U
  0xf8,0xfc,0x0e,0x06,0x0e,0xfc,0xf8,0x00,// V
  0xfc,0xfe,0x06,0x0c,0x06,0xfe,0xfc,0x00,// W
  0xee,0xfe,0x38,0x10,0x38,0xfe,0xee,0x00,// X
  0xe0,0xf0,0x3e,0x1e,0x3e,0xf0,0xe0,0x00,// Y
  0xce,0xde,0xd6,0xd6,0xd6,0xf6,0xe6,0x00,// Z
  0xfe,0xfe,0x00,0x00,0x00,0x00,0x00,0x00,// [
  0xe0,0x38,0x0e,0x00,0x00,0x00,0x00,0x00,// \
  0x00,0xfe,0xfe,0x00,0x00,0x00,0x00,0x00,// ]
  0x00,0xfe,0x02,0xfe,0x00,0x00,0x00,0x00,// ^
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,// _
  0x00,0xfe,0x02,0xfe,0x00,0x00,0x00,0x00,// `
  0xfe,0xfe,0xd8,0xd8,0xd8,0xfe,0xfe,0x00,// a
  0xfe,0xfe,0xd6,0xd6,0xf6,0x7e,0x3e,0x00,// b
  0xfe,0xfe,0xc6,0xc6,0xc6,0xc6,0xc6,0x00,// c
  0xfe,0xfe,0xc6,0xc6,0xe6,0x7e,0x3e,0x00,// d
  0xfe,0xfe,0xd6,0xd6,0xd6,0xd6,0xd6,0x00,// e
  0xfe,0xfe,0xd0,0xd0,0xd0,0xc0,0xc0,0x00,// f
  0xfe,0xfe,0xc6,0xc6,0xd6,0xde,0xde,0x00,// g
  0xfe,0xfe,0x18,0x18,0x18,0xfe,0xfe,0x00,// h
  0xc6,0xc6,0xfe,0xfe,0xc6,0xc6,0xc6,0x00,// i
  0x06,0x06,0x06,0x06,0x06,0xfe,0xfc,0x00,// j
  0xfe,0xfe,0x18,0x18,0x78,0xfe,0x9e,0x00,// k
  0xfe,0xfe,0x06,0x06,0x06,0x06,0x06,0x00,// l
  0xfe,0xfe,0xc0,0x60,0xc0,0xfe,0xfe,0x00,// m
  0xfe,0xfe,0x70,0x38,0x1c,0xfe,0xfe,0x00,// n
  0xfe,0xfe,0xc6,0xc6,0xc6,0xfe,0xfe,0x00,// o
  0xfe,0xfe,0xd8,0xd8,0xd8,0xf8,0xf8,0x00,// p
  0xfe,0xfe,0xc6,0xce,0xce,0xfe,0xfe,0x00,// q
  0xfe,0xfe,0xd8,0xdc,0xde,0xfe,0xfa,0x00,// r
  0xf6,0xf6,0xd6,0xd6,0xd6,0xde,0xde,0x00,// s
  0xc0,0xc0,0xfe,0xfe,0xc0,0xc0,0xc0,0x00,// t
  0xfe,0xfe,0x06,0x06,0x06,0xfe,0xfe,0x00,// u
  0xf8,0xfc,0x0e,0x06,0x0e,0xfc,0xf8,0x00,// v
  0xfc,0xfe,0x06,0x0c,0x06,0xfe,0xfc,0x00,// w
  0xee,0xfe,0x38,0x10,0x38,0xfe,0xee,0x00,// x
  0xe0,0xf0,0x3e,0x1e,0x3e,0xf0,0xe0,0x00,// y
  0xce,0xde,0xd6,0xd6,0xd6,0xf6,0xe6,0x00,// z
  0x38,0xfe,0xfe,0x00,0x00,0x00,0x00,0x00,// {
  0xfe,0x00,0x00,0x00,0x00,0x00,0x00,0x00,// |
  0x00,0xfe,0xfe,0x38,0x00,0x00,0x00,0x00,// }
  0x00,0xfe,0x02,0xfe,0x00,0x00,0x00,0x00,// ~
};


// Keep track of where we are in the color cycle between chars
static uint8_t altbright =0; 

// Send a char with a column-based color cycle
static inline void sendCharAlt( uint8_t c ) {

  const uint8_t *charbase = altfont + (( c -' ')* ALTFONT_WIDTH) ; 

  uint8_t col=ALTFONT_WIDTH; 
  
  while (col--) {

      sendRowRGB(  pgm_read_byte_near( charbase++ ) , altbright , 0 , 0x80  );
   
      altbright+=10;
  }

  sendRowRGB( 0 ,0, 0 , 0 );
  altbright+=10;

}

// Show the passed string with the arcade font and a nice vertical color cycle effect

static inline void sendStringAlt( const char *s  ) {

  unsigned int l=PIXELS/(ALTFONT_WIDTH+INTERCHAR_SPACE); 

  while ( l--) {

    char c;  

    c =   *s++;

    if (!c) break;
    
    sendCharAlt( c  );

  }
}



// Set the specified pins up as digital out

void ledsetup() {

  PIXEL_DDR |= onBits;   // Set all used pins to output

}


void clearInterrupt (byte pin) {

      PCIFR  |= bit (digitalPinToPCICRbit(pin)); // clear any outstanding interrupt
      
}
void pciSetup(byte pin)
{
    *digitalPinToPCMSK(pin) |= bit (digitalPinToPCMSKbit(pin));  // enable pin
    PCIFR  |= bit (digitalPinToPCICRbit(pin)); // clear any outstanding interrupt
    PCICR  |= bit (digitalPinToPCICRbit(pin)); // enable interrupt for the group
} 


// https://learn.adafruit.com/led-tricks-gamma-correction/the-quick-fix

const uint8_t PROGMEM gamma[] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,
    1,  1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,
    2,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  5,  5,  5,
    5,  6,  6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  9,  9,  9, 10,
   10, 10, 11, 11, 11, 12, 12, 13, 13, 13, 14, 14, 15, 15, 16, 16,
   17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 24, 24, 25,
   25, 26, 27, 27, 28, 29, 29, 30, 31, 32, 32, 33, 34, 35, 35, 36,
   37, 38, 39, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 50,
   51, 52, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 66, 67, 68,
   69, 70, 72, 73, 74, 75, 77, 78, 79, 81, 82, 83, 85, 86, 87, 89,
   90, 92, 93, 95, 96, 98, 99,101,102,104,105,107,109,110,112,114,
  115,117,119,120,122,124,126,127,129,131,133,135,137,138,140,142,
  144,146,148,150,152,154,156,158,160,162,164,167,169,171,173,175,
  177,180,182,184,186,189,191,193,196,198,200,203,205,208,210,213,
  215,218,220,223,225,228,231,233,236,239,241,244,247,249,252,255 };

// Map 0-255 visual brightness to 0-255 LED brightness 
#define GAMMA(x) (pgm_read_byte(&gamma[x]))


void showcountdown() {

  // Start sequence.....

  const char *countdownstr = "            FIRE ROLLERBALL IN ";

  unsigned int count = 600; 

  clear();
  while (count>0) {

    count--;

    uint8_t digit1 = count/100;
    uint8_t digit2 = (count - (digit1*100)) / 10;
    uint8_t digit3 = (count - (digit1*100) - (digit2*10));

    uint8_t char1 = digit1 + '0';
    uint8_t char2 = digit2 + '0';
    uint8_t char3 = digit3 + '0';
    
    uint8_t brightness = GAMMA( ((count % 100) * 256)  / 100 );

    cli();
    sendString( countdownstr , 1 , brightness , brightness , brightness );      
 
    sendRowRGB( 0x00 , 0 , 0 , 0xff );
 
  //  sendChar( '0' , 0 , 0x80, 0 , 0 );
    
    sendChar( char1 , 0 , 0x80, 0 , 0 );
    sendChar( '.'   , 0 , 0x80, 0 , 0  );
    sendChar( char2 , 0 , 0x80, 0 , 0  );
    sendChar( char3 , 0 , 0x80, 0 , 0 );
    
    sei();
    show();
  }

  count = 100;

  // One last farewell blink

  while (count>0) {

    count--;

    
    uint8_t brightness = GAMMA( ((count % 100) * 256)  / 100 );

    cli();
    sendString( countdownstr , 1 , brightness , brightness , brightness );      

    sendRowRGB( 0x00 , 0 , 0 , 0xff );   // We need to quickly send a blank byte just to keep from missing our deadlne.
    sendChar( '0' , 0 , brightness, 0 , 0 );
    sendChar( '.' , 0 , brightness, 0 , 0 );
    sendChar( '0' , 0 , brightness, 0 , 0 );
    sendChar( '0' , 0 , brightness, 0 , 0 );
     
    
    sei();
    show();
  }
  
  
}

// Power 0-10

void ringBell( unsigned char power ) {

  pinMode( BELL_PIN , OUTPUT );
  digitalWrite( BELL_PIN , 1 );
  
  //delay(5);  //10500 is loudest, 5500 softest

  _delay_us( 5000 );

  for(int i=0;i<power;i++) {
    _delay_us(500);
  }

  digitalWrite( BELL_PIN , 0 );

}


void bellCycle() {
  
  for(int i=0;i<=10;i++) {
    ringBell(i);
    delay(100);
  }
  for(int i=10;i>0;i--) {
    ringBell(i);
    delay(100);
  }

}


// Bounce a puck with gravity

#define LEDS_PER_METER 96

#define MAX_FREQ 200      // Max update speed in hertz. Based on the fact that NeoPixels has an internal PWM clock for colors so you need to stay on long enough for thge pixel to actually light up

#define MIN_FRAME_TIME (1.0/MAX_FREQ)

#define SHOW_DELAY  (0.000050)    // It takes about 50us to show the string

#define BOTTOM_ROW  3            // The first visibe row 

#define BELL_ROW (300)

// All compuations done in meters

// Bounce is run from an ISR so interrupt assumed to be off (otherwise timing wrong, and WS2812Bs could see a reset pulse)

void bounce(float v0) {  // v0= initial speed meters per second up
  
  static float g=-8.5;   // Gravity accelerating down 

  float t=0;        // Time
 // float x=0;        // Vertical position in meters (up is +)
  float v=v0;       // Velocity 
  float f = 1.2;    // Frictional force - alwasy acts in oposite direction of motion TODO

  int puckRow_prev = 0;     // Location of previous starting s so we can fill inbtween
  int puckRow_prevprev = 0; // Location of dopo previous starting so we can erase behind us

  while (1) {

    float x =  ( v0 * t ) + ( 0.5 * ( g * t * t ) );

    if (x<0) break;  // dont fall though the ground  

    int puckRow = (x * LEDS_PER_METER) + BOTTOM_ROW;

    if (puckRow>=BELL_ROW) {   // Hit bell?
      ringBell(9);
    }

    int rowsSent=0;               // Total number of rows sent to we can calculate how long it took 

    while (rowsSent<BOTTOM_ROW) {           // Just use up the invisible pixels
        sendRowRGB( 0x00, 0x00, 0x00, 0x00 );  
        rowsSent++; 
    }

    if (puckRow>puckRow_prev) {  // going up 

        // Note that since we are going up, we do not need to explicitly erase our tail. 

        // Send padding blank space to work our way up to where the puck goes

        
        while (rowsSent<=puckRow_prev) {        // Get us up to where the previous frame ended
          
          sendRowRGB( 0x00, 0x00, 0x00, 0x00 );  
    
          rowsSent++;
          
        }
    
        while (rowsSent<=puckRow) {
        
          sendRowRGB( 0xff , 0x00, 0x00, 0x30 );   // The actual blured puck  
          rowsSent++;
     
        }
         
    } else if (puckRow<puckRow_prev) {                    // going down

        // Send padding blank space to work our way up to where the puck goes
        
        while (rowsSent<puckRow) {        // Get us up to where the previous frame ended
          
          sendRowRGB( 0x00, 0x00, 0x00, 0x00 );  
    
          rowsSent++;
        }
    
        while (rowsSent<puckRow_prev) {
        
          sendRowRGB( 0xff , 0x00, 0x00, 0x30 );   // The actual blured puck  
          rowsSent++;
     
        }


        while (rowsSent<=puckRow_prevprev) {
        
          sendRowRGB( 0xff , 0x00, 0x00, 0x00 );   // Blank space behind puck  
          rowsSent++;
     
        }


    } else {    // Puck not moved since last frame


        // Send padding blank space to work our way up to where the puck goes
        
        while (rowsSent<puckRow) {        // Get us up to where the previous frame ended
          
          sendRowRGB( 0x00, 0x00, 0x00, 0x00 );  
    
          rowsSent++;
        }
    
        while (rowsSent<=puckRow) {       
          sendRowRGB( 0xff , 0x00, 0x00, 0x30 );   // The actual blured puck  
          rowsSent++;
        }

        while (rowsSent<=puckRow_prevprev) {
        
          sendRowRGB( 0xff , 0x00, 0x00, 0x00 );   // Blank space behind puck  
          rowsSent++;
     
        }
      
    }

  
    float dt = (0.000002 * 8 * 3 * rowsSent ) + SHOW_DELAY;    // It takes about 2us per bit - 8 bits per byte - 3 bytes per row

    if ( dt < MIN_FRAME_TIME)  {      // Fastest the Neopixels can actually show is about 200Hz

      unsigned int delaytime_ms =  ((MIN_FRAME_TIME - dt) * 1000);    // Pause for the difference of how long it took and 200Hz, and convert to millis for delay(). This will always be long enough to activate the show() delay
      
      while (delaytime_ms--) {      // Only good way to make a dynamic delay without INTs enabled..
        _delay_us(800);
      }
        
      dt = MIN_FRAME_TIME;
      
    } else {        // Just transmitting the bits to the string took longer than 200hz so no extra delay needed except the show time

      show();
    }
    
    t = t + dt;

    puckRow_prevprev= puckRow_prev;
    puckRow_prev = puckRow;

   // _delay_ms(100);
    
  }
}

#define FIRST_GROUND_PIN A0
#define FIRST_TRIGGER_PIN A1

#define SECOND_GROUND_PIN A2
#define SECOND_TRIGGER_PIN A3

// Interrupt code from: http://playground.arduino.cc/Main/PinChangeInterrupt

// This is called when the first pin is triggered

ISR (PCINT1_vect) // handle pin change interrupt for A0 to A5 here
{

   // Show pck for instant feedback while we get speed
   for(int i=0; i<BOTTOM_ROW;i++) {
      sendRowRGB( 0xff , 0x00, 0x00, 0x00);  // Starlight blue
    }
    
    sendRowRGB( 61 , 0x00, 0x00, 0x10);  // Starlight blue
    show();
      
     unsigned d=1;     
     while ( d && digitalRead( SECOND_TRIGGER_PIN) ) d++;
     
     #ifdef DEBUG
      Serial.println(d);
    #endif
    if ( d>0 ) {  //d==0 means took to long, somcething wrong
      
      // ranges:
      // d : 600=fast, 5000=slow
      // v : 3 = slow, 10=fast

      if (d<901) d=901;   
      if (d>5000) d=5000;

      // v = x/t. x- constant (the distance between the triggers) 

     float t_normal =  (d-900)/(5000.0-900.0);  // time normalized 0-1 where 0 is fastest and 1 is slowest

     t_normal = sqrt(t_normal); 

     //t_normal = t_normal * t_normal;
     float v =  9 - ( 9 * t_normal );  
   //Serial.print("v"); Serial.println(v);

     
     while (v>1.0) {   // hitting bottom is 70% elastic
        bounce(v);
        v*=0.75;
      }
    }


    show();   // Make sure last frame had time to latch
    // Show a puck on the bottom row for as long as the leaver is pressed...
    clearIntsOff();

    for(int i=0; i<BOTTOM_ROW;i++) {
      sendRowRGB( 0xff , 0x00, 0x00, 0x00);  // Starlight blue
    }
    
    sendRowRGB( 0xff , 0x00, 0x00, 0x30);  // Starlight blue
    show();
    
    //Serial.print("v="); Serial.println(v);
   // Serial.println("Waiting for return");

    while (!digitalRead( SECOND_TRIGGER_PIN ) );
 
     for(int i=0; i<BOTTOM_ROW;i++) {
      sendRowRGB( 0xff , 0x00, 0x00, 0x00);  // Starlight blue
    }
    
    sendRowRGB( 61 , 0x00, 0x00, 0x10);  // Starlight blue
    show();
    
    //Serial.print("v="); Serial.println(v);
   // Serial.println("Waiting for return");

    while (!digitalRead( FIRST_TRIGGER_PIN ) );
    clearIntsOff();
   
    
    _delay_ms(250);     // Debounce (litterally!)
    
    clearInterrupt(FIRST_TRIGGER_PIN);
    sei();
     
}


void showstarfield() {

  const uint8_t field = 40;       // Good size for a field, must be less than 256 so counters fit in a byte
 
  uint8_t sectors = (PIXELS / field);      // Repeating sectors makes for more stars and faster update

  for(unsigned int i=0; i<300;i++) {

    unsigned int r = random( PIXELS * 8 );   // Random slow, so grab one big number and we will break it down. 

    unsigned int x = r /8; 
    uint8_t y = r & 0x07;                // We use 7 rows
    uint8_t bitmask = (2<<y);           // Start at bit #1 since we enver use the bottom bit

    cli();    

      unsigned int l=x; 
    
      while (l--) {
           sendRowRGB( 0 , 0x00, 0x00, 0x00);          
      }
        
      sendRowRGB( bitmask , 0x40, 0x40, 0xff);  // Starlight blue

      l = PIXELS-x;
      
      while (l--) {
           sendRowRGB( 0 , 0x00, 0x00, 0x00);          
      }      
        
          

    sei();

   // show(); // Not needed - random is alwasy slow enough to trigger a reset
       
  }

}

static inline void sendIcon( const uint8_t *fontbase , uint8_t which, int8_t shift , uint8_t width , uint8_t r , uint8_t g , uint8_t b ) {

  const uint8_t *charbase = fontbase + (which*width);

  if (shift <0) {

        uint8_t shiftabs = -1 * shift;
  
        while (width--) {

          uint8_t row = pgm_read_byte_near( charbase++ );
    
          sendRowRGB(  row << shiftabs , r , g , b );
     
    }

  } else {


    
    while (width--) {
  
        sendRowRGB(  (pgm_read_byte_near( charbase++ ) >> shift) & onBits , r , g , b );
     
    }

  }

}


#define ENIMIES_WIDTH 12

const uint8_t enimies[] PROGMEM = {

  0x70,0xf4,0xfe,0xda,0xd8,0xf4,0xf4,0xd8,0xda,0xfe,0xf4,0x70, // Enimie 1 - open
  0x72,0xf2,0xf4,0xdc,0xd8,0xf4,0xf4,0xd8,0xdc,0xf4,0xf2,0x72, // Enimie 1 - close
  0x1c,0x30,0x7c,0xda,0x7a,0x78,0x7a,0xda,0x7c,0x30,0x1c,0x00, // Enimie 2 - open
  0xf0,0x3a,0x7c,0xd8,0x78,0x78,0x78,0xd8,0x7c,0x3a,0xf0,0x00, // Enimie 2 - closed
  0x92,0x54,0x10,0x82,0x44,0x00,0x00,0x44,0x82,0x10,0x54,0x92, // Explosion
};


void showinvaderwipe( uint8_t which , const char *pointsStr , uint8_t r , uint8_t g, uint8_t b) {

  clear();
  delay(500);

  for( uint8_t p = 0 ; p<strlen( pointsStr) ; p++ ) {

      cli();
      sendStringAlt( "                " );
      sendIcon( enimies , which , 0 , ENIMIES_WIDTH , r , g , b );
      for(uint8_t i=0; i<=p ;i++ ){        
        sendChar( *(pointsStr+i) , 0 ,r>>2 , g>>2 , b>>2 );     // Dim text slightly
      }
      sei();
      delay(100);
    
  }

  delay(1500);

  
}

void showinvaders() {
  
  showinvaderwipe(   3 ,  " = 20 POINTS" , 0x80 , 0x80 ,0x80 );
  showinvaderwipe(   1 ,  " = 10 POINTS" , 0x00 , 0xff ,0x00 );

  uint8_t acount = PIXELS/(ENIMIES_WIDTH+FONT_WIDTH);      // How many aliens do we have room for?  

  for( int8_t row = -4 ; row < 6 ; row++ ) {     // Walk down the rows

    //  Walk them 6 pixels per row 

    // ALternate direction on each row

    uint8_t s,e,d;

    if (row & 1) {

      s=1; e=8; d=1;
      
    } else {

      s=7; e=0; d=-1;
            
    }

    
    for( char p = s ; p!=e ; p +=d ) {
   
        // Now slowly move aliens
  
        // work our way though the alines moving each one to the left
        
      
          cli();
    
          // Start with margin

          uint8_t margin = p ;

          while (margin--) {
            sendRowRGB( 0 , 0x00 , 0x00 , 0x00 );
          }

          for( uint8_t l=0; l<acount ; l++ ) {

            sendIcon( enimies , p&1 , row, ENIMIES_WIDTH , 0xFF , 0xFF , 0xFF );
            sendChar( ' ' , 0 , 0x00 , 0x00 , 0x00 ) ; // No over crowding
            
          }
  
          sei();
          delay(70);
            
        }
   }
     // delay(200);            
  
}





void showallyourbase() {
  
  const char *allyourbase = "CAT: ALL YOUR BASE ARE BELONG TO US !!!" ;

  clear();
  for(unsigned int slide=10000; slide ; slide-=10 ) {
      altbright = (slide & 0xff);
      cli();      
      sendChar(' ' , 0 , 0 , 0 , 0 );
      sendStringAlt( allyourbase);
      sei();
      show();
  }
  
}

  

#define JAB_MAX_BRIGHTNESS 0xff
#define JAB_MIN_BRIGHTNESS 0x00
#define JAB_STEPS (JAB_MAX_BRIGHTNESS-JAB_MIN_BRIGHTNESS)

void showjabber() {

  const char *m = 
          
      "Twas brillig, and the slithy toves "
            "Did gyre and gimble in the wabe: "
      "All mimsy were the borogoves, "
            "And the mome raths outgrabe. "
      
      "Beware the Jabberwock, my son! "
            "The jaws that bite, the claws that catch! "
      "Beware the Jubjub bird, and shun "      
            "The frumious Bandersnatch! "
      
      "He took his vorpal sword in hand; "
            "Long time the manxome foe he sought- "
      "So rested he by the Tumtum tree "
            "And stood awhile in thought. "
      
      "And, as in uffish thought he stood, "
            "The Jabberwock, with eyes of flame, "
      "Came whiffling through the tulgey wood, "      
            "And burbled as it came! "
      
      "One, two! One, two! And through and through "
            "The vorpal blade went snicker-snack! "
      "He left it dead, and with its head "
            "He went galumphing back. "
      
      "And hast thou slain the Jabberwock? "
            "Come to my arms, my beamish boy! "
      "O frabjous day! Callooh! Callay! "
            "He chortled in his joy. "
      
      "Twas brillig, and the slithy toves "
            "Did gyre and gimble in the wabe: "
      "All mimsy were the borogoves, "
            "And the mome raths outgrabe."  
      
            ;

  // Text foreground color cycle effect
  uint8_t sector =0;
  uint8_t step=0;
    
  while (*m) {      

      if (step== JAB_STEPS) {
        step=0;
        sector++;
        if (sector==3) {
          sector=0;
        }
      } else {
        step++;
      }

      uint8_t rampup = JAB_MIN_BRIGHTNESS + step;
      uint8_t rampdown = JAB_MIN_BRIGHTNESS + (JAB_STEPS - step); 
      
      uint8_t r,g,b;
      
      switch( sector ) {
        case 0: 
            r=rampup;
            g=rampdown;
            b=JAB_MIN_BRIGHTNESS;
            break;
         case 1:
            r=rampdown;
            g=JAB_MIN_BRIGHTNESS;
            b=rampup;
            break;
         case 2:
            r=JAB_MIN_BRIGHTNESS;
            g=rampup;
            b=rampdown;
            break;
      
      };

      for( uint8_t step=0; step<FONT_WIDTH+INTERCHAR_SPACE  ; step++ ) {   // step though each column of the 1st char for smooth scrolling


       cli();

       sendString( m , step , r, g, b );
      
       sei();

       PORTB|=0x01;      
       delay(1);
       PORTB&=~0x01;

      }

    m++;

  }

}

void setup() {
    
  ledsetup();  

  pinMode( A0 , FIRST_GROUND_PIN );
  digitalWrite( FIRST_GROUND_PIN , 0 ); // Drive low to be the gronud for the sensor

  pinMode( FIRST_TRIGGER_PIN , INPUT_PULLUP );

  pciSetup(FIRST_TRIGGER_PIN);    // Interrupt on that first pin

  pinMode( SECOND_GROUND_PIN , OUTPUT );
  digitalWrite( SECOND_GROUND_PIN , 0 ); // Drive low to be the gronud for the sensor

  pinMode( SECOND_TRIGGER_PIN, INPUT_PULLUP );

  #ifdef DEBUG
    Serial.begin(9600);
    Serial.println("Hello World.");
  #endif

  
}



void loop() {
/*
  pinMode( BELL_PIN , OUTPUT );
  digitalWrite( BELL_PIN , 1 );
  
  //delay(5);  //10500 is loudest, 5500 softest

  _delay_us( 5600 );

  digitalWrite( BELL_PIN , 0 );

  delay(1000);
  return;
/*
  while( digitalRead(A1) );

  bounce();
  return;
*/

return;

for(int i=0;i<=10;i++) {
  ringBell(i);
  delay(100);
}
for(int i=10;i>0;i--) {
  ringBell(i);
  delay(100);
}
return;
while(1);
//cli();
//bounce(3.0);
//sei();
return;
  showcountdown();
  showallyourbase();
  showstarfield();
  showinvaders();
  showjabber();

  // TODO: Actually sample the state of the pullup on unused pins and OR it into the mask so we maintain the state.
  // Must do AFTER the cli(). 
  // TODO: Add offBits also to maintain the pullup state of unused pins. 
  
  return;  
}



