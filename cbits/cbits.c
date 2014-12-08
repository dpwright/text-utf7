/*
 * Copyright (c) 2014 Daniel P. Wright <dani@dpwright.com>
 *
 * Based heavily on the utf8 decoding functions found in Data.Text.  See:
 *   https://github.com/bos/text/blob/master/cbits/cbits.c
 * to see what I mean.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define UTF7_RESULT 0x000000ff
#define UTF7_STATE  0x0000ff00
#define UTF7_REM    0xffff0000

#define UTF7_ACCEPT 0
#define UTF7_CONT   1
#define UTF7_REJECT 2

#define UTF7_ASCII  0
#define UTF7_B64_1  1
#define UTF7_B64_2  2
#define UTF7_B64_3  3
#define UTF7_B64_4  4
#define UTF7_B64_5  5
#define UTF7_B64_6  6
#define UTF7_B64_7  7
#define UTF7_B64_8  8
#define UTF7_B64_AMP 9
#define UTF7_B64_STATES 10

#define BASE64D_OFFSET  0x2b
#define BASE64D_FINAL   0x7a
#define BASE64D_INVALID 0xff
#define BASE64D_SIZE    (BASE64D_FINAL - BASE64D_OFFSET) + 1

#define STATE_MODE_MASK   0x0f00
#define STATE_RES_MASK    0x00c0
#define STATE_AMP_MASK    0x0020
#define STATE_SR_MASK     0x0010
#define STATE_OFFSET_MASK 0x000f
#define PACK_STATE(next_mode, result_if_hyphen, amp_if_hyphen, cp_shift_right, cp_offset) \
	( (next_mode << 8) | (result_if_hyphen << 6) | (amp_if_hyphen) << 5 \
	| (cp_shift_right << 4) | cp_offset)

static const uint8_t base64e[64] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,";

static const uint8_t base64d[BASE64D_SIZE] = {
	62, 63,
	BASE64D_INVALID, BASE64D_INVALID, BASE64D_INVALID,
	52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
	BASE64D_INVALID, BASE64D_INVALID, BASE64D_INVALID, BASE64D_INVALID,
	BASE64D_INVALID, BASE64D_INVALID, BASE64D_INVALID,
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
	10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
	20, 21, 22, 23, 24, 25,
	BASE64D_INVALID, BASE64D_INVALID, BASE64D_INVALID,
	BASE64D_INVALID, BASE64D_INVALID, BASE64D_INVALID,
	26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
	36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
	46, 47, 48, 49, 50, 51
};

static const uint16_t base64states[UTF7_B64_STATES] = {
	0, //invalid
	PACK_STATE(UTF7_B64_2, UTF7_CONT,   false, false, 10),
	PACK_STATE(UTF7_B64_3, UTF7_REJECT, false, false, 4),
	PACK_STATE(UTF7_B64_4, UTF7_REJECT, false, true,  2),
	PACK_STATE(UTF7_B64_5, UTF7_CONT,   false, false, 8),
	PACK_STATE(UTF7_B64_6, UTF7_REJECT, false, false, 2),
	PACK_STATE(UTF7_B64_7, UTF7_REJECT, false, true,  4),
	PACK_STATE(UTF7_B64_8, UTF7_CONT,   false, false, 6),
	PACK_STATE(UTF7_B64_1, UTF7_REJECT, false, true,  0),
	PACK_STATE(UTF7_B64_2, UTF7_ACCEPT, true,  false, 10)
};

static inline _Bool
is_printable_ascii(uint16_t c) {
	return (c >= 0x20 && c <= 0x7e);
}

static inline uint8_t
lookupd(uint8_t c) {
	uint8_t i = c - BASE64D_OFFSET;
	return (i > BASE64D_SIZE) ? BASE64D_INVALID : base64d[i];
}

static inline uint16_t
decode(uint32_t *state, uint16_t *codep, uint8_t byte) {
	uint16_t rem = *state >> 16;
	uint8_t st   = (*state & UTF7_STATE) >> 8;
	uint8_t r    = *state & UTF7_RESULT;

	if(st == UTF7_ASCII) {
		if(byte == '&') {
			*codep = 0;
			st = UTF7_B64_AMP;
			r = UTF7_CONT;
		} else {
			*codep = byte;
			r = UTF7_ACCEPT;
		}
	} else {
		uint16_t packed_state = base64states[st];
		uint8_t next_mode        = (packed_state & STATE_MODE_MASK) >> 8;
		uint8_t result_if_hyphen = (packed_state & STATE_RES_MASK) >> 6;
		bool amp_if_hyphen       = (packed_state & STATE_AMP_MASK) >> 5;
		bool cp_shift_right      = (packed_state & STATE_SR_MASK) >> 4;
		uint8_t cp_offset        = packed_state & STATE_OFFSET_MASK;

		if(byte == '-') {
			if(amp_if_hyphen)
				*codep = '&';
			st = UTF7_ASCII;
			r = result_if_hyphen;
		} else {
			uint8_t b = lookupd(byte);
			if(b != BASE64D_INVALID) {
				if(!cp_shift_right) {
					*codep |= rem | (b << cp_offset);
					rem    = 0;
					r      = UTF7_CONT;
				} else {
					*codep |= b >> cp_offset;
					rem    = b << 16 - cp_offset;
					r      = UTF7_ACCEPT;
				}

				st = next_mode;
			} else {
				r = UTF7_REJECT;
			}
		}
	}

	*state = rem << 16 | st << 8 | r;
	return *state;
}

static inline uint8_t const *
_hs_text_decode_utf7_int(uint16_t *const dest, size_t *destoff,
                         const uint8_t **src, const uint8_t *srcend,
                         uint16_t *codepoint0, uint32_t *state0)
{
	uint16_t *d = dest + *destoff;
	const uint8_t *s = *src, *last = *src;
	uint32_t state = *state0;
	uint16_t codepoint = *codepoint0;

	while (s < srcend) {
		if (decode(&state, &codepoint, *s++) & UTF7_RESULT != UTF7_ACCEPT) {
			if (state & UTF7_RESULT != UTF7_REJECT)
				continue;
			break;
		}

		*d++ = codepoint;
		last = s;
		codepoint = 0;
	}

	*destoff = d - dest;
	*codepoint0 = codepoint;
	*state0 = state;
	*src = last;

	return s;
}

uint8_t const *
_hs_text_decode_utf7_state(uint16_t *const dest, size_t *destoff,
                           const uint8_t **src, const uint8_t *srcend,
                           uint16_t *codepoint0, uint32_t *state0)
{
	uint8_t const *ret = _hs_text_decode_utf7_int(dest, destoff, src, srcend,
	                                              codepoint0, state0);
	if(*state0 & UTF7_RESULT == UTF7_REJECT)
		ret -= 1;

	return ret;
}

/*
 * Helper to decode buffer and discard final decoder state
 */
const uint8_t *
_hs_text_decode_utf7(uint16_t *const dest, size_t *destoff,
                     const uint8_t *src, const uint8_t *const srcend)
{
	uint16_t codepoint;
	uint32_t state = UTF7_ACCEPT;
	uint8_t const *ret = _hs_text_decode_utf7_int(dest, destoff, &src, srcend,
	                                              &codepoint, &state);
	/*Back up if we have an invalid encoding */
	if (state & UTF7_RESULT == UTF7_REJECT) {
		ret -= 1;
	}
	return ret;
}

void
_hs_text_encode_utf7(uint8_t **destp, const uint16_t *src, size_t srcoff,
                     size_t srclen)
{
	const uint16_t *srcend;
	uint8_t *dest = *destp;
	_Bool inAsciiBlock = true;
	int i = 0;

	src += srcoff;
	srcend = src + srclen;

	while (src < srcend) {
		uint16_t w = *src;

		if(inAsciiBlock) {
			if(is_printable_ascii(w)) {
				if(w == '&') {
					*dest++ = '&';
					*dest++ = '-';
				} else {
					*dest++ = w;
				}

				src++;
			} else {
				inAsciiBlock = false;
				*dest++ = '&';
			}
		} else {
			if(is_printable_ascii(w)) {
				inAsciiBlock = true;
				*dest++ = '-';
			} else {
				uint8_t indices[8];
				uint8_t bytes = 3;

				indices[0] = (w & 0xfc00) >> 10;
				indices[1] = (w & 0x03f0) >> 4;
				indices[2] = (w & 0x000f) << 2;

				w = *++src;
				if(src < srcend) {
					if(!is_printable_ascii(w)) {
						bytes = 6;

						indices[2] |= (w & 0xc000) >> 14;

						indices[3] = (w & 0x3f00) >> 8;
						indices[4] = (w & 0x00fc) >> 2;
						indices[5] = (w & 0x0003) << 4;

						w = *++src;
						if(src < srcend) {
							if(!is_printable_ascii(w)) {
								bytes = 8;

								indices[5] |= (w & 0xf000) >> 12;

								indices[6] = (w & 0x0fc0) >> 6;
								indices[7] = w & 0x003f;

								src++;
							}
						}
					}
				}

				for(i = 0; i < bytes; ++i)
					*dest++ = base64e[indices[i]];
			}
		}
	}

	if(!inAsciiBlock)
		*dest++ = '-';

	*destp = dest;
}
