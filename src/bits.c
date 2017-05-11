/*---------------------------------------------------------------------------
   Copyright (c) 2017 Vasil Diadov. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*/

#include <bits.h>

CAMLprim value cprim_bits_get_exn(value bits, value bito) {
  // {length : int; data : bytes}
  register int len = Int_val(Field(bits, 0));
  register int bit_offset = Int_val(bito);
  register int byte_offset = bit_offset >> 3;
  if (bit_offset >= len) {
    caml_invalid_argument("Bits.get");
  }
  bit_offset &= 0x7;
  return Val_int((Byte_u(Field(bits, 1), byte_offset) >> bit_offset) & 0x1);
}

CAMLprim value cprim_bits_set_exn(value bits, value bito, value v) {
  // {length : int; data : bytes}
  register int len = Int_val(Field(bits, 0));
  register int bit_offset = Int_val(bito);
  register int byte_offset = bit_offset >> 3;
  if (bit_offset >= len) {
    caml_invalid_argument("Bits.set");
  }
  bit_offset &= 0x7;
  {
    register int mask = 0x1 << bit_offset;
    register int byte = Byte_u(Field(bits, 1), byte_offset);
    byte &= ~mask;
    byte |= Int_val(v) << bit_offset; // as v is bool, Int_val(v) is in [0,1]
    Byte_u(Field(bits, 1), byte_offset) = byte;
  }
  return Val_unit;
}

CAMLprim value cprim_bits_op(value b1, value b2, value op) {
  // {length : int; data : bytes}
  int len1 = Int_val(Field(b1, 0));
  int len2 = Int_val(Field(b2, 0));
  value d1 = Field(b1, 1);
  value d2 = Field(b2, 1);
  int opc = Int_val(op); // ops = 1 - and, 2 - or, 3 - xor
  register int len = caml_string_length(d1);
  if (len1 != len2) {
    caml_invalid_argument("Bits.and/Bits.or/Bits.xor lengths of bitvectors are different");
  }
  len2 = len-1;
  #define FAST_ITERATE(len, d1, d2, op) \
    { \
      register unsigned char *p1=&(Byte_u(d1, 0)); \
      register unsigned char *p2=&(Byte_u(d2, 0)); \
      for(register int _len = len>>3;_len;_len--) {*((uint64_t *)p1) op *((uint64_t *)p2); p1+=8; p2+=8;}; \
      if (len&0x4) {*((uint32_t *)p1) op *((uint32_t *)p2); p1+=4; p2+=4;}; \
      if (len&0x2) {*((uint16_t *)p1) op *((uint16_t *)p2); p1+=2; p2+=2;}; \
      if (len&0x1) *((uint8_t *)p1) op *((uint8_t *)p2); \
    }
  switch(opc){
    case 0: FAST_ITERATE(len, d1, d2, &=); break;
    case 1: FAST_ITERATE(len, d1, d2, |=); break;
    case 2: FAST_ITERATE(len, d1, d2, ^=); break;
    default: caml_invalid_argument("Bits.and/Bits.or/Bits.xor incorrect code for op: only [0 - and, 1 - or, 2 - xor] are permitted");
  }
  // set highest unused bits to zero
  len1 &= 0x7;
  if(len1) {
    Byte_u(d1, len2) &= (1 << len1 ) - 1;
  }
  return b1;
}

CAMLprim value cprim_bits_not(value b) {
  // {length : int; data : bytes}
  register int bit_len = Int_val(Field(b, 0));
  register int len = bit_len >> 3;
  bit_len &= 0x7; // used bits in last byte
  value d = Field(b, 1);
  if(bit_len) Byte_u(d, len) = ~Byte_u(d, len) & ( (1 << bit_len ) - 1);
  FAST_ITERATE(len,d,d, = ~);
  return b;
}

CAMLprim value cprim_bits_iteri_exn(value f, value b) {
  register int bit_len = Int_val(Field(b, 0));
  register int len = bit_len >> 3;
  register unsigned char *p = &Byte_u(Field(b, 1), 0);
  bit_len &= 0x7; // used bits in last byte
  register int i = 0;
  for(;len;len--) {
    register int byte = (int)*p++;
    register value result;
    #define CHECK_FOR_EXN(v)                         \
      if (Is_exception_result(v))                    \
      {                                              \
        value exn=Extract_exception(v);              \
        if (Wosize_val(exn)==1)                      \
          raise_constant(Field(exn,0));              \
        else                                         \
          raise_with_arg(Field(exn,0),Field(exn,1)); \
      }
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result);
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result);
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result);
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result);
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result);
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result);
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result);
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); CHECK_FOR_EXN(result);
  }
  if(bit_len) {
    register int byte = (int)*p;
    register value result;
    while(bit_len--){
      result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result);
    }
  }
  return Val_unit;
}

CAMLprim value cprim_bits_mapi_exn(value f, value b) {
  register int bit_len = Int_val(Field(b, 0));
  register int len = bit_len >> 3;
  register unsigned char *p = &Byte_u(Field(b, 1), 0);
  bit_len &= 0x7; // used bits in last byte
  register int i = 0;
  for(;len;len--) {
    register int byte = (int)*p;
    register int new_byte = 0;
    register value result;

    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result); new_byte |= Int_val(result);
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 1;
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 2;
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 3;
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 4;
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 5;
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 6;
    result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 7;
    *p++=(unsigned char)new_byte;
  }
  if(bit_len) {
    register int byte = (int)*p;
    register int new_byte = 0;
    register int off = 0;
    register value result;
    while(bit_len--){
      result = caml_callback2_exn(f, Val_int(i++), Val_int(byte&0x1)); byte >>= 1; CHECK_FOR_EXN(result); new_byte |= (Int_val(result) << off++);
    }
    *p = (unsigned char)new_byte;
  }
  return b;
}

CAMLprim value cprim_bits_iteri_on_val_exn(value f, value b, value v) {
  register int bit_len = Int_val(Field(b, 0));
  register int len = bit_len >> 3;
  register unsigned char *p = &Byte_u(Field(b, 1), 0);
  register int val = Int_val(v) ? 0xFF : 0x0;
  bit_len &= 0x7; // used bits in last byte
  register int i = 0;
  for(;len;len--) {
    register int byte = ~(val ^ (int)*p++);
    register value result;

    if (byte&0x01) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); } i++;
    if (byte&0x02) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); } i++;
    if (byte&0x04) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); } i++;
    if (byte&0x08) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); } i++;
    if (byte&0x10) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); } i++;
    if (byte&0x20) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); } i++;
    if (byte&0x40) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); } i++;
    if (byte&0x80) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); } i++;
  }
  if(bit_len) {
    register int byte = ~(val ^ (int)*p);
    register value result;
    while(bit_len--){
      if (byte&0x1) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); }
      i++;
      byte >>= 1;
    }
  }
  return Val_unit;
}

CAMLprim value cprim_bits_mapi_on_val_exn(value f, value b, value v) {
  register int bit_len = Int_val(Field(b, 0));
  register int len = bit_len >> 3;
  register unsigned char *p = &Byte_u(Field(b, 1), 0);
  register int val = Int_val(v) ? 0xFF : 0x0;
  bit_len &= 0x7; // used bits in last byte
  register int i = 0;
  for(;len;len--, p++) {
    register int byte = ~(val ^ (int)*p);
    register int new_byte = 0;
    register value result;

    if (byte&0x01) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); new_byte |= Int_val(result); } i++;
    if (byte&0x02) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 1;} i++;
    if (byte&0x04) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 2;} i++;
    if (byte&0x08) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 3;} i++;
    if (byte&0x10) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 4;} i++;
    if (byte&0x20) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 5;} i++;
    if (byte&0x40) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 6;} i++;
    if (byte&0x80) { result = caml_callback_exn(f, Val_int(i)); CHECK_FOR_EXN(result); new_byte |= Int_val(result) << 7;} i++;

    if(byte) {
      byte = ~byte;
      byte &= (int) *p;
      byte |= new_byte;
      *p = (unsigned char) (byte);
    }
  }
  if(bit_len) {
    register int byte = ~(val ^ (int)*p);
    register value result;
    register int new_byte = 0;
    register int off = 0;
    while(bit_len--){
      if (byte&(1 << off)) {
        result = caml_callback_exn(f, Val_int(i));
        CHECK_FOR_EXN(result);
        new_byte |= Int_val(result) << off;
      }
      i++;
      off++;
    }
    byte = ~byte;
    byte &= (int) *p;
    byte |= new_byte;
    byte &= (1 << off) - 1;
    *p = (unsigned char) (byte);
  }
  return b;
}


CAMLprim value cprim_bits_for_all_values_exn(value f, value b, value v) {
  register int bit_len = Int_val(Field(b, 0));
  register int len = bit_len >> 3;
  register unsigned char *p = &Byte_u(Field(b, 1), 0);
  register int val = Val_int(v) ? 0xFF : 0x0;
  bit_len &= 0x7; // used bits in last byte
  register int i = 0;
  for(;len;len--) {
    register int byte = ~(val ^ (int)*p++);
    register value result;

    if (byte&0x01) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (!Int_val(result)) return Val_int(0);}
    if (byte&0x02) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (!Int_val(result)) return Val_int(0);}
    if (byte&0x04) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (!Int_val(result)) return Val_int(0);}
    if (byte&0x08) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (!Int_val(result)) return Val_int(0);}
    if (byte&0x10) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (!Int_val(result)) return Val_int(0);}
    if (byte&0x20) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (!Int_val(result)) return Val_int(0);}
    if (byte&0x40) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (!Int_val(result)) return Val_int(0);}
    if (byte&0x80) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (!Int_val(result)) return Val_int(0);}
  }
  if(bit_len) {
    register int byte = ~(val ^ (int)*p);
    register value result;
    while(bit_len--){
      if (byte&0x1) { result = caml_callback_exn(f, Val_int(i++)); byte >>= 1; CHECK_FOR_EXN(result); if (!Int_val(result)) return Val_int(0);}
    }
  }
  return Val_int(1);
}

CAMLprim value cprim_bits_exists_for_values_exn(value f, value b, value v) {
  register int bit_len = Int_val(Field(b, 0));
  register int len = bit_len >> 3;
  register unsigned char *p = &Byte_u(Field(b, 1), 0);
  register int val = Val_int(v) ? 0xFF : 0x0;
  bit_len &= 0x7; // used bits in last byte
  register int i = 0;
  for(;len;len--) {
    register int byte = ~(val ^ (int)*p++);
    register value result;

    if (byte&0x01) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (Int_val(result)) return Val_int(1);}
    if (byte&0x02) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (Int_val(result)) return Val_int(1);}
    if (byte&0x04) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (Int_val(result)) return Val_int(1);}
    if (byte&0x08) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (Int_val(result)) return Val_int(1);}
    if (byte&0x10) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (Int_val(result)) return Val_int(1);}
    if (byte&0x20) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (Int_val(result)) return Val_int(1);}
    if (byte&0x40) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (Int_val(result)) return Val_int(1);}
    if (byte&0x80) { result = caml_callback_exn(f, Val_int(i++)); CHECK_FOR_EXN(result); if (Int_val(result)) return Val_int(1);}
  }
  if(bit_len) {
    register int byte = ~(val ^ (int)*p);
    register value result;
    while(bit_len--){
      if (byte&0x1) { result = caml_callback_exn(f, Val_int(i++)); byte >>= 1; CHECK_FOR_EXN(result); if (Int_val(result)) return Val_int(1);}
    }
  }
  return Val_int(0);
}

CAMLprim value cprim_bits_count_val(value b, value v) {
  register int bit_len = Int_val(Field(b, 0));
  register int len = bit_len >> 3;
  register unsigned char *p = &Byte_u(Field(b, 1), 0);
  register int count = 0;
  #define COUNT_BITS_64(v) \
    v = (v & 0x5555555555555555ULL) + ((v & 0xAAAAAAAAAAAAAAAAULL) >> 1);  \
    v = (v & 0x3333333333333333ULL) + ((v & 0xCCCCCCCCCCCCCCCCULL) >> 2);  \
    v = (v & 0x0F0F0F0F0F0F0F0FULL) + ((v & 0xF0F0F0F0F0F0F0F0ULL) >> 4);  \
    v = (v & 0x00FF00FF00FF00FFULL) + ((v & 0xFF00FF00FF00FF00ULL) >> 8);  \
    v = (v & 0x0000FFFF0000FFFFULL) + ((v & 0xFFFF0000FFFF0000ULL) >> 16); \
    v = (v & 0x00000000FFFFFFFFULL) + ((v & 0xFFFFFFFF00000000ULL) >> 32);
  #define COUNT_BITS_32(v) \
    v = (v & 0x55555555UL) + ((v & 0xAAAAAAAAUL) >> 1); \
    v = (v & 0x33333333UL) + ((v & 0xCCCCCCCCUL) >> 2); \
    v = (v & 0x0F0F0F0FUL) + ((v & 0xF0F0F0F0UL) >> 4); \
    v = (v & 0x00FF00FFUL) + ((v & 0xFF00FF00UL) >> 8); \
    v = (v & 0x0000FFFFUL) + ((v & 0xFFFF0000UL) >> 16);
  #define COUNT_BITS_16(v) \
    v = (v & 0x5555) + ((v & 0xAAAA) >> 1); \
    v = (v & 0x3333) + ((v & 0xCCCC) >> 2); \
    v = (v & 0x0F0F) + ((v & 0xF0F0) >> 4); \
    v = (v & 0x00FF) + ((v & 0xFF00) >> 8);
  #define COUNT_BITS_8(v) \
    v = (v & 0x55) + ((v & 0xAA) >> 1); \
    v = (v & 0x33) + ((v & 0xCC) >> 2); \
    v = (v & 0x0F) + ((v & 0xF0) >> 4);
  for(register int _len = len>>3;_len;_len--) {uint64_t v = *((uint64_t *)p); COUNT_BITS_64(v); count += (int) v; p+=8;};
  if (len&0x4) {uint32_t v = *((uint32_t *)p); COUNT_BITS_32(v); count += (int) v; p+=4;};
  if (len&0x2) {uint16_t v = *((uint16_t *)p); COUNT_BITS_16(v); count += (int) v; p+=2;};
  if (len&0x1) {uint8_t v = *((uint8_t *)p); COUNT_BITS_8(v); count += (int) v; p++;}
  if (bit_len & 0x7) {uint8_t v = *((uint8_t *)p); v &= (1 << (bit_len & 0x7)) - 1; COUNT_BITS_8(v); count += (int) v;}
  if (Int_val(v)) return Val_int(count);
  return Val_int(bit_len - count);
}

CAMLprim value cprim_bits_index(value b, value v) {
  register int bit_len = Int_val(Field(b, 0));
  register int len = bit_len >> 3;
  register unsigned char *p = &Byte_u(Field(b, 1), 0);
  int off = len << 3;
  bit_len &= 7;
  if (Int_val(v)) {
    int byte;
    for(;len;len--,p++)
      if (*p) {
        byte = (int)*p;
        off = (int)(uintptr_t)(p - &Byte_u(Field(b, 1), 0));
        off <<= 3;
        #define CHECK_BITS()                    \
          if (byte&0x01) return Val_int(off);   \
          if (byte&0x02) return Val_int(off+1); \
          if (byte&0x04) return Val_int(off+2); \
          if (byte&0x08) return Val_int(off+3); \
          if (byte&0x10) return Val_int(off+4); \
          if (byte&0x20) return Val_int(off+5); \
          if (byte&0x40) return Val_int(off+6); \
          if (byte&0x80) return Val_int(off+7);
        CHECK_BITS();
      };
    if (bit_len) {
      byte = (int)*p;
      byte &= (1 << bit_len) - 1;
      CHECK_BITS();
    }
  } else {
    unsigned int byte;
    for(;len;len--,p++)
      if (*p != 0xFF) {
        byte = ~(unsigned int)*p;
        off = (int)(uintptr_t)(p - &Byte_u(Field(b, 1), 0));
        off <<= 3;
        CHECK_BITS();
      };
    if (bit_len) {
      byte = ~(unsigned int)*p;
      byte &= (1 << bit_len) - 1;
      CHECK_BITS();
    }
  }
  return Val_int(-1);
}

CAMLprim value cprim_bits_rindex(value b, value v) {
  register int bit_len = Int_val(Field(b, 0));
  register int len = bit_len >> 3;
  register unsigned char *p = &Byte_u(Field(b, 1), len);
  int off = len << 3;
  bit_len &= 7;
  if (Int_val(v)) {
    int byte;
    if (bit_len) {
      byte = (int)*p;
      byte &= (1 << bit_len) - 1;
      #undef CHECK_BITS
      #define CHECK_BITS()                    \
        if (byte&0x80) return Val_int(off+7); \
        if (byte&0x40) return Val_int(off+6); \
        if (byte&0x20) return Val_int(off+5); \
        if (byte&0x10) return Val_int(off+4); \
        if (byte&0x08) return Val_int(off+3); \
        if (byte&0x04) return Val_int(off+2); \
        if (byte&0x02) return Val_int(off+1); \
        if (byte&0x01) return Val_int(off);
      CHECK_BITS();
    }
    p--;
    for(;len;len--,p--)
      if (*p) {
        byte = (int)*p;
        off = (int)(uintptr_t)(p - &Byte_u(Field(b, 1), 0));
        off <<= 3;
        CHECK_BITS();
      };
  } else {
    unsigned int byte;
    if (bit_len) {
      byte = ~(unsigned int)*p;
      byte &= (1 << bit_len) - 1;
      CHECK_BITS();
    }
    p--;
    for(;len;len--,p--)
      if (*p != 0xFF) {
        byte = ~(unsigned int)*p;
        off = (int)(uintptr_t)(p - &Byte_u(Field(b, 1), 0));
        off <<= 3;
        CHECK_BITS();
      };
  }
  return Val_int(-1);
}

/*---------------------------------------------------------------------------
   Copyright (c) 2017 Vasil Diadov

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/

