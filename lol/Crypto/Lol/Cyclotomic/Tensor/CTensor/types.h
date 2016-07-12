
#ifndef TENSORTYPES_H_
#define TENSORTYPES_H_

#include <inttypes.h>

typedef int64_t hInt_t ;
typedef int32_t hDim_t ;
typedef int16_t hShort_t ;
typedef int8_t hByte_t ;

typedef struct
{
  hDim_t prime;
  hShort_t exponent;
} PrimeExponent;


#endif /* TENSORTYPES_H_ */
