#ifndef TENSOR_CPP_
#define TENSOR_CPP_

#include "types.h"

hDim_t ipow(hDim_t base, hShort_t exp);

#ifdef __cplusplus
template <typename ring>
using primeFunc = void (*) (ring*, hShort_t, hDim_t, hDim_t, hDim_t);

//for square transforms
template <typename ring> void tensorFuserPrime (ring* y, hShort_t tupSize, primeFunc<ring> f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  hDim_t lts = totm;
  hDim_t rts = 1;
  hShort_t i;

  for (i = 0; i < sizeOfPE; ++i) {
    PrimeExponent pe = peArr[i];
    hDim_t ipow_pe = ipow(pe.prime, (pe.exponent-1));
    hDim_t dim = (pe.prime-1) * ipow_pe;  // the totient of pe
    lts /= dim;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      (*f) (y+tupIdx, tupSize, lts*ipow_pe, rts, pe.prime);
    }
    rts *= dim;
  }
}
#endif /* __cplusplus */
#endif /* TENSOR_CPP_ */
