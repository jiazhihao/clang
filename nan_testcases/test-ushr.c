//
//  test-umul.c
//  
//
//  Created by Zhihao Jia on 10/5/12.
//  
//

#include <stdio.h>
#include <stdlib.h>

void foo(unsigned a, unsigned b)
{
  _nan unsigned na = a, nb = b;
  _nan unsigned nc = na * nb;
  if (isnan(nc))
    printf("%u * %u = overflow!\n", a, b);
  else
    printf("%u * %u = %u\n", a, b, (unsigned)nc);

}

int main(int argc, char** argv)
{
  foo(65536, 65536);
  foo(65535, 65536);
  foo(65535, 65537);
}

/*
 output:
 65536 * 65536 = overflow!
 65535 * 65536 = 4294901760
 65535 * 65537 = overflow!
*/
