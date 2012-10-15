//
//  test-umul.c
//  
//
//  Created by Zhihao Jia on 10/5/12.
//  
//

#include <stdio.h>
#include <stdlib.h>

void ushl(unsigned a, unsigned b)
{
  _nan unsigned na = a, nb = b;
  _nan unsigned nc = na << nb;
  if (isnan(nc))
    printf("%u << %u = overflow!\n", a, b);
  else
    printf("%u << %u = %u\n", a, b, (unsigned)nc);
}

void sshl(int a, int b)
{
  _nan int na = a, nb = b;
  _nan int nc = na << nb;
  if (isnan(nc))
    printf("%d << %d = overflow!\n", a, b);
  else
    printf("%d << %d = %d\n", a, b, (int)nc); 
}

int main(int argc, char** argv)
{
  printf("signed shl:\n");
  sshl(1, 1);
  sshl(10, 31);
  sshl(1, 32);
  sshl(-2147483648, 1);
  sshl(1, 40);
  printf("unsigned shl:\n");
  ushl(1, 31);
  ushl(2, 31);
  ushl(1, 32);
  ushl(2, 40);
}

/*
 output:
 
 signed shl:
 1 << 1 = 2
 10 << 31 = 0
 1 << 32 = overflow!
 -2147483648 << 1 = overflow!
 1 << 40 = overflow!
 
 unsigned shl:
 1 << 31 = 2147483648
 2 << 31 = 0
 1 << 32 = overflow!
 2 << 40 = overflow!
*/
