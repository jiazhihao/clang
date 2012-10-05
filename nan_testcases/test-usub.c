//  
//  test-usub.c
//  
//
//  Created by Zhihao Jia on 10/5/12.
//  
//  nan semantics for unsigned integers.
// 

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
  _nan unsigned a = atoi(argv[1]);
  _nan int b = atoi(argv[2]);
  unsigned c = atoi(argv[3]);
  _nan unsigned d = a - b;
  unsigned ans;
  if (isnan(d))
    ans = c;
  else
    ans = (unsigned) d;
  
  printf("ans = %u\n", ans);
}

/*
 examples:
 demo$ ./a.out 1 1 1
 ans = 0
 demo$ ./a.out 1 -2147483648 88
 ans = 88
 demo$ ./a.out 100 -100 888
 ans = 888 //this is because when calculate ``a-b'', 
             we have to cast b from int -> unsigned, 
             -100 is out of the range of unsigned.
             Overflow happens in casting.
 */
