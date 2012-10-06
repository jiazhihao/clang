//  
//  test-uadd.c
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
  _nan unsigned b = atoi(argv[2]);
  unsigned c = atoi(argv[3]);
  _nan unsigned d = a + b;
  unsigned ans;
  ans = unnan(d, c);
  printf("ans = %u\n", ans);
}

/*
 examples:
 demo$ ./a.out 1 1 1
 ans = 2
 demo$ ./a.out 2147483647 2147483647 88
 ans = 4294967294
 demo$ ./a.out 2147483647 2147483648 88
 ans = 88
 demo$ ./a.out 3000000000 3000000000 88
 ans = 88
 demo$ ./a.out 4294967295 100 88
 ans = 88 
 */

