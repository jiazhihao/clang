//
//  test-ssub.c
//  
//
//  Created by Zhihao Jia on 10/5/12.
//  
//
//

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
  _nan int a = atoi(argv[1]);
  _nan int b = atoi(argv[2]);
  int c = atoi(argv[3]);
  _nan int d = a - b;
  int ans = unnan(d, c);
  
  printf("ans = %d\n", ans);
}

/*
 examples:
 demo$ ./a.out 0 2147483647 88
 ans = -2147483647
 demo$ ./a.out -1 2147483647 88
 ans = 88
 demo$ ./a.out -2147483648 -2147483648 99
 ans = 99
 demo$ ./a.out -2147483647 -2147483647 99
 ans = 0
 */
