//
//  test-smul.c
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
  _nan int d = a * b;
  int ans;
  ans = unnan(d,c);
  
  printf("ans = %d\n", ans);
}

/*
 examples:
 demo$ ./a.out 1 1 88
 ans = 1
 demo$ ./a.out 32768 65536 88
 ans = 88
 demo$ ./a.out 32768 65535 88
 ans = 2147450880
 demo$ ./a.out -2147483648 1 88
 ans = 88
 demo$ ./a.out -32768 65535 88
 ans = -2147450880
 demo$ ./a.out -32768 -65536 88
 ans = 88
 */
