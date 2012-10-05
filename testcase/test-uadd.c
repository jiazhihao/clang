//
//  test-add.c
//  
//
//  Created by Zhihao Jia on 10/5/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
  _nan int a = atoi(argv[1]);
  _nan int b = atoi(argv[2]);
  int c = atoi(argv[3]);
  _nan int d = a + b;
  int ans;
  if (isnan(d))
    ans = c;
  else
    ans = (int) d;
  
  printf("ans = %d\n", ans);
}

/*
 examples:
 demo$ ./a.out 1 1 1  
 ans = 2
 demo$ ./a.out -2147483648 1 88 
 ans = 88
 demo$ ./a.out 10 -2147483648 888 
 ans = 888
 demo$ ./a.out 2147483647 0 88
 ans = 2147483647
 demo$ ./a.out 2147483647 1 88 
 ans = 88
 demo$ ./a.out 0 2147483647 888
 ans = 2147483647
 demo$ ./a.out 1 2147483647 888
 ans = 88
*/
