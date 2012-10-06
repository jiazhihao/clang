/*
 This test case is modified from mq_attr_ok function in the Linux kernel.
 The code will output "overflow!" if the result of 
                  a * b + c * d
 overflows.
 */
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
  if (argc < 5) printf("USAGE: input a, b, c, d.\nCalculate a * b + c * d\n");
  else
  {
    _nan int a = atol(argv[1]), b = atol(argv[2]), c = atol(argv[3]), d = atol(argv[4]);
    _nan int nresult = a * b + c * d;
    if (isnan(nresult)) printf("overflow!\n");
    else {
      printf("result = %d\n", (int)nresult);
    }
  }
  return 0;
}

/*
 demo$ ./a.out 32768 32768 32768 32768
 overflow!
 demo$ ./a.out 32768 32768 32767 32765
 result = 2147352579
 demo$ ./a.out -32768 32768 -32768 32768
 overflow!
 */
