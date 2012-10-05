#include <stdio.h>
#include <stdlib.h>
#define T unsigned long
#define OP *
int main(int argc, char** argv)
{
  if(argc < 3) printf("USAGE: please input two integers.\n");
  else
  {
    T total;
    _nan T size = atol(argv[1]), num = atol(argv[2]), ntotal;
    ntotal = size OP num;
    total = unnan(ntotal, 1);
    printf("%lu\n",total);
  }
  return 0;
}
