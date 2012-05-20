// RUN: %clang_cc1 -emit-llvm-only %s

int main(void)
{
  _nan int a = 5;
  _nan int b = 42;

  return a * b != b * a;
}

_nan int bar(int);
void test(_nan int *);
void takenan(_nan int);

void test2(int c) {
  _nan int X;
  X = bar(1);
  test(&X);
  takenan(X);
}

_nan int g1, g2;
_nan int cf;
int D;

void test3() {
  g1 = g1 + g2;
  g1 = g1 - g2;
  g1 = g1 * g2;
  //g1 = +-~g1;

  //double Gr = __real g1;

  //cf += D;
  // FIXME: Currently unsupported!
  //D += cf;
  //cf /= g1;
  g1 = g1 + D;
  g1 = D + g1;
}
