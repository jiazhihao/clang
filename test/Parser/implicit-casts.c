// RUN: %clang_cc1 -fsyntax-only -verify -pedantic %s
_Complex double X;
_nan int Y;
void test1(int c) {
  X = 5;
  Y = 6;
}
void test2() {
  int i;
  double d = i;
  double _Complex a = 5;
  _nan int b = 6;
  
  test1(a);
  a = 5;
  b = 6;
  d = i;
}
int test3() {
  int a[2];
  a[0] = test3; // expected-warning{{incompatible pointer to integer conversion assigning to 'int' from 'int ()'}}
  return 0;
}
short x; void test4(char c) { x += c; }
int y; void test5(char c) { y += c; }
