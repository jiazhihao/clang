// RUN: %clang_cc1 %s -verify -fsyntax-only

void a() {
_nan int arr;
_nan short brr;
_nan unsigned xx;
_nan signed yy;
_nan int result;
int ii;
int aa = 1;

result = arr*ii;
result = ii*brr;

result = arr*brr;
result = xx*yy;

switch (arr) {
  case brr: ; 
  case xx: ; 
}
}

void Tester() {
_nan short a1;
_nan int a2;
short a5;
int a6;
#define TestPair(m,n) int x##m##n = a##m+a##n;
#define TestPairs(m) TestPair(m,1) TestPair(m,2) \
                    TestPair(m,5) TestPair(m,6)
TestPairs(1); TestPairs(2);
TestPairs(5); TestPairs(6);
}

int i1[unnan((_nan int) 2 * 5 == 10, false) ? 1 : -1];
int i2[unnan((_nan int) -2 * 5 == -10, false) ? 1 : -1];
int i3[unnan((_nan int) -2 * -5 == 10, false) ? 1 : -1];
int i4[unnan((_nan unsigned) -2 * 2 == 3, true) ? 1 : -1];
