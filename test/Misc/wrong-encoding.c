// RUN: %clang_cc1 -fsyntax-only -Wno-unused-value %s 2>&1 | FileCheck -strict-whitespace %s

void foo() {

  "��"; // �
// CHECK: {{^  "<A7><C3>"; // <F8>}}
// CHECK: {{^   \^~~~~~~}}

  /* �� */ const char *d = "�";

// CHECK: {{^  /\* <FE><AB> \*/ const char \*d = "<A5>";}}
// CHECK: {{^                                  \^~~~}}

  "xx鿿�d";
// CHECK: {{^  "xx<U\+9FFF><BF>d";}}
// CHECK: {{^             \^~~~}}

  "xx�bcd";
// CHECK: {{^  "xx<E9><BF>bcd";}}
// CHECK: {{^     \^~~~~~~~}}

  "xx�abcd";
// CHECK: {{^  "xx<E9>abcd";}}
// CHECK: {{^     \^~~~}}

  "xx��d";
// CHECK: {{^  "xx<E9><BF><E9><BF>d";}}
// CHECK: {{^     \^~~~~~~~~~~~~~~}}

  "xx�xxxxxxxxxxxxxxxxxxxxx�xx";
// CHECK: {{^  "xx<E9><BF>xxxxxxxxxxxxxxxxxxxxx<E9><BF>xx";}}
// CHECK: {{^     \^~~~~~~~                     ~~~~~~~~}}
}
