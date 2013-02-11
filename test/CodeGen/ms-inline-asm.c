// REQUIRES: x86-64-registered-target
// RUN: %clang_cc1 %s -triple x86_64-apple-darwin10 -O0 -fms-extensions -fenable-experimental-ms-inline-asm -w -emit-llvm -o - | FileCheck %s

void t1() {
// CHECK: @t1
// CHECK: call void asm sideeffect inteldialect "", "~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: ret void
  __asm {}
}

void t2() {
// CHECK: @t2
// CHECK: call void asm sideeffect inteldialect "nop", "~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: call void asm sideeffect inteldialect "nop", "~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: call void asm sideeffect inteldialect "nop", "~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: ret void
  __asm nop
  __asm nop
  __asm nop
}

void t3() {
// CHECK: @t3
// CHECK: call void asm sideeffect inteldialect "nop\0A\09nop\0A\09nop", "~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: ret void
  __asm nop __asm nop __asm nop
}

void t4(void) {
// CHECK: @t4
// CHECK: call void asm sideeffect inteldialect "mov ebx, eax", "~{ebx},~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: call void asm sideeffect inteldialect "mov ecx, ebx", "~{ecx},~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: ret void
  __asm mov ebx, eax
  __asm mov ecx, ebx
}

void t5(void) {
// CHECK: @t5
// CHECK: call void asm sideeffect inteldialect "mov ebx, eax\0A\09mov ecx, ebx", "~{ebx},~{ecx},~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: ret void
  __asm mov ebx, eax __asm mov ecx, ebx
}

void t6(void) {
  __asm int 0x2c
// CHECK: t6
// CHECK: call void asm sideeffect inteldialect "int $$0x2c", "~{dirflag},~{fpsr},~{flags}"() nounwind
}

void t7() {
  __asm {
    int 0x2c ; } asm comments are fun! }{
  }
  __asm {}
// CHECK: t7
// CHECK: call void asm sideeffect inteldialect "int $$0x2c", "~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: call void asm sideeffect inteldialect "", "~{dirflag},~{fpsr},~{flags}"() nounwind
}

int t8() {
  __asm int 4 ; } comments for single-line asm
  __asm {}
  __asm int 4
  return 10;
// CHECK: t8
// CHECK: call void asm sideeffect inteldialect "int $$4", "~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: call void asm sideeffect inteldialect "", "~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: call void asm sideeffect inteldialect "int $$4", "~{dirflag},~{fpsr},~{flags}"() nounwind
// CHECK: ret i32 10
}

void t9() {
  __asm {
    push ebx
    mov ebx, 0x07
    pop ebx
  }
// CHECK: t9
// CHECK: call void asm sideeffect inteldialect "push ebx\0A\09mov ebx, $$0x07\0A\09pop ebx", "~{ebx},~{dirflag},~{fpsr},~{flags}"() nounwind
}

unsigned t10(void) {
  unsigned i = 1, j;
  __asm {
    mov eax, i
    mov j, eax
  }
  return j;
// CHECK: t10
// CHECK: [[I:%[a-zA-Z0-9]+]] = alloca i32, align 4
// CHECK: [[J:%[a-zA-Z0-9]+]] = alloca i32, align 4
// CHECK: store i32 1, i32* [[I]], align 4
// CHECK: call void asm sideeffect inteldialect "mov eax, dword ptr $1\0A\09mov dword ptr $0, eax", "=*m,*m,~{eax},~{dirflag},~{fpsr},~{flags}"(i32* %{{.*}}, i32* %{{.*}}) nounwind
// CHECK: [[RET:%[a-zA-Z0-9]+]] = load i32* [[J]], align 4
// CHECK: ret i32 [[RET]]
}

void t11(void) {
  __asm mov eax, 1
// CHECK: t11
// CHECK: call void asm sideeffect inteldialect "mov eax, $$1", "~{eax},~{dirflag},~{fpsr},~{flags}"() nounwind
}

unsigned t12(void) {
  unsigned i = 1, j, l = 1, m;
  __asm {
    mov eax, i
    mov j, eax
    mov eax, l
    mov m, eax
  }
  return j + m;
// CHECK: t12
// CHECK: call void asm sideeffect inteldialect "mov eax, dword ptr $2\0A\09mov dword ptr $0, eax\0A\09mov eax, dword ptr $3\0A\09mov dword ptr $1, eax", "=*m,=*m,*m,*m,~{eax},~{dirflag},~{fpsr},~{flags}"(i32* %{{.*}}, i32* %{{.*}}, i32* %{{.*}}, i32* %{{.*}}) nounwind
}

void t13() {
  char i = 1;
  short j = 2;
  __asm movzx eax, i
  __asm movzx eax, j
// CHECK: t13
// CHECK: call void asm sideeffect inteldialect "movzx eax, byte ptr $0", "*m,~{eax},~{dirflag},~{fpsr},~{flags}"(i8* %{{.*}}) nounwind
// CHECK: call void asm sideeffect inteldialect "movzx eax, word ptr $0", "*m,~{eax},~{dirflag},~{fpsr},~{flags}"(i16* %{{.*}}) nounwind
}
