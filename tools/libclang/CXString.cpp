//===- CXString.cpp - Routines for manipulating CXStrings -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines routines for manipulating CXStrings. It should be the
// only file that has internal knowledge of the encoding of the data in
// CXStrings.
//
//===----------------------------------------------------------------------===//

#include "CXString.h"
#include "CXTranslationUnit.h"
#include "clang-c/Index.h"
#include "clang/Frontend/ASTUnit.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"

using namespace clang;
using namespace clang::cxstring;

/// Describes the kind of underlying data in CXString.
enum CXStringFlag {
  /// CXString contains a 'const char *' that it doesn't own.
  CXS_Unmanaged,

  /// CXString contains a 'const char *' that it allocated with malloc().
  CXS_Malloc,

  /// CXString contains a CXStringBuf that needs to be returned to the
  /// CXStringPool.
  CXS_StringBuf
};

//===----------------------------------------------------------------------===//
// Basic generation of CXStrings.
//===----------------------------------------------------------------------===//

CXString cxstring::createEmpty() {
  CXString Str;
  Str.data = "";
  Str.private_flags = CXS_Unmanaged;
  return Str;
}

CXString cxstring::createNull() {
  CXString Str;
  Str.data = 0;
  Str.private_flags = CXS_Unmanaged;
  return Str;
}

CXString cxstring::createCXString(const char *String, bool DupString){
  CXString Str;
  if (DupString) {
    Str.data = strdup(String);
    Str.private_flags = (unsigned) CXS_Malloc;
  } else {
    Str.data = String;
    Str.private_flags = (unsigned) CXS_Unmanaged;
  }
  return Str;
}

CXString cxstring::createCXString(StringRef String, bool DupString) {
  CXString Result;
  if (DupString || (!String.empty() && String.data()[String.size()] != 0)) {
    char *Spelling = static_cast<char *>(malloc(String.size() + 1));
    memmove(Spelling, String.data(), String.size());
    Spelling[String.size()] = 0;
    Result.data = Spelling;
    Result.private_flags = (unsigned) CXS_Malloc;
  } else {
    Result.data = String.data();
    Result.private_flags = (unsigned) CXS_Unmanaged;
  }
  return Result;
}

CXString cxstring::createCXString(CXStringBuf *buf) {
  CXString Str;
  Str.data = buf;
  Str.private_flags = (unsigned) CXS_StringBuf;
  return Str;
}


//===----------------------------------------------------------------------===//
// String pools.
//===----------------------------------------------------------------------===//

cxstring::CXStringPool::~CXStringPool() {
  for (std::vector<CXStringBuf *>::iterator I = Pool.begin(), E = Pool.end();
       I != E; ++I) {
    delete *I;
  }
}

CXStringBuf *cxstring::CXStringPool::getCXStringBuf(CXTranslationUnit TU) {
  if (Pool.empty())
    return new CXStringBuf(TU);

  CXStringBuf *Buf = Pool.back();
  Buf->Data.clear();
  Pool.pop_back();
  return Buf;
}

CXStringBuf *cxstring::getCXStringBuf(CXTranslationUnit TU) {
  return TU->StringPool->getCXStringBuf(TU);
}

void cxstring::CXStringBuf::dispose() {
  TU->StringPool->Pool.push_back(this);
}

bool cxstring::isManagedByPool(CXString str) {
  return ((CXStringFlag) str.private_flags) == CXS_StringBuf;
}

//===----------------------------------------------------------------------===//
// libClang public APIs.
//===----------------------------------------------------------------------===//

extern "C" {
const char *clang_getCString(CXString string) {
  if (string.private_flags == (unsigned) CXS_StringBuf) {
    return static_cast<const CXStringBuf *>(string.data)->Data.data();
  }
  return static_cast<const char *>(string.data);
}

void clang_disposeString(CXString string) {
  switch ((CXStringFlag) string.private_flags) {
    case CXS_Unmanaged:
      break;
    case CXS_Malloc:
      if (string.data)
        free(const_cast<void *>(string.data));
      break;
    case CXS_StringBuf:
      static_cast<CXStringBuf *>(
          const_cast<void *>(string.data))->dispose();
      break;
  }
}
} // end: extern "C"

