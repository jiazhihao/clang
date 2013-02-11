//===--- UnwrappedLineParser.cpp - Format C++ code ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains the implementation of the UnwrappedLineParser,
/// which turns a stream of tokens into UnwrappedLines.
///
/// This is EXPERIMENTAL code under heavy development. It is not in a state yet,
/// where it can be used to format real code.
///
//===----------------------------------------------------------------------===//

#include "UnwrappedLineParser.h"
#include "llvm/Support/raw_ostream.h"

namespace clang {
namespace format {

class ScopedMacroState : public FormatTokenSource {
public:
  ScopedMacroState(UnwrappedLine &Line, FormatTokenSource *&TokenSource,
                   FormatToken &ResetToken)
      : Line(Line), TokenSource(TokenSource), ResetToken(ResetToken),
        PreviousTokenSource(TokenSource) {
    TokenSource = this;
    // FIXME: Back up all other state (errors, line indent, etc) and reset after
    // parsing the macro.
    Line.InPPDirective = true;
  }

  ~ScopedMacroState() {
    TokenSource = PreviousTokenSource;
    ResetToken = Token;
    Line.InPPDirective = false;
    Line.Level = 0;  // FIXME: Test + this is obviously incorrect
  }

  virtual FormatToken getNextToken() {
    // FIXME: Write test that breaks due to a missing
    // if (eof()) return createEOF();
    Token = PreviousTokenSource->getNextToken();
    if (eof())
      return createEOF();
    return Token;
  }

private:
  bool eof() {
    return Token.NewlinesBefore > 0 && Token.HasUnescapedNewline;
  }

  FormatToken createEOF() {
    FormatToken FormatTok;
    FormatTok.Tok.startToken();
    FormatTok.Tok.setKind(tok::eof);
    return FormatTok;
  }

  UnwrappedLine &Line;
  FormatTokenSource *&TokenSource;
  FormatToken &ResetToken;

  FormatTokenSource *PreviousTokenSource;

  FormatToken Token;
};

UnwrappedLineParser::UnwrappedLineParser(const FormatStyle &Style,
                                         FormatTokenSource &Tokens,
                                         UnwrappedLineConsumer &Callback)
    : Style(Style), Tokens(&Tokens), Callback(Callback) {
}

bool UnwrappedLineParser::parse() {
  readToken();
  return parseFile();
}

bool UnwrappedLineParser::parseFile() {
  bool Error = parseLevel();
  // Make sure to format the remaining tokens.
  addUnwrappedLine();
  return Error;
}

bool UnwrappedLineParser::parseLevel() {
  bool Error = false;
  do {
    switch (FormatTok.Tok.getKind()) {
    case tok::comment:
      nextToken();
      addUnwrappedLine();
      break;
    case tok::l_brace:
      Error |= parseBlock();
      addUnwrappedLine();
      break;
    case tok::r_brace:
      // Stray '}' is an error.
      return true;
    default:
      parseStatement();
      break;
    }
  } while (!eof());
  return Error;
}

bool UnwrappedLineParser::parseBlock(unsigned AddLevels) {
  assert(FormatTok.Tok.is(tok::l_brace) && "'{' expected");
  nextToken();

  addUnwrappedLine();

  Line.Level += AddLevels;
  parseLevel();
  Line.Level -= AddLevels;

  // FIXME: Add error handling.
  if (!FormatTok.Tok.is(tok::r_brace))
    return true;

  nextToken();
  if (FormatTok.Tok.is(tok::semi))
    nextToken();
  return false;
}

void UnwrappedLineParser::parsePPDirective() {
  assert(FormatTok.Tok.is(tok::hash) && "'#' expected");
  ScopedMacroState MacroState(Line, Tokens, FormatTok);
  nextToken();

  if (FormatTok.Tok.getIdentifierInfo() == NULL) {
    addUnwrappedLine();
    return;
  }

  switch (FormatTok.Tok.getIdentifierInfo()->getPPKeywordID()) {
  case tok::pp_define:
    parsePPDefine();
    break;
  default:
    parsePPUnknown();
    break;
  }
}

void UnwrappedLineParser::parsePPDefine() {
  nextToken();

  if (FormatTok.Tok.getKind() != tok::identifier) {
    parsePPUnknown();
    return;
  }
  nextToken();
  if (FormatTok.Tok.getKind() == tok::l_paren) {
    parseParens();
  }
  addUnwrappedLine();
  Line.Level = 1;
  parseFile();
}

void UnwrappedLineParser::parsePPUnknown() {
  do {
    nextToken();
  } while (!eof());
  addUnwrappedLine();
}

void UnwrappedLineParser::parseComments() {
  // Consume leading line comments, e.g. for branches without compounds.
  while (FormatTok.Tok.is(tok::comment)) {
    nextToken();
    addUnwrappedLine();
  }
}

void UnwrappedLineParser::parseStatement() {
  parseComments();

  int TokenNumber = 0;
  switch (FormatTok.Tok.getKind()) {
  case tok::kw_namespace:
    parseNamespace();
    return;
  case tok::kw_inline:
    nextToken();
    TokenNumber++;
    if (FormatTok.Tok.is(tok::kw_namespace)) {
      parseNamespace();
      return;
    }
    break;
  case tok::kw_public:
  case tok::kw_protected:
  case tok::kw_private:
    parseAccessSpecifier();
    return;
  case tok::kw_if:
    parseIfThenElse();
    return;
  case tok::kw_for:
  case tok::kw_while:
    parseForOrWhileLoop();
    return;
  case tok::kw_do:
    parseDoWhile();
    return;
  case tok::kw_switch:
    parseSwitch();
    return;
  case tok::kw_default:
    nextToken();
    parseLabel();
    return;
  case tok::kw_case:
    parseCaseLabel();
    return;
  default:
    break;
  }
  do {
    ++TokenNumber;
    switch (FormatTok.Tok.getKind()) {
    case tok::kw_enum:
      parseEnum();
      return;
    case tok::semi:
      nextToken();
      addUnwrappedLine();
      return;
    case tok::l_paren:
      parseParens();
      break;
    case tok::l_brace:
      parseBlock();
      addUnwrappedLine();
      return;
    case tok::identifier:
      nextToken();
      if (TokenNumber == 1 && FormatTok.Tok.is(tok::colon)) {
        parseLabel();
        return;
      }
      break;
    case tok::equal:
      nextToken();
      // Skip initializers as they will be formatted by a later step.
      if (FormatTok.Tok.is(tok::l_brace))
        nextToken();
      break;
    default:
      nextToken();
      break;
    }
  } while (!eof());
}

void UnwrappedLineParser::parseParens() {
  assert(FormatTok.Tok.is(tok::l_paren) && "'(' expected.");
  nextToken();
  do {
    switch (FormatTok.Tok.getKind()) {
    case tok::l_paren:
      parseParens();
      break;
    case tok::r_paren:
      nextToken();
      return;
    default:
      nextToken();
      break;
    }
  } while (!eof());
}

void UnwrappedLineParser::parseIfThenElse() {
  assert(FormatTok.Tok.is(tok::kw_if) && "'if' expected");
  nextToken();
  parseParens();
  bool NeedsUnwrappedLine = false;
  if (FormatTok.Tok.is(tok::l_brace)) {
    parseBlock();
    NeedsUnwrappedLine = true;
  } else {
    addUnwrappedLine();
    ++Line.Level;
    parseStatement();
    --Line.Level;
  }
  if (FormatTok.Tok.is(tok::kw_else)) {
    nextToken();
    if (FormatTok.Tok.is(tok::l_brace)) {
      parseBlock();
      addUnwrappedLine();
    } else if (FormatTok.Tok.is(tok::kw_if)) {
      parseIfThenElse();
    } else {
      addUnwrappedLine();
      ++Line.Level;
      parseStatement();
      --Line.Level;
    }
  } else if (NeedsUnwrappedLine) {
    addUnwrappedLine();
  }
}

void UnwrappedLineParser::parseNamespace() {
  assert(FormatTok.Tok.is(tok::kw_namespace) && "'namespace' expected");
  nextToken();
  if (FormatTok.Tok.is(tok::identifier))
    nextToken();
  if (FormatTok.Tok.is(tok::l_brace)) {
    parseBlock(0);
    addUnwrappedLine();
  }
  // FIXME: Add error handling.
}

void UnwrappedLineParser::parseForOrWhileLoop() {
  assert((FormatTok.Tok.is(tok::kw_for) || FormatTok.Tok.is(tok::kw_while)) &&
         "'for' or 'while' expected");
  nextToken();
  parseParens();
  if (FormatTok.Tok.is(tok::l_brace)) {
    parseBlock();
    addUnwrappedLine();
  } else {
    addUnwrappedLine();
    ++Line.Level;
    parseStatement();
    --Line.Level;
  }
}

void UnwrappedLineParser::parseDoWhile() {
  assert(FormatTok.Tok.is(tok::kw_do) && "'do' expected");
  nextToken();
  if (FormatTok.Tok.is(tok::l_brace)) {
    parseBlock();
  } else {
    addUnwrappedLine();
    ++Line.Level;
    parseStatement();
    --Line.Level;
  }

  // FIXME: Add error handling.
  if (!FormatTok.Tok.is(tok::kw_while)) {
    addUnwrappedLine();
    return;
  }

  nextToken();
  parseStatement();
}

void UnwrappedLineParser::parseLabel() {
  // FIXME: remove all asserts.
  assert(FormatTok.Tok.is(tok::colon) && "':' expected");
  nextToken();
  unsigned OldLineLevel = Line.Level;
  if (Line.Level > 0)
    --Line.Level;
  if (FormatTok.Tok.is(tok::l_brace)) {
    parseBlock();
  }
  addUnwrappedLine();
  Line.Level = OldLineLevel;
}

void UnwrappedLineParser::parseCaseLabel() {
  assert(FormatTok.Tok.is(tok::kw_case) && "'case' expected");
  // FIXME: fix handling of complex expressions here.
  do {
    nextToken();
  } while (!eof() && !FormatTok.Tok.is(tok::colon));
  parseLabel();
}

void UnwrappedLineParser::parseSwitch() {
  assert(FormatTok.Tok.is(tok::kw_switch) && "'switch' expected");
  nextToken();
  parseParens();
  if (FormatTok.Tok.is(tok::l_brace)) {
    parseBlock(Style.IndentCaseLabels ? 2 : 1);
    addUnwrappedLine();
  } else {
    addUnwrappedLine();
    Line.Level += (Style.IndentCaseLabels ? 2 : 1);
    parseStatement();
    Line.Level -= (Style.IndentCaseLabels ? 2 : 1);
  }
}

void UnwrappedLineParser::parseAccessSpecifier() {
  nextToken();
  // Otherwise, we don't know what it is, and we'd better keep the next token.
  if (FormatTok.Tok.is(tok::colon))
    nextToken();
  addUnwrappedLine();
}

void UnwrappedLineParser::parseEnum() {
  bool HasContents = false;
  do {
    switch (FormatTok.Tok.getKind()) {
    case tok::l_brace:
      nextToken();
      addUnwrappedLine();
      ++Line.Level;
      parseComments();
      break;
    case tok::l_paren:
      parseParens();
      break;
    case tok::comma:
      nextToken();
      addUnwrappedLine();
      parseComments();
      break;
    case tok::r_brace:
      if (HasContents)
        addUnwrappedLine();
      --Line.Level;
      nextToken();
      break;
    case tok::semi:
      nextToken();
      addUnwrappedLine();
      return;
    default:
      HasContents = true;
      nextToken();
      break;
    }
  } while (!eof());
}

void UnwrappedLineParser::addUnwrappedLine() {
  // Consume trailing comments.
  while (!eof() && FormatTok.NewlinesBefore == 0 &&
         FormatTok.Tok.is(tok::comment)) {
    nextToken();
  }
  Callback.consumeUnwrappedLine(Line);
  Line.Tokens.clear();
}

bool UnwrappedLineParser::eof() const {
  return FormatTok.Tok.is(tok::eof);
}

void UnwrappedLineParser::nextToken() {
  if (eof())
    return;
  Line.Tokens.push_back(FormatTok);
  readToken();
}

void UnwrappedLineParser::readToken() {
  FormatTok = Tokens->getNextToken();
  while (!Line.InPPDirective && FormatTok.Tok.is(tok::hash)) {
    // FIXME: This is incorrect - the correct way is to create a
    // data structure that will construct the parts around the preprocessor
    // directive as a structured \c UnwrappedLine.
    addUnwrappedLine();
    parsePPDirective();
  }
}

}  // end namespace format
}  // end namespace clang
