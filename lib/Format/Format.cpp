//===--- Format.cpp - Format C++ code -------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file implements functions declared in Format.h. This will be
/// split into separate files as we go.
///
/// This is EXPERIMENTAL code under heavy development. It is not in a state yet,
/// where it can be used to format real code.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "format-formatter"

#include "UnwrappedLineParser.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/OperatorPrecedence.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Format/Format.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Lex/Lexer.h"
#include "llvm/Support/Debug.h"
#include <string>

// Uncomment to get debug output from tests:
// #define DEBUG_WITH_TYPE(T, X) do { X; } while(0)

namespace clang {
namespace format {

enum TokenType {
  TT_BinaryOperator,
  TT_BlockComment,
  TT_CastRParen,
  TT_ConditionalExpr,
  TT_CtorInitializerColon,
  TT_ImplicitStringLiteral,
  TT_LineComment,
  TT_ObjCBlockLParen,
  TT_ObjCDecl,
  TT_ObjCMethodSpecifier,
  TT_ObjCMethodExpr,
  TT_ObjCProperty,
  TT_OverloadedOperator,
  TT_PointerOrReference,
  TT_PureVirtualSpecifier,
  TT_TemplateCloser,
  TT_TemplateOpener,
  TT_TrailingUnaryOperator,
  TT_UnaryOperator,
  TT_Unknown
};

enum LineType {
  LT_Invalid,
  LT_Other,
  LT_BuilderTypeCall,
  LT_PreprocessorDirective,
  LT_VirtualFunctionDecl,
  LT_ObjCDecl, // An @interface, @implementation, or @protocol line.
  LT_ObjCMethodDecl,
  LT_ObjCProperty // An @property line.
};

class AnnotatedToken {
public:
  explicit AnnotatedToken(const FormatToken &FormatTok)
      : FormatTok(FormatTok), Type(TT_Unknown), SpaceRequiredBefore(false),
        CanBreakBefore(false), MustBreakBefore(false),
        ClosesTemplateDeclaration(false), MatchingParen(NULL), Parent(NULL) {}

  bool is(tok::TokenKind Kind) const { return FormatTok.Tok.is(Kind); }
  bool isNot(tok::TokenKind Kind) const { return FormatTok.Tok.isNot(Kind); }

  bool isObjCAtKeyword(tok::ObjCKeywordKind Kind) const {
    return FormatTok.Tok.isObjCAtKeyword(Kind);
  }

  FormatToken FormatTok;

  TokenType Type;

  bool SpaceRequiredBefore;
  bool CanBreakBefore;
  bool MustBreakBefore;

  bool ClosesTemplateDeclaration;

  AnnotatedToken *MatchingParen;

  /// \brief The total length of the line up to and including this token.
  unsigned TotalLength;

  std::vector<AnnotatedToken> Children;
  AnnotatedToken *Parent;

  const AnnotatedToken *getPreviousNoneComment() const {
    AnnotatedToken *Tok = Parent;
    while (Tok != NULL && Tok->is(tok::comment))
      Tok = Tok->Parent;
    return Tok;
  }
};

class AnnotatedLine {
public:
  AnnotatedLine(const UnwrappedLine &Line)
      : First(Line.Tokens.front()), Level(Line.Level),
        InPPDirective(Line.InPPDirective) {
    assert(!Line.Tokens.empty());
    AnnotatedToken *Current = &First;
    for (std::list<FormatToken>::const_iterator I = ++Line.Tokens.begin(),
                                                E = Line.Tokens.end();
         I != E; ++I) {
      Current->Children.push_back(AnnotatedToken(*I));
      Current->Children[0].Parent = Current;
      Current = &Current->Children[0];
    }
    Last = Current;
  }
  AnnotatedLine(const AnnotatedLine &Other)
      : First(Other.First), Type(Other.Type), Level(Other.Level),
        InPPDirective(Other.InPPDirective) {
    Last = &First;
    while (!Last->Children.empty()) {
      Last->Children[0].Parent = Last;
      Last = &Last->Children[0];
    }
  }

  AnnotatedToken First;
  AnnotatedToken *Last;

  LineType Type;
  unsigned Level;
  bool InPPDirective;
};

static prec::Level getPrecedence(const AnnotatedToken &Tok) {
  return getBinOpPrecedence(Tok.FormatTok.Tok.getKind(), true, true);
}

FormatStyle getLLVMStyle() {
  FormatStyle LLVMStyle;
  LLVMStyle.ColumnLimit = 80;
  LLVMStyle.MaxEmptyLinesToKeep = 1;
  LLVMStyle.PointerAndReferenceBindToType = false;
  LLVMStyle.AccessModifierOffset = -2;
  LLVMStyle.SplitTemplateClosingGreater = true;
  LLVMStyle.IndentCaseLabels = false;
  LLVMStyle.SpacesBeforeTrailingComments = 1;
  LLVMStyle.BinPackParameters = true;
  LLVMStyle.ConstructorInitializerAllOnOneLineOrOnePerLine = false;
  LLVMStyle.AllowShortIfStatementsOnASingleLine = false;
  LLVMStyle.ObjCSpaceBeforeProtocolList = true;
  return LLVMStyle;
}

FormatStyle getGoogleStyle() {
  FormatStyle GoogleStyle;
  GoogleStyle.ColumnLimit = 80;
  GoogleStyle.MaxEmptyLinesToKeep = 1;
  GoogleStyle.PointerAndReferenceBindToType = true;
  GoogleStyle.AccessModifierOffset = -1;
  GoogleStyle.SplitTemplateClosingGreater = false;
  GoogleStyle.IndentCaseLabels = true;
  GoogleStyle.SpacesBeforeTrailingComments = 2;
  GoogleStyle.BinPackParameters = false;
  GoogleStyle.ConstructorInitializerAllOnOneLineOrOnePerLine = true;
  GoogleStyle.AllowShortIfStatementsOnASingleLine = false;
  GoogleStyle.ObjCSpaceBeforeProtocolList = false;
  return GoogleStyle;
}

FormatStyle getChromiumStyle() {
  FormatStyle ChromiumStyle = getGoogleStyle();
  ChromiumStyle.AllowShortIfStatementsOnASingleLine = false;
  return ChromiumStyle;
}

struct OptimizationParameters {
  unsigned PenaltyIndentLevel;
  unsigned PenaltyLevelDecrease;
  unsigned PenaltyExcessCharacter;
};

/// \brief Manages the whitespaces around tokens and their replacements.
///
/// This includes special handling for certain constructs, e.g. the alignment of
/// trailing line comments.
class WhitespaceManager {
public:
  WhitespaceManager(SourceManager &SourceMgr) : SourceMgr(SourceMgr) {}

  /// \brief Replaces the whitespace in front of \p Tok. Only call once for
  /// each \c AnnotatedToken.
  void replaceWhitespace(const AnnotatedToken &Tok, unsigned NewLines,
                         unsigned Spaces, unsigned WhitespaceStartColumn,
                         const FormatStyle &Style) {
    // 2+ newlines mean an empty line separating logic scopes.
    if (NewLines >= 2)
      alignComments();

    // Align line comments if they are trailing or if they continue other
    // trailing comments.
    if (Tok.Type == TT_LineComment &&
        (Tok.Parent != NULL || !Comments.empty())) {
      if (Style.ColumnLimit >=
          Spaces + WhitespaceStartColumn + Tok.FormatTok.TokenLength) {
        Comments.push_back(StoredComment());
        Comments.back().Tok = Tok.FormatTok;
        Comments.back().Spaces = Spaces;
        Comments.back().NewLines = NewLines;
        Comments.back().MinColumn = WhitespaceStartColumn + Spaces;
        Comments.back().MaxColumn = Style.ColumnLimit -
                                    Spaces - Tok.FormatTok.TokenLength;
        return;
      }
    }

    // If this line does not have a trailing comment, align the stored comments.
    if (Tok.Children.empty() && Tok.Type != TT_LineComment)
      alignComments();
    storeReplacement(Tok.FormatTok,
                     std::string(NewLines, '\n') + std::string(Spaces, ' '));
  }

  /// \brief Like \c replaceWhitespace, but additionally adds right-aligned
  /// backslashes to escape newlines inside a preprocessor directive.
  ///
  /// This function and \c replaceWhitespace have the same behavior if
  /// \c Newlines == 0.
  void replacePPWhitespace(const AnnotatedToken &Tok, unsigned NewLines,
                           unsigned Spaces, unsigned WhitespaceStartColumn,
                           const FormatStyle &Style) {
    std::string NewLineText;
    if (NewLines > 0) {
      unsigned Offset = std::min<int>(Style.ColumnLimit - 1,
                                      WhitespaceStartColumn);
      for (unsigned i = 0; i < NewLines; ++i) {
        NewLineText += std::string(Style.ColumnLimit - Offset - 1, ' ');
        NewLineText += "\\\n";
        Offset = 0;
      }
    }
    storeReplacement(Tok.FormatTok, NewLineText + std::string(Spaces, ' '));
  }

  /// \brief Returns all the \c Replacements created during formatting.
  const tooling::Replacements &generateReplacements() {
    alignComments();
    return Replaces;
  }

private:
  /// \brief Structure to store a comment for later layout and alignment.
  struct StoredComment {
    FormatToken Tok;
    unsigned MinColumn;
    unsigned MaxColumn;
    unsigned NewLines;
    unsigned Spaces;
  };
  SmallVector<StoredComment, 16> Comments;
  typedef SmallVector<StoredComment, 16>::iterator comment_iterator;

  /// \brief Try to align all stashed comments.
  void alignComments() {
    unsigned MinColumn = 0;
    unsigned MaxColumn = UINT_MAX;
    comment_iterator Start = Comments.begin();
    for (comment_iterator I = Comments.begin(), E = Comments.end(); I != E;
         ++I) {
      if (I->MinColumn > MaxColumn || I->MaxColumn < MinColumn) {
        alignComments(Start, I, MinColumn);
        MinColumn = I->MinColumn;
        MaxColumn = I->MaxColumn;
        Start = I;
      } else {
        MinColumn = std::max(MinColumn, I->MinColumn);
        MaxColumn = std::min(MaxColumn, I->MaxColumn);
      }
    }
    alignComments(Start, Comments.end(), MinColumn);
    Comments.clear();
  }

  /// \brief Put all the comments between \p I and \p E into \p Column.
  void alignComments(comment_iterator I, comment_iterator E, unsigned Column) {
    while (I != E) {
      unsigned Spaces = I->Spaces + Column - I->MinColumn;
      storeReplacement(I->Tok, std::string(I->NewLines, '\n') +
                       std::string(Spaces, ' '));
      ++I;
    }
  }

  /// \brief Stores \p Text as the replacement for the whitespace in front of
  /// \p Tok.
  void storeReplacement(const FormatToken &Tok, const std::string Text) {
    Replaces.insert(tooling::Replacement(SourceMgr, Tok.WhiteSpaceStart,
                                         Tok.WhiteSpaceLength, Text));
  }

  SourceManager &SourceMgr;
  tooling::Replacements Replaces;
};

/// \brief Returns if a token is an Objective-C selector name.
///
/// For example, "bar" is a selector name in [foo bar:(4 + 5)].
static bool isObjCSelectorName(const AnnotatedToken &Tok) {
  return Tok.is(tok::identifier) && !Tok.Children.empty() &&
         Tok.Children[0].is(tok::colon) &&
         Tok.Children[0].Type == TT_ObjCMethodExpr;
}

class UnwrappedLineFormatter {
public:
  UnwrappedLineFormatter(const FormatStyle &Style, SourceManager &SourceMgr,
                         const AnnotatedLine &Line, unsigned FirstIndent,
                         const AnnotatedToken &RootToken,
                         WhitespaceManager &Whitespaces, bool StructuralError)
      : Style(Style), SourceMgr(SourceMgr), Line(Line),
        FirstIndent(FirstIndent), RootToken(RootToken),
        Whitespaces(Whitespaces) {
    Parameters.PenaltyIndentLevel = 20;
    Parameters.PenaltyLevelDecrease = 30;
    Parameters.PenaltyExcessCharacter = 1000000;
  }

  /// \brief Formats an \c UnwrappedLine.
  ///
  /// \returns The column after the last token in the last line of the
  /// \c UnwrappedLine.
  unsigned format() {
    // Initialize state dependent on indent.
    LineState State;
    State.Column = FirstIndent;
    State.NextToken = &RootToken;
    State.Stack.push_back(ParenState(FirstIndent + 4, FirstIndent));
    State.ForLoopVariablePos = 0;
    State.LineContainsContinuedForLoopSection = false;
    State.StartOfLineLevel = 1;

    DEBUG({
      DebugTokenState(*State.NextToken);
    });

    // The first token has already been indented and thus consumed.
    moveStateToNextToken(State);

    // Start iterating at 1 as we have correctly formatted of Token #0 above.
    while (State.NextToken != NULL) {
      if (State.NextToken->Type == TT_ImplicitStringLiteral) {
        // Calculating the column is important for aligning trailing comments.
        // FIXME: This does not seem to happen in conjunction with escaped
        // newlines. If it does, fix!
        State.Column += State.NextToken->FormatTok.WhiteSpaceLength +
                        State.NextToken->FormatTok.TokenLength;
        State.NextToken = State.NextToken->Children.empty() ? NULL :
                          &State.NextToken->Children[0];
      } else if (Line.Last->TotalLength <= getColumnLimit() - FirstIndent) {
        addTokenToState(false, false, State);
      } else {
        unsigned NoBreak = calcPenalty(State, false, UINT_MAX);
        unsigned Break = calcPenalty(State, true, NoBreak);
        DEBUG({
          if (Break < NoBreak)
            llvm::errs() << "\n";
          else
            llvm::errs() << " ";
          llvm::errs() << "<";
          DebugPenalty(Break, Break < NoBreak);
          llvm::errs() << "/";
          DebugPenalty(NoBreak, !(Break < NoBreak));
          llvm::errs() << "> ";
          DebugTokenState(*State.NextToken);
        });
        addTokenToState(Break < NoBreak, false, State);
        if (State.NextToken != NULL &&
            State.NextToken->Parent->Type == TT_CtorInitializerColon) {
          if (Style.ConstructorInitializerAllOnOneLineOrOnePerLine &&
              Line.Last->TotalLength > getColumnLimit() - State.Column - 1)
            State.Stack.back().BreakAfterComma = true;
        }
      }
    }
    DEBUG(llvm::errs() << "\n");
    return State.Column;
  }

private:
  void DebugTokenState(const AnnotatedToken &AnnotatedTok) {
    const Token &Tok = AnnotatedTok.FormatTok.Tok;
    llvm::errs()
        << StringRef(SourceMgr.getCharacterData(Tok.getLocation()),
                     Tok.getLength());
    llvm::errs();
  }

  void DebugPenalty(unsigned Penalty, bool Winner) {
    llvm::errs().changeColor(Winner ? raw_ostream::GREEN : raw_ostream::RED);
    if (Penalty == UINT_MAX)
      llvm::errs() << "MAX";
    else
      llvm::errs() << Penalty;
    llvm::errs().resetColor();
  }

  struct ParenState {
    ParenState(unsigned Indent, unsigned LastSpace)
        : Indent(Indent), LastSpace(LastSpace), FirstLessLess(0),
          BreakBeforeClosingBrace(false), BreakAfterComma(false),
          HasMultiParameterLine(false) {}

    /// \brief The position to which a specific parenthesis level needs to be
    /// indented.
    unsigned Indent;

    /// \brief The position of the last space on each level.
    ///
    /// Used e.g. to break like:
    /// functionCall(Parameter, otherCall(
    ///                             OtherParameter));
    unsigned LastSpace;

    /// \brief The position the first "<<" operator encountered on each level.
    ///
    /// Used to align "<<" operators. 0 if no such operator has been encountered
    /// on a level.
    unsigned FirstLessLess;

    /// \brief Whether a newline needs to be inserted before the block's closing
    /// brace.
    ///
    /// We only want to insert a newline before the closing brace if there also
    /// was a newline after the beginning left brace.
    bool BreakBeforeClosingBrace;

    bool BreakAfterComma;
    bool HasMultiParameterLine;

    bool operator<(const ParenState &Other) const {
      if (Indent != Other.Indent)
        return Indent < Other.Indent;
      if (LastSpace != Other.LastSpace)
        return LastSpace < Other.LastSpace;
      if (FirstLessLess != Other.FirstLessLess)
        return FirstLessLess < Other.FirstLessLess;
      if (BreakBeforeClosingBrace != Other.BreakBeforeClosingBrace)
        return BreakBeforeClosingBrace;
      if (BreakAfterComma != Other.BreakAfterComma)
        return BreakAfterComma;
      if (HasMultiParameterLine != Other.HasMultiParameterLine)
        return HasMultiParameterLine;
      return false;
    }
  };

  /// \brief The current state when indenting a unwrapped line.
  ///
  /// As the indenting tries different combinations this is copied by value.
  struct LineState {
    /// \brief The number of used columns in the current line.
    unsigned Column;

    /// \brief The token that needs to be next formatted.
    const AnnotatedToken *NextToken;

    /// \brief The parenthesis level of the first token on the current line.
    unsigned StartOfLineLevel;

    /// \brief The column of the first variable in a for-loop declaration.
    ///
    /// Used to align the second variable if necessary.
    unsigned ForLoopVariablePos;

    /// \brief \c true if this line contains a continued for-loop section.
    bool LineContainsContinuedForLoopSection;

    /// \brief A stack keeping track of properties applying to parenthesis
    /// levels.
    std::vector<ParenState> Stack;

    /// \brief Comparison operator to be able to used \c LineState in \c map.
    bool operator<(const LineState &Other) const {
      if (Other.NextToken != NextToken)
        return Other.NextToken > NextToken;
      if (Other.Column != Column)
        return Other.Column > Column;
      if (Other.StartOfLineLevel != StartOfLineLevel)
        return Other.StartOfLineLevel > StartOfLineLevel;
      if (Other.ForLoopVariablePos != ForLoopVariablePos)
        return Other.ForLoopVariablePos < ForLoopVariablePos;
      if (Other.LineContainsContinuedForLoopSection !=
          LineContainsContinuedForLoopSection)
        return LineContainsContinuedForLoopSection;
      return Other.Stack < Stack;
    }
  };

  /// \brief Appends the next token to \p State and updates information
  /// necessary for indentation.
  ///
  /// Puts the token on the current line if \p Newline is \c true and adds a
  /// line break and necessary indentation otherwise.
  ///
  /// If \p DryRun is \c false, also creates and stores the required
  /// \c Replacement.
  void addTokenToState(bool Newline, bool DryRun, LineState &State) {
    const AnnotatedToken &Current = *State.NextToken;
    const AnnotatedToken &Previous = *State.NextToken->Parent;
    assert(State.Stack.size());
    unsigned ParenLevel = State.Stack.size() - 1;

    if (Newline) {
      unsigned WhitespaceStartColumn = State.Column;
      if (Current.is(tok::r_brace)) {
        State.Column = Line.Level * 2;
      } else if (Current.is(tok::string_literal) &&
                 Previous.is(tok::string_literal)) {
        State.Column = State.Column - Previous.FormatTok.TokenLength;
      } else if (Current.is(tok::lessless) &&
                 State.Stack[ParenLevel].FirstLessLess != 0) {
        State.Column = State.Stack[ParenLevel].FirstLessLess;
      } else if (ParenLevel != 0 &&
                 (Previous.is(tok::equal) || Current.is(tok::arrow) ||
                  Current.is(tok::period) || Previous.is(tok::question) ||
                  Previous.Type == TT_ConditionalExpr)) {
        // Indent and extra 4 spaces after if we know the current expression is
        // continued.  Don't do that on the top level, as we already indent 4
        // there.
        State.Column = State.Stack[ParenLevel].Indent + 4;
      } else if (RootToken.is(tok::kw_for) && Previous.is(tok::comma)) {
        State.Column = State.ForLoopVariablePos;
      } else if (State.NextToken->Parent->ClosesTemplateDeclaration) {
        State.Column = State.Stack[ParenLevel].Indent - 4;
      } else {
        State.Column = State.Stack[ParenLevel].Indent;
      }

      // A line starting with a closing brace is assumed to be correct for the
      // same level as before the opening brace.
      State.StartOfLineLevel = ParenLevel + (Current.is(tok::r_brace) ? 0 : 1);

      if (RootToken.is(tok::kw_for))
        State.LineContainsContinuedForLoopSection = Previous.isNot(tok::semi);

      if (!DryRun) {
        if (!Line.InPPDirective)
          Whitespaces.replaceWhitespace(Current, 1, State.Column,
                                        WhitespaceStartColumn, Style);
        else
          Whitespaces.replacePPWhitespace(Current, 1, State.Column,
                                          WhitespaceStartColumn, Style);
      }

      State.Stack[ParenLevel].LastSpace = State.Column;
      if (Current.is(tok::colon) && State.NextToken->Type != TT_ConditionalExpr)
        State.Stack[ParenLevel].Indent += 2;
    } else {
      if (Current.is(tok::equal) && RootToken.is(tok::kw_for))
        State.ForLoopVariablePos = State.Column -
                                   Previous.FormatTok.TokenLength;

      unsigned Spaces = State.NextToken->SpaceRequiredBefore ? 1 : 0;
      if (State.NextToken->Type == TT_LineComment)
        Spaces = Style.SpacesBeforeTrailingComments;

      if (!DryRun)
        Whitespaces.replaceWhitespace(Current, 0, Spaces, State.Column, Style);

      // FIXME: Do we need to do this for assignments nested in other
      // expressions?
      if (RootToken.isNot(tok::kw_for) && ParenLevel == 0 &&
          (getPrecedence(Previous) == prec::Assignment ||
           Previous.is(tok::kw_return)))
        State.Stack[ParenLevel].Indent = State.Column + Spaces;
      if (Previous.is(tok::l_paren) || Previous.is(tok::l_brace) ||
          State.NextToken->Parent->Type == TT_TemplateOpener)
        State.Stack[ParenLevel].Indent = State.Column + Spaces;
      if (Current.getPreviousNoneComment()->is(tok::comma) &&
          Current.isNot(tok::comment))
        State.Stack[ParenLevel].HasMultiParameterLine = true;


      // Top-level spaces that are not part of assignments are exempt as that
      // mostly leads to better results.
      State.Column += Spaces;
      if (Spaces > 0 &&
          (ParenLevel != 0 || getPrecedence(Previous) == prec::Assignment))
        State.Stack[ParenLevel].LastSpace = State.Column;
    }

    // If we break after an {, we should also break before the corresponding }.
    if (Newline && Previous.is(tok::l_brace))
      State.Stack.back().BreakBeforeClosingBrace = true;

    // If we are breaking after '(', '{', '<' or ',', we need to break after
    // future commas as well to avoid bin packing.
    if (!Style.BinPackParameters && Newline &&
        (Previous.is(tok::comma) || Previous.is(tok::l_paren) ||
         Previous.is(tok::l_brace) || Previous.Type == TT_TemplateOpener))
      State.Stack.back().BreakAfterComma = true;

    moveStateToNextToken(State);
  }

  /// \brief Mark the next token as consumed in \p State and modify its stacks
  /// accordingly.
  void moveStateToNextToken(LineState &State) {
    const AnnotatedToken &Current = *State.NextToken;
    assert(State.Stack.size());

    if (Current.is(tok::lessless) && State.Stack.back().FirstLessLess == 0)
      State.Stack.back().FirstLessLess = State.Column;

    // If we encounter an opening (, [, { or <, we add a level to our stacks to
    // prepare for the following tokens.
    if (Current.is(tok::l_paren) || Current.is(tok::l_square) ||
        Current.is(tok::l_brace) ||
        State.NextToken->Type == TT_TemplateOpener) {
      unsigned NewIndent;
      if (Current.is(tok::l_brace)) {
        // FIXME: This does not work with nested static initializers.
        // Implement a better handling for static initializers and similar
        // constructs.
        NewIndent = Line.Level * 2 + 2;
      } else {
        NewIndent = 4 + State.Stack.back().LastSpace;
      }
      State.Stack.push_back(
          ParenState(NewIndent, State.Stack.back().LastSpace));

      // If the entire set of parameters will not fit on the current line, we
      // will need to break after commas on this level to avoid bin-packing.
      if (!Style.BinPackParameters && Current.MatchingParen != NULL &&
          !Current.Children.empty()) {
        if (getColumnLimit() < State.Column + Current.FormatTok.TokenLength +
            Current.MatchingParen->TotalLength -
            Current.Children[0].TotalLength) {
          State.Stack.back().BreakAfterComma = true;
        }
      }
    }

    // If we encounter a closing ), ], } or >, we can remove a level from our
    // stacks.
    if (Current.is(tok::r_paren) || Current.is(tok::r_square) ||
        (Current.is(tok::r_brace) && State.NextToken != &RootToken) ||
        State.NextToken->Type == TT_TemplateCloser) {
      State.Stack.pop_back();
    }

    if (State.NextToken->Children.empty())
      State.NextToken = NULL;
    else
      State.NextToken = &State.NextToken->Children[0];

    State.Column += Current.FormatTok.TokenLength;
  }

  /// \brief Calculate the penalty for splitting after the token at \p Index.
  unsigned splitPenalty(const AnnotatedToken &Tok) {
    const AnnotatedToken &Left = Tok;
    const AnnotatedToken &Right = Tok.Children[0];

    if (Left.is(tok::l_brace) && Right.isNot(tok::l_brace))
      return 50;
    if (Left.is(tok::equal) && Right.is(tok::l_brace))
      return 150;

    // In for-loops, prefer breaking at ',' and ';'.
    if (RootToken.is(tok::kw_for) &&
        (Left.isNot(tok::comma) && Left.isNot(tok::semi)))
      return 20;

    if (Left.is(tok::semi) || Left.is(tok::comma))
      return 0;

    // In Objective-C method expressions, prefer breaking before "param:" over
    // breaking after it.
    if (isObjCSelectorName(Right))
      return 0;
    if (Right.is(tok::colon) && Right.Type == TT_ObjCMethodExpr)
      return 20;

    if (Left.is(tok::l_paren))
      return 20;

    if (Left.is(tok::question) || Left.Type == TT_ConditionalExpr)
      return prec::Assignment;
    prec::Level Level = getPrecedence(Left);

    // Breaking after an assignment leads to a bad result as the two sides of
    // the assignment are visually very close together.
    if (Level == prec::Assignment)
      return 50;

    if (Level != prec::Unknown)
      return Level;

    if (Right.is(tok::arrow) || Right.is(tok::period)) {
      if (Left.is(tok::r_paren) && Line.Type == LT_BuilderTypeCall)
        return 15; // Should be smaller than breaking at a nested comma.
      return 150;
    }

    return 3;
  }

  unsigned getColumnLimit() {
    return Style.ColumnLimit - (Line.InPPDirective ? 1 : 0);
  }

  /// \brief Calculate the number of lines needed to format the remaining part
  /// of the unwrapped line.
  ///
  /// Assumes the formatting so far has led to
  /// the \c LineSta \p State. If \p NewLine is set, a new line will be
  /// added after the previous token.
  ///
  /// \param StopAt is used for optimization. If we can determine that we'll
  /// definitely need at least \p StopAt additional lines, we already know of a
  /// better solution.
  unsigned calcPenalty(LineState State, bool NewLine, unsigned StopAt) {
    // We are at the end of the unwrapped line, so we don't need any more lines.
    if (State.NextToken == NULL)
      return 0;

    if (!NewLine && State.NextToken->MustBreakBefore)
      return UINT_MAX;
    if (NewLine && !State.NextToken->CanBreakBefore &&
        !(State.NextToken->is(tok::r_brace) &&
          State.Stack.back().BreakBeforeClosingBrace))
      return UINT_MAX;
    if (!NewLine && State.NextToken->is(tok::r_brace) &&
        State.Stack.back().BreakBeforeClosingBrace)
      return UINT_MAX;
    if (!NewLine && State.NextToken->Parent->is(tok::semi) &&
        State.LineContainsContinuedForLoopSection)
      return UINT_MAX;
    if (!NewLine && State.NextToken->Parent->is(tok::comma) &&
        State.NextToken->isNot(tok::comment) &&
        State.Stack.back().BreakAfterComma)
      return UINT_MAX;
    // Trying to insert a parameter on a new line if there are already more than
    // one parameter on the current line is bin packing.
    if (NewLine && State.NextToken->Parent->is(tok::comma) &&
        State.Stack.back().HasMultiParameterLine && !Style.BinPackParameters)
      return UINT_MAX;
    if (!NewLine && (State.NextToken->Type == TT_CtorInitializerColon ||
                     (State.NextToken->Parent->ClosesTemplateDeclaration &&
                      State.Stack.size() == 1)))
      return UINT_MAX;

    unsigned CurrentPenalty = 0;
    if (NewLine) {
      CurrentPenalty += Parameters.PenaltyIndentLevel * State.Stack.size() +
                        splitPenalty(*State.NextToken->Parent);
    } else {
      if (State.Stack.size() < State.StartOfLineLevel &&
          State.NextToken->is(tok::identifier))
        CurrentPenalty += Parameters.PenaltyLevelDecrease *
                          (State.StartOfLineLevel - State.Stack.size());
    }

    addTokenToState(NewLine, true, State);

    // Exceeding column limit is bad, assign penalty.
    if (State.Column > getColumnLimit()) {
      unsigned ExcessCharacters = State.Column - getColumnLimit();
      CurrentPenalty += Parameters.PenaltyExcessCharacter * ExcessCharacters;
    }

    if (StopAt <= CurrentPenalty)
      return UINT_MAX;
    StopAt -= CurrentPenalty;
    StateMap::iterator I = Memory.find(State);
    if (I != Memory.end()) {
      // If this state has already been examined, we can safely return the
      // previous result if we
      // - have not hit the optimatization (and thus returned UINT_MAX) OR
      // - are now computing for a smaller or equal StopAt.
      unsigned SavedResult = I->second.first;
      unsigned SavedStopAt = I->second.second;
      if (SavedResult != UINT_MAX)
        return SavedResult + CurrentPenalty;
      else if (StopAt <= SavedStopAt)
        return UINT_MAX;
    }

    unsigned NoBreak = calcPenalty(State, false, StopAt);
    unsigned WithBreak = calcPenalty(State, true, std::min(StopAt, NoBreak));
    unsigned Result = std::min(NoBreak, WithBreak);

    // We have to store 'Result' without adding 'CurrentPenalty' as the latter
    // can depend on 'NewLine'.
    Memory[State] = std::pair<unsigned, unsigned>(Result, StopAt);

    return Result == UINT_MAX ? UINT_MAX : Result + CurrentPenalty;
  }

  FormatStyle Style;
  SourceManager &SourceMgr;
  const AnnotatedLine &Line;
  const unsigned FirstIndent;
  const AnnotatedToken &RootToken;
  WhitespaceManager &Whitespaces;

  // A map from an indent state to a pair (Result, Used-StopAt).
  typedef std::map<LineState, std::pair<unsigned, unsigned> > StateMap;
  StateMap Memory;

  OptimizationParameters Parameters;
};

/// \brief Determines extra information about the tokens comprising an
/// \c UnwrappedLine.
class TokenAnnotator {
public:
  TokenAnnotator(const FormatStyle &Style, SourceManager &SourceMgr, Lexer &Lex,
                 AnnotatedLine &Line)
      : Style(Style), SourceMgr(SourceMgr), Lex(Lex), Line(Line) {}

  /// \brief A parser that gathers additional information about tokens.
  ///
  /// The \c TokenAnnotator tries to matches parenthesis and square brakets and
  /// store a parenthesis levels. It also tries to resolve matching "<" and ">"
  /// into template parameter lists.
  class AnnotatingParser {
  public:
    AnnotatingParser(AnnotatedToken &RootToken)
        : CurrentToken(&RootToken), KeywordVirtualFound(false),
          ColonIsObjCMethodExpr(false) {}

    /// \brief A helper class to manage AnnotatingParser::ColonIsObjCMethodExpr.
    struct ObjCSelectorRAII {
      AnnotatingParser &P;
      bool ColonWasObjCMethodExpr;

      ObjCSelectorRAII(AnnotatingParser &P)
          : P(P), ColonWasObjCMethodExpr(P.ColonIsObjCMethodExpr) {}

      ~ObjCSelectorRAII() { P.ColonIsObjCMethodExpr = ColonWasObjCMethodExpr; }

      void markStart(AnnotatedToken &Left) {
        P.ColonIsObjCMethodExpr = true;
        Left.Type = TT_ObjCMethodExpr;
      }

      void markEnd(AnnotatedToken &Right) { Right.Type = TT_ObjCMethodExpr; }
    };


    bool parseAngle() {
      if (CurrentToken == NULL)
        return false;
      AnnotatedToken *Left = CurrentToken->Parent;
      while (CurrentToken != NULL) {
        if (CurrentToken->is(tok::greater)) {
          Left->MatchingParen = CurrentToken;
          CurrentToken->MatchingParen = Left;
          CurrentToken->Type = TT_TemplateCloser;
          next();
          return true;
        }
        if (CurrentToken->is(tok::r_paren) || CurrentToken->is(tok::r_square) ||
            CurrentToken->is(tok::r_brace))
          return false;
        if (CurrentToken->is(tok::pipepipe) || CurrentToken->is(tok::ampamp) ||
            CurrentToken->is(tok::question) || CurrentToken->is(tok::colon))
          return false;
        if (!consumeToken())
          return false;
      }
      return false;
    }

    bool parseParens(bool LookForDecls = false) {
      if (CurrentToken == NULL)
        return false;
      bool StartsObjCMethodExpr = false;
      AnnotatedToken *Left = CurrentToken->Parent;
      if (CurrentToken->is(tok::caret)) {
        // ^( starts a block.
        Left->Type = TT_ObjCBlockLParen;
      } else if (AnnotatedToken *MaybeSel = Left->Parent) {
        // @selector( starts a selector.
        if (MaybeSel->isObjCAtKeyword(tok::objc_selector) && MaybeSel->Parent &&
            MaybeSel->Parent->is(tok::at)) {
          StartsObjCMethodExpr = true;
        }
      }

      ObjCSelectorRAII objCSelector(*this);
      if (StartsObjCMethodExpr)
        objCSelector.markStart(*Left);

      while (CurrentToken != NULL) {
        // LookForDecls is set when "if (" has been seen. Check for
        // 'identifier' '*' 'identifier' followed by not '=' -- this
        // '*' has to be a binary operator but determineStarAmpUsage() will
        // categorize it as an unary operator, so set the right type here.
        if (LookForDecls && !CurrentToken->Children.empty()) {
          AnnotatedToken &Prev = *CurrentToken->Parent;
          AnnotatedToken &Next = CurrentToken->Children[0];
          if (Prev.Parent->is(tok::identifier) &&
              (Prev.is(tok::star) || Prev.is(tok::amp)) &&
              CurrentToken->is(tok::identifier) && Next.isNot(tok::equal)) {
            Prev.Type = TT_BinaryOperator;
            LookForDecls = false;
          }
        }

        if (CurrentToken->is(tok::r_paren)) {
          Left->MatchingParen = CurrentToken;
          CurrentToken->MatchingParen = Left;

          if (StartsObjCMethodExpr)
            objCSelector.markEnd(*CurrentToken);

          next();
          return true;
        }
        if (CurrentToken->is(tok::r_square) || CurrentToken->is(tok::r_brace))
          return false;
        if (!consumeToken())
          return false;
      }
      return false;
    }

    bool parseSquare() {
      if (!CurrentToken)
        return false;

      // A '[' could be an index subscript (after an indentifier or after
      // ')' or ']'), or it could be the start of an Objective-C method
      // expression.
      AnnotatedToken *Left = CurrentToken->Parent;
      bool StartsObjCMethodExpr =
          !Left->Parent || Left->Parent->is(tok::colon) ||
          Left->Parent->is(tok::l_square) || Left->Parent->is(tok::l_paren) ||
          Left->Parent->is(tok::kw_return) || Left->Parent->is(tok::kw_throw) ||
          getBinOpPrecedence(Left->Parent->FormatTok.Tok.getKind(),
                             true, true) > prec::Unknown;

      ObjCSelectorRAII objCSelector(*this);
      if (StartsObjCMethodExpr)
        objCSelector.markStart(*Left);

      while (CurrentToken != NULL) {
        if (CurrentToken->is(tok::r_square)) {
          if (!CurrentToken->Children.empty() &&
              CurrentToken->Children[0].is(tok::l_paren)) {
            // An ObjC method call can't be followed by an open parenthesis.
            // FIXME: Do we incorrectly label ":" with this?
            StartsObjCMethodExpr = false;
            Left->Type = TT_Unknown;
	  }
          if (StartsObjCMethodExpr)
            objCSelector.markEnd(*CurrentToken);
          Left->MatchingParen = CurrentToken;
          CurrentToken->MatchingParen = Left;
          next();
          return true;
        }
        if (CurrentToken->is(tok::r_paren) || CurrentToken->is(tok::r_brace))
          return false;
        if (!consumeToken())
          return false;
      }
      return false;
    }

    bool parseBrace() {
      // Lines are fine to end with '{'.
      if (CurrentToken == NULL)
        return true;
      AnnotatedToken *Left = CurrentToken->Parent;
      while (CurrentToken != NULL) {
        if (CurrentToken->is(tok::r_brace)) {
          Left->MatchingParen = CurrentToken;
          CurrentToken->MatchingParen = Left;
          next();
          return true;
        }
        if (CurrentToken->is(tok::r_paren) || CurrentToken->is(tok::r_square))
          return false;
        if (!consumeToken())
          return false;
      }
      return true;
    }

    bool parseConditional() {
      while (CurrentToken != NULL) {
        if (CurrentToken->is(tok::colon)) {
          CurrentToken->Type = TT_ConditionalExpr;
          next();
          return true;
        }
        if (!consumeToken())
          return false;
      }
      return false;
    }

    bool parseTemplateDeclaration() {
      if (CurrentToken != NULL && CurrentToken->is(tok::less)) {
        CurrentToken->Type = TT_TemplateOpener;
        next();
        if (!parseAngle())
          return false;
        CurrentToken->Parent->ClosesTemplateDeclaration = true;
        return true;
      }
      return false;
    }

    bool consumeToken() {
      AnnotatedToken *Tok = CurrentToken;
      next();
      switch (Tok->FormatTok.Tok.getKind()) {
      case tok::plus:
      case tok::minus:
        // At the start of the line, +/- specific ObjectiveC method
        // declarations.
        if (Tok->Parent == NULL)
          Tok->Type = TT_ObjCMethodSpecifier;
        break;
      case tok::colon:
        // Colons from ?: are handled in parseConditional().
        if (Tok->Parent->is(tok::r_paren))
          Tok->Type = TT_CtorInitializerColon;
        if (ColonIsObjCMethodExpr)
          Tok->Type = TT_ObjCMethodExpr;
        break;
      case tok::kw_if:
      case tok::kw_while:
        if (CurrentToken->is(tok::l_paren)) {
          next();
          if (!parseParens(/*LookForDecls=*/true))
            return false;
        }
        break;
      case tok::l_paren:
        if (!parseParens())
          return false;
        break;
      case tok::l_square:
        if (!parseSquare())
          return false;
        break;
      case tok::l_brace:
        if (!parseBrace())
          return false;
        break;
      case tok::less:
        if (parseAngle())
          Tok->Type = TT_TemplateOpener;
        else {
          Tok->Type = TT_BinaryOperator;
          CurrentToken = Tok;
          next();
        }
        break;
      case tok::r_paren:
      case tok::r_square:
        return false;
      case tok::r_brace:
        // Lines can start with '}'.
        if (Tok->Parent != NULL)
          return false;
        break;
      case tok::greater:
        Tok->Type = TT_BinaryOperator;
        break;
      case tok::kw_operator:
        if (CurrentToken->is(tok::l_paren)) {
          CurrentToken->Type = TT_OverloadedOperator;
          next();
          if (CurrentToken != NULL && CurrentToken->is(tok::r_paren)) {
            CurrentToken->Type = TT_OverloadedOperator;
            next();
          }
        } else {
          while (CurrentToken != NULL && CurrentToken->isNot(tok::l_paren)) {
            CurrentToken->Type = TT_OverloadedOperator;
            next();
          }
        }
        break;
      case tok::question:
        parseConditional();
        break;
      case tok::kw_template:
        parseTemplateDeclaration();
        break;
      default:
        break;
      }
      return true;
    }

    void parseIncludeDirective() {
      next();
      if (CurrentToken != NULL && CurrentToken->is(tok::less)) {
        next();
        while (CurrentToken != NULL) {
          if (CurrentToken->isNot(tok::comment) ||
              !CurrentToken->Children.empty())
            CurrentToken->Type = TT_ImplicitStringLiteral;
          next();
        }
      } else {
        while (CurrentToken != NULL) {
          next();
        }
      }
    }

    void parseWarningOrError() {
      next();
      // We still want to format the whitespace left of the first token of the
      // warning or error.
      next();
      while (CurrentToken != NULL) {
        CurrentToken->Type = TT_ImplicitStringLiteral;
        next();
      }
    }

    void parsePreprocessorDirective() {
      next();
      if (CurrentToken == NULL)
        return;
      // Hashes in the middle of a line can lead to any strange token
      // sequence.
      if (CurrentToken->FormatTok.Tok.getIdentifierInfo() == NULL)
        return;
      switch (
          CurrentToken->FormatTok.Tok.getIdentifierInfo()->getPPKeywordID()) {
      case tok::pp_include:
      case tok::pp_import:
        parseIncludeDirective();
        break;
      case tok::pp_error:
      case tok::pp_warning:
        parseWarningOrError();
        break;
      default:
        break;
      }
    }

    LineType parseLine() {
      int PeriodsAndArrows = 0;
      if (CurrentToken->is(tok::hash)) {
        parsePreprocessorDirective();
        return LT_PreprocessorDirective;
      }
      while (CurrentToken != NULL) {
        
        if (CurrentToken->is(tok::kw_virtual))
          KeywordVirtualFound = true;
        if (CurrentToken->is(tok::period) || CurrentToken->is(tok::arrow))
          ++PeriodsAndArrows;
        if (!consumeToken())
          return LT_Invalid;
      }
      if (KeywordVirtualFound)
        return LT_VirtualFunctionDecl;

      // Assume a builder-type call if there are 2 or more "." and "->".
      if (PeriodsAndArrows >= 2)
        return LT_BuilderTypeCall;

      return LT_Other;
    }

    void next() {
      if (CurrentToken != NULL && !CurrentToken->Children.empty())
        CurrentToken = &CurrentToken->Children[0];
      else
        CurrentToken = NULL;
    }

  private:
    AnnotatedToken *CurrentToken;
    bool KeywordVirtualFound;
    bool ColonIsObjCMethodExpr;
  };

  void calculateExtraInformation(AnnotatedToken &Current) {
    Current.SpaceRequiredBefore = spaceRequiredBefore(Current);

    if (Current.FormatTok.MustBreakBefore) {
      Current.MustBreakBefore = true;
    } else {
      if (Current.Type == TT_LineComment) {
        Current.MustBreakBefore = Current.FormatTok.NewlinesBefore > 0;
      } else if ((Current.Parent->is(tok::comment) &&
                  Current.FormatTok.NewlinesBefore > 0) ||
                 (Current.is(tok::string_literal) &&
                  Current.Parent->is(tok::string_literal))) {
        Current.MustBreakBefore = true;
      } else {
        Current.MustBreakBefore = false;
      }
    }
    Current.CanBreakBefore = Current.MustBreakBefore || canBreakBefore(Current);
    if (Current.MustBreakBefore)
      Current.TotalLength = Current.Parent->TotalLength + Style.ColumnLimit;
    else
      Current.TotalLength = Current.Parent->TotalLength +
                            Current.FormatTok.TokenLength +
                            (Current.SpaceRequiredBefore ? 1 : 0);
    if (!Current.Children.empty())
      calculateExtraInformation(Current.Children[0]);
  }

  void annotate() {
    AnnotatingParser Parser(Line.First);
    Line.Type = Parser.parseLine();
    if (Line.Type == LT_Invalid)
      return;

    determineTokenTypes(Line.First, /*IsRHS=*/false);

    if (Line.First.Type == TT_ObjCMethodSpecifier)
      Line.Type = LT_ObjCMethodDecl;
    else if (Line.First.Type == TT_ObjCDecl)
      Line.Type = LT_ObjCDecl;
    else if (Line.First.Type == TT_ObjCProperty)
      Line.Type = LT_ObjCProperty;

    Line.First.SpaceRequiredBefore = true;
    Line.First.MustBreakBefore = Line.First.FormatTok.MustBreakBefore;
    Line.First.CanBreakBefore = Line.First.MustBreakBefore;

    Line.First.TotalLength = Line.First.FormatTok.TokenLength;
    if (!Line.First.Children.empty())
      calculateExtraInformation(Line.First.Children[0]);
  }

private:
  void determineTokenTypes(AnnotatedToken &Current, bool IsRHS) {
    if (getPrecedence(Current) == prec::Assignment ||
        Current.is(tok::kw_return) || Current.is(tok::kw_throw))
      IsRHS = true;

    if (Current.Type == TT_Unknown) {
      if (Current.is(tok::star) || Current.is(tok::amp)) {
        Current.Type = determineStarAmpUsage(Current, IsRHS);
      } else if (Current.is(tok::minus) || Current.is(tok::plus) ||
                 Current.is(tok::caret)) {
        Current.Type = determinePlusMinusCaretUsage(Current);
      } else if (Current.is(tok::minusminus) || Current.is(tok::plusplus)) {
        Current.Type = determineIncrementUsage(Current);
      } else if (Current.is(tok::exclaim)) {
        Current.Type = TT_UnaryOperator;
      } else if (isBinaryOperator(Current)) {
        Current.Type = TT_BinaryOperator;
      } else if (Current.is(tok::comment)) {
        std::string Data(Lexer::getSpelling(Current.FormatTok.Tok, SourceMgr,
                                            Lex.getLangOpts()));
        if (StringRef(Data).startswith("//"))
          Current.Type = TT_LineComment;
        else
          Current.Type = TT_BlockComment;
      } else if (Current.is(tok::r_paren) &&
                 (Current.Parent->Type == TT_PointerOrReference ||
                  Current.Parent->Type == TT_TemplateCloser) &&
                 (Current.Children.empty() ||
                  (Current.Children[0].isNot(tok::equal) &&
                   Current.Children[0].isNot(tok::semi) &&
                   Current.Children[0].isNot(tok::l_brace)))) {
        // FIXME: We need to get smarter and understand more cases of casts.
        Current.Type = TT_CastRParen;
      } else if (Current.is(tok::at) && Current.Children.size()) {
        switch (Current.Children[0].FormatTok.Tok.getObjCKeywordID()) {
        case tok::objc_interface:
        case tok::objc_implementation:
        case tok::objc_protocol:
          Current.Type = TT_ObjCDecl;
          break;
        case tok::objc_property:
          Current.Type = TT_ObjCProperty;
          break;
        default:
          break;
        }
      }
    }

    if (!Current.Children.empty())
      determineTokenTypes(Current.Children[0], IsRHS);
  }

  bool isBinaryOperator(const AnnotatedToken &Tok) {
    // Comma is a binary operator, but does not behave as such wrt. formatting.
    return getPrecedence(Tok) > prec::Comma;
  }

  /// \brief Returns the previous token ignoring comments.
  const AnnotatedToken *getPreviousToken(const AnnotatedToken &Tok) {
    const AnnotatedToken *PrevToken = Tok.Parent;
    while (PrevToken != NULL && PrevToken->is(tok::comment))
      PrevToken = PrevToken->Parent;
    return PrevToken;
  }

  /// \brief Returns the next token ignoring comments.
  const AnnotatedToken *getNextToken(const AnnotatedToken &Tok) {
    if (Tok.Children.empty())
      return NULL;
    const AnnotatedToken *NextToken = &Tok.Children[0];
    while (NextToken->is(tok::comment)) {
      if (NextToken->Children.empty())
        return NULL;
      NextToken = &NextToken->Children[0];
    }
    return NextToken;
  }

  /// \brief Return the type of the given token assuming it is * or &.
  TokenType determineStarAmpUsage(const AnnotatedToken &Tok, bool IsRHS) {
    const AnnotatedToken *PrevToken = getPreviousToken(Tok);
    if (PrevToken == NULL)
      return TT_UnaryOperator;

    const AnnotatedToken *NextToken = getNextToken(Tok);
    if (NextToken == NULL)
      return TT_Unknown;

    if (NextToken->is(tok::l_square) && NextToken->Type != TT_ObjCMethodExpr)
      return TT_PointerOrReference;

    if (PrevToken->is(tok::l_paren) || PrevToken->is(tok::l_square) ||
        PrevToken->is(tok::l_brace) || PrevToken->is(tok::comma) ||
        PrevToken->is(tok::kw_return) || PrevToken->is(tok::colon) ||
        PrevToken->Type == TT_BinaryOperator ||
        PrevToken->Type == TT_UnaryOperator || PrevToken->Type == TT_CastRParen)
      return TT_UnaryOperator;

    if (PrevToken->FormatTok.Tok.isLiteral() || PrevToken->is(tok::r_paren) ||
        PrevToken->is(tok::r_square) || NextToken->FormatTok.Tok.isLiteral() ||
        NextToken->is(tok::plus) || NextToken->is(tok::minus) ||
        NextToken->is(tok::plusplus) || NextToken->is(tok::minusminus) ||
        NextToken->is(tok::tilde) || NextToken->is(tok::exclaim) ||
        NextToken->is(tok::l_paren) || NextToken->is(tok::l_square) ||
        NextToken->is(tok::kw_alignof) || NextToken->is(tok::kw_sizeof))
      return TT_BinaryOperator;

    if (NextToken->is(tok::comma) || NextToken->is(tok::r_paren) ||
        NextToken->is(tok::greater))
      return TT_PointerOrReference;

    // It is very unlikely that we are going to find a pointer or reference type
    // definition on the RHS of an assignment.
    if (IsRHS)
      return TT_BinaryOperator;

    return TT_PointerOrReference;
  }

  TokenType determinePlusMinusCaretUsage(const AnnotatedToken &Tok) {
    const AnnotatedToken *PrevToken = getPreviousToken(Tok);
    if (PrevToken == NULL)
      return TT_UnaryOperator;

    // Use heuristics to recognize unary operators.
    if (PrevToken->is(tok::equal) || PrevToken->is(tok::l_paren) ||
        PrevToken->is(tok::comma) || PrevToken->is(tok::l_square) ||
        PrevToken->is(tok::question) || PrevToken->is(tok::colon) ||
        PrevToken->is(tok::kw_return) || PrevToken->is(tok::kw_case) ||
        PrevToken->is(tok::at) || PrevToken->is(tok::l_brace))
      return TT_UnaryOperator;

    // There can't be to consecutive binary operators.
    if (PrevToken->Type == TT_BinaryOperator)
      return TT_UnaryOperator;

    // Fall back to marking the token as binary operator.
    return TT_BinaryOperator;
  }

  /// \brief Determine whether ++/-- are pre- or post-increments/-decrements.
  TokenType determineIncrementUsage(const AnnotatedToken &Tok) {
    const AnnotatedToken *PrevToken = getPreviousToken(Tok);
    if (PrevToken == NULL)
      return TT_UnaryOperator;
    if (PrevToken->is(tok::r_paren) || PrevToken->is(tok::r_square) ||
        PrevToken->is(tok::identifier))
      return TT_TrailingUnaryOperator;

    return TT_UnaryOperator;
  }

  bool spaceRequiredBetween(const AnnotatedToken &Left,
                            const AnnotatedToken &Right) {
    if (Right.is(tok::hashhash))
      return Left.is(tok::hash);
    if (Left.is(tok::hashhash) || Left.is(tok::hash))
      return Right.is(tok::hash);
    if (Right.is(tok::r_paren) || Right.is(tok::semi) || Right.is(tok::comma))
      return false;
    if (Right.is(tok::less) &&
        (Left.is(tok::kw_template) ||
         (Line.Type == LT_ObjCDecl && Style.ObjCSpaceBeforeProtocolList)))
      return true;
    if (Left.is(tok::arrow) || Right.is(tok::arrow))
      return false;
    if (Left.is(tok::exclaim) || Left.is(tok::tilde))
      return false;
    if (Left.is(tok::at) &&
        (Right.is(tok::identifier) || Right.is(tok::string_literal) ||
         Right.is(tok::char_constant) || Right.is(tok::numeric_constant) ||
         Right.is(tok::l_paren) || Right.is(tok::l_brace) ||
         Right.is(tok::kw_true) || Right.is(tok::kw_false)))
      return false;
    if (Left.is(tok::coloncolon))
      return false;
    if (Right.is(tok::coloncolon))
      return Left.isNot(tok::identifier) && Left.isNot(tok::greater);
    if (Left.is(tok::less) || Right.is(tok::greater) || Right.is(tok::less))
      return false;
    if (Right.is(tok::amp) || Right.is(tok::star))
      return Left.FormatTok.Tok.isLiteral() ||
             (Left.isNot(tok::star) && Left.isNot(tok::amp) &&
              !Style.PointerAndReferenceBindToType);
    if (Left.is(tok::amp) || Left.is(tok::star))
      return Right.FormatTok.Tok.isLiteral() ||
             Style.PointerAndReferenceBindToType;
    if (Right.is(tok::star) && Left.is(tok::l_paren))
      return false;
    if (Left.is(tok::l_square) || Right.is(tok::r_square))
      return false;
    if (Right.is(tok::l_square) && Right.Type != TT_ObjCMethodExpr)
      return false;
    if (Left.is(tok::period) || Right.is(tok::period))
      return false;
    if (Left.is(tok::colon))
      return Left.Type != TT_ObjCMethodExpr;
    if (Right.is(tok::colon))
      return Right.Type != TT_ObjCMethodExpr;
    if (Left.is(tok::l_paren))
      return false;
    if (Right.is(tok::l_paren)) {
      return Line.Type == LT_ObjCDecl || Left.is(tok::kw_if) ||
             Left.is(tok::kw_for) || Left.is(tok::kw_while) ||
             Left.is(tok::kw_switch) || Left.is(tok::kw_return) ||
             Left.is(tok::kw_catch) || Left.is(tok::kw_new) ||
             Left.is(tok::kw_delete);
    }
    if (Left.is(tok::at) &&
        Right.FormatTok.Tok.getObjCKeywordID() != tok::objc_not_keyword)
      return false;
    if (Left.is(tok::l_brace) && Right.is(tok::r_brace))
      return false;
    return true;
  }

  bool spaceRequiredBefore(const AnnotatedToken &Tok) {
    if (Line.Type == LT_ObjCMethodDecl) {
      if (Tok.is(tok::identifier) && !Tok.Children.empty() &&
          Tok.Children[0].is(tok::colon) && Tok.Parent->is(tok::identifier))
        return true;
      if (Tok.is(tok::colon))
        return false;
      if (Tok.Parent->Type == TT_ObjCMethodSpecifier)
        return true;
      if (Tok.Parent->is(tok::r_paren) && Tok.is(tok::identifier))
        // Don't space between ')' and <id>
        return false;
      if (Tok.Parent->is(tok::colon) && Tok.is(tok::l_paren))
        // Don't space between ':' and '('
        return false;
    }
    if (Line.Type == LT_ObjCProperty &&
        (Tok.is(tok::equal) || Tok.Parent->is(tok::equal)))
      return false;

    if (Tok.Parent->is(tok::comma))
      return true;
    if (Tok.Type == TT_CtorInitializerColon || Tok.Type == TT_ObjCBlockLParen)
      return true;
    if (Tok.Type == TT_OverloadedOperator)
      return Tok.is(tok::identifier) || Tok.is(tok::kw_new) ||
             Tok.is(tok::kw_delete) || Tok.is(tok::kw_bool);
    if (Tok.Parent->Type == TT_OverloadedOperator)
      return false;
    if (Tok.is(tok::colon))
      return Line.First.isNot(tok::kw_case) && !Tok.Children.empty() &&
             Tok.Type != TT_ObjCMethodExpr;
    if (Tok.Parent->Type == TT_UnaryOperator ||
        Tok.Parent->Type == TT_CastRParen)
      return false;
    if (Tok.Type == TT_UnaryOperator)
      return Tok.Parent->isNot(tok::l_paren) &&
             Tok.Parent->isNot(tok::l_square) && Tok.Parent->isNot(tok::at) &&
             (Tok.Parent->isNot(tok::colon) ||
              Tok.Parent->Type != TT_ObjCMethodExpr);
    if (Tok.Parent->is(tok::greater) && Tok.is(tok::greater)) {
      return Tok.Type == TT_TemplateCloser && Tok.Parent->Type ==
             TT_TemplateCloser && Style.SplitTemplateClosingGreater;
    }
    if (Tok.Type == TT_BinaryOperator || Tok.Parent->Type == TT_BinaryOperator)
      return true;
    if (Tok.Parent->Type == TT_TemplateCloser && Tok.is(tok::l_paren))
      return false;
    if (Tok.is(tok::less) && Line.First.is(tok::hash))
      return true;
    if (Tok.Type == TT_TrailingUnaryOperator)
      return false;
    return spaceRequiredBetween(*Tok.Parent, Tok);
  }

  bool canBreakBefore(const AnnotatedToken &Right) {
    const AnnotatedToken &Left = *Right.Parent;
    if (Line.Type == LT_ObjCMethodDecl) {
      if (Right.is(tok::identifier) && !Right.Children.empty() &&
          Right.Children[0].is(tok::colon) && Left.is(tok::identifier))
        return true;
      if (Right.is(tok::identifier) && Left.is(tok::l_paren) &&
          Left.Parent->is(tok::colon))
        // Don't break this identifier as ':' or identifier
        // before it will break.
        return false;
      if (Right.is(tok::colon) && Left.is(tok::identifier) &&
          Left.CanBreakBefore)
        // Don't break at ':' if identifier before it can beak.
        return false;
    }
    if (Right.is(tok::colon) && Right.Type == TT_ObjCMethodExpr)
      return false;
    if (Left.is(tok::colon) && Left.Type == TT_ObjCMethodExpr)
      return true;
    if (isObjCSelectorName(Right))
      return true;
    if (Left.ClosesTemplateDeclaration)
      return true;
    if (Left.Type == TT_PointerOrReference || Left.Type == TT_TemplateCloser ||
        Left.Type == TT_UnaryOperator || Right.Type == TT_ConditionalExpr)
      return false;
    if (Left.is(tok::equal) && Line.Type == LT_VirtualFunctionDecl)
      return false;

    if (Right.Type == TT_LineComment)
      // We rely on MustBreakBefore being set correctly here as we should not
      // change the "binding" behavior of a comment.
      return false;

    // Allow breaking after a trailing 'const', e.g. after a method declaration,
    // unless it is follow by ';', '{' or '='.
    if (Left.is(tok::kw_const) && Left.Parent != NULL &&
        Left.Parent->is(tok::r_paren))
      return Right.isNot(tok::l_brace) && Right.isNot(tok::semi) &&
             Right.isNot(tok::equal);

    // We only break before r_brace if there was a corresponding break before
    // the l_brace, which is tracked by BreakBeforeClosingBrace.
    if (Right.is(tok::r_brace))
      return false;

    if (Right.is(tok::r_paren) || Right.is(tok::greater))
      return false;
    return (isBinaryOperator(Left) && Left.isNot(tok::lessless)) ||
           Left.is(tok::comma) || Right.is(tok::lessless) ||
           Right.is(tok::arrow) || Right.is(tok::period) ||
           Right.is(tok::colon) || Left.is(tok::semi) ||
           Left.is(tok::l_brace) || Left.is(tok::question) || Left.Type ==
           TT_ConditionalExpr || (Left.is(tok::r_paren) && Left.Type !=
                                  TT_CastRParen && Right.is(tok::identifier)) ||
           (Left.is(tok::l_paren) && !Right.is(tok::r_paren));
  }

  FormatStyle Style;
  SourceManager &SourceMgr;
  Lexer &Lex;
  AnnotatedLine &Line;
};

class LexerBasedFormatTokenSource : public FormatTokenSource {
public:
  LexerBasedFormatTokenSource(Lexer &Lex, SourceManager &SourceMgr)
      : GreaterStashed(false), Lex(Lex), SourceMgr(SourceMgr),
        IdentTable(Lex.getLangOpts()) {
    Lex.SetKeepWhitespaceMode(true);
  }

  virtual FormatToken getNextToken() {
    if (GreaterStashed) {
      FormatTok.NewlinesBefore = 0;
      FormatTok.WhiteSpaceStart =
          FormatTok.Tok.getLocation().getLocWithOffset(1);
      FormatTok.WhiteSpaceLength = 0;
      GreaterStashed = false;
      return FormatTok;
    }

    FormatTok = FormatToken();
    Lex.LexFromRawLexer(FormatTok.Tok);
    StringRef Text = rawTokenText(FormatTok.Tok);
    FormatTok.WhiteSpaceStart = FormatTok.Tok.getLocation();
    if (SourceMgr.getFileOffset(FormatTok.WhiteSpaceStart) == 0)
      FormatTok.IsFirst = true;

    // Consume and record whitespace until we find a significant token.
    while (FormatTok.Tok.is(tok::unknown)) {
      FormatTok.NewlinesBefore += Text.count('\n');
      FormatTok.HasUnescapedNewline = Text.count("\\\n") !=
                                      FormatTok.NewlinesBefore;
      FormatTok.WhiteSpaceLength += FormatTok.Tok.getLength();

      if (FormatTok.Tok.is(tok::eof))
        return FormatTok;
      Lex.LexFromRawLexer(FormatTok.Tok);
      Text = rawTokenText(FormatTok.Tok);
    }

    // Now FormatTok is the next non-whitespace token.
    FormatTok.TokenLength = Text.size();

    // In case the token starts with escaped newlines, we want to
    // take them into account as whitespace - this pattern is quite frequent
    // in macro definitions.
    // FIXME: What do we want to do with other escaped spaces, and escaped
    // spaces or newlines in the middle of tokens?
    // FIXME: Add a more explicit test.
    unsigned i = 0;
    while (i + 1 < Text.size() && Text[i] == '\\' && Text[i + 1] == '\n') {
      FormatTok.WhiteSpaceLength += 2;
      FormatTok.TokenLength -= 2;
      i += 2;
    }

    if (FormatTok.Tok.is(tok::raw_identifier)) {
      IdentifierInfo &Info = IdentTable.get(Text);
      FormatTok.Tok.setIdentifierInfo(&Info);
      FormatTok.Tok.setKind(Info.getTokenID());
    }

    if (FormatTok.Tok.is(tok::greatergreater)) {
      FormatTok.Tok.setKind(tok::greater);
      GreaterStashed = true;
    }

    return FormatTok;
  }

private:
  FormatToken FormatTok;
  bool GreaterStashed;
  Lexer &Lex;
  SourceManager &SourceMgr;
  IdentifierTable IdentTable;

  /// Returns the text of \c FormatTok.
  StringRef rawTokenText(Token &Tok) {
    return StringRef(SourceMgr.getCharacterData(Tok.getLocation()),
                     Tok.getLength());
  }
};

class Formatter : public UnwrappedLineConsumer {
public:
  Formatter(DiagnosticsEngine &Diag, const FormatStyle &Style, Lexer &Lex,
            SourceManager &SourceMgr,
            const std::vector<CharSourceRange> &Ranges)
      : Diag(Diag), Style(Style), Lex(Lex), SourceMgr(SourceMgr),
        Whitespaces(SourceMgr), Ranges(Ranges) {}

  virtual ~Formatter() {}

  tooling::Replacements format() {
    LexerBasedFormatTokenSource Tokens(Lex, SourceMgr);
    UnwrappedLineParser Parser(Diag, Style, Tokens, *this);
    StructuralError = Parser.parse();
    unsigned PreviousEndOfLineColumn = 0;
    for (unsigned i = 0, e = AnnotatedLines.size(); i != e; ++i) {
      TokenAnnotator Annotator(Style, SourceMgr, Lex, AnnotatedLines[i]);
      Annotator.annotate();
    }
    for (std::vector<AnnotatedLine>::iterator I = AnnotatedLines.begin(),
                                              E = AnnotatedLines.end();
         I != E; ++I) {
      const AnnotatedLine &TheLine = *I;
      if (touchesRanges(TheLine) && TheLine.Type != LT_Invalid) {
        unsigned Indent = formatFirstToken(TheLine.First, TheLine.Level,
                                           TheLine.InPPDirective,
                                           PreviousEndOfLineColumn);
        tryFitMultipleLinesInOne(Indent, I, E);
        UnwrappedLineFormatter Formatter(Style, SourceMgr, TheLine, Indent,
                                         TheLine.First, Whitespaces,
                                         StructuralError);
        PreviousEndOfLineColumn = Formatter.format();
      } else {
        // If we did not reformat this unwrapped line, the column at the end of
        // the last token is unchanged - thus, we can calculate the end of the
        // last token, and return the result.
        PreviousEndOfLineColumn =
            SourceMgr.getSpellingColumnNumber(
                TheLine.Last->FormatTok.Tok.getLocation()) +
            Lex.MeasureTokenLength(TheLine.Last->FormatTok.Tok.getLocation(),
                                   SourceMgr, Lex.getLangOpts()) -
            1;
      }
    }
    return Whitespaces.generateReplacements();
  }

private:
  /// \brief Tries to merge lines into one.
  ///
  /// This will change \c Line and \c AnnotatedLine to contain the merged line,
  /// if possible; note that \c I will be incremented when lines are merged.
  ///
  /// Returns whether the resulting \c Line can fit in a single line.
  void tryFitMultipleLinesInOne(unsigned Indent,
                                std::vector<AnnotatedLine>::iterator &I,
                                std::vector<AnnotatedLine>::iterator E) {
    unsigned Limit = Style.ColumnLimit - (I->InPPDirective ? 1 : 0) - Indent;

    // We can never merge stuff if there are trailing line comments.
    if (I->Last->Type == TT_LineComment)
      return;

    // Check whether the UnwrappedLine can be put onto a single line. If
    // so, this is bound to be the optimal solution (by definition) and we
    // don't need to analyze the entire solution space.
    if (I->Last->TotalLength > Limit)
      return;
    Limit -= I->Last->TotalLength;

    if (I + 1 == E || (I + 1)->Type == LT_Invalid)
      return;

    if (I->Last->is(tok::l_brace)) {
      tryMergeSimpleBlock(I, E, Limit);
    } else if (I->First.is(tok::kw_if)) {
      tryMergeSimpleIf(I, E, Limit);
    } else if (I->InPPDirective && (I->First.FormatTok.HasUnescapedNewline ||
                                    I->First.FormatTok.IsFirst)) {
      tryMergeSimplePPDirective(I, E, Limit);
    }
    return;
  }

  void tryMergeSimplePPDirective(std::vector<AnnotatedLine>::iterator &I,
                                 std::vector<AnnotatedLine>::iterator E,
                                 unsigned Limit) {
    AnnotatedLine &Line = *I;
    if (!(I + 1)->InPPDirective || (I + 1)->First.FormatTok.HasUnescapedNewline)
      return;
    if (I + 2 != E && (I + 2)->InPPDirective &&
        !(I + 2)->First.FormatTok.HasUnescapedNewline)
      return;
    if (1 + (I + 1)->Last->TotalLength > Limit)
      return;
    join(Line, *(++I));
  }

  void tryMergeSimpleIf(std::vector<AnnotatedLine>::iterator &I,
                        std::vector<AnnotatedLine>::iterator E,
                        unsigned Limit) {
    if (!Style.AllowShortIfStatementsOnASingleLine)
      return;
    if ((I + 1)->InPPDirective != I->InPPDirective ||
        ((I + 1)->InPPDirective &&
         (I + 1)->First.FormatTok.HasUnescapedNewline))
      return;
    AnnotatedLine &Line = *I;
    if (Line.Last->isNot(tok::r_paren))
      return;
    if (1 + (I + 1)->Last->TotalLength > Limit)
      return;
    if ((I + 1)->First.is(tok::kw_if) || (I + 1)->First.Type == TT_LineComment)
      return;
    // Only inline simple if's (no nested if or else).
    if (I + 2 != E && (I + 2)->First.is(tok::kw_else))
      return;
    join(Line, *(++I));
  }

  void tryMergeSimpleBlock(std::vector<AnnotatedLine>::iterator &I,
                        std::vector<AnnotatedLine>::iterator E,
                        unsigned Limit){
    // First, check that the current line allows merging. This is the case if
    // we're not in a control flow statement and the last token is an opening
    // brace.
    AnnotatedLine &Line = *I;
    bool AllowedTokens =
        Line.First.isNot(tok::kw_if) && Line.First.isNot(tok::kw_while) &&
        Line.First.isNot(tok::kw_do) && Line.First.isNot(tok::r_brace) &&
        Line.First.isNot(tok::kw_else) && Line.First.isNot(tok::kw_try) &&
        Line.First.isNot(tok::kw_catch) && Line.First.isNot(tok::kw_for) &&
        // This gets rid of all ObjC @ keywords and methods.
        Line.First.isNot(tok::at) && Line.First.isNot(tok::minus) &&
        Line.First.isNot(tok::plus);
    if (!AllowedTokens)
      return;

    AnnotatedToken *Tok = &(I + 1)->First;
    if (Tok->Children.empty() && Tok->is(tok::r_brace) &&
        !Tok->MustBreakBefore && Tok->TotalLength <= Limit) {
      Tok->SpaceRequiredBefore = false;
      join(Line, *(I + 1));
      I += 1;
    } else {
      // Check that we still have three lines and they fit into the limit.
      if (I + 2 == E || (I + 2)->Type == LT_Invalid ||
          !nextTwoLinesFitInto(I, Limit))
        return;

      // Second, check that the next line does not contain any braces - if it
      // does, readability declines when putting it into a single line.
      if ((I + 1)->Last->Type == TT_LineComment || Tok->MustBreakBefore)
        return;
      do {
        if (Tok->is(tok::l_brace) || Tok->is(tok::r_brace))
          return;
        Tok = Tok->Children.empty() ? NULL : &Tok->Children.back();
      } while (Tok != NULL);

      // Last, check that the third line contains a single closing brace.
      Tok = &(I + 2)->First;
      if (!Tok->Children.empty() || Tok->isNot(tok::r_brace) ||
          Tok->MustBreakBefore)
        return;

      join(Line, *(I + 1));
      join(Line, *(I + 2));
      I += 2;
    }
  }

  bool nextTwoLinesFitInto(std::vector<AnnotatedLine>::iterator I,
                           unsigned Limit) {
    return 1 + (I + 1)->Last->TotalLength + 1 + (I + 2)->Last->TotalLength <=
           Limit;
  }

  void join(AnnotatedLine &A, const AnnotatedLine &B) {
    A.Last->Children.push_back(B.First);
    while (!A.Last->Children.empty()) {
      A.Last->Children[0].Parent = A.Last;
      A.Last = &A.Last->Children[0];
    }
  }

  bool touchesRanges(const AnnotatedLine &TheLine) {
    const FormatToken *First = &TheLine.First.FormatTok;
    const FormatToken *Last = &TheLine.Last->FormatTok;
    CharSourceRange LineRange = CharSourceRange::getTokenRange(
                                    First->Tok.getLocation(),
                                    Last->Tok.getLocation());
    for (unsigned i = 0, e = Ranges.size(); i != e; ++i) {
      if (!SourceMgr.isBeforeInTranslationUnit(LineRange.getEnd(),
                                               Ranges[i].getBegin()) &&
          !SourceMgr.isBeforeInTranslationUnit(Ranges[i].getEnd(),
                                               LineRange.getBegin()))
        return true;
    }
    return false;
  }

  virtual void consumeUnwrappedLine(const UnwrappedLine &TheLine) {
    AnnotatedLines.push_back(AnnotatedLine(TheLine));
  }

  /// \brief Add a new line and the required indent before the first Token
  /// of the \c UnwrappedLine if there was no structural parsing error.
  /// Returns the indent level of the \c UnwrappedLine.
  unsigned formatFirstToken(const AnnotatedToken &RootToken, unsigned Level,
                            bool InPPDirective,
                            unsigned PreviousEndOfLineColumn) {
    const FormatToken &Tok = RootToken.FormatTok;
    if (!Tok.WhiteSpaceStart.isValid() || StructuralError)
      return SourceMgr.getSpellingColumnNumber(Tok.Tok.getLocation()) - 1;

    unsigned Newlines = std::min(Tok.NewlinesBefore,
                                 Style.MaxEmptyLinesToKeep + 1);
    if (Newlines == 0 && !Tok.IsFirst)
      Newlines = 1;
    unsigned Indent = Level * 2;

    bool IsAccessModifier = false;
    if (RootToken.is(tok::kw_public) || RootToken.is(tok::kw_protected) ||
        RootToken.is(tok::kw_private))
      IsAccessModifier = true;
    else if (RootToken.is(tok::at) && !RootToken.Children.empty() &&
             (RootToken.Children[0].isObjCAtKeyword(tok::objc_public) ||
              RootToken.Children[0].isObjCAtKeyword(tok::objc_protected) ||
              RootToken.Children[0].isObjCAtKeyword(tok::objc_package) ||
              RootToken.Children[0].isObjCAtKeyword(tok::objc_private)))
      IsAccessModifier = true;

    if (IsAccessModifier &&
        static_cast<int>(Indent) + Style.AccessModifierOffset >= 0)
      Indent += Style.AccessModifierOffset;
    if (!InPPDirective || Tok.HasUnescapedNewline) {
      Whitespaces.replaceWhitespace(RootToken, Newlines, Indent, 0, Style);
    } else {
      Whitespaces.replacePPWhitespace(RootToken, Newlines, Indent,
                                      PreviousEndOfLineColumn, Style);
    }
    return Indent;
  }

  DiagnosticsEngine &Diag;
  FormatStyle Style;
  Lexer &Lex;
  SourceManager &SourceMgr;
  WhitespaceManager Whitespaces;
  std::vector<CharSourceRange> Ranges;
  std::vector<AnnotatedLine> AnnotatedLines;
  bool StructuralError;
};

tooling::Replacements reformat(const FormatStyle &Style, Lexer &Lex,
                               SourceManager &SourceMgr,
                               std::vector<CharSourceRange> Ranges,
                               DiagnosticConsumer *DiagClient) {
  IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
  OwningPtr<DiagnosticConsumer> DiagPrinter;
  if (DiagClient == 0) {
    DiagPrinter.reset(new TextDiagnosticPrinter(llvm::errs(), &*DiagOpts));
    DiagPrinter->BeginSourceFile(Lex.getLangOpts(), Lex.getPP());
    DiagClient = DiagPrinter.get();
  }
  DiagnosticsEngine Diagnostics(
      IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs()), &*DiagOpts,
      DiagClient, false);
  Diagnostics.setSourceManager(&SourceMgr);
  Formatter formatter(Diagnostics, Style, Lex, SourceMgr, Ranges);
  return formatter.format();
}

LangOptions getFormattingLangOpts() {
  LangOptions LangOpts;
  LangOpts.CPlusPlus = 1;
  LangOpts.CPlusPlus11 = 1;
  LangOpts.Bool = 1;
  LangOpts.ObjC1 = 1;
  LangOpts.ObjC2 = 1;
  return LangOpts;
}

} // namespace format
} // namespace clang
