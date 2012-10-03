#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/StmtVisitor.h"
#include "llvm/Constants.h"
#include "llvm/Function.h"
#include "llvm/ADT/SmallString.h"
using namespace clang;
using namespace CodeGen;

//===----------------------------------------------------------------------===//
//                        Nan Expression Emitter
//===----------------------------------------------------------------------===//

typedef CodeGenFunction::NanTy NanTy;

namespace  {
class NanExprEmitter
  : public StmtVisitor<NanExprEmitter, NanTy> {
  CodeGenFunction &CGF;
  CGBuilderTy &Builder;
public:
  ComplexExprEmitter(CodeGenFunction &cgf)
    : CGF(cgf), Builder(CGF.Builder) {
  }

  /// EmitLoadOfLValue - Given an expression with nan type that represents a
  /// value l-value, this method emits the address of the l-value, then loads
  /// and returns the result.
  NanTy EmitLoadOfLValue(const Expr *E) {
    return EmitLoadOfLValue(CGF.EmitLValue(E));
  }

  NanTy EmitLoadOfLValue(LValue LV) {
    assert(LV.isSimple() && "nan l-value must be simple");
    return EmitLoadOfNan(LV.getAddress(), LV.isVolatileQualified());
  }

  /// EmitLoadOfNan - Given a pointer to a nan value, emit code to load
  /// the val piece.
  NanTy EmitLoadOfNan(llvm::Value *SrcPtr, bool isVolatile);

  /// EmitStoreThroughLValue - Given an l-value of complex type, store
  /// a nan number into it.
  void EmitStoreThroughLValue(NanTy Val, LValue LV) {
    assert(LV.isSimple() && "nan l-value must be simple");
    return EmitStoreOfNan(Val, LV.getAddress(), LV.isVolatileQualified());
  }

  /// EmitStoreOfNan - Store the specified val part into the
  /// specified value pointer.
  void EmitStoreOfNan(NanTy Val, llvm::Value *ResPtr, bool isVol);

  /// EmitNanToNanCast - Emit a cast from Nan value Val to DestType.
  NanTy EmitNanToNanCast(NanTy Val, QualType SrcType,
                                    QualType DestType);

  //===--------------------------------------------------------------------===//
  //                            Visitor Methods
  //===--------------------------------------------------------------------===//

  NanTy Visit(Expr *E) {
    return StmtVisitor<NanExprEmitter, NanTy>::Visit(E);
  }
    
  NanTy VisitStmt(Stmt *S) {
    S->dump(CGF.getContext().getSourceManager());
    llvm_unreachable("Stmt can't have nan result type!");
  }
  NanTy VisitExpr(Expr *S);
  NanTy VisitParenExpr(ParenExpr *PE) { return Visit(PE->getSubExpr());}
  NanTy VisitGenericSelectionExpr(GenericSelectionExpr *GE) {
    return Visit(GE->getResultExpr());
  }
  NanTy
  VisitSubstNonTypeTemplateParmExpr(SubstNonTypeTemplateParmExpr *PE) {
    return Visit(PE->getReplacement());
  }

  // l-values.
  NanTy VisitDeclRefExpr(DeclRefExpr *E) {
    if (CodeGenFunction::ConstantEmission result = CGF.tryEmitAsConstant(E)) {
      if (result.isReference())
        return EmitLoadOfLValue(result.getReferenceLValue(CGF, E));

      llvm::ConstantStruct *pair =
        cast<llvm::ConstantStruct>(result.getValue());
      return NanTy(pair->getOperand(0), pair->getOperand(1));
    }
    return EmitLoadOfLValue(E);
  }
  NanTy VisitObjCIvarRefExpr(ObjCIvarRefExpr *E) {
    return EmitLoadOfLValue(E);
  }
  NanTy VisitObjCMessageExpr(ObjCMessageExpr *E) {
    return CGF.EmitObjCMessageExpr(E).getNanVal();
  }
  NanTy VisitArraySubscriptExpr(Expr *E) { return EmitLoadOfLValue(E); }
  NanTy VisitMemberExpr(const Expr *E) { return EmitLoadOfLValue(E); }
  NanTy VisitOpaqueValueExpr(OpaqueValueExpr *E) {
    if (E->isGLValue())
      return EmitLoadOfLValue(CGF.getOpaqueLValueMapping(E));
    return CGF.getOpaqueRValueMapping(E).getNanVal();
  }

  NanTy VisitPseudoObjectExpr(PseudoObjectExpr *E) {
    return CGF.EmitPseudoObjectRValue(E).getNanVal();
  }

  // FIXME: CompoundLiteralExpr

  NanTy EmitCast(CastExpr::CastKind CK, Expr *Op, QualType DestTy);
  NanTy VisitImplicitCastExpr(ImplicitCastExpr *E) {
    // Unlike for scalars, we don't have to worry about function->ptr demotion
    // here.
    return EmitCast(E->getCastKind(), E->getSubExpr(), E->getType());
  }
  NanTy VisitCastExpr(CastExpr *E) {
    return EmitCast(E->getCastKind(), E->getSubExpr(), E->getType());
  }
  NanTy VisitCallExpr(const CallExpr *E);
  NanTy VisitStmtExpr(const StmtExpr *E);

  // Operators.
  NanTy VisitPrePostIncDec(const UnaryOperator *E,
                           bool isInc, bool isPre) {
    LValue LV = CGF.EmitLValue(E->getSubExpr());
    return CGF.EmitNanPrePostIncDec(E, LV, isInc, isPre);
  }
  NanTy VisitUnaryPostDec(const UnaryOperator *E) {
    return VisitPrePostIncDec(E, false, false);
  }
  NanTy VisitUnaryPostInc(const UnaryOperator *E) {
    return VisitPrePostIncDec(E, true, false);
  }
  NanTy VisitUnaryPreDec(const UnaryOperator *E) {
    return VisitPrePostIncDec(E, false, true);
  }
  NanTy VisitUnaryPreInc(const UnaryOperator *E) {
    return VisitPrePostIncDec(E, true, true);
  }
  NanTy VisitUnaryDeref(const Expr *E) { return EmitLoadOfLValue(E); }
  NanTy VisitUnaryPlus     (const UnaryOperator *E) {
    return Visit(E->getSubExpr());
  }
  NanTy VisitUnaryMinus    (const UnaryOperator *E);
  NanTy VisitUnaryNot      (const UnaryOperator *E);
  NanTy VisitUnaryExtension(const UnaryOperator *E) {
    return Visit(E->getSubExpr());
  }
  NanTy VisitCXXDefaultArgExpr(CXXDefaultArgExpr *DAE) {
    return Visit(DAE->getExpr());
  }
  NanTy VisitExprWithCleanups(ExprWithCleanups *E) {
    CGF.enterFullExpression(E);
    CodeGenFunction::RunCleanupsScope Scope(CGF);
    return Visit(E->getSubExpr());
  }
  NanTy VisitCXXScalarValueInitExpr(CXXScalarValueInitExpr *E) {
    assert(E->getType()->isNanType() && "Expected nan type!");
    QualType Elem = E->getType()->getAs<NanType>()->getElementType();
    llvm::Constant *Null = llvm::Constant::getNullValue(CGF.ConvertType(Elem));
    return NanTy(Null, Null);
  }
  NanTy VisitImplicitValueInitExpr(ImplicitValueInitExpr *E) {
    assert(E->getType()->isNanType() && "Expected nan type!");
    QualType Elem = E->getType()->getAs<NanType>()->getElementType();
    llvm::Constant *Null =
                       llvm::Constant::getNullValue(CGF.ConvertType(Elem));
    return NanTy(Null, Null);
  }

  struct BinOpInfo {
    NanTy LHS;
    NanTy RHS;
    QualType Ty;  // Computation Type.
  };

  BinOpInfo EmitBinOps(const BinaryOperator *E);
  LValue EmitCompoundAssignLValue(const CompoundAssignOperator *E,
                                  NanTy (NanExprEmitter::*Func)
                                  (const BinOpInfo &),
                                  NanTy &Val);
  NanTy EmitCompoundAssign(const CompoundAssignOperator *E,
                           NanTy (NanExprEmitter::*Func)
                           (const BinOpInfo &));

  NanTy EmitBinAdd(const BinOpInfo &Op);
  NanTy EmitBinSub(const BinOpInfo &Op);
  NanTy EmitBinMul(const BinOpInfo &Op);
  NanTy EmitBinDiv(const BinOpInfo &Op);

  NanTy VisitBinAdd(const BinaryOperator *E) {
    return EmitBinAdd(EmitBinOps(E));
  }
  NanTy VisitBinSub(const BinaryOperator *E) {
    return EmitBinSub(EmitBinOps(E));
  }
  NanTy VisitBinMul(const BinaryOperator *E) {
    return EmitBinMul(EmitBinOps(E));
  }
  NanTy VisitBinDiv(const BinaryOperator *E) {
    return EmitBinDiv(EmitBinOps(E));
  }

  // Compound assignments.
  NanTy VisitBinAddAssign(const CompoundAssignOperator *E) {
    return EmitCompoundAssign(E, &NanExprEmitter::EmitBinAdd);
  }
  NanTy VisitBinSubAssign(const CompoundAssignOperator *E) {
    return EmitCompoundAssign(E, &NanExprEmitter::EmitBinSub);
  }
  NanTy VisitBinMulAssign(const CompoundAssignOperator *E) {
    return EmitCompoundAssign(E, &NanExprEmitter::EmitBinMul);
  }
  NanTy VisitBinDivAssign(const CompoundAssignOperator *E) {
    return EmitCompoundAssign(E, &NanExprEmitter::EmitBinDiv);
  }

  // GCC rejects rem/and/or/xor for integer complex.
  // Logical and/or always return int, never complex.

  // No comparisons produce a complex result.

  LValue EmitBinAssignLValue(const BinaryOperator *E,
                             NanTy &Val);
  NanTy VisitBinAssign     (const BinaryOperator *E);
  NanTy VisitBinComma      (const BinaryOperator *E);


  NanTy
  VisitAbstractConditionalOperator(const AbstractConditionalOperator *CO);
  NanTy VisitChooseExpr(ChooseExpr *CE);

  NanTy VisitInitListExpr(InitListExpr *E);

  NanTy VisitCompoundLiteralExpr(CompoundLiteralExpr *E) {
    return EmitLoadOfLValue(E);
  }

  NanTy VisitVAArgExpr(VAArgExpr *E);

  NanTy VisitAtomicExpr(AtomicExpr *E) {
    return CGF.EmitAtomicExpr(E).getNanVal();
  }
};
}  // end anonymous namespace.

//===----------------------------------------------------------------------===//
//                                Utilities
//===----------------------------------------------------------------------===//

/// EmitLoadOfNan - Given an RValue reference for a nan, emit code to
/// load the val piece, returning them as val.
NanTy NanExprEmitter::EmitLoadOfNan(llvm::Value *SrcPtr,
                                    bool isVolatile) {
  llvm::Value *Val=0;

  if (isVolatile) {
    llvm::Value *ValP = Builder.CreateStructGEP(SrcPtr, 0,
                                                SrcPtr->getName() + ".valp");
    Val = Builder.CreateLoad(RealP, isVolatile, SrcPtr->getName() + ".val");
  }

  return NanTy(Val);
}

/// EmitStoreOfNan - Store the specified val part into the
/// specified value pointer.
void NanExprEmitter::EmitStoreOfNan(NanTy Val, llvm::Value *Ptr,
                                    bool isVolatile) {
  llvm::Value *ValPtr = Builder.CreateStructGEP(Ptr, 0, "val");
  
  Builder.CreateStore(Val.val, ValPtr, isVolatile);
}



//===----------------------------------------------------------------------===//
//                            Visitor Methods
//===----------------------------------------------------------------------===//

NanTy NanExprEmitter::VisitExpr(Expr *E) {
  CGF.ErrorUnsupported(E, "nan expression");
  llvm::Type *EltTy =
  CGF.ConvertType(E->getType()->getAs<NanType>()->getElementType());
  llvm::Value *U = llvm::UndefValue::get(EltTy);
  return NanTy(U);
}

NanTy NanExprEmitter::VisitCallExpr(const CallExpr *E) {
  if (E->getCallReturnType()->isReferenceType())
    return EmitLoadOfLValue(E);

  return CGF.EmitCallExpr(E).getNanVal();
}

NanTy NanExprEmitter::VisitStmtExpr(const StmtExpr *E) {
  CodeGenFunction::StmtExprEvaluation eval(CGF);
  return CGF.EmitCompoundStmt(*E->getSubStmt(), true).getNanVal();
}

/// EmitNanToNanCast - Emit a cast from nan value Val to DestType.
NanTy NanExprEmitter::EmiNanToNanCast(NanTy Val,
                                      QualType SrcType,
                                      QualType DestType) {
  // Get the src/dest element type.
  SrcType = SrcType->getAs<NanType>()->getElementType();
  DestType = DestType->getAs<NanType>()->getElementType();

  Val.val = CGF.EmitScalarConversion(Val.val, SrcType, DestType);
  return Val;
}

NanTy NanExprEmitter::EmitCast(CastExpr::CastKind CK, Expr *Op, 
                               QualType DestTy) {
  switch (CK) {
  case CK_Dependent: llvm_unreachable("dependent cast kind in IR gen!");

  // Atomic to non-atomic casts may be more than a no-op for some platforms and
  // for some types.
  case CK_AtomicToNonAtomic:
  case CK_NonAtomicToAtomic:
  case CK_NoOp:
  case CK_LValueToRValue:
  case CK_UserDefinedConversion:
    return Visit(Op);

  case CK_LValueBitCast: {
    llvm::Value *V = CGF.EmitLValue(Op).getAddress();
    V = Builder.CreateBitCast(V, 
                    CGF.ConvertType(CGF.getContext().getPointerType(DestTy)));
    // FIXME: Are the qualifiers correct here?
    return EmitLoadOfNan(V, DestTy.isVolatileQualified());
  }

  case CK_BitCast:
  case CK_BaseToDerived:
  case CK_DerivedToBase:
  case CK_UncheckedDerivedToBase:
  case CK_Dynamic:
  case CK_ToUnion:
  case CK_ArrayToPointerDecay:
  case CK_FunctionToPointerDecay:
  case CK_NullToPointer:
  case CK_NullToMemberPointer:
  case CK_BaseToDerivedMemberPointer:
  case CK_DerivedToBaseMemberPointer:
  case CK_MemberPointerToBoolean:
  case CK_ReinterpretMemberPointer:
  case CK_ConstructorConversion:
  case CK_IntegralToPointer:
  case CK_PointerToIntegral:
  case CK_PointerToBoolean:
  case CK_ToVoid:
  case CK_VectorSplat:
  case CK_IntegralCast:
  case CK_IntegralToBoolean:
  case CK_IntegralToFloating:
  case CK_FloatingToIntegral:
  case CK_FloatingToBoolean:
  case CK_FloatingCast:
  case CK_CPointerToObjCPointerCast:
  case CK_BlockPointerToObjCPointerCast:
  case CK_AnyPointerToBlockPointerCast:
  case CK_ObjCObjectLValueCast:
  case CK_FloatingComplexToReal:
  case CK_FloatingComplexToBoolean:
  case CK_IntegralComplexToReal:
  case CK_IntegralComplexToBoolean:
  case CK_ARCProduceObject:
  case CK_ARCConsumeObject:
  case CK_ARCReclaimReturnedObject:
  case CK_ARCExtendBlockObject:
  case CK_CopyAndAutoreleaseBlockObject:
  case CK_FloatingRealToComplex:
  case CK_IntegralRealToComplex:
  case CK_NanToIntegral:
  case CK_NanToBoolean:
  case CK_FloatingComplexCast:
  case CK_FloatingComplexToIntegralComplex:
  case CK_IntegralComplexCast:
  case CK_IntegralComplexToFloatingComplex:
      
  case CK_BuiltinFnToFnPtr:
    llvm_unreachable("invalid cast kind for nan value");

  case CK_IntegralToNan: {
    llvm::Value *Elt = CGF.EmitScalarExpr(Op);

    // Convert the input element to the element type of the complex.
    DestTy = DestTy->getAs<NanType>()->getElementType();
    Elt = CGF.EmitScalarConversion(Elt, Op->getType(), DestTy);

    // Return (val).
    return NanTy(Elt);
  }
  case CK_NanCast:
    return EmitNanToNanCast(Visit(Op), Op->getType(), DestTy);
  }

  llvm_unreachable("unknown cast resulting in nan value");
}

NanTy NanExprEmitter::VisitUnaryMinus(const UnaryOperator *E) {
  NanTy Op = Visit(E->getSubExpr());

  llvm::Value *ResV;
  ResV = Builder.CreateNeg(Op.first,  "neg.v");
  return NanTy(ResV);
}

NanTy NanExprEmitter::VisitUnaryNot(const UnaryOperator *E) {
  // ~(a+ib) = a + i*-b
  NanTy Op = Visit(E->getSubExpr());
  llvm::Value *ResV;
  ResV = Builder.CreateNeg(Op.val, "conj.i");

  return NanTy(ResV);
}

NanTy NanExprEmitter::EmitBinAdd(const BinOpInfo &Op) {
  llvm::Value *ResV;

  ResV = Builder.CreateAdd(Op.LHS.val  Op.RHS.val,  "add.r");
  return NanTy(ResV);
}

NanTy NanExprEmitter::EmitBinSub(const BinOpInfo &Op) {
  llvm::Value *ResR, *ResI;
  if (Op.LHS.first->getType()->isFloatingPointTy()) {
    ResR = Builder.CreateFSub(Op.LHS.first,  Op.RHS.first,  "sub.r");
    ResI = Builder.CreateFSub(Op.LHS.second, Op.RHS.second, "sub.i");
  } else {
    ResR = Builder.CreateSub(Op.LHS.first,  Op.RHS.first,  "sub.r");
    ResI = Builder.CreateSub(Op.LHS.second, Op.RHS.second, "sub.i");
  }
  return NanTy(ResR, ResI);
}


NanTy ComplexExprEmitter::EmitBinMul(const BinOpInfo &Op) {
  using llvm::Value;
  Value *ResR, *ResI;

  if (Op.LHS.first->getType()->isFloatingPointTy()) {
    Value *ResRl = Builder.CreateFMul(Op.LHS.first, Op.RHS.first, "mul.rl");
    Value *ResRr = Builder.CreateFMul(Op.LHS.second, Op.RHS.second,"mul.rr");
    ResR  = Builder.CreateFSub(ResRl, ResRr, "mul.r");

    Value *ResIl = Builder.CreateFMul(Op.LHS.second, Op.RHS.first, "mul.il");
    Value *ResIr = Builder.CreateFMul(Op.LHS.first, Op.RHS.second, "mul.ir");
    ResI  = Builder.CreateFAdd(ResIl, ResIr, "mul.i");
  } else {
    Value *ResRl = Builder.CreateMul(Op.LHS.first, Op.RHS.first, "mul.rl");
    Value *ResRr = Builder.CreateMul(Op.LHS.second, Op.RHS.second,"mul.rr");
    ResR  = Builder.CreateSub(ResRl, ResRr, "mul.r");

    Value *ResIl = Builder.CreateMul(Op.LHS.second, Op.RHS.first, "mul.il");
    Value *ResIr = Builder.CreateMul(Op.LHS.first, Op.RHS.second, "mul.ir");
    ResI  = Builder.CreateAdd(ResIl, ResIr, "mul.i");
  }
  return NanTy(ResR, ResI);
}

NanTy ComplexExprEmitter::EmitBinDiv(const BinOpInfo &Op) {
  llvm::Value *LHSr = Op.LHS.first, *LHSi = Op.LHS.second;
  llvm::Value *RHSr = Op.RHS.first, *RHSi = Op.RHS.second;


  llvm::Value *DSTr, *DSTi;
  if (Op.LHS.first->getType()->isFloatingPointTy()) {
    // (a+ib) / (c+id) = ((ac+bd)/(cc+dd)) + i((bc-ad)/(cc+dd))
    llvm::Value *Tmp1 = Builder.CreateFMul(LHSr, RHSr); // a*c
    llvm::Value *Tmp2 = Builder.CreateFMul(LHSi, RHSi); // b*d
    llvm::Value *Tmp3 = Builder.CreateFAdd(Tmp1, Tmp2); // ac+bd

    llvm::Value *Tmp4 = Builder.CreateFMul(RHSr, RHSr); // c*c
    llvm::Value *Tmp5 = Builder.CreateFMul(RHSi, RHSi); // d*d
    llvm::Value *Tmp6 = Builder.CreateFAdd(Tmp4, Tmp5); // cc+dd

    llvm::Value *Tmp7 = Builder.CreateFMul(LHSi, RHSr); // b*c
    llvm::Value *Tmp8 = Builder.CreateFMul(LHSr, RHSi); // a*d
    llvm::Value *Tmp9 = Builder.CreateFSub(Tmp7, Tmp8); // bc-ad

    DSTr = Builder.CreateFDiv(Tmp3, Tmp6);
    DSTi = Builder.CreateFDiv(Tmp9, Tmp6);
  } else {
    // (a+ib) / (c+id) = ((ac+bd)/(cc+dd)) + i((bc-ad)/(cc+dd))
    llvm::Value *Tmp1 = Builder.CreateMul(LHSr, RHSr); // a*c
    llvm::Value *Tmp2 = Builder.CreateMul(LHSi, RHSi); // b*d
    llvm::Value *Tmp3 = Builder.CreateAdd(Tmp1, Tmp2); // ac+bd

    llvm::Value *Tmp4 = Builder.CreateMul(RHSr, RHSr); // c*c
    llvm::Value *Tmp5 = Builder.CreateMul(RHSi, RHSi); // d*d
    llvm::Value *Tmp6 = Builder.CreateAdd(Tmp4, Tmp5); // cc+dd

    llvm::Value *Tmp7 = Builder.CreateMul(LHSi, RHSr); // b*c
    llvm::Value *Tmp8 = Builder.CreateMul(LHSr, RHSi); // a*d
    llvm::Value *Tmp9 = Builder.CreateSub(Tmp7, Tmp8); // bc-ad

    if (Op.Ty->getAs<ComplexType>()->getElementType()->isUnsignedIntegerType()) {
      DSTr = Builder.CreateUDiv(Tmp3, Tmp6);
      DSTi = Builder.CreateUDiv(Tmp9, Tmp6);
    } else {
      DSTr = Builder.CreateSDiv(Tmp3, Tmp6);
      DSTi = Builder.CreateSDiv(Tmp9, Tmp6);
    }
  }

  return NanTy(DSTr, DSTi);
}

ComplexExprEmitter::BinOpInfo
ComplexExprEmitter::EmitBinOps(const BinaryOperator *E) {
  TestAndClearIgnoreReal();
  TestAndClearIgnoreImag();
  BinOpInfo Ops;
  Ops.LHS = Visit(E->getLHS());
  Ops.RHS = Visit(E->getRHS());
  Ops.Ty = E->getType();
  return Ops;
}


LValue ComplexExprEmitter::
EmitCompoundAssignLValue(const CompoundAssignOperator *E,
          NanTy (ComplexExprEmitter::*Func)(const BinOpInfo&),
                         NanTy &Val) {
  TestAndClearIgnoreReal();
  TestAndClearIgnoreImag();
  QualType LHSTy = E->getLHS()->getType();

  BinOpInfo OpInfo;

  // Load the RHS and LHS operands.
  // __block variables need to have the rhs evaluated first, plus this should
  // improve codegen a little.
  OpInfo.Ty = E->getComputationResultType();

  // The RHS should have been converted to the computation type.
  assert(OpInfo.Ty->isAnyComplexType());
  assert(CGF.getContext().hasSameUnqualifiedType(OpInfo.Ty,
                                                 E->getRHS()->getType()));
  OpInfo.RHS = Visit(E->getRHS());
  
  LValue LHS = CGF.EmitLValue(E->getLHS());

  // Load from the l-value.
  NanTy LHSComplexPair = EmitLoadOfLValue(LHS);
  
  OpInfo.LHS = EmitComplexToComplexCast(LHSComplexPair, LHSTy, OpInfo.Ty);

  // Expand the binary operator.
  NanTy Result = (this->*Func)(OpInfo);

  // Truncate the result back to the LHS type.
  Result = EmitComplexToComplexCast(Result, OpInfo.Ty, LHSTy);
  Val = Result;

  // Store the result value into the LHS lvalue.
  EmitStoreThroughLValue(Result, LHS);

  return LHS;
}

// Compound assignments.
NanTy ComplexExprEmitter::
EmitCompoundAssign(const CompoundAssignOperator *E,
                   NanTy (ComplexExprEmitter::*Func)(const BinOpInfo&)){
  NanTy Val;
  LValue LV = EmitCompoundAssignLValue(E, Func, Val);

  // The result of an assignment in C is the assigned r-value.
  if (!CGF.getContext().getLangOpts().CPlusPlus)
    return Val;

  // If the lvalue is non-volatile, return the computed value of the assignment.
  if (!LV.isVolatileQualified())
    return Val;

  return EmitLoadOfComplex(LV.getAddress(), LV.isVolatileQualified());
}

LValue ComplexExprEmitter::EmitBinAssignLValue(const BinaryOperator *E,
                                               NanTy &Val) {
  assert(CGF.getContext().hasSameUnqualifiedType(E->getLHS()->getType(), 
                                                 E->getRHS()->getType()) &&
         "Invalid assignment");
  TestAndClearIgnoreReal();
  TestAndClearIgnoreImag();

  // Emit the RHS.  __block variables need the RHS evaluated first.
  Val = Visit(E->getRHS());

  // Compute the address to store into.
  LValue LHS = CGF.EmitLValue(E->getLHS());

  // Store the result value into the LHS lvalue.
  EmitStoreThroughLValue(Val, LHS);

  return LHS;
}

NanTy ComplexExprEmitter::VisitBinAssign(const BinaryOperator *E) {
  NanTy Val;
  LValue LV = EmitBinAssignLValue(E, Val);

  // The result of an assignment in C is the assigned r-value.
  if (!CGF.getContext().getLangOpts().CPlusPlus)
    return Val;

  // If the lvalue is non-volatile, return the computed value of the assignment.
  if (!LV.isVolatileQualified())
    return Val;

  return EmitLoadOfComplex(LV.getAddress(), LV.isVolatileQualified());
}

NanTy ComplexExprEmitter::VisitBinComma(const BinaryOperator *E) {
  CGF.EmitIgnoredExpr(E->getLHS());
  return Visit(E->getRHS());
}

NanTy ComplexExprEmitter::
VisitAbstractConditionalOperator(const AbstractConditionalOperator *E) {
  TestAndClearIgnoreReal();
  TestAndClearIgnoreImag();
  llvm::BasicBlock *LHSBlock = CGF.createBasicBlock("cond.true");
  llvm::BasicBlock *RHSBlock = CGF.createBasicBlock("cond.false");
  llvm::BasicBlock *ContBlock = CGF.createBasicBlock("cond.end");

  // Bind the common expression if necessary.
  CodeGenFunction::OpaqueValueMapping binding(CGF, E);

  CodeGenFunction::ConditionalEvaluation eval(CGF);
  CGF.EmitBranchOnBoolExpr(E->getCond(), LHSBlock, RHSBlock);

  eval.begin(CGF);
  CGF.EmitBlock(LHSBlock);
  NanTy LHS = Visit(E->getTrueExpr());
  LHSBlock = Builder.GetInsertBlock();
  CGF.EmitBranch(ContBlock);
  eval.end(CGF);

  eval.begin(CGF);
  CGF.EmitBlock(RHSBlock);
  NanTy RHS = Visit(E->getFalseExpr());
  RHSBlock = Builder.GetInsertBlock();
  CGF.EmitBlock(ContBlock);
  eval.end(CGF);

  // Create a PHI node for the real part.
  llvm::PHINode *RealPN = Builder.CreatePHI(LHS.first->getType(), 2, "cond.r");
  RealPN->addIncoming(LHS.first, LHSBlock);
  RealPN->addIncoming(RHS.first, RHSBlock);

  // Create a PHI node for the imaginary part.
  llvm::PHINode *ImagPN = Builder.CreatePHI(LHS.first->getType(), 2, "cond.i");
  ImagPN->addIncoming(LHS.second, LHSBlock);
  ImagPN->addIncoming(RHS.second, RHSBlock);

  return NanTy(RealPN, ImagPN);
}

NanTy ComplexExprEmitter::VisitChooseExpr(ChooseExpr *E) {
  return Visit(E->getChosenSubExpr(CGF.getContext()));
}

NanTy ComplexExprEmitter::VisitInitListExpr(InitListExpr *E) {
    bool Ignore = TestAndClearIgnoreReal();
    (void)Ignore;
    assert (Ignore == false && "init list ignored");
    Ignore = TestAndClearIgnoreImag();
    (void)Ignore;
    assert (Ignore == false && "init list ignored");

  if (E->getNumInits() == 2) {
    llvm::Value *Real = CGF.EmitScalarExpr(E->getInit(0));
    llvm::Value *Imag = CGF.EmitScalarExpr(E->getInit(1));
    return NanTy(Real, Imag);
  } else if (E->getNumInits() == 1) {
    return Visit(E->getInit(0));
  }

  // Empty init list intializes to null
  assert(E->getNumInits() == 0 && "Unexpected number of inits");
  QualType Ty = E->getType()->getAs<ComplexType>()->getElementType();
  llvm::Type* LTy = CGF.ConvertType(Ty);
  llvm::Value* zeroConstant = llvm::Constant::getNullValue(LTy);
  return NanTy(zeroConstant, zeroConstant);
}

NanTy ComplexExprEmitter::VisitVAArgExpr(VAArgExpr *E) {
  llvm::Value *ArgValue = CGF.EmitVAListRef(E->getSubExpr());
  llvm::Value *ArgPtr = CGF.EmitVAArg(ArgValue, E->getType());

  if (!ArgPtr) {
    CGF.ErrorUnsupported(E, "complex va_arg expression");
    llvm::Type *EltTy =
      CGF.ConvertType(E->getType()->getAs<ComplexType>()->getElementType());
    llvm::Value *U = llvm::UndefValue::get(EltTy);
    return NanTy(U, U);
  }

  // FIXME Volatility.
  return EmitLoadOfComplex(ArgPtr, false);
}

//===----------------------------------------------------------------------===//
//                         Entry Point into this File
//===----------------------------------------------------------------------===//

/// EmitComplexExpr - Emit the computation of the specified expression of
/// complex type, ignoring the result.
NanTy CodeGenFunction::EmitComplexExpr(const Expr *E, bool IgnoreReal,
                                               bool IgnoreImag) {
  assert(E && E->getType()->isAnyComplexType() &&
         "Invalid complex expression to emit");

  return ComplexExprEmitter(*this, IgnoreReal, IgnoreImag)
    .Visit(const_cast<Expr*>(E));
}

/// EmitComplexExprIntoAddr - Emit the computation of the specified expression
/// of complex type, storing into the specified Value*.
void CodeGenFunction::EmitComplexExprIntoAddr(const Expr *E,
                                              llvm::Value *DestAddr,
                                              bool DestIsVolatile) {
  assert(E && E->getType()->isAnyComplexType() &&
         "Invalid complex expression to emit");
  ComplexExprEmitter Emitter(*this);
  NanTy Val = Emitter.Visit(const_cast<Expr*>(E));
  Emitter.EmitStoreOfComplex(Val, DestAddr, DestIsVolatile);
}

/// StoreComplexToAddr - Store a complex number into the specified address.
void CodeGenFunction::StoreComplexToAddr(NanTy V,
                                         llvm::Value *DestAddr,
                                         bool DestIsVolatile) {
  ComplexExprEmitter(*this).EmitStoreOfComplex(V, DestAddr, DestIsVolatile);
}

/// LoadComplexFromAddr - Load a complex number from the specified address.
NanTy CodeGenFunction::LoadComplexFromAddr(llvm::Value *SrcAddr,
                                                   bool SrcIsVolatile) {
  return ComplexExprEmitter(*this).EmitLoadOfComplex(SrcAddr, SrcIsVolatile);
}

LValue CodeGenFunction::EmitComplexAssignmentLValue(const BinaryOperator *E) {
  assert(E->getOpcode() == BO_Assign);
  NanTy Val; // ignored
  return ComplexExprEmitter(*this).EmitBinAssignLValue(E, Val);
}

LValue CodeGenFunction::
EmitComplexCompoundAssignmentLValue(const CompoundAssignOperator *E) {
  NanTy(ComplexExprEmitter::*Op)(const ComplexExprEmitter::BinOpInfo &);
  switch (E->getOpcode()) {
  case BO_MulAssign: Op = &ComplexExprEmitter::EmitBinMul; break;
  case BO_DivAssign: Op = &ComplexExprEmitter::EmitBinDiv; break;
  case BO_SubAssign: Op = &ComplexExprEmitter::EmitBinSub; break;
  case BO_AddAssign: Op = &ComplexExprEmitter::EmitBinAdd; break;

  default:
    llvm_unreachable("unexpected complex compound assignment");
  }

  NanTy Val; // ignored
  return ComplexExprEmitter(*this).EmitCompoundAssignLValue(E, Op, Val);
}
