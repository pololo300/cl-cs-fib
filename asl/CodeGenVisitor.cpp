//////////////////////////////////////////////////////////////////////
//
//    CodeGenVisitor - Walk the parser tree to do
//                     the generation of code
//
//    Copyright (C) 2020-2030  Universitat Politecnica de Catalunya
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License
//    as published by the Free Software Foundation; either version 3
//    of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Affero General Public License for more details.
//
//    You should have received a copy of the GNU Affero General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//
//    contact: José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
//////////////////////////////////////////////////////////////////////

#include "CodeGenVisitor.h"
#include "antlr4-runtime.h"

#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/TypesMgr.h"
#include "../common/code.h"

#include <cstddef> // std::size_t
#include <string>

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;

// Constructor
CodeGenVisitor::CodeGenVisitor(TypesMgr &Types, SymTable &Symbols,
                               TreeDecoration &Decorations)
    : Types{Types}, Symbols{Symbols}, Decorations{Decorations} {}

// Accessor/Mutator to the attribute currFunctionType
TypesMgr::TypeId CodeGenVisitor::getCurrentFunctionTy() const {
  return currFunctionType;
}

void CodeGenVisitor::setCurrentFunctionTy(TypesMgr::TypeId type) {
  currFunctionType = type;
}

// Methods to visit each kind of node:
//
antlrcpp::Any CodeGenVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  code my_code;
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function()) {
    subroutine subr = visit(ctxFunc);
    my_code.add_subroutine(subr);
  }
  Symbols.popScope();
  DEBUG_EXIT();
  return my_code;
}

antlrcpp::Any CodeGenVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  subroutine subr(ctx->ID()->getText());
  codeCounters.reset();
  std::vector<var> &&lvars = visit(ctx->declarations());
  for (auto &onevar : lvars) {
    subr.add_var(onevar);
  }
  instructionList &&code = visit(ctx->statements());
  code = code || instruction(instruction::RETURN());
  subr.set_instructions(code);
  Symbols.popScope();
  DEBUG_EXIT();
  return subr;
}

antlrcpp::Any
CodeGenVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
  DEBUG_ENTER();
  std::vector<var> lvars;
  for (auto &varDeclCtx : ctx->variable_decl()) {
    std::vector<var> vars = visit(varDeclCtx);
    for (auto v : vars)
      lvars.push_back(v);
  }
  DEBUG_EXIT();
  return lvars;
}

antlrcpp::Any
CodeGenVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId t1 = getTypeDecor(ctx->type());
  std::size_t size = Types.getSizeOfType(t1);
  DEBUG_EXIT();
  std::vector<var> v;
  for (int i = 0; i < ctx->ID().size(); ++i)
    v.push_back(var{ctx->ID(i)->getText(), Types.to_string(t1), size});
  return v;
}

antlrcpp::Any
CodeGenVisitor::visitStatements(AslParser::StatementsContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  for (auto stCtx : ctx->statement()) {
    instructionList &&codeS = visit(stCtx);
    code = code || codeS;
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any
CodeGenVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs &&codAtsE1 = visit(ctx->left_expr());
  std::string addr1 = codAtsE1.addr;
  // std::string           offs1 = codAtsE1.offs;
  instructionList &code1 = codAtsE1.code;
  // TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());
  CodeAttribs codAtsE2 = visit(ctx->expr());
  std::string addr2 = codAtsE2.addr;
  // std::string           offs2 = codAtsE2.offs;
  instructionList &code2 = codAtsE2.code;
  // TypesMgr::TypeId tid2 = getTypeDecor(ctx->expr());
  code = code1 || code2 || instruction::LOAD(addr1, addr2);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs &&codAtsE = visit(ctx->expr());
  std::string addr1 = codAtsE.addr;
  instructionList &code1 = codAtsE.code;
  //
  //
  // FALTA POSAR EL ELSE
  //
  //
  instructionList &&code2 = visit(ctx->statements(0));
  std::string label = codeCounters.newLabelIF();
  std::string labelEndIf = "endif" + label;
  code = code1 || instruction::FJUMP(addr1, labelEndIf) || code2 ||
         instruction::LABEL(labelEndIf);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitProcCall(AslParser::ProcCallContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  // std::string name = ctx->ident()->ID()->getSymbol()->getText();
  std::string name = ctx->ident()->getText();
  code = instruction::CALL(name);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codAtsE = visit(ctx->left_expr());
  std::string addr1 = codAtsE.addr;
  // std::string          offs1 = codAtsE.offs;
  instructionList &code1 = codAtsE.code;
  instructionList &code = code1;
  // TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());
  code = code1 || instruction::READI(addr1);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codAt1 = visit(ctx->expr());
  std::string addr1 = codAt1.addr;
  // std::string         offs1 = codAt1.offs;
  instructionList &code1 = codAt1.code;
  instructionList &code = code1;
  // TypesMgr::TypeId tid1 = getTypeDecor(ctx->expr());
  code = code1 || instruction::WRITEI(addr1);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any
CodeGenVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string s = ctx->STRING()->getText();
  code = code || instruction::WRITES(s);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitLeft_expr(AslParser::Left_exprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitUnari(AslParser::UnariContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codAts = visit(ctx->expr());
  instructionList code = codAts.code;
  std::string addr = codAts.addr;
  std::string temp = "%" + codeCounters.newTEMP();

  TypesMgr::TypeId t = getTypeDecor(ctx);

  if (ctx->NOT())
    code = code || instruction::NOT(temp, addr);
  else if (ctx->MINUS()) {
    if (Types.isFloatTy(t))
      code = code || instruction::FNEG(temp, addr);
    else if (Types.isIntegerTy(t))
      code = code || instruction::NEG(temp, addr);
  }

  codAts.code = code;
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any
CodeGenVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();

  CodeAttribs &&codAt1 = visit(ctx->expr(0));
  std::string addr1 = codAt1.addr;
  instructionList &code1 = codAt1.code;

  CodeAttribs &&codAt2 = visit(ctx->expr(1));
  std::string addr2 = codAt2.addr;
  instructionList &code2 = codAt2.code;

  instructionList &&code = code1 || code2;

  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  TypesMgr::TypeId t = getTypeDecor(ctx);

  std::string temp = "%" + codeCounters.newTEMP();
  std::string temp1;
  std::string temp2;

  if (Types.isFloatTy(t)) {
    if (Types.isIntegerTy(t1)) {
      temp1 = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp1, addr1);
    } else
      temp1 = addr1;

    if (Types.isIntegerTy(t2)) {
      temp2 = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp2, addr2);
    } else
      temp2 = addr2;

    if (ctx->MUL())
      code = code || instruction::FMUL(temp, addr1, addr2);
    else if (ctx->DIV())
      code = code || instruction::FDIV(temp, addr1, addr2);
    else if (ctx->PLUS())
      code = code || instruction::FADD(temp, addr1, addr2);
    else if (ctx->MINUS())
      code = code || instruction::FSUB(temp, addr1, addr2);

  } else {
    if (ctx->MUL())
      code = code || instruction::MUL(temp, addr1, addr2);
    else if (ctx->PLUS())
      code = code || instruction::ADD(temp, addr1, addr2);
    else if (ctx->DIV())
      code = code || instruction::DIV(temp, addr1, addr2);
    else if (ctx->MINUS())
      code = code || instruction::SUB(temp, addr1, addr2);
    else if (ctx->MOD())
      FALTA("falta calcular residu divisio entera (%)");
  }
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any
CodeGenVisitor::visitRelational(AslParser::RelationalContext *ctx) {
  DEBUG_ENTER();

  CodeAttribs &&codAt1 = visit(ctx->expr(0));
  std::string addr1 = codAt1.addr;
  instructionList &code1 = codAt1.code;

  CodeAttribs &&codAt2 = visit(ctx->expr(1));
  std::string addr2 = codAt2.addr;
  instructionList &code2 = codAt2.code;

  instructionList &&code = code1 || code2;

  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  TypesMgr::TypeId t = getTypeDecor(ctx);

  std::string temp = "%" + codeCounters.newTEMP();

  if (Types.isFloatTy(t)) {
    std::string temp1;
    if (not Types.isFloatTy(t1)) {
      temp1 = "%" + codeCounters.newTEMP();
      code.push_back(instruction::FLOAT(temp1, addr1));
    }

    std::string temp2;
    if (not Types.isFloatTy(t2)) {
      temp2 = "%" + codeCounters.newTEMP();
      code.push_back(instruction::FLOAT(temp2, addr2));
    }

    std::string temp3 = "%" + codeCounters.newTEMP();
    if (ctx->EQUAL()) {
      code = code || instruction::EQ(temp, temp1, temp2);
    } else if (ctx->NEQUAL()) {
      code = code || instruction::EQ(temp3, temp1, temp2);
      code = code || instruction::NOT(temp, temp3);
    } else if (ctx->LESS()) {
      code = code || instruction::LT(temp, temp1, temp2);
    } else if (ctx->LESSEQ()) {
      code = code || instruction::LE(temp, temp1, temp2);
    } else if (ctx->GREAT()) {
      code = code || instruction::LE(temp3, temp2, temp1);
      code = code || instruction::NOT(temp, temp3);
    } else if (ctx->GREATEQ()) {
      code = code || instruction::LE(temp3, temp2, temp1);
      code = code || instruction::NOT(temp, temp3);
    }
  } else {
    std::string temp3 = "%" + codeCounters.newTEMP();
    if (ctx->EQUAL()) {
      code = code || instruction::EQ(temp, addr1, addr2);
    } else if (ctx->NEQUAL()) {
      code = code || instruction::EQ(temp3, addr1, addr2);
      code = code || instruction::NOT(temp, temp3);
    } else if (ctx->LESS()) {
      code = code || instruction::LT(temp, addr1, addr2);
    } else if (ctx->LESSEQ()) {
      code = code || instruction::LE(temp, addr1, addr2);
    } else if (ctx->GREAT()) {
      code = code || instruction::LE(temp3, addr2, addr1);
      code = code || instruction::NOT(temp, temp3);
    } else if (ctx->GREATEQ()) {
      code = code || instruction::LE(temp3, addr2, addr1);
      code = code || instruction::NOT(temp, temp3);
    }
  }

  CodeAttribs codeAts(temp, "", code);

  DEBUG_EXIT();
  return codeAts;
}

antlrcpp::Any CodeGenVisitor::visitBoolean(AslParser::BooleanContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codeAt1 = visit(ctx->expr(0));
  std::string addr1 = codeAt1.addr;
  instructionList &code1 = codeAt1.code;

  CodeAttribs &&codAt2 = visit(ctx->expr(1));
  std::string addr2 = codAt2.addr;
  instructionList &code2 = codAt2.code;

  instructionList &&code = code1 || code2;

  std::string temp = "%" + codeCounters.newTEMP();
  if (ctx->AND())
    code = code || instruction::AND(temp, addr1, addr2);
  else if (ctx->OR())
    code = code || instruction::OR(temp, addr1, addr2);

  CodeAttribs codeAt(temp, "", code);

  DEBUG_EXIT();
  return codeAt;
}

antlrcpp::Any
CodeGenVisitor::visitParenthesis(AslParser::ParenthesisContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codeAt = visit(ctx->expr());
  DEBUG_EXIT();
  return codeAt;
}

antlrcpp::Any CodeGenVisitor::visitValue(AslParser::ValueContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string temp = "%" + codeCounters.newTEMP();
  if (ctx->INTVAL())
    code = instruction::ILOAD(temp, ctx->getText());
  else if (ctx->FLOATVAL())
    code = instruction::FLOAD(temp, ctx->getText());
  else if (ctx->CHARVAL())
    code = instruction::CHLOAD(temp, ctx->getText());
  else
    FALTA("no se com incialitzar var = true");
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitIdent(AslParser::IdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs codAts(ctx->ID()->getText(), "", instructionList());
  DEBUG_EXIT();
  return codAts;
}

// Getters for the necessary tree node atributes:
//   Scope and Type
SymTable::ScopeId
CodeGenVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId
CodeGenVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getType(ctx);
}

// Constructors of the class CodeAttribs:
//
CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string &addr,
                                         const std::string &offs,
                                         instructionList &code)
    : addr{addr}, offs{offs}, code{code} {}

CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string &addr,
                                         const std::string &offs,
                                         instructionList &&code)
    : addr{addr}, offs{offs}, code{code} {}
