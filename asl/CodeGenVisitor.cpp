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
#include <vector>

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

  TypesMgr::TypeId function_type =
      Symbols.getGlobalFunctionType(ctx->ID()->getText());

  TypesMgr::TypeId return_type = Types.getFuncReturnType(function_type);
  if (not Types.isVoidTy(return_type))
    subr.add_param("_result", Types.to_string(return_type));

  if (ctx->parameters()) {
    std::vector<std::pair<std::string, TypesMgr::TypeId>> params =
        visit(ctx->parameters());
    for (auto p : params)
      subr.add_param(p.first, Types.to_string(p.second));
  }

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
CodeGenVisitor::visitParameters(AslParser::ParametersContext *ctx) {
  DEBUG_ENTER();
  std::vector<std::pair<std::string, TypesMgr::TypeId>> params;
  for (auto p : ctx->parameter())
    params.push_back(visit(p));

  DEBUG_EXIT();
  return params;
}
antlrcpp::Any CodeGenVisitor::visitParameter(AslParser::ParameterContext *ctx) {
  DEBUG_ENTER();

  TypesMgr::TypeId t = getTypeDecor(ctx);
  std::string name = ctx->ID()->getText();

  DEBUG_EXIT();
  return std::make_pair(name, t);
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
  CodeAttribs &&expr = visit(ctx->expr());
  instructionList body = visit(ctx->statements(0));

  std::string s = codeCounters.newLabelIF();
  std::string elseLbl = "else" + s;
  std::string endifLbl = "endif" + s;

  if (ctx->ELSE()) {
    instructionList elsebody = visit(ctx->statements(1));
    code = code || expr.code;
    code = code || instruction::FJUMP(expr.addr, elseLbl);
    code = code || body;
    code = code || instruction::UJUMP(endifLbl);
    code = code || instruction::LABEL(elseLbl);
    code = code || elsebody;
    code = code || instruction::LABEL(endifLbl);
  } else {
    code = code || expr.code;
    code = code || instruction::FJUMP(expr.addr, endifLbl);
    code = code || body;
    code = code || instruction::LABEL(endifLbl);
  }

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
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->expr());
  if (Types.isIntegerTy(tid1))
    code = code1 || instruction::WRITEI(addr1);
  else if (Types.isFloatTy(tid1))
    code = code1 || instruction::WRITEF(addr1);
  else if (Types.isCharacterTy(tid1))
    code = code1 || instruction::WRITEC(addr1);
  else if (Types.isBooleanTy(tid1))
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

antlrcpp::Any CodeGenVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&expr = visit(ctx->expr());
  instructionList body = visit(ctx->statements());
  instructionList code;

  std::string s = codeCounters.newLabelWHILE();
  std::string ini = "while" + s;
  std::string endwhile = "endwhile" + s;

  code = code || instruction::LABEL(ini);
  code = code || expr.code;
  code = code || instruction::FJUMP(expr.addr, endwhile);
  code = code || body;
  code = code || instruction::UJUMP(ini);
  code = code || instruction::LABEL(endwhile);

  // CodeAttribs codeAts(0, "", code);
  // DEBUG_EXIT();
  // return codeAts;

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any
CodeGenVisitor::visitReturnStmt(AslParser::ReturnStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  if (ctx->expr()) {
    CodeAttribs &&retCode = visit(ctx->expr());
    code = code || retCode.code;
    code = code || instruction::LOAD("_result", retCode.addr);
  }
  code = code || instruction::RETURN();
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitFunCall(AslParser::FunCallContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codAts = visit(ctx->fun_call());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitFun_call(AslParser::Fun_callContext *ctx) {
  DEBUG_ENTER();

  TypesMgr::TypeId tfun =
      Symbols.getGlobalFunctionType(ctx->ident()->getText());
  std::vector<TypesMgr::TypeId> params = Types.getFuncParamsTypes(tfun);
  std::vector<CodeAttribs> paramsCode;
  for (auto c : ctx->expr())
    paramsCode.push_back(visit(c));
  size_t n_param = paramsCode.size();

  instructionList code;
  for (int i = 0; i < n_param; ++i) {
    code = code || paramsCode[i].code;
    if (Types.isFloatTy(params[i]) and
        Types.isIntegerTy(getTypeDecor(ctx->expr(i)))) {
      std::string temp = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp, paramsCode[i].addr);
      paramsCode[i].addr = temp;
    }
  }

  code = code || instruction::PUSH();
  for (int i = 0; i < n_param; ++i) {
    code = code || instruction::PUSH(paramsCode[i].addr);
  }

  code = code || instruction::CALL(ctx->ident()->getText());
  for (int i = 0; i < n_param; ++i)
    code = code || instruction::POP();
  std::string ret = "%" + codeCounters.newTEMP();
  code = code || instruction::POP(ret);

  CodeAttribs CodeAtr(ret, "", code);
  DEBUG_EXIT();
  return CodeAtr;
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
  CodeAttribs codAt(temp, "", code);
  DEBUG_EXIT();
  return codAt;
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
      code = code || instruction::FMUL(temp, temp1, temp2);
    else if (ctx->DIV())
      code = code || instruction::FDIV(temp, temp1, temp2);
    else if (ctx->PLUS())
      code = code || instruction::FADD(temp, temp1, temp2);
    else if (ctx->MINUS())
      code = code || instruction::FSUB(temp, temp1, temp2);

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
      code = code || instruction::FEQ(temp, temp1, temp2);
    } else if (ctx->NEQUAL()) {
      code = code || instruction::FEQ(temp3, temp1, temp2);
      code = code || instruction::NOT(temp, temp3);
    } else if (ctx->LESS()) {
      code = code || instruction::FLT(temp, temp1, temp2);
    } else if (ctx->LESSEQ()) {
      code = code || instruction::FLE(temp, temp1, temp2);
    } else if (ctx->GREAT()) {
      code = code || instruction::FLE(temp3, temp2, temp1);
      code = code || instruction::NOT(temp, temp3);
    } else if (ctx->GREATEQ()) {
      code = code || instruction::FLE(temp3, temp2, temp1);
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
      code = code || instruction::LE(temp3, addr1, addr2);
      code = code || instruction::NOT(temp, temp3);
    } else if (ctx->GREATEQ()) {
      code = code || instruction::LT(temp3, addr1, addr2);
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
  else if (ctx->TRUEVAL())
    code = instruction::ILOAD(temp, "1");
  else if (ctx->FALSEVAL())
    code = instruction::ILOAD(temp, "0");
  // FALTA("no se com incialitzar var = true");
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
