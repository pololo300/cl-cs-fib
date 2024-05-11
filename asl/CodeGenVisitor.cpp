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
  setCurrentFunctionTy(function_type);

  TypesMgr::TypeId return_type = Types.getFuncReturnType(function_type);
  if (not Types.isVoidTy(return_type))
    subr.add_param("_result", Types.to_string(return_type));

  if (ctx->parameters()) {
    std::vector<std::pair<std::string, TypesMgr::TypeId>> params =
        visit(ctx->parameters());
    for (auto p : params) {
      if (Types.isArrayTy(p.second))
        subr.add_param(p.first,
                       Types.to_string(Types.getArrayElemType(p.second)) +
                           " array");
      else
        subr.add_param(p.first, Types.to_string(p.second));
    }
  }

  std::vector<var> &&lvars = visit(ctx->declarations());
  for (auto &onevar : lvars) {
    subr.add_var(onevar);
  }

  Symbols.addLocalVar("__idex__aux__", Types.createIntegerTy());
  subr.add_var(var{"__idex__aux__", "integer", 1});

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

  if (Types.isArrayTy(t1))
    t1 = Types.getArrayElemType(t1);

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
  std::string offs1 = codAtsE1.offs;
  instructionList &code1 = codAtsE1.code;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());

  CodeAttribs codAtsE2 = visit(ctx->expr());
  std::string addr2 = codAtsE2.addr;
  std::string offs2 = codAtsE2.offs;
  instructionList &code2 = codAtsE2.code;
  TypesMgr::TypeId tid2 = getTypeDecor(ctx->expr());

  code = code1 || code2;

  std::string addr = addr2;
  if (Types.isFloatTy(tid1) and Types.isIntegerTy(tid2)) {
    addr = "%" + codeCounters.newTEMP();
    code = code || instruction::FLOAT(addr, addr2);
  }

  if (offs1 == "" and offs2 == "" and
      not Types.isArrayTy(tid1)) { // cap dels dos es array acces ni array
    code = code || instruction::LOAD(addr1, addr);
  } else if (offs1 != "" and offs2 == "") { // array acces a la esquerra
    code = code || instruction::XLOAD(addr1, offs1, addr2);
  }

  if (Types.isArrayTy(tid1) and Types.isArrayTy(tid2)) {

    std::string size = "%" + codeCounters.newTEMP();
    code = code ||
           instruction::ILOAD(size, std::to_string(Types.getArraySize(tid1)));
    code = code || instruction::ILOAD("__idex__aux__", "0");
    std::string one = "%" + codeCounters.newTEMP();
    code = code || instruction::ILOAD(one, "1");

    // while tags
    std::string wl = codeCounters.newTEMP();
    std::string winit = "whileArrayAs" + wl;
    std::string wend = "endwhilearrayAs" + wl;
    code = code || instruction::LABEL(winit);

    std::string cond = "%" + codeCounters.newTEMP();
    code = code || instruction::EQ(cond, "__idex__aux__", size);
    std::string neg_cond = "%" + codeCounters.newTEMP();
    code = code || instruction::NOT(neg_cond, cond);
    code = code || instruction::FJUMP(neg_cond, wend);

    std::string value = "%" + codeCounters.newTEMP();
    code = code || instruction::LOADX(value, addr2, "__idex__aux__");
    code = code || instruction::XLOAD(addr1, "__idex__aux__", value);

    code = code || instruction::ADD("__idex__aux__", "__idex__aux__", one);

    code = code || instruction::UJUMP(winit);
    code = code || instruction::LABEL(wend);
  }
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

antlrcpp::Any CodeGenVisitor::visitVoid_call(AslParser::Void_callContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codAt = visit(ctx->fun_call());

  TypesMgr::TypeId tfun =
      Symbols.getGlobalFunctionType(ctx->fun_call()->ident()->getText());
  TypesMgr::TypeId tret = Types.getFuncReturnType(tfun);

  DEBUG_EXIT();
  return codAt.code;
}

antlrcpp::Any CodeGenVisitor::visitProcCall(AslParser::ProcCallContext *ctx) {
  DEBUG_ENTER();

  instructionList code;
  std::string name = ctx->ident()->getText();
  code = instruction::CALL(name);
  TypesMgr::TypeId tfun = Symbols.getGlobalFunctionType(name);
  TypesMgr::TypeId tret = Types.getFuncReturnType(tfun);

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codAtsE = visit(ctx->left_expr());
  std::string addr1 = codAtsE.addr;
  std::string offs1 = codAtsE.offs;
  instructionList &code1 = codAtsE.code;
  instructionList &code = code1;

  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());

  std::string addr = "%" + codeCounters.newTEMP();

  if (offs1 == "") {
    addr = addr1;
  }

  if (Types.isIntegerTy(tid1) or Types.isBooleanTy(tid1))
    code = code1 || instruction::READI(addr);
  else if (Types.isFloatTy(tid1))
    code = code1 || instruction::READF(addr);
  else if (Types.isCharacterTy(tid1))
    code = code1 || instruction::READC(addr);

  if (offs1 != "") {
    code = code || instruction::XLOAD(addr1, offs1, addr);
  }

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&codAt1 = visit(ctx->expr());
  std::string addr1 = codAt1.addr;
  std::string offs1 = codAt1.offs;
  instructionList &code1 = codAt1.code;
  instructionList &code = code1;

  if (offs1 != "") {
    std::string temp = "%" + codeCounters.newTEMP();
    code = code || instruction::LOADX(temp, addr1, offs1);
    addr1 = temp;
  }

  TypesMgr::TypeId tid1 = getTypeDecor(ctx->expr());
  if (Types.isIntegerTy(tid1))
    code = code1 || instruction::WRITEI(addr1);
  else if (Types.isFloatTy(tid1))
    code = code1 || instruction::WRITEF(addr1);
  else if (Types.isCharacterTy(tid1)) {
    code = code1 || instruction::WRITEC(addr1);
  } else if (Types.isBooleanTy(tid1))
    code = code1 || instruction::WRITEI(addr1);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any
CodeGenVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string s = ctx->STRING()->getText();
  s.pop_back();

  std::string subs;

  for (int i = 1; i < s.size(); ++i) {
    if (s[i] == '\\' and s[1 + i] == 'n') {
      if (subs.size() > 0)
        code = code || instruction::WRITES("\"" + subs + "\"");
      code = code || instruction::WRITELN();
      i++;
      subs = "";
    } else {
      subs.push_back(s[i]);
    }
  }

  if (subs.size() > 0)
    code = code || instruction::WRITES("\"" + subs + "\"");

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitLeft_expr(AslParser::Left_exprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs codeAts("", "", instructionList());
  if (ctx->ident())
    codeAts = visit(ctx->ident());
  else if (ctx->accesor()) {
    codeAts = visit(ctx->accesor());
  }

  DEBUG_EXIT();
  return codeAts;
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
  // instructionList code;
  // if (ctx->expr()) {
  //   CodeAttribs &&retCode = visit(ctx->expr());
  //   code = code || retCode.code;
  //   code = code || instruction::LOAD("_result", retCode.addr);
  // }
  // code = code || instruction::RETURN();

  instructionList code;
  if (ctx->expr()) {
    ;
    CodeAttribs &&retCode = visit(ctx->expr());
    TypesMgr::TypeId tfun = getCurrentFunctionTy();
    TypesMgr::TypeId tret = Types.getFuncReturnType(tfun);
    TypesMgr::TypeId t = getTypeDecor(ctx->expr());

    code = retCode.code;

    std::string addr = retCode.addr;

    if (Types.isFloatTy(tret) and Types.isIntegerTy(t)) {
      std::string temp = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp, addr);
      addr = temp;
    }

    code = code || instruction::LOAD("_result", addr);
  }
  code = code || instruction::RETURN();
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitArray(AslParser::ArrayContext *ctx) {
  DEBUG_ENTER();

  CodeAttribs &&codeAtr = visit(ctx->accesor());
  instructionList code = codeAtr.code;

  std::string temp = "%" + codeCounters.newTEMP();
  code = code || instruction::LOADX(temp, codeAtr.addr, codeAtr.offs);

  DEBUG_EXIT();
  return CodeAttribs(temp, "", code);
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
  TypesMgr::TypeId tret = Types.getFuncReturnType(tfun);
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
    } else if (Types.isArrayTy(params[i])) {
      std::string temp = "%" + codeCounters.newTEMP();
      code = code || instruction::ALOAD(temp, paramsCode[i].addr);
      paramsCode[i].addr = temp;
    }
  }

  if (not Types.isVoidTy(tret))
    code = code || instruction::PUSH();

  for (int i = 0; i < n_param; ++i) {
    code = code || instruction::PUSH(paramsCode[i].addr);
  }

  code = code || instruction::CALL(ctx->ident()->getText());
  for (int i = 0; i < n_param; ++i)
    code = code || instruction::POP();
  std::string ret = "%" + codeCounters.newTEMP();

  if (not Types.isVoidTy(tret))
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
  } else if (ctx->PLUS()) {
    temp = addr;
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
    else if (ctx->MOD()) { // a % b = a - (a/b) * b
      std::string temp3 = "%" + codeCounters.newTEMP();
      code = code || instruction::DIV(temp3, addr1, addr2); // 3 = a/b
      std::string temp4 = "%" + codeCounters.newTEMP();
      code = code || instruction::MUL(temp4, temp3, addr2); // 4 = (a/b)*b
      std::string temp5 = "%" + codeCounters.newTEMP();
      code = code || instruction::NEG(temp5, temp4);       // 5 = -(a/b)*b
      code = code || instruction::ADD(temp, addr1, temp5); // temp = a -(a/b)*b
    }
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
  std::string temp1;
  std::string temp2;

  if (Types.isFloatTy(t1) or Types.isFloatTy(t2)) {
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
      code = code || instruction::FLE(temp3, temp1, temp2);
      code = code || instruction::NOT(temp, temp3);
    } else if (ctx->GREATEQ()) {
      code = code || instruction::FLT(temp3, temp1, temp2);
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
  else if (ctx->CHARVAL()) {
    std::string s = ctx->getText();
    std::string c;
    if (s[1] == '\\')
      c = (s[2] == 'n') ? ("\\n") : ("\\t");
    else
      c = s[1];
    code = instruction::CHLOAD(temp, c);
  } else if (ctx->TRUEVAL())
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

antlrcpp::Any CodeGenVisitor::visitAccesor(AslParser::AccesorContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&identCode = visit(ctx->ident());
  CodeAttribs &&indxCode = visit(ctx->expr());

  std::string temp = "%" + codeCounters.newTEMP();

  instructionList code;
  code = code || indxCode.code;

  CodeAttribs CodeAtr("", "", instructionList());
  std::string var = identCode.addr;
  if (Symbols.isParameterClass(var)) {
    std::string temp2 = "%" + codeCounters.newTEMP();
    code = code || instruction::LOAD(temp2, identCode.addr);
    CodeAtr = CodeAttribs(temp2, indxCode.addr, code);
  } else {
    CodeAtr = CodeAttribs(identCode.addr, indxCode.addr, code);
  }

  // CodeAttribs CodeAtr(identCode.addr, indxCode.addr, code);
  DEBUG_EXIT();
  return CodeAtr;
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
