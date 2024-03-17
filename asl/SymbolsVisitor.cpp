//////////////////////////////////////////////////////////////////////
//
//    SymbolsVisitor - Walk the parser tree to register symbols
//                     for the Asl programming language
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

#include "SymbolsVisitor.h"
#include "antlr4-runtime.h"

#include "../common/SemErrors.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/TypesMgr.h"

#include <iostream>
#include <string>
#include <vector>

#include <cstddef> // std::size_t

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;

// Constructor
SymbolsVisitor::SymbolsVisitor(TypesMgr &Types, SymTable &Symbols,
                               TreeDecoration &Decorations, SemErrors &Errors)
    : Types{Types}, Symbols{Symbols}, Decorations{Decorations}, Errors{Errors} {
}

// Methods to visit each kind of node:
//
antlrcpp::Any SymbolsVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = Symbols.pushNewScope(SymTable::GLOBAL_SCOPE_NAME);
  putScopeDecor(ctx, sc);
  for (auto ctxFunc : ctx->function()) {
    visit(ctxFunc);
  }
  // Symbols.print();
  Symbols.popScope();
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any SymbolsVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  std::string funcName = ctx->ID()->getText();
  SymTable::ScopeId sc = Symbols.pushNewScope(funcName);
  putScopeDecor(ctx, sc);

  // declarations
  if (ctx->parameters())
    visit(ctx->parameters());

  // declarations
  visit(ctx->declarations());

  // Symbols.print();
  Symbols.popScope();
  std::string ident = ctx->ID()->getText();
  if (Symbols.findInCurrentScope(ident)) {
    Errors.declaredIdent(ctx->ID());
  } else {
    // return type
    TypesMgr::TypeId tRet;
    if (ctx->type()) {
      visit(ctx->type());
      tRet = getTypeDecor(ctx->type());
    } else
      tRet = Types.createVoidTy();

    // paramters types list
    std::vector<TypesMgr::TypeId> lParamsTy;
    if (ctx->parameters()) {
      for (auto pa : ctx->parameters()->parameter())
        lParamsTy.push_back(getTypeDecor(pa));
    }

    if (Symbols.findInCurrentScope(ident))
      Errors.declaredIdent(ctx->ID());
    else {
      TypesMgr::TypeId tFunc = Types.createFunctionTy(lParamsTy, tRet);
      Symbols.addFunction(ident, tFunc);
    }
  }

  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any
SymbolsVisitor::visitParameters(AslParser::ParametersContext *ctx) {
  DEBUG_ENTER();
  for (auto p : ctx->parameter())
    visit(p);
  // visitChildren(ctx);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any SymbolsVisitor::visitParameter(AslParser::ParameterContext *ctx) {
  DEBUG_ENTER();

  visit(ctx->type());
  TypesMgr::TypeId t = getTypeDecor(ctx->type());
  std::string ident = ctx->ID()->getText();
  if (Symbols.findInCurrentScope(ident)) {
    Errors.declaredIdent(ctx->ID());
  } else {
    Symbols.addLocalVar(ident, t);
    putTypeDecor(ctx, t);
  }

  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any
SymbolsVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any
SymbolsVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->type());

  int n = ctx->ID().size();
  for (int i = 0; i < n; ++i) {
    std::string ident = ctx->ID(i)->getText();
    if (Symbols.findInCurrentScope(ident)) {
      Errors.declaredIdent(ctx->ID(i));
    } else {
      TypesMgr::TypeId t1 = getTypeDecor(ctx->type());
      Symbols.addLocalVar(ident, t1);
    }
  }
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any
SymbolsVisitor::visitBasic_type(AslParser::Basic_typeContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId t;
  if (ctx->INT())
    t = Types.createIntegerTy();
  else if (ctx->FLOAT())
    t = Types.createFloatTy();
  else if (ctx->CHAR())
    t = Types.createCharacterTy();
  else if (ctx->BOOL())
    t = Types.createBooleanTy();

  putTypeDecor(ctx, t);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any SymbolsVisitor::visitT_array(AslParser::T_arrayContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->basic_type());
  int size = std::stoi(ctx->INTVAL()->getText());
  if (size < 1) {
    Errors.nonIntegerIndexInArrayAccess(ctx);
    size = 1;
  }

  TypesMgr::TypeId bt = getTypeDecor(ctx->basic_type());
  TypesMgr::TypeId t = Types.createArrayTy(size, bt);
  putTypeDecor(ctx, t);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any SymbolsVisitor::visitT_basic(AslParser::T_basicContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->basic_type());
  TypesMgr::TypeId t = getTypeDecor(ctx->basic_type());
  putTypeDecor(ctx, t);
  DEBUG_EXIT();
  return 0;
}

// antlrcpp::Any
// SymbolsVisitor::visitStatements(AslParser::StatementsContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitAssignStmt(AslParser::AssignStmtContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitIfStmt(AslParser::IfStmtContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitProcCall(AslParser::ProcCallContext
// *ctx)
// {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitReadStmt(AslParser::ReadStmtContext
// *ctx)
// {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitWriteExpr(AslParser::WriteExprContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitWriteString(AslParser::WriteStringContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitLeft_expr(AslParser::Left_exprContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitExprIdent(AslParser::ExprIdentContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitArithmetic(AslParser::ArithmeticContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitRelational(AslParser::RelationalContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitValue(AslParser::ValueContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any
// SymbolsVisitor::visitIdent(AslParser::IdentContext
// *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r =
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// Getters for the necessary tree
// node atributes:
//   Scope and Type
SymTable::ScopeId
SymbolsVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId SymbolsVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getType(ctx);
}

// Setters for the necessary tree
// node attributes:
//   Scope and Type
void SymbolsVisitor::putScopeDecor(antlr4::ParserRuleContext *ctx,
                                   SymTable::ScopeId s) {
  Decorations.putScope(ctx, s);
}
void SymbolsVisitor::putTypeDecor(antlr4::ParserRuleContext *ctx,
                                  TypesMgr::TypeId t) {
  Decorations.putType(ctx, t);
}
