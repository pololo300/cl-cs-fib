/////////////////////////////////////////////////////////////////
//
//    Main program - Front-end for the Asl programming language.
//                   It performs the translation and
//                   writes the generated code
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
//    contact: Lluís Padró (padro@cs.upc.edu)
//             José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
////////////////////////////////////////////////////////////////

#include "AslLexer.h"
#include "AslParser.h"
#include "antlr4-runtime.h"

#include "../common/SemErrors.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/TypesMgr.h"
#include "../common/code.h"
#include "CodeGenVisitor.h"
#include "SymbolsVisitor.h"
#include "TypeCheckVisitor.h"

#include <fstream> // ifstream
#include <iostream>

#include <cstdio>  // fopen
#include <cstdlib> // EXIT_FAILURE, EXIT_SUCCESS
#include <cstring> // strcmp

// using namespace std;
// using namespace antlr4;

int main(int argc, const char *argv[]) {
  // early stop options
  bool onlySyntaxOpt = (argc > 1 and std::strcmp(argv[1], "--onlySyntax") == 0);
  bool onlySyntaxOpt2 =
      (argc > 2 and std::strcmp(argv[2], "--onlySyntax") == 0);
  bool noCodegenOpt = (argc > 1 and std::strcmp(argv[1], "--noCodegen") == 0);
  bool noCodeGenOpt2 = (argc > 2 and std::strcmp(argv[2], "--noCodegen") == 0);
  // check options and correct use of the program
  if (argc > 3 or onlySyntaxOpt2 or noCodeGenOpt2 or
      (argc == 3 and not onlySyntaxOpt and not noCodegenOpt)) {
    std::cout << "Usage: ./asl [--onlySyntax|--noCodegen] [<file>]"
              << std::endl;
    return EXIT_FAILURE;
  }
  if (argc == 3 or (argc == 2 and not onlySyntaxOpt and not noCodegenOpt)) {
    if (not std::fopen(argv[argc - 1], "r")) {
      std::cout << "No such file: " << argv[argc - 1] << std::endl;
      return EXIT_FAILURE;
    }
  }

  // open input file (or std::cin) and create a character stream
  antlr4::ANTLRInputStream input;
  if ((argc == 3) or (argc == 2 and not onlySyntaxOpt and
                      not noCodegenOpt)) { // read from <file>
    std::ifstream stream;
    stream.open(argv[argc - 1]);
    input = antlr4::ANTLRInputStream(stream);
  } else { // read fron std::cin
    input = antlr4::ANTLRInputStream(std::cin);
  }

  // create a lexer that consumes the character stream and produces a token
  // stream
  AslLexer lexer(&input);
  antlr4::CommonTokenStream tokens(&lexer);

  // create a parser that consumes the token stream, and parses it.
  AslParser parser(&tokens);

  // call the parser and get the parse tree
  antlr4::tree::ParseTree *tree = parser.program();

  // check for lexical or syntactical errors
  if (lexer.getNumberOfSyntaxErrors() > 0 or
      parser.getNumberOfSyntaxErrors() > 0) {
    std::cout << "Lexical and/or syntactical errors have been found."
              << std::endl;
    return EXIT_FAILURE;
  }

  // print the parse tree (for debugging purposes)
  // std::cout << tree->toStringTree(&parser) << std::endl;

  if (onlySyntaxOpt) {
    std::cout << "-- Early stop: no typecheck has been made." << std::endl;
    return EXIT_SUCCESS;
  }

  // auxililary classes we are going to need to store information while
  // traversing the tree. They are described below in this document
  TypesMgr types;
  SymTable symbols(types);
  TreeDecoration decorations;
  SemErrors errors;

  // create a visitor that looks for variables and function declarations
  // in the tree and stores required information
  SymbolsVisitor symboldecl(types, symbols, decorations, errors);
  symboldecl.visit(tree);

  // std::cout << "-- SIMBOLS VISITOR DONE " << std::endl;

  // create another visitor that will perform type checkings wherever
  // it is needed (on expressions, assignments, parameter passing, etc)
  TypeCheckVisitor typecheck(types, symbols, decorations, errors);
  typecheck.visit(tree);

  // std::cout << "-- TYPE CHECK VISITOR DONE " << std::endl;

  if (errors.getNumberOfSemanticErrors() > 0) {
    std::cout << "There are semantic errors: no code generated." << std::endl;
    return EXIT_FAILURE;
  }

  if (noCodegenOpt) {
    std::cout << "-- Early stop: no code generated." << std::endl;
    return EXIT_SUCCESS;
  }

  // create a third visitor that will return the generated code
  // for each part of the tree, and will store it in 'mycode'
  CodeGenVisitor codegenerator(types, symbols, decorations);
  code mycode = codegenerator.visit(tree);

  // print generated code as output
  std::cout << mycode.dump() << std::endl;

  // uncomment the following lines to generate LLVM code
  // and write it to a .ll file
  std::string llvmStr = mycode.dumpLLVM(types, symbols);
  std::string llvmFileName;
  if (argc == 2) { // read from <file>
    std::string inputFileName = std::string(argv[1]);
    std::size_t slashPos = inputFileName.rfind("/");
    std::size_t dotPos = inputFileName.rfind(".");
    llvmFileName =
        inputFileName.substr(slashPos + 1, dotPos - slashPos - 1) + ".ll";
  } else { // read fron std::cin
    llvmFileName = "output.ll";
  }
  std::ofstream myLLVMFile(llvmFileName, std::ofstream::out);
  myLLVMFile << llvmStr << std::endl;

  return EXIT_SUCCESS;
}
