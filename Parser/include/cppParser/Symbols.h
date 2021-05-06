#pragma once
#include "cppParser/CppTokens.h"
#include "cppParser/Ast.h"

#include <vector>

namespace CppParser
{
	struct DefineSymbol
	{
		PreprocessingAstNode* symbolTree;
		Token token;
		unsigned long hash;
		int lineDefined;
		int lineUndefined;
	};

	struct PPSymbolTable
	{
		std::vector<DefineSymbol> DefineSymbols;
	};

	namespace Symbols
	{
		void AddUndefine(PPSymbolTable& symbolTable, const Token& token, int lineUndefined);
		void AddDefineSymbol(PPSymbolTable& symbolTable, const Token& macroIdentifierToken, int lineDefined, PreprocessingAstNode* symbolTree);
		std::vector<Token> ExpandMacro(const PPSymbolTable& symbolTable, int currentToken, const std::vector<Token>& tokens);

		bool IsDefined(const PPSymbolTable& symbolTable, const Token& token);
		bool IsFunctionMacroDefine(const PPSymbolTable& symbolTable, const Token& token);
		bool IsSymbol(const PPSymbolTable& symbolTable, const Token& token);
	}
}