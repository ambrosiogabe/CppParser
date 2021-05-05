#pragma once
#include "cppParser/CppTokens.h"
#include "cppParser/Ast.h"

#include <vector>

namespace CppParser
{
	struct TokenHashInfo
	{
		PreprocessingAstNode* replacementList;
		Token token;
		unsigned long hash;
		int lineDefined;
		int lineUndefined;
	};

	struct PPSymbolTable
	{
		std::vector<TokenHashInfo> SimpleDefines;
		std::vector<TokenHashInfo> FunctionDefines;
	};

	namespace Symbols
	{
		void AddUndefine(PPSymbolTable& symbolTable, const Token& token, int lineUndefined);
		void AddSimpleDefine(PPSymbolTable& symbolTable, const Token& token, int lineDefined, PreprocessingAstNode* replacementList);
		void AddFunctionDefine(PPSymbolTable& symbolTable, const Token& token, int lineDefined, PreprocessingAstNode* replacementList);
		std::vector<Token> ExpandMacroFunction(const PPSymbolTable& symbolTable, PreprocessingAstNode* preprocessingNode);
		std::vector<Token> ExpandSimpleMacro(const PPSymbolTable& symbolTable, PreprocessingAstNode* preprocessingNode, int newLine);
		std::vector<Token> ExpandMacro(const PPSymbolTable& symbolTable, const Token& token);

		bool IsDefined(const PPSymbolTable& symbolTable, const Token& token);
		bool IsSymbol(const PPSymbolTable& symbolTable, const Token& token);
	}
}