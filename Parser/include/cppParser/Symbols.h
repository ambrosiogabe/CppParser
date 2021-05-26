#ifndef GABE_CPP_PARSER_SYMBOLS
#define GABE_CPP_PARSER_SYMBOLS
#include "cppParser/CppTokens.h"
#include "cppParser/Ast.h"
#include "CppUtils/CppUtils.h"
#include "cppParser/ParserStructs.h"

namespace CppParser
{
	using namespace CppUtils;

	namespace Symbols
	{
		void Undefine(PPSymbolTable& symbolTable, const Token& token, int lineUndefined);
		void AddDefineSymbol(PPSymbolTable& symbolTable, const Token& macroIdentifierToken, int lineDefined, PreprocessingAstNode* symbolTree);
		void AddGlobalDefineSymbol(PPSymbolTable& symbolTable, const char* symbolName);
		const char* ExpandMacro(ParserData& data, const Token& token);

		bool IsDefined(const PPSymbolTable& symbolTable, const Token& token);
		bool IsFunctionMacroDefine(const PPSymbolTable& symbolTable, const Token& token);
		bool IsSymbol(const PPSymbolTable& symbolTable, const Token& token);

		void FreeSymbols(PPSymbolTable& symbolTable);
	}
}

#endif