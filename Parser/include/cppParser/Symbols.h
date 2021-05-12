#ifndef GABE_CPP_PARSER_SYMBOLS
#define GABE_CPP_PARSER_SYMBOLS
#include "cppParser/CppTokens.h"
#include "cppParser/Ast.h"
#include "CppUtils/CppUtils.h"

namespace CppParser
{
	using namespace CppUtils;

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
	public:
		~PPSymbolTable();

		List<DefineSymbol> DefineSymbols;
	};

	namespace Symbols
	{
		void AddUndefine(PPSymbolTable& symbolTable, const Token& token, int lineUndefined);
		void AddDefineSymbol(PPSymbolTable& symbolTable, const Token& macroIdentifierToken, int lineDefined, PreprocessingAstNode* symbolTree);
		void AddGlobalDefineSymbol(PPSymbolTable& symbolTable, const char* symbolName);
		List<Token> ExpandMacro(const PPSymbolTable& symbolTable, int currentToken, const List<Token>& tokens);

		bool IsDefined(const PPSymbolTable& symbolTable, const Token& token);
		bool IsFunctionMacroDefine(const PPSymbolTable& symbolTable, const Token& token);
		bool IsSymbol(const PPSymbolTable& symbolTable, const Token& token);
	}
}

#endif