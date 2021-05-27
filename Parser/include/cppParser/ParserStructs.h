#pragma once
#include "cppParser/ScriptScanner.h"
#include "cppParser/Ast.h"
#include "CppUtils/CppUtils.h"

#include <list>
#include <filesystem>

namespace CppParser
{
	using namespace CppUtils;
	typedef void(*AstWalkTreeCallbackFn)(AstNode* node);
	typedef void(*AstWalkPpTreeCallbackFn)(PreprocessingAstNode* node);
	typedef void(*AstWalkTreeUserDataCallbackFn)(AstNode* node, void* userData);
	typedef void(*AstWalkPpTreeUserDataCallbackFn)(PreprocessingAstNode* node, void* userData);

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
		List<DefineSymbol> DefineSymbols;
	};

	// This is not a POD because of the list
	struct ParserData
	{
		AstNode* Tree;
		PPSymbolTable PreprocessingSymbolTable;
		ScannerData Scanner;
		FileStream PreprocessOutputStream;
		int indentLevel;
		const char* fileBeingParsed;
		std::vector<std::filesystem::path> includeDirs;
	};
}