#ifndef GABE_PARSER_SCRIPT_PARSER
#define GABE_PARSER_SCRIPT_PARSER
#include "cppParser/ScriptScanner.h"
#include "cppParser/Ast.h"
#include "cppParser/Symbols.h"
#include "CppUtils/CppUtils.h"

#include <filesystem>

namespace CppParser
{
	using namespace CppUtils;
	typedef void(*AstWalkTreeCallbackFn)(AstNode* node);
	typedef void(*AstWalkPpTreeCallbackFn)(PreprocessingAstNode* node);
	typedef void(*AstWalkTreeUserDataCallbackFn)(AstNode* node, void* userData);
	typedef void(*AstWalkPpTreeUserDataCallbackFn)(PreprocessingAstNode* node, void* userData);

	// This is not a POD because of the list
	struct ParserData
	{
		//List<Token> Tokens;
		int CurrentToken;
		AstNode* Tree;
		PPSymbolTable PreprocessingSymbolTable;
		ScannerData Scanner;
		FileStream PreprocessOutputStream;
		int indentLevel = 0;
	};

	namespace Parser
	{
		using namespace CppUtils;

		ParserData Parse(const char* file, std::vector<std::filesystem::path>& includeDirs, int osDefinitions);
		void FreeParserData(ParserData& parserData);

		// Walk tree functions
		void WalkTree(AstNode* tree, AstWalkTreeCallbackFn callbackFn, AstNodeType notificationType = AstNodeType::All, bool postTraversalCallback = false);
		void WalkPreprocessingTree(PreprocessingAstNode* tree, AstWalkPpTreeCallbackFn callbackFn, PreprocessingAstNodeType notificationType = PreprocessingAstNodeType::All,
			bool postTraversalCallback = false);

		// Walk tree with userData functions
		void WalkTree(AstNode* tree, void* userData, AstWalkTreeUserDataCallbackFn callbackFn, 
			AstNodeType notificationType = AstNodeType::All, bool postTraversalCallback = false);
		void WalkPreprocessingTree(PreprocessingAstNode* tree, void* userData, AstWalkPpTreeUserDataCallbackFn callbackFn, 
			PreprocessingAstNodeType notificationType = PreprocessingAstNodeType::All, bool postTraversalCallback = false);
	}
}

#endif