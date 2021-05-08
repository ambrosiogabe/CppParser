#ifndef GABE_PARSER_SCRIPT_PARSER
#define GABE_PARSER_SCRIPT_PARSER
#include "cppParser/ScriptScanner.h"
#include "cppParser/Ast.h"
#include "CppUtils/CppUtils.h"

#include <filesystem>

namespace CppParser
{
	typedef void(*AstWalkTreeCallbackFn)(AstNode* node);
	typedef void(*AstWalkPpTreeCallbackFn)(PreprocessingAstNode* node);
	typedef void(*AstWalkTreeUserDataCallbackFn)(AstNode* node, void* userData);
	typedef void(*AstWalkPpTreeUserDataCallbackFn)(PreprocessingAstNode* node, void* userData);

	namespace Parser
	{
		using namespace CppUtils;

		AstNode* Parse(const char* fileBeingParsed, std::vector<std::filesystem::path>& includeDirs, List<Token>& tokens);

		void FreeTree(AstNode* tree);

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