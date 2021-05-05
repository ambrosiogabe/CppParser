#pragma once
#include "cppParser/ScriptScanner.h"
#include "cppParser/Ast.h"
#include "cppParser/Types.h"

#include <filesystem>

namespace CppParser
{
	namespace Parser
	{
		AstNode* Parse(const char* fileBeingParsed, std::vector<std::filesystem::path>& includeDirs, std::vector<Token>& tokens);
		void WalkTree(AstNode* tree, void(*callbackFn)(AstNode* node), AstNodeType notificationType = AstNodeType::All);
		void WalkPreprocessingTree(PreprocessingAstNode* tree, void(*callbackFn)(PreprocessingAstNode* node), PreprocessingAstNodeType notificationType = PreprocessingAstNodeType::All);
		void FreeTree(AstNode* tree);
	}
}