#pragma once
#include "cppParser/ScriptScanner.h"
#include "cppParser/Ast.h"
#include "cppParser/Types.h"

namespace CppParser
{
	namespace Parser
	{
		AstNode* Parse(std::vector<Token>& tokens);
		void WalkTree(AstNode* tree, void(*callbackFn)(AstNode* node), AstNodeType notificationType = AstNodeType::All);
		void FreeTree(AstNode* tree);
	}
}