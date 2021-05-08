#define GABE_CPP_UTILS_IMPL
#include "CppUtils/CppUtils.h"
#undef GABE_CPP_UTILS_IMPL

#include "cppParser/ScriptScanner.h"
#include "cppParser/ScriptParser.h"

#include <filesystem>

namespace CppParser
{
	void PrintClasses(AstNode* classVirtualHead)
	{
		if (ParserString::Compare(classVirtualHead->classVirtualHead.classKey->classKey.token.m_Lexeme , "struct"))
			printf("Struct: %s\n", classVirtualHead->classVirtualHead.classHeadName->classHeadName.className->className.identifier.m_Lexeme);
		else if (ParserString::Compare(classVirtualHead->classVirtualHead.classKey->classKey.token.m_Lexeme, "class"))
			printf("Class: %s\n", classVirtualHead->classVirtualHead.classHeadName->classHeadName.className->className.identifier.m_Lexeme);
	}
}

int main()
{
	using namespace CppParser;
	using namespace CppUtils;

	List<Token> tokens = ScriptScanner::ScanTokens("testParser.cpp");
	ScriptScanner::DebugPrint(tokens);
	std::vector<std::filesystem::path> includeDirs = {};
	AstNode* parseTree = Parser::Parse("testParser.cpp", includeDirs, tokens);
	Parser::WalkTree(parseTree, CppParser::PrintClasses, false, AstNodeType::ClassVirtualHead);
	Parser::FreeTree(parseTree);

	return 0;
}