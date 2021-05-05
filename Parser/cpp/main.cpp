#include "cppParser/Types.h"
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

	std::vector<Token> tokens = ScriptScanner::ScanTokens("testParser.cpp");
	ScriptScanner::DebugPrint(tokens);
	std::vector<std::filesystem::path> includeDirs = {};
	AstNode* parseTree = Parser::Parse("testParser.cpp", includeDirs, tokens);
	Parser::WalkTree(parseTree, CppParser::PrintClasses, AstNodeType::ClassVirtualHead);
	Parser::FreeTree(parseTree);

	return 0;
}