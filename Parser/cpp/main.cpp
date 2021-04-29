#include "cppParser/Types.h"
#include "cppParser/ScriptScanner.h"
#include "cppParser/ScriptParser.h"

namespace CppParser
{
	void PrintClasses(AstNode* classVirtualHead)
	{
		if (ParserString::Compare(classVirtualHead->classVirtualHead.classKey->classKey.token.m_Lexeme , "struct"))
			printf("Struct: %s\n", classVirtualHead->classVirtualHead.classHeadName->classHeadName.className->className.identifier.m_Lexeme);
		else if (ParserString::Compare(classVirtualHead->classVirtualHead.classKey->classKey.token.m_Lexeme, "class"))
			printf("Class: %s\n", classVirtualHead->classVirtualHead.classHeadName->classHeadName.className->className.identifier.m_Lexeme);
	}

	void PrintClassHeadNames(AstNode* classHeadName)
	{
		printf("Class Head Name: %s\n", classHeadName->classHeadName.className->className.identifier.m_Lexeme);
	}
}

int main()
{
	using namespace CppParser;

	std::vector<Token> tokens = ScriptScanner::ScanTokens("C:/dev/C++/C++Parser/Parser/test/testParser.cpp");
	//ScriptScanner::DebugPrint(tokens);
	AstNode* parseTree = Parser::Parse(tokens);
	Parser::WalkTree(parseTree, CppParser::PrintClasses, AstNodeType::ClassVirtualHead);
	Parser::FreeTree(parseTree);

	return 0;
}