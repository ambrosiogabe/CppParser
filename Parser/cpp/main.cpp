#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#define GABE_CPP_UTILS_IMPL
#include "CppUtils/CppUtils.h"
#undef GABE_CPP_UTILS_IMPL

#include "cppParser/ScriptScanner.h"
#include "cppParser/ScriptParser.h"
#include "cppParser/ParserString.h"

#include <filesystem>

#include <crtdbg.h>

namespace CppParser
{
	void PrintClasses(AstNode* classVirtualHead)
	{
		if (ParserString::Compare(classVirtualHead->classVirtualHead.classKey->classKey.token.m_Lexeme, "struct"))
			printf("Struct: %s\n", classVirtualHead->classVirtualHead.classHeadName->classHeadName.className->className.identifier.m_Lexeme);
		else if (ParserString::Compare(classVirtualHead->classVirtualHead.classKey->classKey.token.m_Lexeme, "class"))
			printf("Class: %s\n", classVirtualHead->classVirtualHead.classHeadName->classHeadName.className->className.identifier.m_Lexeme);
	}
}

int main()
{
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF | _CRTDBG_CHECK_ALWAYS_DF);
	{
		using namespace CppParser;
		using namespace CppUtils;

		std::vector<std::filesystem::path> includeDirs = {};
		ParserData parserData = Parser::Parse("testParser.cpp", includeDirs);
		//Parser::WalkTree(parseTree, CppParser::PrintClasses, false, AstNodeType::ClassVirtualHead);
		Parser::FreeParserData(parserData);
	}

	return 0;
}