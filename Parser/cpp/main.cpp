//#define _CRTDBG_MAP_ALLOC
//#include <stdlib.h>
//#include <crtdbg.h>

#define GABE_CPP_UTILS_IMPL
#include "CppUtils/CppUtils.h"
#undef GABE_CPP_UTILS_IMPL

#include "cppParser/ScriptScanner.h"
#include "cppParser/ScriptParser.h"
#include "cppParser/ParserString.h"
#include "cppParser/PredefinedMacros.h"

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
	//_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF | _CRTDBG_CHECK_ALWAYS_DF);
	{
		using namespace CppParser;
		using namespace CppUtils;

		int osDefinitions = 0;
		osDefinitions |= ANDROID_DEFINED ? (int)PredefinedMacros::Android : (int)PredefinedMacros::None;
		osDefinitions |= APPLE_DEFINED ? (int)PredefinedMacros::Apple : (int)PredefinedMacros::None;
		osDefinitions |= WIN32_DEFINED ? (int)PredefinedMacros::Win32 : (int)PredefinedMacros::None;
		osDefinitions |= WIN64_DEFINED ? (int)PredefinedMacros::Win64 : (int)PredefinedMacros::None;
		osDefinitions |= LINUX_DEFINED ? (int)PredefinedMacros::Linux : (int)PredefinedMacros::None;
		osDefinitions |= UNIX_DEFINED ? (int)PredefinedMacros::Unix : (int)PredefinedMacros::None;
		osDefinitions |= FREE_BSD_DEFINED ? (int)PredefinedMacros::FreeBSD : (int)PredefinedMacros::None;
		std::vector<std::filesystem::path> includeDirs = {};

		const char* fileToTest = "testParser.cpp";
		List<Token> tokens = ScriptScanner::ScanTokens(fileToTest);
		ScriptScanner::DebugPrint(tokens);
		ScriptScanner::FreeTokens(tokens);

		ParserData parserData = Parser::Parse(fileToTest, includeDirs, osDefinitions);
		//Parser::WalkTree(parseTree, CppParser::PrintClasses, false, AstNodeType::ClassVirtualHead);
		Parser::FreeParserData(parserData);
	}

	return 0;
}