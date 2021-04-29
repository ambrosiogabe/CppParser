#pragma once
#include "cppParser/Types.h"
#include "cppParser/CppTokens.h"
#include "cppParser/ParserString.h"

namespace CppParser
{
	namespace ScriptScanner 
	{
		CPP_PARSER_VECTOR(Token) ScanTokens(const CPP_PARSER_PATH& filepath, bool includeWhitespace=false);

		void DebugPrint(const CPP_PARSER_VECTOR(Token)& tokens, bool printLineAndCol=true, bool printWhitespace=false);
		const char* TokenName(TokenType type);

		void FreeTokens(CPP_PARSER_VECTOR(Token)& tokens);

		inline Token GenerateErrorToken();
	};
}
