#pragma once
#include "cppParser/Types.h"
#include "cppParser/CppTokens.h"
#include "cppParser/ParserString.h"

#include <vector>

namespace CppParser
{
	namespace ScriptScanner 
	{
		std::vector<Token> ScanTokens(const char* filepath, bool includeWhitespace=false);

		void DebugPrint(const std::vector<Token>& tokens, bool printLineAndCol=true, bool printWhitespace=false);
		const char* TokenName(TokenType type);

		void FreeTokens(std::vector<Token>& tokens);

		inline Token GenerateErrorToken();
	};
}
