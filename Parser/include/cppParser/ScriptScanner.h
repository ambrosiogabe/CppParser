#ifndef GABE_PARSER_SCRIPT_SCANNER
#define GABE_PARSER_SCRIPT_SCANNER
#include "cppParser/CppTokens.h"
#include "CppUtils/CppUtils.h"

namespace CppParser
{
	namespace ScriptScanner 
	{
		using namespace CppUtils;

		List<Token> ScanTokens(const char* filepath, bool includeWhitespace=false);

		void DebugPrint(const List<Token>& tokens, bool printLineAndCol=true, bool printWhitespace=false);
		const char* TokenName(TokenType type);

		void FreeTokens(List<Token>& tokens);
	};
}

#endif
