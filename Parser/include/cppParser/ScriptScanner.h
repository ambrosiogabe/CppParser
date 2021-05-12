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
		void WriteTokensToFile(const List<Token>& tokens, const char* filename);
		const char* TokenName(TokenType type);

		void FreeTokens(List<Token>& tokens);
	};
}

#endif
