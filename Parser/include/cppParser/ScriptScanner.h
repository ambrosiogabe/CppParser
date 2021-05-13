#ifndef GABE_PARSER_SCRIPT_SCANNER
#define GABE_PARSER_SCRIPT_SCANNER
#include "cppParser/CppTokens.h"
#include "cppParser/FileIO.h"
#include "CppUtils/CppUtils.h"

namespace CppParser
{
	struct ScannerData
	{
		int Line;
		int Column;
		int64_t Start;
		FileStream Stream;
		const char* Filepath;
	};

	namespace ScriptScanner 
	{
		using namespace CppUtils;
		ScannerData OpenScanner(const char* filepath);
		void CloseScanner(ScannerData& scanner);

		List<Token> ScanTokens(const char* filepath, bool includeWhitespace = false);
		Token ScanToken(ScannerData& scannerData, bool includeWhitespace = false);

		void DebugPrint(const List<Token>& tokens, bool printLineAndCol = true, bool printWhitespace = false);
		void WriteTokensToFile(const List<Token>& tokens, const char* filename);
		const char* TokenName(TokenType type);

		void FreeTokens(List<Token>& tokens);
	};
}

#endif
