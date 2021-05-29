#ifndef GABE_PARSER_SCRIPT_SCANNER
#define GABE_PARSER_SCRIPT_SCANNER
#include "cppParser/CppTokens.h"
#include "cppParser/FileIO.h"
#include "CppUtils/CppUtils.h"
#include "cppParser/ParserString.h"

namespace CppParser
{
	struct ScannerData
	{
		int64_t Start;
		CountingFileStream Stream;
		const char* Filepath;
	};

	namespace ScriptScanner 
	{
		using namespace CppUtils;
		ScannerData OpenScanner(const char* filepath);
		ScannerData OpenScanner(CountingFileStream stream);
		void CloseScanner(ScannerData& scanner);

		List<Token> ScanTokens(const char* filepath, bool includeWhitespace = false);
		Token ScanToken(ScannerData& scannerData, bool includeWhitespace = false);
		TokenType PeekToken(const ScannerData& scannerData, bool includeWhitespace = false);

		void DebugPrint(const List<Token>& tokens, bool printLineAndCol = true, bool printWhitespace = false);
		void WriteTokensToFile(const List<Token>& tokens, const char* filename);
		const char* TokenName(TokenType type);

		void FreeTokens(List<Token>& tokens);

		inline bool AtEnd(const ScannerData& data)
		{
			return FileIO::StreamAtEnd(data.Stream);
		}
	};
}

#endif
