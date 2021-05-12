#ifndef GABE_PARSER_FILE_IO
#define GABE_PARSER_FILE_IO

namespace CppParser
{
	namespace FileIO
	{
		const char* DefaultReadFile(const char* filepath);
		void DefaultFreeFile(const char* fileContents);
		void DefaultWriteFile(const char* filepath, const char* fileContents);
	}
}

#endif