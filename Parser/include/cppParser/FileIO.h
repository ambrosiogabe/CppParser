#ifndef GABE_PARSER_FILE_IO
#define GABE_PARSER_FILE_IO

namespace CppParser
{
	namespace FileIO
	{
		char* DefaultReadFile(const char* filepath);
		void DefaultFreeFile(char* fileContents);
	}
}

#endif