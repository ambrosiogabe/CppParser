#ifndef GABE_PARSER_FILE_IO
#define GABE_PARSER_FILE_IO

namespace CppParser
{
	namespace FileIO
	{
		const char* DefaultReadFile(const char* filepath);
		void DefaultFreeFile(const char* fileContents);
	}
}

#endif