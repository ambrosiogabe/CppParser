#pragma once

namespace CppParser
{
	namespace FileIO
	{
		char* DefaultReadFile(const char* filepath);
		void DefaultFreeFile(char* fileContents);
	}
}