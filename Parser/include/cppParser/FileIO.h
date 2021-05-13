#ifndef GABE_PARSER_FILE_IO
#define GABE_PARSER_FILE_IO

#include <stdio.h>
#include <cstdint>

namespace CppParser
{
	struct FileStream
	{
		int64_t ChunkStart;
		int64_t Cursor;
		int64_t Size;
		char* Data;
		const char* Filepath;
		FILE* fp;
	};

	namespace FileIO
	{
		const char* DefaultReadFile(const char* filepath);
		void DefaultFreeFile(const char* fileContents);
		void DefaultWriteFile(const char* filepath, const char* fileContents);

		FileStream OpenFileStream(const char* filepath);
		void CloseFileStream(FileStream& file);
		char StreamReadChar(FileStream& file);
		char StreamCharAt(const FileStream& file, int index);
		char StreamPeek(const FileStream& file, int numBytesToPeek);
		void StreamGoTo(FileStream& file, int newCursorPosition);
		const char* StreamSubstring(const FileStream& file, int start, int size);
		bool StreamAtEnd(const FileStream& fileStream);
	}
}

#endif