#ifndef GABE_PARSER_FILE_IO
#define GABE_PARSER_FILE_IO

#include <stdio.h>
#include <cstdint>

namespace CppParser
{
	enum class StreamType
	{
		Read,
		Write
	};

	struct FileStream
	{
		StreamType Type;
		int64_t ChunkStart;
		int64_t Cursor;
		int64_t Size;
		char* Data;
		const char* Filepath;
		FILE* fp;
	};

	struct CountingFileStream
	{
		FileStream Stream;
		int32_t Line;
		int16_t Column;
	};

	namespace FileIO
	{
		const char* DefaultReadFile(const char* filepath);
		void DefaultFreeFile(const char* fileContents);
		void DefaultWriteFile(const char* filepath, const char* fileContents);

		CountingFileStream OpenCountingFileStreamRead(const char* filepath);
		CountingFileStream CountingFileStreamReadFromString(const char* source);
		void CloseCountingFileStreamRead(CountingFileStream& stream);
		char StreamReadChar(CountingFileStream& stream);
		char StreamCharAt(const CountingFileStream& stream, int index);
		char StreamPeek(const CountingFileStream& stream, int numBytesToPeekAhead);
		void StreamGoTo(CountingFileStream& stream, int newCursorPosition);
		const char* StreamSubstring(const CountingFileStream& stream, int start, int size);
		bool StreamAtEnd(const CountingFileStream& stream);

		FileStream OpenFileStreamRead(const char* filepath);
		FileStream FileStreamReadFromString(const char* source);
		void CloseFileStreamRead(FileStream& stream);
		char StreamReadChar(FileStream& stream);
		char StreamCharAt(const FileStream& stream, int index);
		char StreamPeek(const FileStream& stream, int numBytesToPeekAhead);
		void StreamGoTo(FileStream& stream, int newCursorPosition);
		const char* StreamSubstring(const FileStream& stream, int start, int size);
		bool StreamAtEnd(const FileStream& stream);

		FileStream OpenFileStreamWrite(const char* filepath);
		void CloseFileStreamWrite(FileStream& stream);
		void WriteToStream(FileStream& stream, const char* strToWrite);
	}
}

#endif