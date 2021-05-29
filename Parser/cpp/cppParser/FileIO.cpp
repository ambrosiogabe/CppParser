#include "cppParser/FileIO.h"
#include "CppUtils/CppUtils.h"
#include "cppParser/ParserString.h"

namespace CppParser
{
	namespace FileIO
	{
		using namespace CppUtils;

		// Internal Variables
		static const int STREAM_BUFFER_SIZE = 1024;
		static void StreamReadChunk(FileStream& file, int numBytesToRead, bool forceRead = false);

		const char* DefaultReadFile(const char* filepath)
		{
			FILE* filePointer;
			filePointer = fopen(filepath, "rb");
			if (filePointer)
			{
				fseek(filePointer, 0, SEEK_END);
				int fileSize = ftell(filePointer);
				rewind(filePointer);

				// Cast 1 to a bigger int so that we don't get warnings
				char* data = (char*)AllocMem(sizeof(char) * (fileSize + (unsigned long long)1));
				if (!data)
				{
					fclose(filePointer);
					Logger::Error("Memory allocation failed.");
					return nullptr;
				}

				size_t elementsRead = fread(data, fileSize, 1, filePointer);
				if (elementsRead != 1)
				{
					fclose(filePointer);
					FreeMem(data);
					data = nullptr;
					Logger::Error("Failed to read file properly.");
					return nullptr;
				}

				data[fileSize] = '\0';
				fclose(filePointer);

				return data;
			}

			return nullptr;
		}

		void DefaultFreeFile(const char* fileContents)
		{
			if (fileContents)
			{
				FreeMem((void*)fileContents);
			}
		}

		void DefaultWriteFile(const char* filepath, const char* fileContents)
		{
			FILE* filePointer;
			filePointer = fopen(filepath, "w+");
			if (filePointer)
			{
				fprintf(filePointer, "%s", fileContents);
				fclose(filePointer);
			}
		}

		CountingFileStream OpenCountingFileStreamRead(const char* filepath)
		{
			FileStream stream = OpenFileStreamRead(filepath);
			CountingFileStream countingStream;
			countingStream.Line = 1;
			countingStream.Column = 1;
			countingStream.Stream = stream;
			return countingStream;
		}

		CountingFileStream CountingFileStreamReadFromString(const char* source)
		{
			FileStream stream = FileStreamReadFromString(source);
			CountingFileStream countingStream;
			countingStream.Line = 1;
			countingStream.Column = 1;
			countingStream.Stream = stream;
			return countingStream;
		}

		void CloseCountingFileStreamRead(CountingFileStream& stream)
		{
			CloseFileStreamRead(stream.Stream);
		}

		char StreamReadChar(CountingFileStream& stream)
		{
			char c = StreamReadChar(stream.Stream);
			stream.Column += c == '\t' ? 4 : 1;
			if (c == '\n')
			{
				stream.Line++;
				stream.Column = 1;
			}

			return c;
		}

		char StreamCharAt(CountingFileStream& stream, int index)
		{
			return StreamCharAt(stream.Stream, index);
		}

		char StreamPeek(CountingFileStream& stream, int numBytesToPeekAhead)
		{
			return StreamPeek(stream.Stream, numBytesToPeekAhead);
		}

		void StreamGoTo(CountingFileStream& stream, int newCursorPosition)
		{
			// Get the right line number
			if (newCursorPosition < stream.Stream.Cursor)
			{
				for (int i = stream.Stream.Cursor - 1; i >= newCursorPosition; i--)
				{
					StreamGoTo(stream.Stream, i);
					char c = StreamPeek(stream.Stream, 0);
					if (c == '\n')
					{
						stream.Line--;
					}
				}

				// Once we get to the appropriate line number, we need to find the beginning
				// of the line to get the right column number
				int lineStart = stream.Stream.Cursor - 1;
				int cursorBegin = stream.Stream.Cursor;
				while (lineStart >= 0)
				{
					StreamGoTo(stream.Stream, lineStart);
					char c = StreamPeek(stream.Stream, 0);
					if (c == '\n')
					{
						break;
					}
					lineStart--;
				}

				stream.Column = cursorBegin - lineStart;
			}
			else if (newCursorPosition > stream.Stream.Cursor)
			{
				for (int i = stream.Stream.Cursor; i < newCursorPosition; i++)
				{
					StreamGoTo(stream.Stream, i);
					char c = StreamPeek(stream.Stream, 0);
					stream.Column += c == '\t' ? 4 : 1;
					if (c == '\n')
					{
						stream.Line++;
						stream.Column = 1;
					}
				}
			}

			StreamGoTo(stream.Stream, newCursorPosition);
		}

		const char* StreamSubstring(CountingFileStream& stream, int start, int size)
		{
			return StreamSubstring(stream.Stream, start, size);
		}

		bool StreamAtEnd(const CountingFileStream& stream)
		{
			return StreamAtEnd(stream.Stream);
		}

		FileStream FileStreamReadFromString(const char* source)
		{
			Logger::AssertCritical(source != nullptr, "Invalid file stream source");
			Logger::AssertCritical(ParserString::StringLength(source) < STREAM_BUFFER_SIZE, "Invalid source string length. Length must be shorter than '%d' characters", STREAM_BUFFER_SIZE);

			FileStream res;
			res.Type = StreamType::Read;
			res.fp = nullptr;
			res.ChunkStart = 0;
			res.Cursor = 0;
			res.Size = ParserString::StringLength(source);
			res.Data = (char*)ParserString::CreateString(source);
			res.Filepath = ParserString::CreateString("");
			return res;
		}

		FileStream OpenFileStreamRead(const char* filepath)
		{
			Logger::AssertCritical(filepath != nullptr, "Invalid filepath. Filepath cannot be null.");

			FileStream res;
			res.Type = StreamType::Read;
			res.fp = fopen(filepath, "rb");
			res.ChunkStart = 0;
			res.Cursor = 0;
			res.Size = 0;
			res.Data = nullptr;
			res.Filepath = ParserString::CreateString(filepath);

			if (res.fp)
			{
				fseek(res.fp, 0, SEEK_END);
				res.Size = ftell(res.fp);
				rewind(res.fp);

				// Cast 1 to a bigger int so that we don't get warnings
				res.Data = (char*)AllocMem(sizeof(char) * (STREAM_BUFFER_SIZE + (unsigned long long)1));
				if (!res.Data)
				{
					fclose(res.fp);
					Logger::Error("Memory allocation failed for opening file stream.");
					res.fp = nullptr;
					res.Size = 0;
					return res;
				}

				StreamReadChunk(res, 1, true);
				size_t numBytesRead = STREAM_BUFFER_SIZE > res.Size - res.Cursor ? res.Size - res.Cursor : STREAM_BUFFER_SIZE;
				res.Data[STREAM_BUFFER_SIZE] = '\0';
				res.Data[numBytesRead] = '\0';
			}

			return res;
		}

		void CloseFileStreamRead(FileStream& file)
		{
			Logger::AssertCritical(file.Type == StreamType::Read, "Invalid file stream. This function is only valid for read file streams.");

			if (file.Data)
			{
				FreeMem((void*)file.Data);
				file.Data = nullptr;
			}

			if (file.fp)
			{
				fclose(file.fp);
				file.fp = nullptr;
			}

			if (file.Filepath)
			{
				ParserString::FreeString(file.Filepath);
				file.Filepath = nullptr;
			}
		}

		char StreamReadChar(FileStream& file)
		{
			Logger::AssertCritical(file.Type == StreamType::Read, "Invalid file stream. This function is only valid for read file streams.");

			if (file.Cursor >= file.Size)
			{
				Logger::Warning("Reading character out of stream bounds. Returning null byte.");
				return '\0';
			}
			if (file.Cursor < 0)
			{
				Logger::Warning("Reading character out of stream bounds. Returning null byte.");
				return '\0';
			}

			StreamReadChunk(file, 1);
			Logger::AssertCritical(file.Cursor - file.ChunkStart >= 0 && file.Cursor - file.ChunkStart < STREAM_BUFFER_SIZE && file.Cursor <= file.Size, "Invalid chunk cursor boundaries in file stream. 273");

			char c = file.Data[file.Cursor - file.ChunkStart];
			file.Cursor++;
			return c;
		}

		char StreamCharAt(FileStream& file, int index)
		{
			Logger::AssertCritical(file.Type == StreamType::Read, "Invalid file stream. This function is only valid for read file streams.");

			int64_t ogCursor = file.Cursor;
			file.Cursor = index;
			char c = StreamReadChar(file);
			StreamGoTo(file, ogCursor);
			return c;
		}

		char StreamPeek(FileStream& file, int numBytesToPeekAhead)
		{
			Logger::AssertCritical(file.Type == StreamType::Read, "Invalid file stream. This function is only valid for read file streams.");

			if (file.Cursor + numBytesToPeekAhead < 0 || file.Cursor + numBytesToPeekAhead >= file.Size)
			{
				return '\0';
			}

			if (numBytesToPeekAhead == 0 && (file.Cursor - file.ChunkStart) < STREAM_BUFFER_SIZE)
			{
				return file.Data[file.Cursor - file.ChunkStart];
			}

			int64_t ogCursor = file.Cursor;
			file.Cursor += numBytesToPeekAhead;
			char c = StreamReadChar(file);
			StreamGoTo(file, ogCursor);
			return c;
		}

		void StreamGoTo(FileStream& file, int newCursorPosition)
		{
			Logger::AssertCritical(file.Type == StreamType::Read, "Invalid file stream. This function is only valid for read file streams.");

			file.Cursor = newCursorPosition;
			StreamReadChunk(file, 1);
		}

		const char* StreamSubstring(FileStream& file, int start, int size)
		{
			Logger::AssertCritical(file.Type == StreamType::Read, "Invalid file stream. This function is only valid for read file streams.");
			Logger::AssertCritical(size < STREAM_BUFFER_SIZE, "This File I/O library does not support substrings greater than '%d' characters right now.", STREAM_BUFFER_SIZE);
			int64_t ogCursor = file.Cursor;
			file.Cursor = start;
			StreamReadChunk(file, size);
			const char* substring = ParserString::Substring(file.Data, start - file.ChunkStart, size);
			StreamGoTo(file, ogCursor);
			return substring;
		}

		bool StreamAtEnd(const FileStream& stream)
		{
			Logger::AssertCritical(stream.Type == StreamType::Read, "Invalid file stream. This function is only valid for read file streams.");

			return stream.Cursor == stream.Size;
		}

		FileStream OpenFileStreamWrite(const char* filepath)
		{
			Logger::AssertCritical(filepath != nullptr, "Invalid filepath. Filepath cannot be null.");

			FileStream res;
			res.Type = StreamType::Write;
			res.fp = fopen(filepath, "w+");
			if (!res.fp)
			{
				return res;
			}

			res.ChunkStart = 0;
			res.Cursor = 0;
			res.Size = 0;
			res.Data = nullptr;
			res.Filepath = ParserString::CreateString(filepath);

			FILE* filePointer;
			filePointer = fopen(filepath, "w+");
			return res;
		}

		void CloseFileStreamWrite(FileStream& stream)
		{
			Logger::AssertCritical(stream.Type == StreamType::Write, "Invalid file stream. This function is only valid for write file streams.");

			if (stream.fp)
			{
				fclose(stream.fp);
				stream.fp = nullptr;
			}

			if (stream.Filepath)
			{
				ParserString::FreeString(stream.Filepath);
				stream.Filepath = nullptr;
			}
		}

		void WriteToStream(FileStream& stream, const char* strToWrite)
		{
			Logger::AssertCritical(stream.Type == StreamType::Write, "Invalid file stream. This function is only valid for write file streams.");

			fprintf(stream.fp, "%s", strToWrite);
		}

		// Internal functions
		static void StreamReadChunk(FileStream& file, int numBytesToRead, bool forceRead)
		{
			if (!(file.Cursor >= 0 && file.Cursor <= file.Size && numBytesToRead < STREAM_BUFFER_SIZE))
			{
				printf("HERE");
			}
			Logger::AssertCritical(file.Cursor >= 0 && file.Cursor <= file.Size && numBytesToRead < STREAM_BUFFER_SIZE, "Invalid chunk cursor boundaries in file stream. 377");
			if (!file.fp)
			{
				return;
			}

			// Only read file contents if the file cursor is out of bounds of our "view" of the file
			if (forceRead || file.Cursor + numBytesToRead > file.ChunkStart + STREAM_BUFFER_SIZE || file.Cursor < file.ChunkStart || file.Cursor + numBytesToRead < file.ChunkStart)
			{
				Logger::AssertCritical(file.Cursor >= 0 && file.Cursor < file.Size, "Invalid file chunk to read.");
				int64_t seekLocation = file.Cursor;
				// Attempt to keep the last line in memory if it's small enough (we'll use 256 as our max line size to keep in memory)
				for (int64_t i = file.Cursor; i > file.Cursor - 256; i--)
				{
					if (file.Data[i - file.ChunkStart] == '\n')
					{
						seekLocation = i;
						break;
					}
				}

				fseek(file.fp, seekLocation, SEEK_SET);
				file.ChunkStart = seekLocation;

				size_t numBytesToRead = STREAM_BUFFER_SIZE > file.Size - file.Cursor ? file.Size - file.Cursor : STREAM_BUFFER_SIZE;
				size_t elementsRead = fread(file.Data, numBytesToRead, 1, file.fp);
				if (elementsRead != 1)
				{
					fclose(file.fp);
					FreeMem(file.Data);
					file.Data = nullptr;
					file.Size = 0;
					file.Cursor = 0;
					Logger::Error("Failed to read file properly.");
				}
			}
		}
	}
}