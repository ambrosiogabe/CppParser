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

		FileStream OpenFileStream(const char* filepath)
		{
			Logger::AssertCritical(filepath != nullptr, "Invalid filepath. Filepath cannot be null.");

			FileStream res;
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

		void CloseFileStream(FileStream& file)
		{
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
			Logger::AssertCritical(file.Cursor - file.ChunkStart >= 0 && file.Cursor - file.ChunkStart < STREAM_BUFFER_SIZE, "Invalid chunk cursor boundaries in file stream.");

			char c = file.Data[file.Cursor - file.ChunkStart];
			file.Cursor++;
			return c;
		}

		char StreamCharAt(FileStream& file, int index)
		{
			int backupCursor = file.Cursor;
			file.Cursor = index;
			char c = StreamReadChar(file);
			file.Cursor = backupCursor;
			return c;
		}

		char StreamPeek(FileStream& file, int numBytesToPeek)
		{
			if (numBytesToPeek == 0)
			{
				return file.Data[file.Cursor];
			}

			int backupCursor = file.Cursor;
			file.Cursor += numBytesToPeek;
			char c = StreamReadChar(file);
			file.Cursor = backupCursor;
			return c;
		}

		void StreamGoTo(FileStream& file, int newCursorPosition)
		{
			file.Cursor = newCursorPosition;
			StreamReadChunk(file, 1);
		}

		const char* StreamSubstring(FileStream& file, int start, int size)
		{
			Logger::AssertCritical(size < STREAM_BUFFER_SIZE, "This File I/O library does not support substrings greater than '%d' characters right now.", STREAM_BUFFER_SIZE);
			int backupCursor = file.Cursor;
			file.Cursor = start;
			StreamReadChunk(file, size);
			const char* substring = ParserString::Substring(file.Data, start - file.ChunkStart, size);
			file.Cursor = backupCursor;
			StreamReadChunk(file, 1);
			return substring;
		}

		bool StreamAtEnd(const FileStream& stream)
		{
			return stream.Cursor == stream.Size;
		}

		// Internal functions
		static void StreamReadChunk(FileStream& file, int numBytesToRead, bool forceRead)
		{
			Logger::AssertCritical(file.Cursor - file.ChunkStart >= 0 && file.Cursor - file.ChunkStart < STREAM_BUFFER_SIZE, "Invalid chunk cursor boundaries in file stream.");

			// Only read file contents if the file cursor is out of bounds of our "view" of the file
			if (forceRead || file.Cursor + numBytesToRead > file.ChunkStart + STREAM_BUFFER_SIZE || file.Cursor < file.ChunkStart)
			{
				Logger::AssertCritical(file.Cursor >= 0 && file.Cursor < file.Size, "Invalid file chunk to read.");
				fseek(file.fp, file.Cursor, SEEK_SET);
				file.ChunkStart = file.Cursor;

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