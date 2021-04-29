#include "cppParser/FileIO.h"
#include "cppParser/Types.h"

#include <fstream>

namespace CppParser
{
	namespace FileIO
	{
		char* DefaultReadFile(const char* filepath)
		{
			FILE* filePointer;
			filePointer = fopen(filepath, "rb");
			if (filePointer)
			{
				fseek(filePointer, 0, SEEK_END);
				int fileSize = ftell(filePointer);
				rewind(filePointer);

				char* data = (char*)AllocMem(sizeof(char) * (fileSize + 1));
				if (!data)
				{
					fclose(filePointer);
					CPP_PARSER_LOG_WARNING("Memory allocation failed.");
					return nullptr;
				}

				int elementsRead = fread(data, fileSize, 1, filePointer);
				if (elementsRead != 1)
				{
					fclose(filePointer);
					FreeMem(data);
					data = nullptr;
					CPP_PARSER_LOG_WARNING("Failed to read file properly.");
					return nullptr;
				}

				data[fileSize] = '\0';
				fclose(filePointer);

				return data;
			}
			
			return nullptr;
		}

		void DefaultFreeFile(char* fileContents)
		{
			if (fileContents)
			{
				FreeMem(fileContents);
			}
		}
	}
}