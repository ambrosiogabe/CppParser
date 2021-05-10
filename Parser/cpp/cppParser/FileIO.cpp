#include "cppParser/FileIO.h"
#include "CppUtils/CppUtils.h"

#include <stdio.h>

namespace CppParser
{
	namespace FileIO
	{
		using namespace CppUtils;

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
	}
}