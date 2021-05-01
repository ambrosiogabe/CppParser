#include "cppParser/Types.h"
#include "cppParser/Logger.h"

#include <cstring>

namespace CppParser
{
	namespace ParserString
	{
		char* CreateString(const char* strToCopy)
		{
			size_t size = strlen(strToCopy);
			size_t sizeWithNullByte = size + (unsigned long long)1;
			char* newStr = (char*)AllocMem(sizeWithNullByte);

			if (newStr)
			{
				strcpy_s(newStr, sizeWithNullByte, strToCopy);
			}
			else
			{
				Logger::Error("Failed to allocate memory for string.");
			}
			return newStr;
		}

		int StringLength(const char* str)
		{
			return (int)strlen(str);
		}

		bool Compare(const char* str1, const char* str2)
		{
			return strcmp(str1, str2) == 0;
		}
	}
}