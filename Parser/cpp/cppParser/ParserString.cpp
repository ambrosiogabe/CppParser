#include "CppUtils/CppUtils.h"

#include <cstring>

namespace CppParser
{
	namespace ParserString
	{
		using namespace CppUtils;

		char* CreateString(const char* strToCopy)
		{
			size_t size = strlen(strToCopy);
			size_t sizeWithNullByte = size + (unsigned long long)1;
			char* newStr = (char*)AllocMem(sizeWithNullByte);

			if (newStr)
			{
				strcpy(newStr, strToCopy);
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

		char* Join(const char* str1, const char* str2)
		{
			size_t strLength1 = strlen(str1);
			size_t strLength2 = strlen(str2);
			size_t newStrLength = strLength1 + strLength2;
			char* newStr = (char*)AllocMem(sizeof(char) * (newStrLength + 1));
			if (newStr)
			{
				memcpy(newStr, str1, sizeof(char) * strLength1);
				memcpy(newStr + strLength1, str2, sizeof(char) * strLength2);
				newStr[newStrLength] = '\0';
				return newStr;
			}

			Logger::Error("Failed to allocate memory for string.");
			return nullptr;
		}

		void FreeString(const char* str)
		{
			if (str)
			{
				FreeMem((void*)str);
			}
		}
	}
}
