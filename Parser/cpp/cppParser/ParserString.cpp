#include "cppParser/Types.h"

#include <cstring>

namespace CppParser
{
	namespace ParserString
	{
		char* CreateString(const char* strToCopy)
		{
			int size = strlen(strToCopy);
			char* newStr = (char*)AllocMem(size + 1);
			strcpy(newStr, strToCopy);
			return newStr;
		}

		int StringLength(const char* str)
		{
			return strlen(str);
		}

		bool Compare(const char* str1, const char* str2)
		{
			return strcmp(str1, str2) == 0;
		}
	}
}