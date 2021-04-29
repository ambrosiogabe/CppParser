#pragma once

namespace CppParser
{
	namespace ParserString
	{
		char* CreateString(const char* strToCopy);
		int StringLength(const char* str);
		bool Compare(const char* str1, const char* str2);
	}
}