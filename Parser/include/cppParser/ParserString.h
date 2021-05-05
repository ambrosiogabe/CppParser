#pragma once

// TODO: Make strings always const char*, and everytime you make a new one just
// TODO: make a copy of it...
namespace CppParser
{
	namespace ParserString
	{
		char* CreateString(const char* strToCopy);
		void FreeString(char* str);
		int StringLength(const char* str);
		bool Compare(const char* str1, const char* str2);
		char* Join(const char* str1, const char* str2);
	}
}