#ifndef GABE_PARSER_STRING
#define GABE_PARSER_STRING
#include "CppUtils/CppUtils.h"

// TODO: Make strings always const char*, and everytime you make a new one just
// TODO: make a copy of it...
using namespace CppUtils;
namespace CppParser
{
	namespace ParserString
	{
		const char* CreateString(const char* strToCopy);
		void FreeString(const char* str);
		const char* Substring(const char* strToCopyFrom, int startIndex, int size);
		int StringLength(const char* str);
		bool Compare(const char* str1, const char* str2);
		const char* Join(const char* str1, const char* str2);
		const char* Copy(const char* strToCopy);
		const char* Copy(const char* strToCopy, int numCharactersToCopy);
	}

	class StringBuilder
	{
	public:
		StringBuilder();

		void Append(const char* strToAppend);
		void Append(char character);
		char Pop();
		const char* c_str();
		const char* c_str_copy();

	private:
		List<char> contents;
	};
}

#endif