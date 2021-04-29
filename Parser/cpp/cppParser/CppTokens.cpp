#pragma once
#include "cppParser/Types.h"
#include "cppParser/CppTokens.h"
#include "cppParser/ParserString.h"

namespace CppParser
{
	namespace CppTokens
	{
		Token CreateToken(int line, int column, TokenType type, const char* str)
		{
			Token token;
			token.m_Column = column;
			token.m_Line = line;
			token.m_Type = type;
			token.m_Lexeme = ParserString::CreateString(str);

			return token;
		}
	}
}