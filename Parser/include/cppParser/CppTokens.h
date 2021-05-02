#pragma once
#include "cppParser/Types.h"

#undef CONST
#undef DELETE
#undef FALSE
#undef THIS
#undef TRUE
#undef VOID
namespace CppParser
{
	enum class TokenType
	{
		None,

		// Keywords
		KW_ALIGN_AS, KW_ALIGN_OF, KW_AUTO, KW_BOOL, KW_BREAK, KW_CASE, KW_CATCH, KW_CHAR,
		KW_CHAR8_T, KW_CHAR16_T, KW_CHAR32_T, KW_CLASS, KW_CONST, KW_CONST_CAST, KW_CONST_EXPR,
		KW_CONTINUE, KW_DECLTYPE, KW_DEFAULT, KW_DELETE, KW_DO, KW_DOUBLE, KW_DYNAMIC_CAST,
		KW_ELSE, KW_ENUM, KW_EXPLICIT, KW_EXTERN, KW_FALSE, KW_FLOAT, KW_FOR, KW_FRIEND, KW_GOTO,
		KW_IF, KW_INLINE, KW_INT, KW_LONG, KW_MUTABLE, KW_NAMESPACE, KW_NEW, KW_NOEXCEPT, KW_NULLPTR,
		KW_OPERATOR, KW_PRIVATE, KW_PROTECTED, KW_PUBLIC, KW_RETURN, KW_SHORT, KW_SIGNED,
		KW_SIZEOF, KW_STATIC, KW_STATIC_ASSERT, KW_STATIC_CAST, KW_STRUCT, KW_SWITCH, KW_TEMPLATE,
		KW_THIS, KW_THREAD_LOCAL, KW_THROW, KW_TRUE, KW_TRY, KW_TYPEDEF, KW_TYPEID, KW_TYPENAME,
		KW_UNION, KW_UNSIGNED, KW_USING, KW_VIRTUAL, KW_VOID, KW_VOLATILE, KW_WCHAR_T, KW_WHILE,
		KW_ASM, KW_FINAL, KW_OVERRIDE, KW_REINTERPRET_CAST, KW_REGISTER,

		// Identifiers (basically anything user-defined)
		IDENTIFIER,

		// Macros
		MACRO_INCLUDE, MACRO_IFDEF, MACRO_IFNDEF, MACRO_DEFINE, MACRO_IF, MACRO_ELIF, MACRO_ENDIF,
		MACRO_ELSE, MACRO_ERROR, MACRO_LINE, MACRO_PRAGMA, MACRO_REGION, MACRO_USING, MACRO_UNDEF,

		// Character tokens
		DOT, ARROW, LEFT_BRACKET, RIGHT_BRACKET, LEFT_PAREN, RIGHT_PAREN,
		PLUS, PLUS_PLUS, MINUS, MINUS_MINUS, TILDE, BANG, AND, STAR, DIV, MODULO,
		LEFT_SHIFT, RIGHT_SHIFT, LEFT_ANGLE_BRACKET, RIGHT_ANGLE_BRACKET, LESS_THAN_EQ, GREATER_THAN_EQ,
		EQUAL_EQUAL, BANG_EQUAL, EQUAL, CARET, PIPE, QUESTION, COLON, LOGICAL_AND,
		LOGICAL_OR, STAR_EQUAL, DIV_EQUAL, MODULO_EQUAL, PLUS_EQUAL, MINUS_EQUAL, LEFT_SHIFT_EQUAL,
		RIGHT_SHIFT_EQUAL, AND_EQUAL, PIPE_EQUAL, CARET_EQUAL, COMMA, LEFT_CURLY_BRACKET, RIGHT_CURLY_BRACKET,
		SEMICOLON, POINTER_TO_MEMBER,

		// Literals
		STRING_LITERAL, INTEGER_LITERAL, FLOATING_POINT_LITERAL, CHARACTER_LITERAL,

		// Custom Keywords (special macros)
		USYSTEM,

		// Special
		HASHTAG,
		NEWLINE,
		COMMENT,
		WHITESPACE,
		END_OF_FILE,
		ERROR_TYPE
	};

	struct Token
	{
		int m_Line;
		int m_Column;
		TokenType m_Type;
		// TODO: should either be made `const char*` or not assigned `const char*`s.
		char* m_Lexeme;
		// void* m_Literal;
	};

	namespace CppTokens
	{
		Token CreateToken(int line, int column, TokenType type, const char* str);
	}
}
