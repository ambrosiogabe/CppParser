#include "cppParser/ScriptScanner.h"
#include "CppUtils/CppUtils.h"
#include "cppParser/ParserString.h"

#include <cstdint>
#include <unordered_map>

namespace CppParser
{
	namespace ScriptScanner
	{
		using namespace CppUtils;

		// Forward Declarations
		static Token ScanTokenInternal(ScannerData& data);
		static Token PropertyIdentifier(ScannerData& data);
		static Token Number(ScannerData& data, char firstDigit);
		static Token NumberDecimal(ScannerData& data, char firstDigit);
		static Token NumberBinary(ScannerData& data);
		static Token NumberHexadecimal(ScannerData& data);
		static Token NumberOctal(ScannerData& data);

		static Token Character(ScannerData& data);
		static Token String(ScannerData& data, bool isRawStringLiteral = false);
		static inline void ConsumeTrailingUnsignedLong(ScannerData& data);

		// Inline functions
		static inline char Advance(ScannerData& data)
		{
			char c = FileIO::StreamReadChar(data.Stream);
			return c;
		}

		static inline char Peek(const ScannerData& data)
		{
			return FileIO::StreamPeek(data.Stream, 0);
		}

		static inline char PeekNext(const ScannerData& data)
		{
			return FileIO::StreamPeek(data.Stream, 1);
		}

		static inline char PeekNextNext(const ScannerData& data)
		{
			return FileIO::StreamPeek(data.Stream, 2);
		}

		static bool Match(ScannerData& data, char expected)
		{
			if (AtEnd(data)) return false;
			if (FileIO::StreamPeek(data.Stream, 0) != expected) return false;

			FileIO::StreamReadChar(data.Stream);
			return true;
		}

		static inline char PeekPrevious(const ScannerData& data, int amount)
		{
			return FileIO::StreamPeek(data.Stream, -amount);
		}

		static inline bool IsDigit(char c, bool acceptApostrophe = false)
		{
			return c >= '0' && c <= '9' || (acceptApostrophe && c == '\'');
		}

		static inline bool IsHexDigit(char c, bool acceptApostrophe)
		{
			return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || (acceptApostrophe && c == '\'');

		}
		static inline bool IsAlpha(char c)
		{
			return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
		}

		static inline bool IsAlphaNumeric(char c)
		{
			return IsAlpha(c) || IsDigit(c);
		}

		static inline bool IsSameChar(char c, char lowerCase, char upperCase)
		{
			return c == lowerCase || c == upperCase;
		}

		static inline Token GenerateToken(const ScannerData& data, TokenType m_Type, const char* lexeme)
		{
			return Token{
				data.Stream.Line,
				data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start),
				m_Type,
				ParserString::CreateString(lexeme)
			};
		}

		inline Token GenerateErrorToken(const ScannerData& data)
		{
			return Token{
				data.Stream.Line,
				data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start),
				TokenType::ERROR_TYPE,
				ParserString::CreateString("")
			};
		}

		inline Token GenerateWhitespaceToken()
		{
			return Token{
				-1,
				-1,
				TokenType::WHITESPACE,
				ParserString::CreateString("")
			};
		}

		inline Token GenerateCommentToken()
		{
			return Token{
				-1,
				-1,
				TokenType::COMMENT,
				ParserString::CreateString("")
			};
		}

		class StringHasher
		{
		public:
			size_t operator() (const char* key) const
			{
				size_t hash = 0;
				int strLength = ParserString::StringLength(key);
				for (size_t i = 0; i < strLength; i++)
				{
					hash += (71 * hash + key[i]) % 5;
				}
				return hash;
			}
		};

		class StringComparer
		{
		public:
			bool operator() (const char* str1, const char* str2) const
			{
				return ParserString::Compare(str1, str2);
			}
		};

		// Debug maps
		static const std::unordered_map<const char*, TokenType, StringHasher, StringComparer> keywords = {
			// Custom Keywords for Cocoa Engine
			{ "USYSTEM",       TokenType::USYSTEM },

			// Standard C++ Keywords (up to a subset of C++17)
			{ "alignas",       TokenType::KW_ALIGN_AS },
			{ "alignof",       TokenType::KW_ALIGN_OF },
			{ "asm",           TokenType::KW_ASM },
			{ "auto",          TokenType::KW_AUTO },
			{ "bool",          TokenType::KW_BOOL },
			{ "break",         TokenType::KW_BREAK },
			{ "case",          TokenType::KW_CASE },
			{ "catch",         TokenType::KW_CATCH },
			{ "char",          TokenType::KW_CHAR },
			{ "char8_t",       TokenType::KW_CHAR8_T },
			{ "char16_t",      TokenType::KW_CHAR16_T },
			{ "char32_t",      TokenType::KW_CHAR32_T },
			{ "class",         TokenType::KW_CLASS },
			{ "const",         TokenType::KW_CONST },
			{ "const_cast",    TokenType::KW_CONST_CAST },
			{ "constexpr",     TokenType::KW_CONST_EXPR },
			{ "continue",      TokenType::KW_CONTINUE },
			{ "decltype",      TokenType::KW_DECLTYPE },
			{ "default",       TokenType::KW_DEFAULT },
			{ "delete",        TokenType::KW_DELETE },
			{ "do",            TokenType::KW_DO },
			{ "double",        TokenType::KW_DOUBLE },
			{ "dynamic_cast",  TokenType::KW_DYNAMIC_CAST },
			{ "else",          TokenType::KW_ELSE },
			{ "enum",          TokenType::KW_ENUM },
			{ "explicit",      TokenType::KW_EXPLICIT },
			{ "extern",        TokenType::KW_EXTERN },
			{ "false",         TokenType::KW_FALSE },
			{ "final",         TokenType::KW_FINAL },
			{ "float",         TokenType::KW_FLOAT },
			{ "for",           TokenType::KW_FOR },
			{ "friend",        TokenType::KW_FRIEND },
			{ "goto",          TokenType::KW_GOTO },
			{ "if",            TokenType::KW_IF },
			{ "inline",        TokenType::KW_INLINE },
			{ "int",           TokenType::KW_INT },
			{ "long",          TokenType::KW_LONG },
			{ "mutable",       TokenType::KW_MUTABLE },
			{ "namespace",     TokenType::KW_NAMESPACE },
			{ "new",           TokenType::KW_NEW },
			{ "noexcept",      TokenType::KW_NOEXCEPT },
			{ "nullptr",       TokenType::KW_NULLPTR },
			{ "operator",      TokenType::KW_OPERATOR },
			{ "override",      TokenType::KW_OVERRIDE },
			{ "private",       TokenType::KW_PRIVATE },
			{ "protected",     TokenType::KW_PROTECTED },
			{ "public",        TokenType::KW_PUBLIC },
			{ "reinterpret_cast", TokenType::KW_REINTERPRET_CAST },
			{ "return",        TokenType::KW_RETURN },
			{ "register",      TokenType::KW_REGISTER },
			{ "short",         TokenType::KW_SHORT },
			{ "signed",        TokenType::KW_SIGNED },
			{ "sizeof",        TokenType::KW_SIZEOF },
			{ "static",        TokenType::KW_STATIC },
			{ "static_assert", TokenType::KW_STATIC_ASSERT },
			{ "static_cast",   TokenType::KW_STATIC_CAST },
			{ "struct",        TokenType::KW_STRUCT },
			{ "switch",        TokenType::KW_SWITCH },
			{ "template",      TokenType::KW_TEMPLATE },
			{ "this",          TokenType::KW_THIS },
			{ "thread_local",  TokenType::KW_THREAD_LOCAL },
			{ "throw",         TokenType::KW_THROW },
			{ "true",          TokenType::KW_TRUE },
			{ "try",           TokenType::KW_TRY },
			{ "typedef",       TokenType::KW_TYPEDEF },
			{ "typeid",        TokenType::KW_TYPEID },
			{ "typename",      TokenType::KW_TYPENAME },
			{ "union",         TokenType::KW_UNION },
			{ "unsigned",      TokenType::KW_UNSIGNED },
			{ "using",         TokenType::KW_USING },
			{ "virtual",       TokenType::KW_VIRTUAL },
			{ "void",          TokenType::KW_VOID },
			{ "volatile",      TokenType::KW_VOLATILE },
			{ "wchar_t",       TokenType::KW_WCHAR_T },
			{ "while",         TokenType::KW_WHILE },
		};

		static const std::unordered_map<TokenType, const char*> tokenTypeToString = {
			// Custom Keywords for Cocoa Engine
			{ TokenType::USYSTEM,          "USYSTEM" },

			// Standard C++ Keywords
			{ TokenType::KW_ALIGN_AS,      "kw_alignas" },
			{ TokenType::KW_ALIGN_OF,      "kw_alignof" },
			{ TokenType::KW_ASM,           "kw_asm" },
			{ TokenType::KW_AUTO,          "kw_auto" },
			{ TokenType::KW_BOOL,          "kw_bool" },
			{ TokenType::KW_BREAK,         "kw_break" },
			{ TokenType::KW_CASE,          "kw_case" },
			{ TokenType::KW_CATCH,         "kw_catch" },
			{ TokenType::KW_CHAR,          "kw_char" },
			{ TokenType::KW_CHAR8_T,       "kw_char8_t" },
			{ TokenType::KW_CHAR16_T,      "kw_char16_t" },
			{ TokenType::KW_CHAR32_T,      "kw_char32_t" },
			{ TokenType::KW_CLASS,         "kw_class" },
			{ TokenType::KW_CONST,         "kw_const" },
			{ TokenType::KW_CONST_CAST,    "kw_const_cast" },
			{ TokenType::KW_CONST_EXPR,    "kw_constexpr" },
			{ TokenType::KW_CONTINUE,      "kw_continue" },
			{ TokenType::KW_DECLTYPE,      "kw_decltype"},
			{ TokenType::KW_DEFAULT,       "kw_default"},
			{ TokenType::KW_DELETE,        "kw_delete"},
			{ TokenType::KW_DO,            "kw_do"},
			{ TokenType::KW_DOUBLE,        "kw_double"},
			{ TokenType::KW_DYNAMIC_CAST,  "kw_dynamic_cast"},
			{ TokenType::KW_ELSE,          "kw_else"},
			{ TokenType::KW_ENUM,          "kw_enum"},
			{ TokenType::KW_EXPLICIT,      "kw_explicit"},
			{ TokenType::KW_EXTERN,        "kw_extern"},
			{ TokenType::KW_FALSE,         "kw_false"},
			{ TokenType::KW_FINAL,         "kw_final" },
			{ TokenType::KW_FLOAT,         "kw_float"},
			{ TokenType::KW_FOR,           "kw_for"},
			{ TokenType::KW_FRIEND,        "kw_friend"},
			{ TokenType::KW_GOTO,          "kw_goto"},
			{ TokenType::KW_IF,            "kw_if"},
			{ TokenType::KW_INLINE,        "kw_inline"},
			{ TokenType::KW_INT,           "kw_int"},
			{ TokenType::KW_LONG,          "kw_long"},
			{ TokenType::KW_MUTABLE,       "kw_mutable"},
			{ TokenType::KW_NAMESPACE,     "kw_namespace"},
			{ TokenType::KW_NEW,           "kw_new"},
			{ TokenType::KW_NOEXCEPT,      "kw_noexcept"},
			{ TokenType::KW_NULLPTR,       "kw_nullptr"},
			{ TokenType::KW_OPERATOR,      "kw_operator"},
			{ TokenType::KW_OVERRIDE,      "kw_override" },
			{ TokenType::KW_PRIVATE,       "kw_private"},
			{ TokenType::KW_PROTECTED,     "kw_protected"},
			{ TokenType::KW_PUBLIC,        "kw_public"},
			{ TokenType::KW_REINTERPRET_CAST, "kw_reinterpret_cast" },
			{ TokenType::KW_RETURN,        "kw_return"},
			{ TokenType::KW_REGISTER,      "kw_register" },
			{ TokenType::KW_SHORT,         "kw_short"},
			{ TokenType::KW_SIGNED,        "kw_signed"},
			{ TokenType::KW_SIZEOF,        "kw_sizeof"},
			{ TokenType::KW_STATIC,        "kw_static"},
			{ TokenType::KW_STATIC_ASSERT, "kw_static_assert"},
			{ TokenType::KW_STATIC_CAST,   "kw_static_cast"},
			{ TokenType::KW_STRUCT,        "kw_struct"},
			{ TokenType::KW_SWITCH,        "kw_switch"},
			{ TokenType::KW_TEMPLATE,      "kw_template"},
			{ TokenType::KW_THIS,          "kw_this"},
			{ TokenType::KW_THREAD_LOCAL,  "kw_thread_local"},
			{ TokenType::KW_THROW,         "kw_throw"},
			{ TokenType::KW_TRUE,          "kw_true"},
			{ TokenType::KW_TRY,           "kw_try"},
			{ TokenType::KW_TYPEDEF,       "kw_typedef"},
			{ TokenType::KW_TYPEID,        "kw_typeid"},
			{ TokenType::KW_TYPENAME,      "kw_typename"},
			{ TokenType::KW_UNION,         "kw_union"},
			{ TokenType::KW_UNSIGNED,      "kw_unsigned"},
			{ TokenType::KW_USING,         "kw_using"},
			{ TokenType::KW_VIRTUAL,       "kw_virtual"},
			{ TokenType::KW_VOID,          "kw_void",},
			{ TokenType::KW_VOLATILE,      "kw_volatile"},
			{ TokenType::KW_WCHAR_T,       "kw_wchar_t"},
			{ TokenType::KW_WHILE,         "kw_while"},
			{ TokenType::IDENTIFIER,       "identifier"},
			{ TokenType::DOT,              "ch_dot"},
			{ TokenType::ARROW,            "ch_arrow"},
			{ TokenType::LEFT_BRACKET,     "ch_left_bracket"},
			{ TokenType::RIGHT_BRACKET,    "ch_right_bracket"},
			{ TokenType::LEFT_PAREN,       "ch_left_paren"},
			{ TokenType::RIGHT_PAREN,      "ch_right_paren"},
			{ TokenType::PLUS,             "ch_plus"},
			{ TokenType::PLUS_PLUS,        "ch_plus_plus"},
			{ TokenType::MINUS,            "ch_minus"},
			{ TokenType::MINUS_MINUS,      "ch_minus_minus"},
			{ TokenType::TILDE,            "ch_tilde"},
			{ TokenType::BANG,             "ch_bang"},
			{ TokenType::AND,              "ch_and"},
			{ TokenType::STAR,             "ch_star"},
			{ TokenType::DIV,              "ch_div"},
			{ TokenType::MODULO,           "ch_modulo"},
			{ TokenType::LEFT_SHIFT,       "ch_left_shift"},
			{ TokenType::RIGHT_SHIFT,      "ch_right_shift"},
			{ TokenType::LEFT_ANGLE_BRACKET,  "ch_left_angle_bracket"},
			{ TokenType::RIGHT_ANGLE_BRACKET, "ch_right_angle_bracket"},
			{ TokenType::LESS_THAN_EQ,     "ch_less_than_eq"},
			{ TokenType::GREATER_THAN_EQ,  "ch_greater_than_eq"},
			{ TokenType::EQUAL_EQUAL,      "ch_equal_equal"},
			{ TokenType::BANG_EQUAL,       "ch_bang_equal"},
			{ TokenType::EQUAL,            "ch_equal"},
			{ TokenType::CARET,            "ch_caret"},
			{ TokenType::PIPE,             "ch_pipe"},
			{ TokenType::QUESTION,         "ch_question"},
			{ TokenType::COLON,            "ch_colon"},
			{ TokenType::LOGICAL_AND,      "ch_logical_and"},
			{ TokenType::LOGICAL_OR,       "ch_logical_or"},
			{ TokenType::STAR_EQUAL,       "ch_star_equal"},
			{ TokenType::DIV_EQUAL,        "ch_div_equal"},
			{ TokenType::MODULO_EQUAL,     "ch_modulo_equal"},
			{ TokenType::PLUS_EQUAL,       "ch_plus_equal"},
			{ TokenType::MINUS_EQUAL,      "ch_minus_equal"},
			{ TokenType::LEFT_SHIFT_EQUAL, "ch_left_shift_equal"},
			{ TokenType::RIGHT_SHIFT_EQUAL, "ch_right_shift_equal"},
			{ TokenType::AND_EQUAL,        "ch_and_equal"},
			{ TokenType::PIPE_EQUAL,       "ch_pipe_equal"},
			{ TokenType::CARET_EQUAL,      "ch_caret_equal"},
			{ TokenType::COMMA,            "ch_comma"},
			{ TokenType::LEFT_CURLY_BRACKET,  "ch_left_curly_bracket"},
			{ TokenType::RIGHT_CURLY_BRACKET, "ch_right_curly_bracket"},
			{ TokenType::SEMICOLON,        "ch_semicolon"},
			{ TokenType::POINTER_TO_MEMBER, "ch_pointer_to_member"},
			{ TokenType::STRING_LITERAL,   "string_literal"},
			{ TokenType::INTEGER_LITERAL,  "integer_literal"},
			{ TokenType::FLOATING_POINT_LITERAL, "floating_point_literal"},
			{ TokenType::CHARACTER_LITERAL,"character_literal"},
			{ TokenType::COMMENT,          "comment"},
			{ TokenType::WHITESPACE,       "whitespace"},
			{ TokenType::NEWLINE,          "newline" },
			{ TokenType::HASHTAG,          "#" },
			{ TokenType::END_OF_FILE,      "EOF"},
			{ TokenType::ERROR_TYPE,       "ERROR_TYPE"},
			{ TokenType::PREPROCESSING_FILE_BEGIN, "Special Preprocessing File Begin" },
			{ TokenType::PREPROCESSING_FILE_END, "Special Preprocessing File End" },
		};

		// Main declarations
		const char* TokenName(TokenType type)
		{
			auto iter = tokenTypeToString.find(type);
			if (iter == tokenTypeToString.end())
			{
				return "ERROR";
			}

			return iter->second;
		}

		void FreeTokens(List<Token>& tokens)
		{
			for (Token& token : tokens)
			{
				ParserString::FreeString(token.m_Lexeme);
			}
		}

		void WriteTokensToFile(const List<Token>& tokens, const char* filename)
		{
			StringBuilder sb = StringBuilder();
			for (const Token& token : tokens)
			{
				const Token& nextToken = &token == tokens.end() ? Token() : *(&(token)+1);
				if (token.m_Type != TokenType::NEWLINE && token.m_Type != TokenType::STRING_LITERAL && token.m_Type != TokenType::PREPROCESSING_FILE_BEGIN &&
					token.m_Type != TokenType::PREPROCESSING_FILE_END)
				{
					sb.Append(token.m_Lexeme);
				}
				else if (token.m_Type == TokenType::STRING_LITERAL)
				{
					sb.Append('"');
					sb.Append(token.m_Lexeme);
					sb.Append('"');
				}
				else if (token.m_Type == TokenType::PREPROCESSING_FILE_BEGIN || token.m_Type == TokenType::PREPROCESSING_FILE_END)
				{
					if (token.m_Type == TokenType::PREPROCESSING_FILE_BEGIN)
						sb.Append("// File Begin: ");
					else
						sb.Append("// File End: ");
					sb.Append(token.m_Lexeme);
				}
				else
				{
					sb.Append('\n');
				}

				if ((token.m_Type == TokenType::IDENTIFIER && (nextToken.m_Type == TokenType::IDENTIFIER || nextToken.m_Type == TokenType::EQUAL)) ||
					token.m_Type == TokenType::EQUAL || keywords.find(token.m_Lexeme) != keywords.end())
				{
					sb.Append(' ');
				}
			}

			FileIO::DefaultWriteFile(filename, sb.c_str());
		}

		ScannerData OpenScanner(const char* filepath)
		{
			CountingFileStream stream = FileIO::OpenCountingFileStreamRead(filepath);
			ScannerData data;
			data.Stream = stream;
			data.Start = 0;
			return data;
		}

		ScannerData OpenScanner(CountingFileStream stream)
		{
			ScannerData data;
			data.Stream = stream;
			data.Start = 0;
			return data;
		}

		void CloseScanner(ScannerData& scanner)
		{
			FileIO::CloseCountingFileStreamRead(scanner.Stream);
		}

		List<Token> ScanTokens(const char* filepath, bool includeWhitespace)
		{
			List<Token> tokens;
			Logger::Log("Scanning file '%s'", filepath);

			ScannerData data = OpenScanner(filepath);

			while (!AtEnd(data))
			{
				Token token = ScanTokenInternal(data);
				if (!includeWhitespace)
				{
					if (token.m_Type == TokenType::WHITESPACE || token.m_Type == TokenType::COMMENT)
					{
						ParserString::FreeString(token.m_Lexeme);
						continue;
					}
				}
				if (token.m_Type != TokenType::ERROR_TYPE)
					tokens.push(token);
			}

			tokens.push(Token{ -1, data.Stream.Column, TokenType::NEWLINE, ParserString::CreateString("\\n") });
			tokens.push(Token{ -1, data.Stream.Column, TokenType::END_OF_FILE, ParserString::CreateString("EOF") });

			CloseScanner(data);
			return tokens;
		}

		Token ScanToken(ScannerData& scannerData, bool includeWhitespace)
		{
			Token token = ScanTokenInternal(scannerData);
			while ((!includeWhitespace && (token.m_Type == TokenType::WHITESPACE || token.m_Type == TokenType::COMMENT)) || token.m_Type == TokenType::ERROR_TYPE)
			{
				if (!includeWhitespace)
				{
					if (token.m_Type == TokenType::WHITESPACE || token.m_Type == TokenType::COMMENT)
					{
						ParserString::FreeString(token.m_Lexeme);
					}
				}
				token = ScanTokenInternal(scannerData);
			}

			return token;
		}

		TokenType PeekToken(const ScannerData& scannerData, bool includeWhitespace)
		{
			ScannerData shallowCopyScannerData = scannerData;
			Token token = ScanTokenInternal(shallowCopyScannerData);
			while ((!includeWhitespace && (token.m_Type == TokenType::WHITESPACE || token.m_Type == TokenType::COMMENT)) || token.m_Type == TokenType::ERROR_TYPE)
			{
				if (!includeWhitespace)
				{
					if (token.m_Type == TokenType::WHITESPACE || token.m_Type == TokenType::COMMENT)
					{
						ParserString::FreeString(token.m_Lexeme);
					}
				}
				token = ScanTokenInternal(shallowCopyScannerData);
			}

			ParserString::FreeString(token.m_Lexeme);
			FileIO::StreamGoTo(shallowCopyScannerData.Stream, scannerData.Stream.Stream.Cursor);
			return token.m_Type;
		}

		void DebugPrint(const List<Token>& tokens, bool printLineAndCol, bool printWhitespace)
		{
			for (auto token = tokens.begin(); token != tokens.end(); token++)
			{
				if ((token->m_Type == TokenType::WHITESPACE || token->m_Type == TokenType::COMMENT))
				{
					if (!printWhitespace)
					{
						continue;
					}
					else
					{
						if (token->m_Type == TokenType::WHITESPACE)
							Logger::Info("Whitespace");
						else if (token->m_Type == TokenType::COMMENT)
							Logger::Info("Comment");
						continue;
					}
				}
				if (!printLineAndCol)
				{
					auto iter = tokenTypeToString.find(token->m_Type);
					Logger::Assert(iter != tokenTypeToString.end(), "Invalid token while debug printing.");
					Logger::Info("Token<%s>: %s", iter->second, token->m_Lexeme);
				}
				else
				{
					auto iter = tokenTypeToString.find(token->m_Type);
					Logger::Assert(iter != tokenTypeToString.end(), "Invalid token while debug printing.");
					Logger::Info("Line: %d:%d Token<%s>: %s", token->m_Line, token->m_Column, iter->second, token->m_Lexeme);
				}
			}
		}

		void AppendTokenToStringBuilder(StringBuilder& sb, const Token& token)
		{
			if (token.m_Type == TokenType::STRING_LITERAL)
			{
				sb.Append('"');
				sb.Append(token.m_Lexeme);
				sb.Append('"');
			}
			else
			{
				sb.Append(token.m_Lexeme);
			}

			if (token.m_Type != TokenType::LEFT_CURLY_BRACKET && token.m_Type != TokenType::RIGHT_CURLY_BRACKET && token.m_Type != TokenType::LEFT_BRACKET &&
				token.m_Type != TokenType::LEFT_PAREN && token.m_Type != TokenType::LEFT_ANGLE_BRACKET &&
				token.m_Type != TokenType::COLON && token.m_Type != TokenType::SEMICOLON && token.m_Type != TokenType::DOT && token.m_Type != TokenType::ARROW &&
				token.m_Type != TokenType::TILDE && token.m_Type != TokenType::BANG && token.m_Type != TokenType::PLUS_PLUS && token.m_Type != TokenType::MINUS_MINUS)
			{
				sb.Append(' ');
			}
		}

		void AppendTokenToStream(FileStream& stream, const Token& token)
		{
			FileIO::WriteToStream(stream, token.m_Lexeme);
			if (token.m_Type != TokenType::LEFT_CURLY_BRACKET && token.m_Type != TokenType::RIGHT_CURLY_BRACKET && token.m_Type != TokenType::LEFT_BRACKET &&
				token.m_Type != TokenType::LEFT_PAREN && token.m_Type != TokenType::LEFT_ANGLE_BRACKET &&
				token.m_Type != TokenType::COLON && token.m_Type != TokenType::SEMICOLON && token.m_Type != TokenType::DOT && token.m_Type != TokenType::ARROW &&
				token.m_Type != TokenType::TILDE && token.m_Type != TokenType::BANG && token.m_Type != TokenType::PLUS_PLUS && token.m_Type != TokenType::MINUS_MINUS)
			{
				FileIO::WriteToStream(stream, " ");
			}
		}

		static Token ScanTokenInternal(ScannerData& data)
		{
			data.Start = data.Stream.Stream.Cursor;
			if (AtEnd(data))
			{
				Token dummy;
				dummy.m_Type = TokenType::END_OF_FILE;
				dummy.m_Lexeme = "";
				return dummy;
			}

			char c = Advance(data);
			switch (c)
			{
				// Single character tokens
			case '(': return GenerateToken(data, TokenType::LEFT_PAREN, "(");
			case ')': return GenerateToken(data, TokenType::RIGHT_PAREN, ")");
			case '{': return GenerateToken(data, TokenType::LEFT_CURLY_BRACKET, "{");
			case '}': return GenerateToken(data, TokenType::RIGHT_CURLY_BRACKET, "}");
			case ';': return GenerateToken(data, TokenType::SEMICOLON, ";");
			case '[': return GenerateToken(data, TokenType::LEFT_BRACKET, "[");
			case ']': return GenerateToken(data, TokenType::RIGHT_BRACKET, "]");
			case '?': return GenerateToken(data, TokenType::QUESTION, "?");
			case '~': return GenerateToken(data, TokenType::TILDE, "~");
			case ',': return GenerateToken(data, TokenType::COMMA, ",");
			case '#': return GenerateToken(data, TokenType::HASHTAG, "#");
			case '"': return String(data);
			case '\'': return Character(data);
			case ':':
			{
				return GenerateToken(data, TokenType::COLON, ":");
			}
			case '<':
			{
				if (Match(data, '<'))
				{
					if (Match(data, '='))
					{
						return GenerateToken(data, TokenType::LEFT_SHIFT_EQUAL, "<<=");
					}
					return GenerateToken(data, TokenType::LEFT_SHIFT, "<<");
				}
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::LESS_THAN_EQ, "<=");
				}
				return GenerateToken(data, TokenType::LEFT_ANGLE_BRACKET, "<");
			}
			case '>':
			{
				if (Match(data, '>'))
				{
					if (Match(data, '='))
					{
						return GenerateToken(data, TokenType::RIGHT_SHIFT_EQUAL, ">>=");
					}
					return GenerateToken(data, TokenType::RIGHT_SHIFT, ">>");
				}
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::GREATER_THAN_EQ, ">=");
				}
				return GenerateToken(data, TokenType::RIGHT_ANGLE_BRACKET, ">");
			}
			case '*':
			{
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::STAR_EQUAL, "*=");
				}
				return GenerateToken(data, TokenType::STAR, "*");
			}
			case '!':
			{
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::BANG_EQUAL, "!=");
				}
				return GenerateToken(data, TokenType::BANG, "!");
			}
			case '^':
			{
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::CARET_EQUAL, "^=");
				}
				return GenerateToken(data, TokenType::CARET, "^");
			}
			case '%':
			{
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::MODULO_EQUAL, "%=");
				}
				return GenerateToken(data, TokenType::MODULO, "%");
			}
			case '&':
			{
				if (Match(data, '&'))
				{
					return GenerateToken(data, TokenType::LOGICAL_AND, "&&");
				}
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::AND_EQUAL, "&=");
				}
				return GenerateToken(data, TokenType::AND, "&");
			}
			case '=':
			{
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::EQUAL_EQUAL, "==");
				}
				return GenerateToken(data, TokenType::EQUAL, "=");
			}
			case '|':
			{
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::PIPE_EQUAL, "|=");
				}
				if (Match(data, '|'))
				{
					return GenerateToken(data, TokenType::LOGICAL_OR, "||");
				}
				return GenerateToken(data, TokenType::PIPE, "|");
			}
			case '.':
			{
				if (IsDigit(Peek(data)))
				{
					return Number(data, c);
				}
				if (Match(data, '*'))
				{
					return GenerateToken(data, TokenType::POINTER_TO_MEMBER, ".*");
				}
				return GenerateToken(data, TokenType::DOT, ".");
			}
			case '+':
			{
				if (Match(data, '+'))
				{
					return GenerateToken(data, TokenType::PLUS_PLUS, "++");
				}
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::PLUS_EQUAL, "+=");
				}
				return GenerateToken(data, TokenType::PLUS, "+");
			}
			case '-':
			{
				if (Match(data, '-'))
				{
					return GenerateToken(data, TokenType::MINUS_MINUS, "--");
				}
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::MINUS_EQUAL, "-=");
				}
				if (Match(data, '>'))
				{
					if (Match(data, '*'))
					{
						return GenerateToken(data, TokenType::POINTER_TO_MEMBER, "->*");
					}
					return GenerateToken(data, TokenType::ARROW, "->");
				}
				return GenerateToken(data, TokenType::MINUS, "-");
			}
			case '/':
			{
				if (Match(data, '/'))
				{
					while (Peek(data) != '\n' && !AtEnd(data))
						Advance(data);
					return GenerateCommentToken();
				}
				if (Match(data, '*'))
				{
					while (!AtEnd(data) && !(Peek(data) == '*' && PeekNext(data) == '/'))
					{
						c = Advance(data);
					}

					// Consume */
					if (!AtEnd(data)) Match(data, '*');
					if (!AtEnd(data)) Match(data, '/');
					return GenerateCommentToken();
				}
				if (Match(data, '='))
				{
					return GenerateToken(data, TokenType::DIV_EQUAL, "/=");
				}
				return GenerateToken(data, TokenType::DIV, "/");
			}
			case 'u':
			case 'U':
			case 'L':
			{
				// I can't Match these characters because if it's an identifier
				// that starts with u8 I don't want to accidentally consume part of
				// it before going to the identifier function
				if (c == 'u' && Peek(data) == '8')
				{
					if (PeekNext(data) == '\'')
					{
						Match(data, '8'); Match(data, '\'');
						return Character(data);
					}
					if (PeekNext(data) == '"')
					{
						Match(data, '8'); Match(data, '"');
						return String(data);
					}
					if (PeekNext(data) == 'R' && PeekNextNext(data) == '"')
					{
						Match(data, '8'); Match(data, 'R'); Match(data, '"');
						return String(data, true);
					}
				}
				if (c == 'u')
				{
					if (Peek(data) == '\'')
					{
						Match(data, '\'');
						return Character(data);
					}
					if (Peek(data) == '"')
					{
						Match(data, '"');
						return String(data);
					}
					if (Peek(data) == 'R' && PeekNext(data) == '"')
					{
						Match(data, 'R'); Match(data, '"');
						return String(data, true);
					}
				}
				if (c == 'U')
				{
					if (Peek(data) == '\'')
					{
						Match(data, '\'');
						return Character(data);
					}
					if (Peek(data) == '"')
					{
						Match(data, '"');
						return String(data);
					}
					if (Peek(data) == 'R' && PeekNext(data) == '"')
					{
						Match(data, 'R'); Match(data, '"');
						return String(data, true);
					}
				}
				if (c == 'L')
				{
					if (Peek(data) == '\'')
					{
						Match(data, '\'');
						return Character(data);
					}
					if (Peek(data) == '"')
					{
						Match(data, '"');
						return String(data);
					}
					if (Peek(data) == 'R' && PeekNext(data) == '"')
					{
						Match(data, 'R'); Match(data, '"');
						return String(data, true);
					}
				}
				return PropertyIdentifier(data);
			}
			case 'R':
			{
				if (Peek(data) == '"')
				{
					Match(data, 'R'); Match(data, '"');
					return String(data, true);
				}
				return PropertyIdentifier(data);
			}
			case ' ':
			case '\r':
				// Ignore whitespace
				return GenerateWhitespaceToken();
			case '\t':
				return GenerateWhitespaceToken();
			case '\n':
				// Record the new line, then continue
				if (PeekPrevious(data, 2) == '\\' || (PeekPrevious(data, 2) == '\r' && PeekPrevious(data, 3) == '\\'))
				{
					return GenerateWhitespaceToken();
				}
				return GenerateToken(data, TokenType::NEWLINE, "\\n");
			default:
				if (IsDigit(c))
				{
					return Number(data, c);
				}
				if (IsAlpha(c) || c == '_')
				{
					return PropertyIdentifier(data);
				}
				break;
			}

			return GenerateErrorToken(data);
		}

		static Token PropertyIdentifier(ScannerData& data)
		{
			while (!AtEnd(data) && (IsAlphaNumeric(Peek(data)) || Peek(data) == '_')) Advance(data);

			const char* text = FileIO::StreamSubstring(data.Stream, data.Start, data.Stream.Stream.Cursor - data.Start);
			TokenType type = TokenType::IDENTIFIER;
			auto iter = keywords.find(text);
			if (iter != keywords.end())
			{
				type = iter->second;
			}

			return Token{ data.Stream.Line, data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start), type, text };
		}

		static Token Number(ScannerData& data, char firstDigit)
		{
			bool isHexadecimal = false;
			bool isOctal = false;
			bool isBinary = false;
			if (firstDigit == '0')
			{
				// If the number starts with a 0x, it's hex, otherwise
				// if it starts with a 0 and it's not followed by a '.' it's octal
				if (Match(data, 'x') || Match(data, 'X'))
				{
					isHexadecimal = true;
				}
				else if (Match(data, 'b') || Match(data, 'B'))
				{
					isBinary = true;
				}
				else if (Peek(data) != '.')
				{
					isOctal = true;
				}
			}

			if (isHexadecimal)
			{
				return NumberHexadecimal(data);
			}
			else if (isOctal)
			{
				return NumberOctal(data);
			}
			else if (isBinary)
			{
				return NumberBinary(data);
			}
			return NumberDecimal(data, firstDigit);
		}

		static Token NumberDecimal(ScannerData& data, char firstDigit)
		{
			while (firstDigit != '.' && IsDigit(Peek(data), true))
			{
				Advance(data);
			}

			bool isFloatingPoint = false;
			if (Match(data, '.') || firstDigit == '.')
			{
				isFloatingPoint = true;

				while (IsDigit(Peek(data), true))
				{
					Advance(data);
				}
			}

			if (IsSameChar(Peek(data), 'e', 'E') &&
				(IsDigit(PeekNext(data)) ||
					(
						(PeekNext(data) == '-' && IsDigit(PeekNextNext(data))) ||
						(PeekNext(data) == '+' && IsDigit(PeekNextNext(data)))
						)
					)
				)
			{
				isFloatingPoint = true;
				Advance(data);
				Advance(data);
				while (IsDigit(Peek(data), true))
				{
					Advance(data);
				}

				if ((Peek(data) == '-' || Peek(data) == '+') && IsDigit(PeekNext(data)))
				{
					Advance(data);
					while (IsDigit(Peek(data))) Advance(data);
				}

				if (Peek(data) == '.')
				{
					Logger::Error("Unexpected number literal at %d col:%d", data.Stream.Line, data.Stream.Column);
					return GenerateErrorToken(data);
				}
			}

			// This bit is just to consume the trailing 'f' or 'l'
			if (isFloatingPoint)
			{
				if (IsSameChar(Peek(data), 'f', 'F'))
				{
					Match(data, Peek(data));
				}
				else if (IsSameChar(Peek(data), 'l', 'L'))
				{
					Match(data, Peek(data));
				}
			}

			if (!isFloatingPoint)
			{
				ConsumeTrailingUnsignedLong(data);

				return Token{ data.Stream.Line, data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start), TokenType::INTEGER_LITERAL,
					FileIO::StreamSubstring(data.Stream, data.Start, data.Stream.Stream.Cursor - data.Start) };
			}

			return Token{ data.Stream.Line, data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start), TokenType::FLOATING_POINT_LITERAL,
				FileIO::StreamSubstring(data.Stream, data.Start, data.Stream.Stream.Cursor - data.Start) };
		}

		static Token NumberHexadecimal(ScannerData& data)
		{
			while (IsHexDigit(Peek(data), true)) Advance(data);
			ConsumeTrailingUnsignedLong(data);

			return Token{ data.Stream.Line, data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start), TokenType::INTEGER_LITERAL,
				FileIO::StreamSubstring(data.Stream, data.Start, data.Stream.Stream.Cursor - data.Start) };
		}

		static Token NumberBinary(ScannerData& data)
		{
			while (Peek(data) == '0' || Peek(data) == '1' || Peek(data) == '\'') Advance(data);
			ConsumeTrailingUnsignedLong(data);

			return Token{ data.Stream.Line, data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start), TokenType::INTEGER_LITERAL,
				FileIO::StreamSubstring(data.Stream, data.Start, data.Stream.Stream.Cursor - data.Start) };
		}

		static Token NumberOctal(ScannerData& data)
		{
			while (Peek(data) >= '0' && Peek(data) <= '7' || Peek(data) == '\'') Advance(data);
			ConsumeTrailingUnsignedLong(data);

			return Token{ data.Stream.Line, data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start), TokenType::INTEGER_LITERAL,
				FileIO::StreamSubstring(data.Stream, data.Start, data.Stream.Stream.Cursor - data.Start) };
		}

		static void ConsumeTrailingUnsignedLong(ScannerData& data)
		{
			// consume up to 3 'u' or 'l' characters
			if (IsSameChar(Peek(data), 'u', 'U'))
			{
				Match(data, Peek(data));
			}

			if (IsSameChar(Peek(data), 'l', 'L'))
			{
				Match(data, Peek(data));
			}

			if (IsSameChar(Peek(data), 'l', 'L'))
			{
				Match(data, Peek(data));
			}
		}


		static Token Character(ScannerData& data)
		{
			// The first apostrophe ' has already been consume at this point
			while (Peek(data) != '\'' && !AtEnd(data))
			{
				if (Peek(data) == '\n')
				{
					Logger::Warning("Invalid character literal encountered while scanning at line: %d:%d", data.Stream.Line, data.Stream.Column);
					break;
				}
				// Skip over any escaped quotes
				if (Peek(data) == '\\' && PeekNext(data) == '\'')
				{
					Advance(data);
				}
				// Skip over escaped back slashes so that it doesn't accidentally skip an end quote
				if (Peek(data) == '\\' && PeekNext(data) == '\\')
				{
					Advance(data);
				}
				Advance(data);
			}

			if (AtEnd(data))
			{
				// TODO: This might not need to be here
				Logger::Warning("Unexpected character literal encountered while scanning at line %d:%d. We hit the end of the file.", data.Stream.Line, data.Stream.Column);
				return GenerateErrorToken(data);
			}

			Advance(data);

			return Token{ data.Stream.Line, data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start), TokenType::CHARACTER_LITERAL,
				FileIO::StreamSubstring(data.Stream, data.Start, data.Stream.Stream.Cursor - data.Start) };
		}

		static Token String(ScannerData& data, bool isRawStringLiteral)
		{
			if (isRawStringLiteral)
			{
				int matchStart = data.Stream.Stream.Cursor;
				while (Peek(data) != '(')
				{
					Advance(data);
				}
				const char* strToMatch = ")";
				if (data.Stream.Stream.Cursor > matchStart)
				{
					const char* newStrToMatch = FileIO::StreamSubstring(data.Stream, matchStart, data.Stream.Stream.Cursor - matchStart);
					ParserString::FreeString(strToMatch);
					strToMatch = newStrToMatch;
				}
				int strToMatchLength = ParserString::StringLength(strToMatch);

				while (!AtEnd(data))
				{
					if (Peek(data) == ')')
					{
						const char* fileSubstr = FileIO::StreamSubstring(data.Stream, data.Stream.Stream.Cursor, strToMatchLength);
						if (fileSubstr == strToMatch)
						{
							for (int i = 0; i < strToMatchLength; i++)
							{
								Advance(data);
							}
							ParserString::FreeString(fileSubstr);
							break;
						}
						ParserString::FreeString(fileSubstr);
					}
					Advance(data);
				}

				ParserString::FreeString(strToMatch);
			}
			else
			{
				while (((PeekPrevious(data, 1) == '\\' && Peek(data) == '"') || Peek(data) != '"') && !AtEnd(data))
				{
					Advance(data);
				}
			}

			if (AtEnd(data))
			{
				Logger::Error("Unexpected string literal at %d col:%d", data.Stream.Line, data.Stream.Column);
				return GenerateErrorToken(data);
			}

			Advance(data);

			if (!isRawStringLiteral)
			{
				// Remove thes start and end quotes
				const char* value = FileIO::StreamSubstring(data.Stream, data.Start + (unsigned long long)1, (unsigned long long)data.Stream.Stream.Cursor - data.Start - (unsigned long long)2);
				return Token{ data.Stream.Line, data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start), TokenType::STRING_LITERAL, value };
			}
			else
			{
				const char* value = FileIO::StreamSubstring(data.Stream, data.Start, (unsigned long long)data.Stream.Stream.Cursor - (unsigned long long)data.Start);
				return Token{ data.Stream.Line, data.Stream.Column - (int)(data.Stream.Stream.Cursor - data.Start), TokenType::STRING_LITERAL, value };
			}
		}
	}
}