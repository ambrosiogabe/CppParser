#include "cppParser/ScriptScanner.h"
#include "cppParser/CppTokens.h"
#include "cppParser/FileIO.h"
#include "CppUtils/CppUtils.h"

#include <unordered_map>

namespace CppParser
{
	namespace ScriptScanner
	{
		using namespace CppUtils;

		// Internal Variables
		static int m_Cursor;
		static std::string m_FileContents;
		static const char* m_Filepath;
		static int m_FileContentsSize;
		static int m_Line;
		static int m_Column;
		static int m_Start;

		// Forward Declarations
		static Token ScanToken();
		static Token PropertyIdentifier();
		static Token Number(char firstDigit);
		static Token NumberDecimal(char firstDigit);
		static Token NumberBinary();
		static Token NumberHexadecimal();
		static Token NumberOctal();

		static Token Character();
		static Token String(bool isRawStringLiteral = false);

		// Inline functions
		static inline char Advance();
		static inline char Peek();
		static inline char PeekNext();
		static inline char PeekNextNext();
		static inline bool Match(char expected);
		static inline void ConsumeTrailingUnsignedLong();
		static inline char PeekPrevious(int amount) { return m_Cursor > amount && m_FileContentsSize > amount ? m_FileContents[m_Cursor - amount] : '\0'; }

		static inline bool IsDigit(char c, bool acceptApostrophe = false) { return c >= '0' && c <= '9' || (acceptApostrophe && c == '\''); }
		static inline bool IsHexDigit(char c, bool acceptApostrophe) { return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || (acceptApostrophe && c == '\''); }
		static inline bool IsAlpha(char c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }
		static inline bool IsAlphaNumeric(char c) { return IsAlpha(c) || IsDigit(c); }
		static inline bool IsSameChar(char c, char lowerCase, char upperCase) { return c == lowerCase || c == upperCase; }
		static inline bool AtEnd() { return m_Cursor == m_FileContentsSize; }

		static inline Token GenerateToken(TokenType m_Type, char* lexeme)
		{
			return CppTokens::CreateToken(
				m_Line,
				m_Column - (m_Cursor - m_Start),
				m_Type,
				ParserString::CreateString(lexeme)
			);
		}

		inline Token GenerateErrorToken()
		{
			return CppTokens::CreateToken(
				m_Line,
				m_Column - (m_Cursor - m_Start),
				TokenType::ERROR_TYPE,
				ParserString::CreateString("")
			);
		}

		inline Token GenerateWhitespaceToken()
		{
			return CppTokens::CreateToken(
				-1,
				-1,
				TokenType::WHITESPACE,
				ParserString::CreateString("")
			);
		}

		inline Token GenerateCommentToken()
		{
			return CppTokens::CreateToken(
				-1,
				-1,
				TokenType::COMMENT,
				ParserString::CreateString("")
			);
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

		List<Token> ScanTokens(const char* filepath, bool includeWhitespace)
		{
			List<Token> tokens;
			Logger::Log("Scanning file '%s'", filepath);
			char* rawFileContents = FileIO::DefaultReadFile(filepath);
			m_FileContents = std::string(rawFileContents);
			m_Filepath = filepath;
			m_FileContentsSize = (int)m_FileContents.length();
			m_Line = 1;
			m_Column = 0;
			m_Start = 0;

			m_Cursor = 0;
			while (!AtEnd())
			{
				m_Start = m_Cursor;
				Token token = ScanToken();
				if (!includeWhitespace)
				{
					if (token.m_Type == TokenType::WHITESPACE || token.m_Type == TokenType::COMMENT)
					{
						continue;
					}
				}
				if (token.m_Type != TokenType::ERROR_TYPE)
					tokens.push(token);
			}

			tokens.push(Token{ -1, m_Column, TokenType::END_OF_FILE, ParserString::CreateString("EOF") });

			FileIO::DefaultFreeFile(rawFileContents);
			return tokens;
		}

		void DebugPrint(const List<Token>& tokens, bool printLineAndCol, bool printWhitespace)
		{
			Logger::Info("Tokens for file: '%s'", m_Filepath);

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

		static Token ScanToken()
		{
			char c = Advance();
			switch (c)
			{
				// Single character tokens
			case '(': return GenerateToken(TokenType::LEFT_PAREN, "(");
			case ')': return GenerateToken(TokenType::RIGHT_PAREN, ")");
			case '{': return GenerateToken(TokenType::LEFT_CURLY_BRACKET, "{");
			case '}': return GenerateToken(TokenType::RIGHT_CURLY_BRACKET, "}");
			case ';': return GenerateToken(TokenType::SEMICOLON, ";");
			case '[': return GenerateToken(TokenType::LEFT_BRACKET, "[");
			case ']': return GenerateToken(TokenType::RIGHT_BRACKET, "]");
			case '?': return GenerateToken(TokenType::QUESTION, "?");
			case '~': return GenerateToken(TokenType::TILDE, "~");
			case ',': return GenerateToken(TokenType::COMMA, ",");
			case '#': return GenerateToken(TokenType::HASHTAG, "#");
			case '"': return String();
			case '\'': return Character();
			case ':':
			{
				return GenerateToken(TokenType::COLON, ":");
			}
			case '<':
			{
				if (Match('<'))
				{
					if (Match('='))
					{
						return GenerateToken(TokenType::LEFT_SHIFT_EQUAL, "<<=");
					}
					return GenerateToken(TokenType::LEFT_SHIFT, "<<");
				}
				if (Match('='))
				{
					return GenerateToken(TokenType::LESS_THAN_EQ, "<=");
				}
				return GenerateToken(TokenType::LEFT_ANGLE_BRACKET, "<");
			}
			case '>':
			{
				if (Match('>'))
				{
					if (Match('='))
					{
						return GenerateToken(TokenType::RIGHT_SHIFT_EQUAL, ">>=");
					}
					return GenerateToken(TokenType::RIGHT_SHIFT, ">>");
				}
				if (Match('='))
				{
					return GenerateToken(TokenType::GREATER_THAN_EQ, ">=");
				}
				return GenerateToken(TokenType::RIGHT_ANGLE_BRACKET, ">");
			}
			case '*':
			{
				if (Match('='))
				{
					return GenerateToken(TokenType::STAR_EQUAL, "*=");
				}
				return GenerateToken(TokenType::STAR, "*");
			}
			case '!':
			{
				if (Match('='))
				{
					return GenerateToken(TokenType::BANG_EQUAL, "!=");
				}
				return GenerateToken(TokenType::BANG, "!");
			}
			case '^':
			{
				if (Match('='))
				{
					return GenerateToken(TokenType::CARET_EQUAL, "^=");
				}
				return GenerateToken(TokenType::CARET, "^");
			}
			case '%':
			{
				if (Match('='))
				{
					return GenerateToken(TokenType::MODULO_EQUAL, "%=");
				}
				return GenerateToken(TokenType::MODULO, "%");
			}
			case '&':
			{
				if (Match('&'))
				{
					return GenerateToken(TokenType::LOGICAL_AND, "&&");
				}
				if (Match('='))
				{
					return GenerateToken(TokenType::AND_EQUAL, "&=");
				}
				return GenerateToken(TokenType::AND, "&");
			}
			case '=':
			{
				if (Match('='))
				{
					return GenerateToken(TokenType::EQUAL_EQUAL, "==");
				}
				return GenerateToken(TokenType::EQUAL, "=");
			}
			case '|':
			{
				if (Match('='))
				{
					return GenerateToken(TokenType::PIPE_EQUAL, "|=");
				}
				if (Match('|'))
				{
					return GenerateToken(TokenType::LOGICAL_OR, "||");
				}
				return GenerateToken(TokenType::PIPE, "|");
			}
			case '.':
			{
				if (IsDigit(Peek()))
				{
					return Number(c);
				}
				if (Match('*'))
				{
					return GenerateToken(TokenType::POINTER_TO_MEMBER, ".*");
				}
				return GenerateToken(TokenType::DOT, ".");
			}
			case '+':
			{
				if (Match('+'))
				{
					return GenerateToken(TokenType::PLUS_PLUS, "++");
				}
				if (Match('='))
				{
					return GenerateToken(TokenType::PLUS_EQUAL, "+=");
				}
				return GenerateToken(TokenType::PLUS, "+");
			}
			case '-':
			{
				if (Match('-'))
				{
					return GenerateToken(TokenType::MINUS_MINUS, "--");
				}
				if (Match('='))
				{
					return GenerateToken(TokenType::MINUS_EQUAL, "-=");
				}
				if (Match('>'))
				{
					if (Match('*'))
					{
						return GenerateToken(TokenType::POINTER_TO_MEMBER, "->*");
					}
					return GenerateToken(TokenType::ARROW, "->");
				}
				return GenerateToken(TokenType::MINUS, "-");
			}
			case '/':
			{
				if (Match('/'))
				{
					while (Peek() != '\n' && !AtEnd())
						Advance();
					return GenerateCommentToken();
				}
				if (Match('*'))
				{
					while (!AtEnd() && !(Peek() == '*' && PeekNext() == '/'))
					{
						c = Advance();
						if (c == '\n')
						{
							m_Column = 0;
							m_Line++;
						}
					}

					// Consume */
					if (!AtEnd()) Match('*');
					if (!AtEnd()) Match('/');
					return GenerateCommentToken();
				}
				if (Match('='))
				{
					return GenerateToken(TokenType::DIV_EQUAL, "/=");
				}
				return GenerateToken(TokenType::DIV, "/");
			}
			case 'u':
			case 'U':
			case 'L':
			{
				// I can't Match these characters because if it's an identifier
				// that starts with u8 I don't want to accidentally consume part of
				// it before going to the identifier function
				if (c == 'u' && Peek() == '8')
				{
					if (PeekNext() == '\'')
					{
						Match('8'); Match('\'');
						return Character();
					}
					if (PeekNext() == '"')
					{
						Match('8'); Match('"');
						return String();
					}
					if (PeekNext() == 'R' && PeekNextNext() == '"')
					{
						Match('8'); Match('R'); Match('"');
						return String(true);
					}
				}
				if (c == 'u')
				{
					if (Peek() == '\'')
					{
						Match('\'');
						return Character();
					}
					if (Peek() == '"')
					{
						Match('"');
						return String();
					}
					if (Peek() == 'R' && PeekNext() == '"')
					{
						Match('R'); Match('"');
						return String(true);
					}
				}
				if (c == 'U')
				{
					if (Peek() == '\'')
					{
						Match('\'');
						return Character();
					}
					if (Peek() == '"')
					{
						Match('"');
						return String();
					}
					if (Peek() == 'R' && PeekNext() == '"')
					{
						Match('R'); Match('"');
						return String(true);
					}
				}
				if (c == 'L')
				{
					if (Peek() == '\'')
					{
						Match('\'');
						return Character();
					}
					if (Peek() == '"')
					{
						Match('"');
						return String();
					}
					if (Peek() == 'R' && PeekNext() == '"')
					{
						Match('R'); Match('"');
						return String(true);
					}
				}
				return PropertyIdentifier();
			}
			case 'R':
			{
				if (Peek() == '"')
				{
					Match('R'); Match('"');
					return String(true);
				}
				return PropertyIdentifier();
			}
			case ' ':
			case '\r':
				// Ignore whitespace
				m_Column++;
				return GenerateWhitespaceToken();
			case '\t':
				m_Column += 4;
				return GenerateWhitespaceToken();
			case '\n':
				// Record the new line, then continue
				m_Column = 0;
				m_Line++;
				if (PeekPrevious(2) == '\\' || (PeekPrevious(2) == '\r' && PeekPrevious(3) == '\\'))
				{
					return GenerateWhitespaceToken();
				}
				return GenerateToken(TokenType::NEWLINE, "\\n");
			default:
				if (IsDigit(c))
				{
					return Number(c);
				}
				if (IsAlpha(c) || c == '_')
				{
					return PropertyIdentifier();
				}
				break;
			}

			return GenerateErrorToken();
		}

		static Token PropertyIdentifier()
		{
			while (IsAlphaNumeric(Peek()) || Peek() == '_') Advance();

			std::string text = std::string(m_FileContents.substr(m_Start, m_Cursor - m_Start));
			TokenType type = TokenType::IDENTIFIER;
			auto iter = keywords.find(text.c_str());
			if (iter != keywords.end())
			{
				type = iter->second;
			}

			return Token{ m_Line, m_Column - (m_Cursor - m_Start), type, ParserString::CreateString(text.c_str()) };
		}

		static Token Number(char firstDigit)
		{
			bool isHexadecimal = false;
			bool isOctal = false;
			bool isBinary = false;
			if (firstDigit == '0')
			{
				// If the number starts with a 0x, it's hex, otherwise
				// if it starts with a 0 and it's not followed by a '.' it's octal
				if (Match('x') || Match('X'))
				{
					isHexadecimal = true;
				}
				else if (Match('b') || Match('B'))
				{
					isBinary = true;
				}
				else if (Peek() != '.')
				{
					isOctal = true;
				}
			}

			if (isHexadecimal)
			{
				return NumberHexadecimal();
			}
			else if (isOctal)
			{
				return NumberOctal();
			}
			else if (isBinary)
			{
				return NumberBinary();
			}
			return NumberDecimal(firstDigit);
		}

		static Token NumberDecimal(char firstDigit)
		{
			while (firstDigit != '.' && IsDigit(Peek(), true))
			{
				Advance();
			}

			bool isFloatingPoint = false;
			if (Match('.') || firstDigit == '.')
			{
				isFloatingPoint = true;

				while (IsDigit(Peek(), true))
				{
					Advance();
				}
			}

			if (IsSameChar(Peek(), 'e', 'E') &&
				(IsDigit(PeekNext()) ||
					(
						(PeekNext() == '-' && IsDigit(PeekNextNext())) ||
						(PeekNext() == '+' && IsDigit(PeekNextNext()))
						)
					)
				)
			{
				isFloatingPoint = true;
				Advance();
				Advance();
				while (IsDigit(Peek(), true))
				{
					Advance();
				}

				if ((Peek() == '-' || Peek() == '+') && IsDigit(PeekNext()))
				{
					Advance();
					while (IsDigit(Peek())) Advance();
				}

				if (Peek() == '.')
				{
					Logger::Error("Unexpected number literal at %d col:%d", m_Line, m_Column);
					return GenerateErrorToken();
				}
			}

			// This bit is just to consume the trailing 'f' or 'l'
			if (isFloatingPoint)
			{
				if (IsSameChar(Peek(), 'f', 'F'))
				{
					Match(Peek());
				}
				else if (IsSameChar(Peek(), 'l', 'L'))
				{
					Match(Peek());
				}
			}

			if (!isFloatingPoint)
			{
				ConsumeTrailingUnsignedLong();

				return Token{ m_Line, m_Column - (m_Cursor - m_Start), TokenType::INTEGER_LITERAL, ParserString::CreateString(m_FileContents.substr(m_Start, m_Cursor - m_Start).c_str()) };
			}

			return Token{ m_Line, m_Column - (m_Cursor - m_Start), TokenType::FLOATING_POINT_LITERAL, ParserString::CreateString(m_FileContents.substr(m_Start, m_Cursor - m_Start).c_str()) };
		}

		static Token NumberHexadecimal()
		{
			while (IsHexDigit(Peek(), true)) Advance();
			ConsumeTrailingUnsignedLong();

			return Token{ m_Line, m_Column - (m_Cursor - m_Start), TokenType::INTEGER_LITERAL, ParserString::CreateString(m_FileContents.substr(m_Start, m_Cursor - m_Start).c_str()) };
		}

		static Token NumberBinary()
		{
			while (Peek() == '0' || Peek() == '1' || Peek() == '\'') Advance();
			ConsumeTrailingUnsignedLong();

			return Token{ m_Line, m_Column - (m_Cursor - m_Start), TokenType::INTEGER_LITERAL, ParserString::CreateString(m_FileContents.substr(m_Start, m_Cursor - m_Start).c_str()) };
		}

		static Token NumberOctal()
		{
			while (Peek() >= '0' && Peek() <= '7' || Peek() == '\'') Advance();
			ConsumeTrailingUnsignedLong();

			return Token{ m_Line, m_Column - (m_Cursor - m_Start), TokenType::INTEGER_LITERAL, ParserString::CreateString(m_FileContents.substr(m_Start, m_Cursor - m_Start).c_str()) };
		}

		static void ConsumeTrailingUnsignedLong()
		{
			// consume up to 3 'u' or 'l' characters
			if (IsSameChar(Peek(), 'u', 'U'))
			{
				Match(Peek());
			}

			if (IsSameChar(Peek(), 'l', 'L'))
			{
				Match(Peek());
			}

			if (IsSameChar(Peek(), 'l', 'L'))
			{
				Match(Peek());
			}
		}


		static Token Character()
		{
			// The first apostrophe ' has already been consume at this point
			while (Peek() != '\'' && !AtEnd())
			{
				if (Peek() == '\n')
				{
					Logger::Warning("Invalid character literal encountered while scanning at line: %d:%d", m_Line, m_Column);
					m_Line++;
					m_Column = -1;
					break;
				}
				// Skip over any escaped quotes
				if (Peek() == '\\' && PeekNext() == '\'')
				{
					Advance();
				}
				// Skip over escaped back slashes so that it doesn't accidentally skip an end quote
				if (Peek() == '\\' && PeekNext() == '\\')
				{
					Advance();
				}
				Advance();
			}

			if (AtEnd())
			{
				// TODO: This might not need to be here
				Logger::Warning("Unexpected character literal encountered while scanning at line %d:%d. We hit the end of the file.", m_Line, m_Column);
				return GenerateErrorToken();
			}

			Advance();

			std::string value = m_FileContents.substr(m_Start, m_Cursor - m_Start);
			return Token{ m_Line, m_Column - (m_Cursor - m_Start), TokenType::CHARACTER_LITERAL, ParserString::CreateString(value.c_str()) };
		}

		static Token String(bool isRawStringLiteral)
		{
			if (isRawStringLiteral)
			{
				int matchStart = m_Cursor;
				while (Peek() != '(')
				{
					Advance();
				}
				std::string strToMatch = ")";
				if (m_Cursor > matchStart)
				{
					strToMatch += m_FileContents.substr(matchStart, m_Cursor - matchStart);
				}

				while (!AtEnd())
				{
					if (Peek() == ')')
					{
						std::string fileSubstr = m_FileContents.substr(m_Cursor, strToMatch.size());
						if (fileSubstr == strToMatch)
						{
							for (int i = 0; i < strToMatch.size(); i++)
							{
								Advance();
							}
							break;
						}
					}
					Advance();
				}
			}
			else
			{

				while (Peek() != '"' && !AtEnd())
				{
					if (Peek() == '\n')
					{
						m_Line++;
						m_Column = -1;
					}
					Advance();
				}
			}

			if (AtEnd())
			{
				Logger::Error("Unexpected string literal at %d col:%d", m_Line, m_Column);
				return GenerateErrorToken();
			}

			Advance();

			if (!isRawStringLiteral)
			{
				// Remove thes start and end quotes
				std::string value = m_FileContents.substr(m_Start + (unsigned long long)1, (unsigned long long)m_Cursor - m_Start - (unsigned long long)2);
				return Token{ m_Line, m_Column - (m_Cursor - m_Start), TokenType::STRING_LITERAL, ParserString::CreateString(value.c_str()) };
			}
			else
			{
				std::string value = m_FileContents.substr(m_Start, (unsigned long long)m_Cursor - (unsigned long long)m_Start);
				return Token{ m_Line, m_Column - (m_Cursor - m_Start), TokenType::STRING_LITERAL, ParserString::CreateString(value.c_str()) };
			}
		}

		static char Advance()
		{
			char c = m_FileContents[m_Cursor];
			m_Cursor++;
			m_Column++;
			return c;
		}

		static char Peek()
		{
			if (AtEnd()) return '\0';
			return m_FileContents[m_Cursor];
		}

		static char PeekNext()
		{
			if (AtEnd() || m_Cursor == m_FileContents.size() - 1) return '\0';
			return m_FileContents[m_Cursor + 1];
		}

		static char PeekNextNext()
		{
			if (AtEnd() || m_Cursor == m_FileContents.size() - 1 || m_Cursor == m_FileContents.size() - 2) return '\0';
			return m_FileContents[m_Cursor + 2];
		}

		static bool Match(char expected)
		{
			if (AtEnd()) return false;
			if (m_FileContents[m_Cursor] != expected) return false;

			m_Cursor++;
			m_Column++;
			return true;
		}
	}
}