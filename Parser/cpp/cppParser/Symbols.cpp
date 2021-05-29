#include "cppParser/Symbols.h"
#include "cppParser/ParserString.h"
#include "cppParser/ScriptParser.h"
#include "CppUtils/CppUtils.h"

namespace CppParser
{
	namespace Symbols
	{
		using namespace CppUtils;

		// Internal variables
		static unsigned long HashToken(const Token& token);
		static bool IsSameHashData(const Token& t1, unsigned long hash1, int lineDefined, const DefineSymbol& hash2);
		static void MacroAddToReplacementList(PreprocessingAstNode* node, void* userData);
		static const char* ExpandSimpleMacro(const PPSymbolTable& symbolTable, PreprocessingAstNode* preprocessingNode, int newLine);
		static void FunctionMacroAddToIdentifierList(PreprocessingAstNode* node, void* userData);
		static const char* ExpandFunctionMacro(const PPSymbolTable& symbolTable,
			PreprocessingAstNode* preprocessingNode, int newLine, ParserData& data);

		// ===========================================================================================
		// Public functions
		// ===========================================================================================

		void Undefine(PPSymbolTable& symbolTable, const Token& token, int lineUndefined)
		{
			unsigned long tokenHash = HashToken(token);
			for (DefineSymbol& hash : symbolTable.DefineSymbols)
			{
				if (hash.hash == tokenHash && token.m_Line != hash.lineDefined && ParserString::Compare(token.m_Lexeme, hash.token.m_Lexeme))
				{
					hash.lineUndefined = lineUndefined;
					symbolTable.DefineSymbols.removeByElement(hash);
					return;
				}
			}

			Logger::Error("Cannot undefine macro '%s'. Macro has not been defined.", token.m_Lexeme);
		}

		void AddDefineSymbol(PPSymbolTable& symbolTable, const Token& macroIdentifierToken, int lineDefined, PreprocessingAstNode* symbolTree)
		{
			unsigned long tokenHash = HashToken(macroIdentifierToken);
			for (const DefineSymbol& hash : symbolTable.DefineSymbols)
			{
				if (IsSameHashData(macroIdentifierToken, tokenHash, lineDefined, hash))
				{
					Logger::Warning("Tried to redefine macro '%s' at line %d:%d", macroIdentifierToken.m_Lexeme, macroIdentifierToken.m_Line, macroIdentifierToken.m_Column);
					return;
				}
			}

			Token newToken = macroIdentifierToken;
			newToken.m_Lexeme = ParserString::CreateString(macroIdentifierToken.m_Lexeme);
			symbolTable.DefineSymbols.push(DefineSymbol{ symbolTree, newToken, tokenHash, lineDefined, INT_MAX });
		}

		void AddGlobalDefineSymbol(PPSymbolTable& symbolTable, const char* symbolName)
		{
			Token tmpToken{ -1, -1, TokenType::IDENTIFIER, ParserString::CreateString(symbolName) };
			unsigned long tokenHash = HashToken(tmpToken);
			for (const DefineSymbol& hash : symbolTable.DefineSymbols)
			{
				if (IsSameHashData(tmpToken, tokenHash, tmpToken.m_Line, hash))
				{
					Logger::Warning("Tried to redefine global macro '%s'", tmpToken.m_Lexeme);
					return;
				}
			}

			symbolTable.DefineSymbols.push(DefineSymbol{ nullptr, tmpToken, tokenHash, -1, INT_MAX });
		}

		const char* ExpandMacro(ParserData& data, const Token& token)
		{
			const PPSymbolTable& symbolTable = data.PreprocessingSymbolTable;
			unsigned long tokenHash = HashToken(token);
			for (const DefineSymbol& hash : symbolTable.DefineSymbols)
			{
				if (hash.hash == tokenHash && token.m_Line != hash.lineDefined && ParserString::Compare(token.m_Lexeme, hash.token.m_Lexeme))
				{
					if (token.m_Line > hash.lineUndefined)
					{
						Logger::Error("Macro '%s' was undefined at line %d. Cannot use macro at line %d.", token.m_Lexeme, hash.lineUndefined, token.m_Line);
						return {};
					}

					if (hash.symbolTree != nullptr)
					{
						if (hash.symbolTree->type == PreprocessingAstNodeType::MacroDefine)
						{
							return ExpandSimpleMacro(symbolTable, hash.symbolTree, token.m_Line);
						}
						else if (hash.symbolTree->type == PreprocessingAstNodeType::MacroDefineFunction)
						{
							return ExpandFunctionMacro(symbolTable, hash.symbolTree, token.m_Line, data);
						}
					}
				}
			}

			Logger::Warning("Unable to expand macro '%s'", token.m_Lexeme);
			return "";
		}

		bool IsDefined(const PPSymbolTable& symbolTable, const Token& token)
		{
			unsigned long tokenHash = HashToken(token);
			for (const DefineSymbol& hash : symbolTable.DefineSymbols)
			{
				if (hash.hash == tokenHash && ParserString::Compare(token.m_Lexeme, hash.token.m_Lexeme))
				{
					return token.m_Line > hash.lineDefined;
				}
			}

			return false;
		}

		bool IsSymbol(const PPSymbolTable& symbolTable, const Token& token)
		{
			unsigned long tokenHash = HashToken(token);
			for (const DefineSymbol& hash : symbolTable.DefineSymbols)
			{
				if (hash.hash == tokenHash && token.m_Line != hash.lineDefined && ParserString::Compare(token.m_Lexeme, hash.token.m_Lexeme))
				{
					return true;
				}
			}

			return false;
		}

		bool IsFunctionMacroDefine(const PPSymbolTable& symbolTable, const Token& token)
		{
			unsigned long tokenHash = HashToken(token);
			for (const DefineSymbol& hash : symbolTable.DefineSymbols)
			{
				if (hash.hash == tokenHash && token.m_Line != hash.lineDefined && ParserString::Compare(token.m_Lexeme, hash.token.m_Lexeme))
				{
					if (hash.symbolTree->type == PreprocessingAstNodeType::MacroDefine)
					{
						return false;
					}
					else if (hash.symbolTree->type == PreprocessingAstNodeType::MacroDefineFunction)
					{
						return true;
					}
				}
			}

			return false;
		}

		void FreeSymbols(PPSymbolTable& symbols)
		{
			for (int i = 0; i < symbols.DefineSymbols.size(); i++)
			{
				ParserString::FreeString(symbols.DefineSymbols[i].token.m_Lexeme);
			}
		}

		// ===========================================================================================
		// Private functions
		// ===========================================================================================

		static unsigned long HashToken(const Token& token)
		{
			unsigned long hash = 5381;
			int c;

			int strLength = ParserString::StringLength(token.m_Lexeme);
			for (int i = 0; i < strLength; i++)
				hash = ((hash << 5) + hash) + token.m_Lexeme[i]; /* hash * 33 + c */

			return hash;
		}

		static bool IsSameHashData(const Token& t1, unsigned long hash1, int lineDefined, const DefineSymbol& hash2)
		{
			if (hash2.hash == hash1 && ParserString::Compare(hash2.token.m_Lexeme, t1.m_Lexeme) && hash2.lineDefined == lineDefined)
			{
				return true;
			}

			return false;
		}

		static void MacroAddToReplacementList(PreprocessingAstNode* node, void* userData)
		{
			Logger::Assert(userData != nullptr, "Invalid replacement list user data while replacing macro.");

			List<Token>& replacementListResult = *(List<Token>*)userData;
			if (node->type == PreprocessingAstNodeType::PPTokens)
			{
				PreprocessingAstNode* ppToken = node->ppTokens.preprocessingToken;
				if (ppToken->type == PreprocessingAstNodeType::Identifier)
				{
					replacementListResult.push(ppToken->identifier.identifier);
				}
				else if (ppToken->type == PreprocessingAstNodeType::PreprocessingOpOrPunc)
				{
					replacementListResult.push(ppToken->preprocessingOpOrPunc.opOrPunc);
				}
				else if (ppToken->type == PreprocessingAstNodeType::HeaderName)
				{
					replacementListResult.push(ppToken->headerName.identifier);
				}
				else if (ppToken->type == PreprocessingAstNodeType::HeaderNameString)
				{
					replacementListResult.push(ppToken->headerNameString.stringLiteral);
				}
				else if (ppToken->type == PreprocessingAstNodeType::NumberLiteral)
				{
					replacementListResult.push(ppToken->numberLiteral.numberLiteral);
				}
				else if (ppToken->type == PreprocessingAstNodeType::CharacterLiteral)
				{
					replacementListResult.push(ppToken->characterLiteral.characterLiteral);
				}
				else if (ppToken->type == PreprocessingAstNodeType::StringLiteral)
				{
					replacementListResult.push(ppToken->stringLiteral.stringLiteral);
				}
				else if (ppToken->type == PreprocessingAstNodeType::PPTokens || ppToken->type == PreprocessingAstNodeType::None)
				{
					return;
				}
				else if (ppToken->type == PreprocessingAstNodeType::Newline)
				{
					replacementListResult.push(Token{ -1, -1, TokenType::NEWLINE, "\n" });
				}
				else if (ppToken->type == PreprocessingAstNodeType::Whitespace)
				{
					replacementListResult.push(ppToken->whitespace.token);
				}
				else
				{
					Logger::Warning("Unknown replacement list preprocessing token while expanding macro.");
				}
			}
		}

		static void CondenseWhitespace(StringBuilder& sb)
		{
			for (int i = 0; i < sb.Size(); i++)
			{
				if (sb.CharAt(i) == ' ' || sb.CharAt(i) == '\t')
				{
					i++;
					while (i < sb.Size() && (sb.CharAt(i) == ' ' || sb.CharAt(i) == '\t'))
					{
						sb.RemoveCharAt(i);
					}
					i--;
				}

				if (i < sb.Size() && sb.CharAt(i) == '\n')
				{
					sb.RemoveCharAt(i);
					i--;
				}
			}
		}

		static const char* ExpandSimpleMacro(const PPSymbolTable& symbolTable, PreprocessingAstNode* preprocessingNode, int newLine)
		{
			List<Token> replacementListResult;
			Parser::WalkPreprocessingTree(preprocessingNode->macroDefine.replacementList, (void*)&replacementListResult, MacroAddToReplacementList);

			StringBuilder sb;
			for (Token& token : replacementListResult)
			{
				token.m_Line = newLine;
				if (token.m_Type == TokenType::STRING_LITERAL)
				{
					sb.Append('"');
					sb.Append(token.m_Lexeme);
					sb.Append('"');
				}
				else if (token.m_Type == TokenType::NEWLINE)
				{
					sb.Append(' ');
				}
				else
				{
					sb.Append(token.m_Lexeme);
				}
			}

			CondenseWhitespace(sb);
			return sb.c_str_copy();
		}

		static void FunctionMacroAddToIdentifierList(PreprocessingAstNode* node, void* userData)
		{
			Logger::Assert(userData != nullptr, "Invalid replacement list user data while replacing macro.");

			List<Token>& replacementListResult = *(List<Token>*)userData;
			if (node->type == PreprocessingAstNodeType::Identifier)
			{
				replacementListResult.push(node->identifier.identifier);
			}
			else if (node->type == PreprocessingAstNodeType::IdentifierList)
			{
				return;
			}
			else if (node->type == PreprocessingAstNodeType::None)
			{
				return;
			}
			else
			{
				Logger::Warning("Ran into non-identifier in macro function identifier list.");
			}
		}

		static struct FunctionMacroDTO
		{
			List<Token>& functionIdentifiers;
			List<Token>& replacementListResult;
		};

		static const char* ExpandFunctionMacro(const PPSymbolTable& symbolTable,
			PreprocessingAstNode* preprocessingNode, int newLine, ParserData& data)
		{
			List<Token> functionIdentifiers;
			List<Token> replacementListResult;
			// Get function identifiers
			Parser::WalkPreprocessingTree(preprocessingNode->macroDefineFunction.identifierList, (void*)&functionIdentifiers, FunctionMacroAddToIdentifierList);

			// Replace function identifiers here
			Parser::WalkPreprocessingTree(preprocessingNode, (void*)&replacementListResult, MacroAddToReplacementList);
			bool endsInVariadicMacro = preprocessingNode->macroDefineFunction.endsInVariadicMacro;

			List<const char*> functionIdentifierReplacements;
			Token token = ScriptScanner::ScanToken(data.Scanner, true);
			Logger::Assert(token.m_Type == TokenType::LEFT_PAREN, "Macro function must begin with a left parenthesis.");

			int grouping = 1;
			// Get function identifier replacements
			while (!ScriptScanner::AtEnd(data.Scanner) && grouping > 0)
			{
				StringBuilder sb;
				while (!ScriptScanner::AtEnd(data.Scanner))
				{
					token = ScriptScanner::ScanToken(data.Scanner, true);
					if (token.m_Type == TokenType::LEFT_PAREN)
					{
						grouping++;
					}
					else if (token.m_Type == TokenType::RIGHT_PAREN)
					{
						grouping--;
						if (grouping == 0)
						{
							break;
						}
					}
					else if (token.m_Type == TokenType::COMMA && grouping == 1)
					{
						break;
					}

					if (token.m_Type == TokenType::STRING_LITERAL)
					{
						sb.Append('"');
						sb.Append(token.m_Lexeme);
						sb.Append('"');
					}
					else if (token.m_Type == TokenType::NEWLINE)
					{
						sb.Append(' ');
					}
					else
					{
						sb.Append(token.m_Lexeme);
					}
				}
				sb.StripWhitespace();
				functionIdentifierReplacements.push(sb.c_str_copy());
			}

			Logger::Assert(functionIdentifierReplacements.size() == functionIdentifiers.size(),
				"Invalid number of arguments for function macro. You gave '%d' arguments, was expecting '%d' arguments.",
				functionIdentifierReplacements.size(), functionIdentifiers.size());

			// Create the final string by replacing all function identifiers with the appropriate replacement
			StringBuilder sb;
			for (int tokenIndex = 0; tokenIndex < replacementListResult.size(); tokenIndex++)
			{
				Token token = replacementListResult[tokenIndex];
				int functionIdentifierSlot = -1;
				int index = 0;
				for (const Token& functionId : functionIdentifiers)
				{
					if (token.m_Type == TokenType::IDENTIFIER && ParserString::Compare(token.m_Lexeme, functionId.m_Lexeme))
					{
						functionIdentifierSlot = index;
						break;
					}
					index++;
				}

				if (functionIdentifierSlot != -1)
				{
					Logger::AssertCritical(functionIdentifierSlot < functionIdentifierReplacements.size(), "Invalid function id slot. We should never hit this exception...");
					sb.Append(functionIdentifierReplacements[functionIdentifierSlot]);
				}
				else if (ParserString::Compare(token.m_Lexeme, "__VA_ARGS__"))
				{
					Logger::AssertCritical(endsInVariadicMacro, "Cannot expand variadic argument in macro that does not contain an elipsis '...' in macro '%s'", preprocessingNode->macroDefineFunction.identifier.m_Lexeme);
					int index = 0;
					for (const char* funcReplacement : functionIdentifierReplacements)
					{
						sb.Append(funcReplacement);
						if (index != functionIdentifierReplacements.size() - 1)
						{
							sb.Append(',');
							sb.Append(' ');
						}
						index++;
					}
				}
				else
				{
					if (token.m_Type == TokenType::HASHTAG)
					{
						tokenIndex++;
						Token nextToken = replacementListResult[tokenIndex];
						if (nextToken.m_Type != TokenType::HASHTAG)
						{
							sb.Append('"');

							functionIdentifierSlot = -1;
							index = 0;
							for (const Token& functionId : functionIdentifiers)
							{
								if (nextToken.m_Type == TokenType::IDENTIFIER && ParserString::Compare(nextToken.m_Lexeme, functionId.m_Lexeme))
								{
									functionIdentifierSlot = index;
									break;
								}
								index++;
							}
							if (functionIdentifierSlot != -1)
							{
								Logger::AssertCritical(functionIdentifierSlot < functionIdentifierReplacements.size(), "Invalid function id slot. We should never hit this exception...");
								sb.Append(functionIdentifierReplacements[functionIdentifierSlot]);
							}
							else
							{
								sb.Append(nextToken.m_Lexeme);
							}

							sb.Append('"');
						}
					}
					else
					{
						if (token.m_Type == TokenType::STRING_LITERAL)
						{
							sb.Append('"');
							sb.Append(token.m_Lexeme);
							sb.Append('"');
						}
						else if (token.m_Type == TokenType::NEWLINE)
						{
							sb.Append(' ');
						}
						else
						{
							sb.Append(token.m_Lexeme);
						}
					}
				}
			}

			for (const char* str : functionIdentifierReplacements)
			{
				ParserString::FreeString(str);
			}

			CondenseWhitespace(sb);
			return sb.c_str_copy();
		}
	}
}