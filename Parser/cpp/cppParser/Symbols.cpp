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
		static List<Token> ExpandSimpleMacro(const PPSymbolTable& symbolTable, PreprocessingAstNode* preprocessingNode, int newLine);
		static void FunctionMacroAddToIdentifierList(PreprocessingAstNode* node, void* userData);
		static List<Token> ExpandFunctionMacro(const PPSymbolTable& symbolTable,
			PreprocessingAstNode* preprocessingNode, int newLine, const List<Token>& tokens, int currentToken);

		// ===========================================================================================
		// Public functions
		// ===========================================================================================

		void AddUndefine(PPSymbolTable& symbolTable, const Token& token, int lineUndefined)
		{
			unsigned long tokenHash = HashToken(token);
			for (DefineSymbol& hash : symbolTable.DefineSymbols)
			{
				if (hash.hash == tokenHash && token.m_Line != hash.lineDefined && ParserString::Compare(token.m_Lexeme, hash.token.m_Lexeme))
				{
					hash.lineUndefined = lineUndefined;
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

			symbolTable.DefineSymbols.push(DefineSymbol{ symbolTree, macroIdentifierToken, tokenHash, lineDefined, INT_MAX });
		}

		List<Token> ExpandMacro(const PPSymbolTable& symbolTable, int currentToken, const List<Token>& tokens)
		{
			const Token& token = tokens.get(currentToken);
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

					if (hash.symbolTree->type == PreprocessingAstNodeType::MacroDefine)
					{
						return ExpandSimpleMacro(symbolTable, hash.symbolTree, token.m_Line);
					}
					else if (hash.symbolTree->type == PreprocessingAstNodeType::MacroDefineFunction)
					{
						return ExpandFunctionMacro(symbolTable, hash.symbolTree, token.m_Line, tokens, currentToken);
					}
				}
			}

			Logger::Warning("Unable to expand macro '%s'", token.m_Lexeme);
			return {};
		}

		bool IsDefined(const PPSymbolTable& symbolTable, const Token& token)
		{
			unsigned long tokenHash = HashToken(token);
			for (const DefineSymbol& hash : symbolTable.DefineSymbols)
			{
				if (hash.hash == tokenHash)
				{
					return true;
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
				else
				{
					Logger::Warning("Unknown replacement list preprocessing token while expanding macro.");
				}
			}
		}

		static List<Token> ExpandSimpleMacro(const PPSymbolTable& symbolTable, PreprocessingAstNode* preprocessingNode, int newLine)
		{
			List<Token> replacementListResult;
			Parser::WalkPreprocessingTree(preprocessingNode->macroDefine.replacementList, (void*)&replacementListResult, MacroAddToReplacementList);
			for (Token& token : replacementListResult)
			{
				token.m_Line = newLine;
			}
			return replacementListResult;
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

		static List<Token> ExpandFunctionMacro(const PPSymbolTable& symbolTable,
			PreprocessingAstNode* preprocessingNode, int newLine, const List<Token>& tokens, int currentToken)
		{
			List<Token> functionIdentifiers;
			List<Token> replacementListResult;
			// Get function identifiers
			Parser::WalkPreprocessingTree(preprocessingNode->macroDefineFunction.identifierList, (void*)&functionIdentifiers, FunctionMacroAddToIdentifierList);

			// Replace function identifiers here
			Parser::WalkPreprocessingTree(preprocessingNode, (void*)&replacementListResult, MacroAddToReplacementList);

			currentToken++;
			Logger::Assert(currentToken < tokens.size() && tokens[currentToken].m_Type == TokenType::LEFT_PAREN, "Macro function definition must begin with a left parenthesis");
			currentToken++;
			int replacementListTokenStart = currentToken;

			for (int i = 0; i < replacementListResult.size(); i++)
			{
				Token& token = replacementListResult[i];
				int parameterIndex = 0;
				currentToken = replacementListTokenStart;
				for (Token& identifier : functionIdentifiers)
				{
					// If we have the same lexeme here
					if (ParserString::Compare(identifier.m_Lexeme, token.m_Lexeme))
					{
						List<Token> replacementForIdentifier;
						int argumentIndex = 0;
						bool atParameter = parameterIndex == argumentIndex;
						// Replace the lexeme with the actual tokens that would have been supplied
						int grouping = 0;
						while (currentToken < tokens.size())
						{
							const Token& replacement = tokens[currentToken];
							if (replacement.m_Type == TokenType::LEFT_PAREN)
							{
								grouping++;
							}
							else if (replacement.m_Type == TokenType::RIGHT_PAREN)
							{
								grouping--;
								if (grouping < 0)
								{
									break;
								}
							}
							else if (replacement.m_Type == TokenType::COMMA && grouping <= 0)
							{
								if (atParameter)
								{
									break;
								}
								argumentIndex++;
								if (argumentIndex > parameterIndex)
								{
									break;
								}
							}

							if (atParameter)
							{
								replacementForIdentifier.push(replacement);
							}
							currentToken++;
							atParameter = parameterIndex == argumentIndex;
						}

						replacementListResult.removeByIndex(i);
						replacementListResult.insert(replacementForIdentifier.begin(), replacementForIdentifier.end(), i);
						break;
					}
					parameterIndex++;
				}
				token.m_Line = newLine;
			}
			return replacementListResult;
		}
	}
}