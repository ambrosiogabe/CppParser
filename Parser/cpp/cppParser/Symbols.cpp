#include "cppParser/Symbols.h"
#include "cppParser/ParserString.h"
#include "cppParser/Logger.h"
#include "cppParser/ScriptParser.h"

namespace CppParser
{
	namespace Symbols
	{
		// Internal variables
		static unsigned long HashToken(const Token& token);
		static bool IsSameHashData(const Token& t1, unsigned long hash1, int lineDefined, const TokenHashInfo& hash2);

		void AddUndefine(PPSymbolTable& symbolTable, const Token& token, int lineUndefined)
		{
			unsigned long tokenHash = HashToken(token);
			for (TokenHashInfo& hash : symbolTable.SimpleDefines)
			{
				if (hash.hash == tokenHash && token.m_Line != hash.lineDefined && ParserString::Compare(token.m_Lexeme, hash.token.m_Lexeme))
				{
					hash.lineUndefined = lineUndefined;
					return;
				}
			}

			for (TokenHashInfo& hash : symbolTable.FunctionDefines)
			{
				if (hash.hash == tokenHash)
				{
					hash.lineUndefined = lineUndefined;
					return;
				}
			}

			Logger::Error("Cannot undefine macro '%s'. Macro has not been defined.", token.m_Lexeme);
		}

		void AddSimpleDefine(PPSymbolTable& symbolTable, const Token& token, int lineDefined, PreprocessingAstNode* replacementList)
		{
			unsigned long tokenHash = HashToken(token);
			for (const TokenHashInfo& hash : symbolTable.SimpleDefines)
			{
				if (IsSameHashData(token, tokenHash, lineDefined, hash))
				{
					Logger::Warning("Tried to redefine macro '%s' at line %d:%d", token.m_Lexeme, token.m_Line, token.m_Column);
					return;
				}
			}

			symbolTable.SimpleDefines.push_back(TokenHashInfo{ replacementList, token, tokenHash, lineDefined, INT_MAX });
		}

		void AddFunctionDefine(PPSymbolTable& symbolTable, const Token& token, int lineDefined, PreprocessingAstNode* replacementList)
		{
			unsigned long tokenHash = HashToken(token);
			for (const TokenHashInfo& hash : symbolTable.SimpleDefines)
			{
				if (IsSameHashData(token, tokenHash, lineDefined, hash))
				{
					Logger::Warning("Tried to redefine macro '%s' at line %d:%d", token.m_Lexeme, token.m_Line, token.m_Column);
					return;
				}
			}

			symbolTable.FunctionDefines.push_back(TokenHashInfo{ replacementList, token, tokenHash, lineDefined, INT_MAX });
		}

		std::vector<Token> ExpandMacroFunction(const PPSymbolTable& symbolTable, PreprocessingAstNode* preprocessingNode)
		{
			return {};
		}

		static std::vector<Token> replacementListResult;
		static void AddToReplacementList(PreprocessingAstNode* node)
		{
			if (node->type == PreprocessingAstNodeType::PPTokens)
			{
				PreprocessingAstNode* ppToken = node->ppTokens.preprocessingToken;
				if (ppToken->type == PreprocessingAstNodeType::Identifier)
				{
					replacementListResult.insert(replacementListResult.begin(), ppToken->identifier.identifier);
				}
				else if (ppToken->type == PreprocessingAstNodeType::PreprocessingOpOrPunc)
				{
					replacementListResult.insert(replacementListResult.begin(), ppToken->preprocessingOpOrPunc.opOrPunc);
				}
				else if (ppToken->type == PreprocessingAstNodeType::HeaderName)
				{
					replacementListResult.insert(replacementListResult.begin(), ppToken->headerName.identifier);
				}
				else if (ppToken->type == PreprocessingAstNodeType::HeaderNameString)
				{
					replacementListResult.insert(replacementListResult.begin(), ppToken->headerNameString.stringLiteral);
				}
				else if (ppToken->type == PreprocessingAstNodeType::NumberLiteral)
				{
					replacementListResult.insert(replacementListResult.begin(), ppToken->numberLiteral.numberLiteral);
				}
				else if (ppToken->type == PreprocessingAstNodeType::CharacterLiteral)
				{
					replacementListResult.insert(replacementListResult.begin(), ppToken->characterLiteral.characterLiteral);
				}
				else if (ppToken->type == PreprocessingAstNodeType::StringLiteral)
				{
					replacementListResult.insert(replacementListResult.begin(), ppToken->stringLiteral.stringLiteral);
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

		std::vector<Token> ExpandSimpleMacro(const PPSymbolTable& symbolTable, PreprocessingAstNode* preprocessingNode, int newLine)
		{
			replacementListResult.clear();
			Parser::WalkPreprocessingTree(preprocessingNode, AddToReplacementList);
			for (Token& token : replacementListResult)
			{
				token.m_Line = newLine;
			}
			return replacementListResult;
		}

		std::vector<Token> ExpandMacro(const PPSymbolTable& symbolTable, const Token& token)
		{
			unsigned long tokenHash = HashToken(token);
			for (const TokenHashInfo& hash : symbolTable.SimpleDefines)
			{
				if (hash.hash == tokenHash && token.m_Line != hash.lineDefined && ParserString::Compare(token.m_Lexeme, hash.token.m_Lexeme))
				{
					if (token.m_Line > hash.lineUndefined)
					{
						Logger::Error("Macro '%s' was undefined at line %d. Cannot use macro at line %d.", token.m_Lexeme, hash.lineUndefined, token.m_Line);
						return {};
					}
					return ExpandSimpleMacro(symbolTable, hash.replacementList, token.m_Line);
				}
			}

			for (const TokenHashInfo& hash : symbolTable.FunctionDefines)
			{
				if (hash.hash == tokenHash)
				{
					if (token.m_Line > hash.lineUndefined)
					{
						Logger::Error("Macro '%s' was undefined at line %d. Cannot use macro at line %d.", token.m_Lexeme, hash.lineUndefined, token.m_Line);
						return {};
					}
					return ExpandMacroFunction(symbolTable, hash.replacementList);
				}
			}

			Logger::Warning("Unable to expand macro '%s'", token.m_Lexeme);
			return {};
		}

		bool IsDefined(const PPSymbolTable& symbolTable, const Token& token)
		{
			unsigned long tokenHash = HashToken(token);
			for (const TokenHashInfo& hash : symbolTable.SimpleDefines)
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
			for (const TokenHashInfo& hash : symbolTable.SimpleDefines)
			{
				if (hash.hash == tokenHash && token.m_Line != hash.lineDefined && ParserString::Compare(token.m_Lexeme, hash.token.m_Lexeme))
				{
					return true;
				}
			}

			for (const TokenHashInfo& hash : symbolTable.FunctionDefines)
			{
				if (hash.hash == tokenHash)
				{
					return true;
				}
			}

			return false;
		}

		static unsigned long HashToken(const Token& token)
		{
			unsigned long hash = 5381;
			int c;

			int strLength = ParserString::StringLength(token.m_Lexeme);
			for (int i = 0; i < strLength; i++)
				hash = ((hash << 5) + hash) + token.m_Lexeme[i]; /* hash * 33 + c */

			return hash;
		}

		static bool IsSameHashData(const Token& t1, unsigned long hash1, int lineDefined, const TokenHashInfo& hash2)
		{
			if (hash2.hash == hash1 && ParserString::Compare(hash2.token.m_Lexeme, t1.m_Lexeme) && hash2.lineDefined == lineDefined)
			{
				return true;
			}

			return false;
		}
	}
}