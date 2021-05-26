#include "cppParser/ParserString.h"
#include "CppUtils/CppUtils.h"

#include <cstring>

namespace CppParser
{
	namespace ParserString
	{
		using namespace CppUtils;
		static const char* DefaultEmptyString = "";

		const char* CreateString(const char* strToCopy)
		{
			return Copy(strToCopy);
		}

		const char* Copy(const char* strToCopy)
		{
			if (!Compare(strToCopy, DefaultEmptyString))
			{
				size_t size = StringLength(strToCopy);
				size_t sizeWithNullByte = size + (unsigned long long)1;
				char* newStr = (char*)AllocMem(sizeWithNullByte);

				if (newStr)
				{
					memcpy(newStr, strToCopy, size);
					newStr[size] = '\0';
				}
				else
				{
					Logger::Error("Failed to allocate memory for string.");
					return DefaultEmptyString;
				}
				return newStr;
			}

			return DefaultEmptyString;
		}

		const char* Copy(const char* strToCopy, int numCharactersToCopy)
		{
			if (Compare(strToCopy, DefaultEmptyString) != 0)
			{
				size_t length = StringLength(strToCopy);
				if (numCharactersToCopy <= length)
				{
					size_t sizeWithNullByte = numCharactersToCopy + (unsigned long long)1;
					char* newStr = (char*)AllocMem(sizeWithNullByte);

					if (newStr)
					{
						memcpy(newStr, strToCopy, numCharactersToCopy);
						newStr[numCharactersToCopy] = '\0';
					}
					else
					{
						Logger::Error("Failed to allocate memory for string.");
						return DefaultEmptyString;
					}
					return newStr;
				}
			}

			return DefaultEmptyString;
		}

		const char* Substring(const char* strToCopyFrom, int startIndex, int size)
		{
			if (strcmp(strToCopyFrom, DefaultEmptyString))
			{
				size_t strToCopyFromSize = StringLength(strToCopyFrom);
				size_t sizeWithNullByte = size + (unsigned long long)1;
				char* newStr = (char*)AllocMem(sizeWithNullByte);

				if (newStr)
				{
					if (startIndex >= 0 && startIndex + size <= strToCopyFromSize)
					{
						memcpy(newStr, &(strToCopyFrom[startIndex]), sizeof(char) * size);
						newStr[size] = '\0';
					}
					else
					{
						Logger::Error("Invalid range for substring.");
						return DefaultEmptyString;
					}
				}
				else
				{
					Logger::Error("Failed to allocate memory for string.");
					return DefaultEmptyString;
				}
				return newStr;
			}

			return DefaultEmptyString;
		}

		int StringLength(const char* str)
		{
			return (int)strlen(str);
		}

		bool Compare(const char* str1, const char* str2)
		{
			return strcmp(str1, str2) == 0;
		}

		const char* Join(const char* str1, const char* str2)
		{
			size_t strLength1 = strlen(str1);
			size_t strLength2 = strlen(str2);
			size_t newStrLength = strLength1 + strLength2;
			char* newStr = (char*)AllocMem(sizeof(char) * (newStrLength + 1));
			if (newStr)
			{
				memcpy(newStr, str1, sizeof(char) * strLength1);
				memcpy(newStr + strLength1, str2, sizeof(char) * strLength2);
				newStr[newStrLength] = '\0';
				return newStr;
			}

			Logger::Error("Failed to allocate memory for string.");
			return nullptr;
		}

		void FreeString(const char* str)
		{
			if (str && str != DefaultEmptyString)
			{
				FreeMem((void*)str);
			}
		}
	}

	StringBuilder::StringBuilder()
	{
		contents = List<char>(10);
	}

	void StringBuilder::Append(const char* strToAppend)
	{
		const char* c = strToAppend;
		while (*c != '\0')
		{
			contents.push(*c);
			c++;
		}
	}

	void StringBuilder::Append(char character)
	{
		contents.push(character);
	}

	const char* StringBuilder::c_str()
	{
		// This will put a '\0' nullbyte at the end of the string
		// and then remove it from the array, but the nullbyte will
		// still be there until another element is pushed to the stack
		contents.push('\0');
		contents.pop();
		return contents.begin();
	}

	const char* StringBuilder::c_str_copy()
	{
		const char* copy = ParserString::CreateString(c_str());
		return copy;
	}
}
