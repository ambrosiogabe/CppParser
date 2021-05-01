#pragma once

namespace CppParser
{
	namespace Logger
	{
		void Log(const char* format, ...);
		void Info(const char* format, ...);
		void Warning(const char* format, ...);
		void Error(const char* format, ...);
		void Assert(int condition, const char* format, ...);
	}
}