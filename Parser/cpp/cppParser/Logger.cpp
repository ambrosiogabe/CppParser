#include "cppParser/Logger.h"

#include <stdio.h>
#include <stdarg.h>

namespace CppParser
{
	namespace Logger
	{
		void Log(const char* format, ...)
		{
			printf("Log: ");

			va_list args;
			va_start(args, format);
			vprintf(format, args);
			va_end(args);

			printf("\n");
		}

		void Info(const char* format, ...)
		{
			printf("Info: ");

			va_list args;
			va_start(args, format);
			vprintf(format, args);
			va_end(args);

			printf("\n");
		}

		void Warning(const char* format, ...)
		{
			printf("Warning: ");

			va_list args;
			va_start(args, format);
			vprintf(format, args);
			va_end(args);

			printf("\n");
		}

		void Error(const char* format, ...)
		{
			printf("Error: ");

			va_list args;
			va_start(args, format);
			vprintf(format, args);
			va_end(args);

			printf("\n");
		}

		void Assert(int condition, const char* format, ...)
		{
			if (!condition)
			{
				printf("Assertion Failure: ");

				va_list args;
				va_start(args, format);
				vprintf(format, args);
				va_end(args);

				printf("\n");
			}
		}
	}
}