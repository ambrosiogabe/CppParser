#ifndef TEST_PARSER_CPP
#define TEST_PARSER_CPP

//#include <vector>
#define MACRO_MAGIC           \
	printf("Macro magic!\n"); \
	printf("Really!");

#define FUN_MACRO_MAGIC(a, b, c) \
	printf("%d, %d, %d", a, b, c)

#define SUPER_SPECIAL_MACRO(a, b) \
	MACRO_MAGIC                   \
	printf("%d, %d", a, b)

#define SUPER_SPECIAL_MACRO2 \
	MACRO_MAGIC              \
	printf("Plus some extra!\n")

#define M(a, b) printf("%d", a##b);
#define M2(a, b) #a##b
#define VARIADIC(...) __VA_ARGS__

namespace Parser
{
	class Test
	{
	public:
		Test()
		{
			printf("%d %d %d %d", VARIADIC(1, 2, 3, 4));

			MACRO_MAGIC
			FUN_MACRO_MAGIC(1, 2, 3);
			FUN_MACRO_MAGIC([ x = y, b = 2 ], abc);

			SUPER_SPECIAL_MACRO("hello", "there");
			SUPER_SPECIAL_MACRO2;

			M(abc, def)

			M2(blahde, dah)
		}

		~Test();

		void Func();

	private:
		float m_Member;
		std::vector m_Vec;
	};
}

#else

int main()
{
	printf("HELLO WOOOOOOOOOOOOOORLD\n");
	return 0;
}

#endif