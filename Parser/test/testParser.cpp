#ifndef TEST_PARSER_CPP
#define TEST_PARSER_CPP

// #include <vector>
#define MACRO_MAGIC           \
	printf("Macro magic!\n"); \
	printf("Really!");

#define FUN_MACRO_MAGIC(a, b, c) \
	printf("%d, %d, %d", a, b, c)

namespace Parser
{
	class Test
	{
	public:
		Test()
		{
			MACRO_MAGIC
			FUN_MACRO_MAGIC(1, 2, 3);
			FUN_MACRO_MAGIC([ x = y, b = 2 ], abc);
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