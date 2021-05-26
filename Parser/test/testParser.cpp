#ifdef TEST_PARSER_CPP
#define TEST_PARSER_CPP

// #include <vector>

namespace Parser
{
	class Test
	{
	public:
		Test();
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