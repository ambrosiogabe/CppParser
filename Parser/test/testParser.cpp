#ifdef TEST_PARSER_CPP
#define TEST_PARSER_CPP

#define min(X, Y) ((X) < (Y) ? (X) : (Y))
int a = 0;
int b = 0;
int x = min(a, b);

#define macro(something, somethingElse) \
	something;                          \
	somethingElse;
macro(array[x = y, x + 1]);

class TestParser
{
public:
	TestParser();
	~TestParser();

	void foo();
	void fooBar();

private:
	void internalFoo();

private:
	float m_Member;
}

#elif (5 + 1) * 2 == 12

class CompletelyDifferentParser
{
public:
	CompletelyDifferentParser();
	~CompletelyDifferentParser();
	void WoahThere();

#ifdef _WIN32
private:
	float m_DumbMember;
#endif
};

#endif