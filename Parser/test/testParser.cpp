#include <vector>
#include <string>

namespace Cocoa
{
	struct SomeStructure
	{
		float aFloat;
		int anInt;
		const char* aString;
		const std::vector<int>& complicatedVectorRef;
		const std::string& stringRef;
	};

	struct Vec3
	{
		float x;
		float y;
		float z;
	};

	namespace MyNamespace
	{
		void Update(float dt);
		int DoSomething();
	}

	class ACoolClass
	{
	public:
		ACoolClass();

		void MemberDecl();

	private:
		float m_Member;
		const int* m_Member2;
	};
}