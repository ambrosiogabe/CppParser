#include <vector.h>
#include <string.h>
#include <stdio.h>

#define COMPLEX(x) do \
{ \
	printf("Something"); \
} while (false)

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
		float m_Member;
		Vec3 m_Vector;

		inline void Update(float dt)
		{
			float verySmallUpdate = dt;
			m_Member += dt * m_Vector.x;
		}

		int DoSomething();
	}

	class ACoolClass
	{
	public:
		ACoolClass()
		{
			m_Vector = new Vec3();
			m_Vector->x = 10;
			m_Vector->y = 12;
			m_Vector->z = 33;
		}

		~ACoolClass()
		{
			delete m_Vector;
		}

		[[deprecated]]
		void MemberDecl()
		{
			printf("This is a deprecated function %d\n", 10);
		}

		template<typename T>
		T TemplateAdd(const T& a, const T& b)
		{
			return a + b;
		}

		void* AllocateMemory(size_t size);

	private:
		float m_Member;
		const int* m_Member2;
		Vec3* m_Vector;
	};

	namespace FizzBuzz
	{
		void DoFizzBuzz(int maxNumber)
		{
			for (int i = 0; i < maxNumber; i++)
			{
				if (i % 3 == 0)
				{
					printf("Fizz");
				}
				if (i % 5 == 0)
				{
					printf("Buzz");
				}
				printf("\n");
			}
		}

		float Average(std::vector<int> numbers)
		{
			float average = 0.0f;
			for (int num : numbers)
			{
				average += (float)num;
			}

			return average / (int)numbers.size();
		}
	}
}