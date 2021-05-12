#pragma once

#ifdef __linux__
static const bool LINUX_DEFINED = true;
#else 
static const bool LINUX_DEFINED = false;
#endif 

#ifdef _WIN32
static const bool WIN32_DEFINED = true;
#else 
static const bool WIN32_DEFINED = false;
#endif

#ifdef _WIN64
static const bool WIN64_DEFINED = true;
#else 
static const bool WIN64_DEFINED = false;
#endif

#if defined(unix) || defined(__unix) || defined(__unix__)
static const bool UNIX_DEFINED = true;
#else 
static const bool UNIX_DEFINED = false;
#endif

#if defined(__APPLE__) || defined(__MACH__)
static const bool APPLE_DEFINED = true;
#else 
static const bool APPLE_DEFINED = false;
#endif

#ifdef __FreeBSD__
static const bool FREE_BSD_DEFINED = true;
#else 
static const bool FREE_BSD_DEFINED = false;
#endif

#ifdef __ANDROID__
static const bool ANDROID_DEFINED = true;
#else 
static const bool ANDROID_DEFINED = false;
#endif

namespace CppParser
{
	enum class PredefinedMacros
	{
		None = 0,
		Linux = 1,
		Win32 = 2,
		Win64 = 4,
		Unix = 8,
		Apple = 16,
		FreeBSD = 32,
		Android = 64
	};
}