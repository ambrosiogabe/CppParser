#pragma once

#ifndef CPP_PARSER_PATH
#include <filesystem>
#define CPP_PARSER_PATH std::filesystem::path
#endif

#ifndef CPP_PARSER_CREATE_PATH
#define CPP_PARSER_CREATE_PATH(path) std::filesystem::path(path);
#endif

#ifndef CPP_PARSER_GET_PATH_CSTR
#define CPP_PARSER_GET_PATH_CSTR(path) path.string().c_str()
#endif

#ifndef CPP_PARSER_VECTOR
#include <vector>
#define CPP_PARSER_VECTOR(type) std::vector<type>
#endif

#ifndef CPP_PARSER_VECTOR_BEGIN
#define CPP_PARSER_VECTOR_BEGIN(vector) vector.begin()
#endif

#ifndef CPP_PARSER_VECTOR_END
#define CPP_PARSER_VECTOR_END(vector) vector.end()
#endif

#ifndef CPP_PARSER_LOG
#include <stdio.h>
#define CPP_PARSER_LOG(format, ...) do { printf(format, __VA_ARGS__); printf("\n"); } while(false)
#endif

#ifndef CPP_PARSER_LOG_INFO
#define CPP_PARSER_LOG_INFO(format, ...) do { printf(format, __VA_ARGS__); printf("\n"); } while (false)
#endif

#ifndef CPP_PARSER_LOG_WARNING
#define CPP_PARSER_LOG_WARNING(format, ...) do { printf(format, __VA_ARGS__); printf("\n"); } while (false)
#endif

#ifndef CPP_PARSER_LOG_ERROR
#define CPP_PARSER_LOG_ERROR(format, ...) do { printf(format, __VA_ARGS__); printf("\n"); } while (false)
#endif

#ifndef CPP_PARSER_LOG_ASSERT
#include <assert.h>
#define CPP_PARSER_LOG_ASSERT(condition, format, ...) if(!(condition)) printf(format, __VA_ARGS__)
#endif

#ifndef CPP_PARSER_FILE_INCLUDE_HEADER
#define CPP_PARSER_FILE_INCLUDE_HEADER "cppParser/FileIO.h"
#endif

#ifndef CPP_PARSER_READ_FILE
#include CPP_PARSER_FILE_INCLUDE_HEADER
#define CPP_PARSER_READ_FILE(filepathCStr) FileIO::DefaultReadFile(filepathCStr)
#endif

#ifndef CPP_PARSER_FREE_FILE
#define CPP_PARSER_FREE_FILE(filepathCStr) FileIO::DefaultFreeFile(filepathCStr)
#endif

#ifndef AllocMem
#define AllocMem(bytes) malloc(bytes)
#endif

#ifndef FreeMem
#define FreeMem(ptr) free(ptr)
#endif