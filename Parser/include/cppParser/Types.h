#pragma once
#ifndef AllocMem
#include <memory>
#define AllocMem(bytes) malloc(bytes)
#endif

#ifndef FreeMem
#include <memory>
#define FreeMem(ptr) free(ptr)
#endif