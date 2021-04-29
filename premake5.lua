workspace "CppParser"
    architecture "x64"
    
    configurations { 
        "Debug", 
        "Release"
    }

    startproject "CppParser"

-- This is a helper variable, to concatenate the sys-arch
outputdir = "%{cfg.buildcfg}-%{cfg.system}-%{cfg.architecture}"

include "Parser"
