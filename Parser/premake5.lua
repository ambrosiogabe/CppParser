project "CppParser"
    kind "ConsoleApp"
    language "C++"
    cppdialect "C++17"
    staticruntime "off"
    
	targetdir ("bin/" .. outputdir .. "/%{prj.name}")
    objdir ("bin-int/" .. outputdir .. "/%{prj.name}")

    outputPath = "bin/" .. outputdir .. "/%{prj.name}/"
    debugdir(outputPath)

	files {
        "cpp/**.cpp",
        "include/**.h"
    }

    includedirs {
        "include"
    }
    
    filter { "system:Unix", "system:Mac" }
        systemversion "latest"

        postbuildcommands {
            "cp ./test/testParser.cpp ./bin" ..outputdir .. "/%{prj.name}/testParser.cpp"
        }

	filter "system:Windows"
        systemversion "latest"

        postbuildcommands {
            "copy .\\test\\testParser.cpp .\\bin\\" .. outputdir .. "\\%{prj.name}\\testParser.cpp"
        }
        
    filter { "system:windows", "configurations:Debug" }
        buildoptions "/MTd"        

    filter { "system:windows", "configurations:Release" }
        buildoptions "/MT"
