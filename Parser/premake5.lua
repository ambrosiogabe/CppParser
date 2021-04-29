project "CppParser"
    kind "ConsoleApp"
    language "C++"
    cppdialect "C++17"
    staticruntime "off"
    
	targetdir ("bin/" .. outputdir .. "/%{prj.name}")
    objdir ("bin-int/" .. outputdir .. "/%{prj.name}")

	files {
        "cpp/**.cpp",
        "include/**.h"
    }

    includedirs {
        "include"
    }
    
	filter "system:windows"
        systemversion "latest"
        
    filter { "system:windows", "configurations:Debug" }
        buildoptions "/MDd"        

    filter { "system:windows", "configurations:Release" }
        buildoptions "/MD"
