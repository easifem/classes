#!/usr/bin/env python3

# This program is a part of EASIFEM library.
# See. www.easifem.com
# Copyright (c) 2020-2021, All right reserved, Vikas Sharma, Ph.D.
#

import os

# import sys
import platform

print("Detecting OS type...")
_os = platform.system()
if _os == "Windows":
    print("ERROR: INSTALLATION on windows is work in progress")
    exit
    # print("Please use Windows Subsystem Linux(WSL) ")
    # print("Installation DONE!!")
else:
    cmake_def = ""
    cmake_def += '-G "Ninja"'
    cmake_def += " -D USE_GMSH_SDK:BOOL=ON"
    cmake_def += " -D CMAKE_BUILD_TYPE=Debug"
    cmake_def += " -D BUILD_SHARED_LIBS:BOOL=ON"
    cmake_def += " -D USE_LIS:BOOL=ON"
    cmake_def += " -D CMAKE_INSTALL_PREFIX:PATH=${EASIFEM_CLASSES}"
    print("CMAKE DEF : ", cmake_def)
    _build0 = os.path.join(os.environ["HOME"], "temp")
    build_dir = os.path.join(
        os.environ.get("EASIFEM_BUILD_DIR",
                       _build0), "easifem", "classes", "build"
    )
    # build_dir = os.environ["HOME"] + "/temp/easifem-base/build"
    os.makedirs(build_dir, exist_ok=True)
    os.system(f"cmake -S ./ -B {build_dir} {cmake_def}")
    os.system(f"cmake --build {build_dir}")
    print("Build DONE!!")
