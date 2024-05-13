# This program is a part of EASIFEM library Copyright (C) 2020-2021  Vikas
# Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https: //www.gnu.org/licenses/>

option(USE_GMSH_SDK OFF)
if(USE_GMSH_SDK)
  message(STATUS "USING GMSH SDK")
  list(APPEND TARGET_COMPILE_DEF "-DUSE_GMSH_SDK")
  find_package(Gmsh REQUIRED)
  if(GMSH_FOUND)
    message(STATUS "FOUND Gmsh")
    message(STATUS "GMSH_LIBRARIES : ${GMSH_LIB}")
    target_link_libraries(${PROJECT_NAME} PUBLIC ${GMSH_LIB})
  else()
    message(STATUS "NOT FOUND Gmsh")
  endif()
else()
  message(STATUS "NOT USING GMSH SDK")
endif()
