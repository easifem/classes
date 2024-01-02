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
#

if(${PROJECT_NAME} MATCHES "easifemBase")
  option(USE_LIS OFF)
  if(USE_LIS)
    list(APPEND TARGET_COMPILE_DEF "-DUSE_LIS")
    if(UNIX)
      if(APPLE)
        set(LIS_LIBRARIES "$ENV{EASIFEM_EXTPKGS}/lib/liblis.dylib")
      else()
        set(LIS_LIBRARIES "$ENV{EASIFEM_EXTPKGS}/lib/liblis.so")
      endif()
    endif()
    target_link_libraries(${PROJECT_NAME} PUBLIC ${LIS_LIBRARIES})
    message(STATUS "LIS_LIBRARIES : ${LIS_LIBRARIES}")
  else()
    message(STATUS "NOT USING LIS LIBRARIES")
  endif()
endif()
