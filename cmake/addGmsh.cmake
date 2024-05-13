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

  # download the python virtual env

  message(STATUS "USING GMSH SDK")
  message(STATUS "Making Python Virtual Environment")
  list(APPEND TARGET_COMPILE_DEF "-DUSE_GMSH_SDK")

  find_package(Python3 REQUIRED COMPONENTS Interpreter)
  set(PY_VENV "${CMAKE_INSTALL_PREFIX}/gmsh")
  set(PY_BIN_DIR "${PY_VENV}/bin")
  set(GMSH_LIBRARIES "${PY_VENV}/lib/libgmsh.so")

  install(
    CODE "
      MESSAGE(\"Creating PY_VENV from ${Python3_EXECUTABLE} to ${PY_VENV}\")
      execute_process(COMMAND_ECHO STDOUT COMMAND ${Python3_EXECUTABLE} -m venv ${PY_VENV} )
      execute_process(COMMAND_ECHO STDOUT COMMAND ${PY_BIN_DIR}/pip install --upgrade gmsh )
")

  # find_library(GMSH_LIBRARIES NAME gmsh HINTS "${PY_VENV}/lib" REQUIRED)
  target_link_libraries(${PROJECT_NAME} INTERFACE ${GMSH_LIBRARIES})
  message(STATUS "GMSH_LIBRARIES : ${GMSH_LIBRARIES}")

else()

  message(STATUS "NOT USING GMSH SDK")

endif()
