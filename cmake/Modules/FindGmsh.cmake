# ##############################################################################
# Try to find Gmsh                                           # # Once done this
# will define:                                # GMSH_FOUND   - system has Gmsh #
# GMSH_INC     - Gmsh include directory (static or dynamic) # GMSH_LIB     -
# Gmsh library                               # # Usage: # find_package(Gmsh) # #
# Setting these changes the behavior of the search           # GMSH_INC - Gmsh
# include directory                     # GMSH_LIB     - Gmsh library path
# (static or dynamic)      #
# ##############################################################################

# Try to set GMSH_LIB and GMSH_INC from environment variables ##
# ##############################################################################

if(NOT DEFINED GMSH_LIB)

  find_package(Python3 COMPONENTS Interpreter Development)

  if(Python3_FOUND)
    message(STATUS "FOUND Python3")
    message(STATUS "Python3_LIBRARY_DIRS ${Python3_LIBRARY_DIRS}")
    find_library(GMSH_LIB "gmsh" HINTS $ENV{GMSH_LIB} ${Python3_LIBRARY_DIRS}
                                       NO_CACHE)
  else()
    message(STATUS "NOT FOUND Python3")
    find_library(GMSH_LIB "gmsh" HINTS $ENV{GMSH_LIB} NO_CACHE)
  endif()
endif()

# if(NOT DEFINED GMSH_INC) find_path(GMSH_INC "gmsh.h" HINTS $ENV{GMSH_INC}
# NO_CACHE) endif()

# CMake check and done ##
# ##############################################################################
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
  Gmsh
  "Gmsh could not be found: be sure to set GMSH_LIB and GMSH_INC in your environment variables"
  GMSH_LIB)
# GMSH_INC)
