# This program is a part of EASIFEM library
# Copyright (C) 2020-2021  Vikas Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https: //www.gnu.org/licenses/>
#

# Get all propreties that cmake supports
EXECUTE_PROCESS(COMMAND cmake --help-property-list OUTPUT_VARIABLE CMAKE_PROPERTY_LIST)

# Convert command output into a CMake list
STRING(REGEX REPLACE ";" "\\\\;" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")
STRING(REGEX REPLACE "\n" ";" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")

FUNCTION(print_properties)
  MESSAGE ("CMAKE_PROPERTY_LIST = ${CMAKE_PROPERTY_LIST}")
ENDFUNCTION(print_properties)

FUNCTION(print_target_properties tgt)

  IF(NOT TARGET ${tgt})
    MESSAGE("There is no target named '${tgt}'")
    RETURN()
  ENDIF()

  FOREACH (prop ${CMAKE_PROPERTY_LIST})

    STRING(REPLACE "<CONFIG>" "${CMAKE_BUILD_TYPE}" prop ${prop})
    # Fix https://stackoverflow.com/questions/32197663/how-can-i-remove-the-the-location-property-may-not-be-read-from-target-error-i
    IF(prop STREQUAL "LOCATION" OR prop MATCHES "^LOCATION_" OR prop MATCHES "_LOCATION$")
      CONTINUE()
    ENDIF()
    # message ("Checking ${prop}")
    GET_PROPERTY(propval TARGET ${tgt} PROPERTY ${prop} SET)
    IF (propval)
        GET_TARGET_PROPERTY(propval ${tgt} ${prop})
        MESSAGE ("${tgt} ${prop} = ${propval}")
    ENDIF()
  ENDFOREACH(prop)
ENDFUNCTION(print_target_properties)