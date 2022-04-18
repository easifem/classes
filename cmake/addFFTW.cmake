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
#FFTW
IF( ${PROJECT_NAME} MATCHES "easifemBase" )
  OPTION( USE_FFTW OFF )
  IF( USE_FFTW )
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_FFTW" )
    SET(FFTW_LIBRARY
      "$ENV{EASIFEM_EXTPKGS}/lib/libfftw3.a" )
    TARGET_LINK_LIBRARIES(
      ${PROJECT_NAME}
      PUBLIC
      ${FFTW_LIBRARY}
    )
    MESSAGE( STATUS "FFTW_LIBRARY : ${FFTW_LIBRARY}" )
  ELSE()
    MESSAGE( STATUS "NOT USING FFTW LIBRARIES" )
  ENDIF()
ENDIF()