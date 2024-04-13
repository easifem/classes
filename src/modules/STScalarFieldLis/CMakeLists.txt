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

if(USE_LIS)

  set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
  target_sources(${PROJECT_NAME} PRIVATE ${src_path}/STScalarFieldLis_Class.F90)

else()

  set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
  target_sources(${PROJECT_NAME}
                 PRIVATE ${src_path}/NO_STScalarFieldLis_Class.F90)
endif()
