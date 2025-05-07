# This program is a part of EASIFEM library Copyright (C) Vikas Sharma, Ph.D
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

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME}
  PRIVATE ${src_path}/HashTables.F90
          ${src_path}/IntIntDict_Class.F90
          ${src_path}/IntSet_Class.F90
          ${src_path}/fhash_sll.F90
          ${src_path}/fhash_fnv.F90
          ${src_path}/HashDataContainer_Class.F90
          ${src_path}/HashTable_Class.F90
          ${src_path}/HashTableIter_Class.F90
          ${src_path}/Hashkey/Hashkey_Class.F90
          ${src_path}/Hashkey/HashkeyInt32_Class.F90
          ${src_path}/Hashkey/HashkeyInt64_Class.F90
          ${src_path}/Hashkey/HashkeyChar_Class.F90
          ${src_path}/Hashkey/HashkeyInt32Vec_Class.F90
          ${src_path}/Hashkey/HashkeyInt64Vec_Class.F90)
