# This program is taken from https://github.com/LKedward/fhash

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME}
  PRIVATE ${src_path}/fhash.F90
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
