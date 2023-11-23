# This program is taken from
# https://github.com/LKedward/fhash

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/fhash.F90
  ${src_path}/fhash_sll.F90
  ${src_path}/fhash_data_container.F90
  ${src_path}/fhash_tbl.F90
  ${src_path}/fhash_tbl_iter.F90
  ${src_path}/fhash_fnv.F90
  ${src_path}/fhash_key/base.F90
  ${src_path}/fhash_key/int32.F90
  ${src_path}/fhash_key/int64.F90
  ${src_path}/fhash_key/char.F90
  ${src_path}/fhash_key/int32_1d.F90
  ${src_path}/fhash_key/int64_1d.F90
)
