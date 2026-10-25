# Bug report: HDF5File_

- Module: `src/modules/HDF5File/src/HDF5File_Class.F90`
- Submodules: `src/submodules/HDF5File/src/`
- Tests: `src/modules/HDF5File/examples/` (`run_examples.fish`)
- Date: 2026-10-26

This report comes from a code review focused on storage and I/O
efficiency. No library code is changed yet. File names below are short
for the submodules (`Constructor` is `HDF5File_Class@ConstructorMethods.F90`,
and so on).

Status: **Fixed** means the source is changed and the library is rebuilt;
the listed test passes. **Open** means not changed.

## Tests

The 36 tests from 2026-10-22 cover the file lifecycle, groups, the
queries, Write/Read/Read_ for every type and the attributes. These new
tests target the items below; the ones marked "expected to fail" document
a bug and should pass after the fix:

| Test | Checks | Now |
| --- | --- | --- |
| Write_test_7 | Real64 rank 4-7, Real32 and integer rank 3 | expected to pass |
| Write_test_8 | empty String, scalar and rank 1 | expected to fail (A4) |
| Overwrite_test_1 | overwrite with the same and a smaller shape | expected to fail (A3) |
| Compression_test_1 | rank-2 data is compressed; file is smaller | expected to fail (B1) |
| OpenHandles_test_1 | no open HDF5 objects after each method | expected to fail (A1) |
| WriteAttribute_test_2 | longer String attribute replaces the old one | expected to fail (A5) |
| Storage_test_1 | prints file sizes and times (baseline) | expected to pass |

Storage_test_1 prints `#` lines with the file size and the write/read
times. Run it before and after each change to compare.

## Fixed

None.

## Open

### A. Correctness

#### A1. Leaked HDF5 handles

- `obj_GetChunkSize` (Constructor:1009): `dset_id`, `dspace_id` and `dcpl`
  are never closed. The file uses `H5F_CLOSE_SEMI`, so a later `H5Fclose`
  **fails** while the dataset is open: the file stays open and the error is
  only seen in DEBUG mode. `Close` then sets the open status to false
  anyway.
- `preWrite` (Constructor:1205, :1330): `gspace_id` is created by
  `H5Screate_simple` and then overwritten by `H5Dget_space`, so one
  dataspace leaks on every Write of rank >= 1. In the overwrite branch the
  datatype `oldmem` (Constructor:1268) is not closed.
- ReadString: 13 calls of `H5Dget_type` and no `H5Tclose`, so one datatype
  leaks per String read. Each `*_helper` also opens the dataset a second
  time (helper and then `preRead`).
- ReadAttribute:54: the type of a String attribute is not closed.
- Effect: library memory grows with the number of calls. The
  `GetChunkSize` leak can stop the file from being closed.
- Test: OpenHandles_test_1.

#### A2. Errors are checked only in DEBUG mode

- Almost every `ierr` check is inside `#ifdef DEBUG_VER`. In a release
  build a failed `H5Fopen` still marks the file as open (Constructor:222),
  and a failed `H5Dcreate`, `H5Dwrite` or `H5Dread` returns silently with
  wrong or untouched data.
- Proposal: keep the checks of `H5Fopen`/`H5Fcreate`, `H5Dcreate`,
  `H5Dopen`, `H5Dwrite` and `H5Dread` in release builds. They cost
  nothing next to the I/O.

#### A3. Overwrite does not change the shape of a dataset

- `preWrite` (Constructor:1257): when the dataset exists it is opened
  and written with its old dataspace. Data with a different number of
  elements fails in `H5Dwrite` because the memory space and the file space
  do not match. A smaller array is documented as allowed, but it fails too.
- The DEBUG check `oldsize >= newsize*PRODUCT(gdims)` uses
  `H5Dget_storage_size` (Constructor:1303). For a compressed dataset that
  is the compressed size, so overwriting compressed data raises a false
  error.
- Proposal: if the shape or type differs, delete the link (`H5Ldelete`)
  and create the dataset again. Note that HDF5 does not reuse freed space
  unless a file-space strategy is set (see B5). For datasets that are
  rewritten often (time steps), use chunked datasets with unlimited
  `maxdims` and `H5Dset_extent`.
- Test: Overwrite_test_1.

#### A4. Empty strings cannot be written

- `obj_Write_st0` (WriteString:70) sets the type size to `LEN_TRIM`;
  `obj_Write_st1/2/3` set it to the longest `LEN_TRIM`. For an empty
  string (or an array of empty strings) the size is 0, and
  `H5Tset_size(0)` fails. The same happens for an empty String attribute
  (WriteAttribute:238) and for `Write_c1` with `LEN(vals)=0` (zero-size
  dataspace).
- Proposal: use `MAX(1, len)`; Read already removes `C_NULL_CHAR`.
- Test: Write_test_8.

#### A5. A String attribute keeps its old length

- `createAttribute` (WriteAttribute:121) opens an existing attribute
  with its old type. A longer value is converted to the old size and cut
  without any message.
- Proposal: delete the attribute (`H5Adelete`) and create it again when the
  size differs.
- Test: WriteAttribute_test_2.

#### A6. Deallocate(delete=.TRUE.) deletes an open file

- `obj_Deallocate` (Constructor:491) calls `Delete` without `Close`. The
  HDF5 file id stays open while the file is removed, and
  `nhdf5fileinuse` is decremented anyway. The docblock asks the user to
  call Close first; Deallocate should do it.

#### A7. Rank-3 String write is not bound

- `obj_Write_st3` and `obj_Write_st3_helper` are implemented, but they are
  not in the `WRITE` generic. `Read_st3` and `Read_st3_helper` are also not
  in `READ`; only `READ_` has rank 3. So a rank-3 String can be read with
  Read_ but not written.

#### A8. Minor

- `INT(length_max, REAL64)` (WriteString:129, :354): REAL64 is a real
  kind used as an integer kind; use `SIZE_T`.
- `compute_chunk_size`: for a type larger than 1 MB, `mb = 0` and the chunk
  size is 0, so `H5Pset_chunk` fails.
- `GetDataType` returns `'Rea'` for both Real32 and Real64, and `'Int'` for
  both 32- and 64-bit integers.
- `Mkdir` on an existing group fails when overwriting is not allowed (mode
  WRITE), even though nothing is overwritten.
- `Initiate` creates the directory of the file (Constructor:372) also in
  mode READ.
- Debug name: `obj_Write_st3_helper` uses `myName = "obj_Write_st0()"`;
  `obj_Read_ca0` uses `"obj_Read_st0_helper()"`.

### B. Storage

#### B1. Compression is almost never applied to arrays of rank >= 2

- `compute_chunk_size` (Constructor:1353-1370) caps **each** dimension at
  1 MB of elements: `cdims(i) = MIN(gdims(i), 1MB/elemsize)`. Compression is
  used only if `cdims /= gdims`, so a rank-2 array is compressed only when
  one dimension alone holds more than 1 MB (131072 Real64 values). A
  1000 x 1000 Real64 field (8 MB) or a 3 x 10^5 coordinate array is stored
  uncompressed, even with `zlibOpt`.
- When it does apply, the chunk size is not bounded: for 200000 x 200
  Real64 the chunk is 131072 x 200 x 8 B = 200 MB. Every read decompresses
  whole chunks, so one such chunk costs 200 MB of memory. This is the case
  the warning in `postRead` describes.
- Proposal: choose the chunk by the **total** size, for example
  64 KB-1 MB per chunk. Keep the fastest-varying (first, Fortran)
  dimension whole when possible, and halve the slowest dimensions until the
  chunk fits. Compress every dataset above a small threshold (for example
  16 KB), not only those with one dimension over 1 MB.
- Test: Compression_test_1; Storage_test_1 prints the sizes.

#### B2. No shuffle filter

- Only deflate is set (Constructor:1234). For floating-point and integer
  arrays, `H5Pset_shuffle` before deflate usually gives a much better
  ratio at almost no CPU cost. It is a standard HDF5 setting and needs no
  extra library.
- Proposal: `H5Pset_shuffle_f` and then `H5Pset_deflate_f`, for numeric
  types.

#### B3. Old file format

- The file access list (Constructor:180) sets only the close degree.
  Without `H5Pset_libver_bounds`, HDF5 writes the earliest format: groups
  use symbol tables (a B-tree and a local heap per group, about 1 KB or
  more even for an empty group). Attributes are limited to 64 KB in the object
  header, and there is no compact or dense link storage.
- Proposal: `H5Pset_libver_bounds_f(fapl, H5F_LIBVER_V18_F,
  H5F_LIBVER_LATEST_F)` (or V110). Files then need HDF5 >= 1.8 to read;
  ParaView, h5py and current HDF5 tools are fine with that.

#### B4. Many small datasets

- Mesh and field files hold many scalars and small arrays (sizes, flags,
  names). Each one is a contiguous dataset with its own object header and
  data block. The `H5D_COMPACT` layout (`H5Pset_layout_f`) stores data under
  64 KB in the object header: this saves space and one disk access per
  read.
- Proposal: use the compact layout for datasets below a threshold (for
  example 8 KB). Measure with the `small` part of Storage_test_1.

#### B5. Space of overwritten or deleted data is not reused

- Once A3 deletes and recreates datasets, the old space becomes free space
  that HDF5 does not track by default, so the file only grows (h5repack is
  needed).
- Proposal: in mode NEW set `H5Pset_file_space_strategy_f` (HDF5 >= 1.10)
  with persistent free-space tracking, or document h5repack.

#### B6. Booleans and strings

- Logical data is stored as the characters `'T'`/`'F'` (1 byte). The size
  is fine, but other tools (h5py, ParaView) see characters, not booleans.
  An 8-bit integer or the h5py-style enum (FALSE=0, TRUE=1) is just as
  small and easy to read elsewhere. Changing it changes the file format, so
  Read should accept both.
- String arrays use fixed length padded to the longest entry, and Fortran
  pads with blanks, so the padding is stored as blanks, not NULs. Reading
  returns the blanks; `LEN` of a read value is the maximum length.
  Fixed-length strings compress well, so this is acceptable for big
  arrays; set `H5T_STR_SPACEPAD` (or trim on read) so that the stored form
  matches the data.

### C. I/O efficiency

#### C1. No partial I/O

- `gdims_in`, `cnt_in` and `offset_in` are accepted but not used
  (`preWrite` has no `H5Sselect_hyperslab`). A slice of a dataset cannot be
  read or written: a time step of a field, a block of nodes, or a parallel
  write from a part of the mesh all need the whole dataset. Passing
  `gdims_in` different from `SHAPE(vals)` makes the memory and file spaces
  disagree.
- Proposal: add hyperslab selection to `preWrite` and `preRead` (offset
  and count). Add Read_/Write overloads that take `offset` and `count`, and
  an append for extendible (chunked, unlimited) datasets.

#### C2. Path checks on every write

- Each Write calls `PathExists(parent)`. That splits the path into a
  `String` array and calls `H5Lexists` once per level. `Mkdir` then
  calls `PathExists` again for each level (recursive), and `preWrite`
  calls `H5Lexists` on the dataset. Writing N datasets of depth d costs
  O(N d) link lookups plus string allocations, and `Mkalldir` is
  O(d^2).
- Proposal: create the dataset with a link creation list that has
  `H5Pset_create_inter_group_f(lcpl, 1)`; HDF5 then creates the missing
  groups itself. Then only the dataset path needs one existence check;
  walk the levels only when that check reports an error (older HDF5
  versions fail, instead of returning false, when a parent is missing).

#### C3. Extra copies and passes on read and write

- `Read` allocates with `Reallocate`, which zero-fills the array before
  HDF5 overwrites it. That is one extra pass over the data.
- Write of logical and String data builds a full converted copy in an
  automatic (stack) array: `charvals` in `include/write.F90` and `valss`
  in WriteString:93, :215, :313. Large arrays can overflow the stack
  (8 MB by default on macOS). Use allocatable arrays.
- `Read__st1` reads into a temporary array and copies it, so it holds
  twice the memory.
- `obj_Read_ca0` and `obj_Read_ca1` (ReadString:160) build the string
  one character at a time (`vals = vals // valsc(i)`), which is O(n^2) for
  long strings. Use one assignment from a `CHARACTER(LEN=n)` buffer, as
  `convert_char_array_to_str` already does.

#### C4. Ls is quadratic for large groups

- `obj_Ls` (Constructor:636) calls `H5Lget_name_by_idx` for each index.
  For old-format groups (B3) each call walks the group, so listing n
  members costs O(n^2). `H5Literate` lists in one pass. The name buffer
  is a fixed `CHARACTER(1024)`.

#### C5. Smaller items

- `CreateHardLink` opens the source object only to pass its id as the
  location; `H5Lcreate_hard(file_id, src, file_id, dst)` is enough. It also
  declares its own C interface to `H5Oclose` instead of `H5Oclose_f`.
- No raw-data chunk cache tuning (`H5Pset_chunk_cache`). The default
  1 MB cache is smaller than the chunks of B1, so a chunk is decompressed
  again for every partial access. It matters once C1 exists.

## Suggested order of work

1. A1, A2 (handle leaks, release checks): small and safe changes that
   make the rest measurable.
2. B1 + B2 (chunk size by total bytes, shuffle): biggest storage gain;
   check with Compression_test_1 and Storage_test_1.
3. B3 (libver bounds), B4 (compact layout): file format; measure with
   Storage_test_1, check that ParaView and h5py still read the files.
4. A3 + B5 (overwrite with shape change, free-space reuse).
5. C2, C3, A4, A5, A6, A7, A8.
6. C1 (hyperslab and extendible datasets): new API, plan it separately.
