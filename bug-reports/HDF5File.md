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
| Write_test_8 | empty String, scalar and rank 1 | passes, HDF5 prints errors (A4) |
| Overwrite_test_1 | overwrite with the same and a smaller shape | expected to fail (A3) |
| Compression_test_1 | rank-2 data is compressed; file is smaller | passes (B1 fixed) |
| ChunkPolicy_test_1 | default and custom chunk policy | passes (B1 fixed) |
| OpenHandles_test_1 | no open HDF5 objects after each method | passes (A1 fixed) |
| WriteAttribute_test_2 | longer String attribute replaces the old one | passes (A5 fixed) |
| WriteAttribute_test_3 | shorter value, other type, empty String attribute | passes (A5 fixed) |
| AttributeExists_test_1 | AttributeExists for groups, datasets, missing names | passes (A5 fixed) |
| Storage_test_1 | prints file sizes and times (baseline) | expected to pass |
| Layout_test_1 | compact/contiguous/chunked layout, format version | passes (B3, B4) |

Storage_test_1 prints `#` lines with the file size and the write/read
times. Run it before and after each change to compare.

Run of 2026-10-26 (after A1 and A2 part 1): 43 tests, 39 pass. Not passing:
Compression_test_1 (B1), Overwrite_test_1 (A3, stops in `postWrite`:
"src and dest dataspaces have different number of elements selected"),
WriteAttribute_test_2 (A5), and Ls_test_1 (A9, not expected). Baseline of
Storage_test_1 (HDF5 2.2.0, debug build):

| Case | File size | Write | Read |
| --- | --- | --- | --- |
| 4 x (1000 x 1000) Real64, no compression | 32002048 B | 0.004 s | 0.010 s |
| same, zlibOpt=4 | 32002048 B | 0.008 s | 0.009 s |
| 2000 scalar datasets | 646993 B | 0.087 s | |

The compressed file has the same size as the raw one: the rank-2 fields
are not compressed (B1). The 2000 scalars take 323 B each for 8 B of
data (B3, B4).

## Fixed

### A1. Leaked HDF5 handles

- Commit: ed886938 (2026-10-26). Test: OpenHandles_test_1 passes.
- `obj_GetChunkSize` did not close `dset_id`, `dspace_id` and `dcpl`.
  The file uses `H5F_CLOSE_SEMI`, so a later `H5Fclose` failed while the
  dataset was open; the file stayed open, and the error was only seen in
  DEBUG mode. Now all three are closed.
- `preWrite` created `gspace_id` with `H5Screate_simple` and then
  overwrote it with `H5Dget_space`, so one dataspace leaked per Write.
  The created dataspace is now closed first. The old data type `oldmem`
  of the overwrite branch is now closed.
- ReadString: the 12 types from `H5Dget_type` were never closed; now each
  is closed (`Read_c1` uses the predefined `H5T_NATIVE_CHARACTER`, which
  must not be closed). The helpers still open each dataset twice (helper
  and `preRead`); see C3.
- ReadAttribute: the type of a String attribute is now closed.
- The checks of the close calls are only in DEBUG mode.
- Also in this commit: `obj_Delete` uses `ACTION='READ'` and `IOMSG` in
  `OPEN` and `CLOSE`.

### B1. Compression is almost never applied to arrays of rank >= 2

- Fixed 2026-10-26. New public type `HDF5ChunkPolicy_` (targetBytes =
  1 MiB, minBytes = 64 KiB, stepsPerChunk = 1 (not used yet), shuffle =
  true, deflateLevel = 1), passed to `Initiate` as the optional
  `chunkPolicy`; the defaults are used when it is absent. `zlibOpt` is kept
  and replaces `deflateLevel`. `compute_chunk_size` limits the total chunk
  size to `targetBytes` and reduces the last dimension first. Datasets with
  less than `minBytes` of data stay contiguous.
- Tests: Compression_test_1, ChunkPolicy_test_1 (new), IsCompressed_test_1
  (updated), Storage_test_1, all pass.
- Result (Storage_test_1, debug build, zlibOpt=4):

  | Case | Before | After |
  | --- | --- | --- |
  | 4 x (1000 x 1000) Real64 | 32002048 B | 21727414 B (-32 %) |
  | write / read time | 0.008 s / 0.009 s | 0.349 s / 0.029 s |
  | Compression_test_1 (400 x 400) | 1294078 B | 10081 B |

  The smooth field compresses by 1.5x; compression costs write time (about
  0.09 s per 8 MB field at level 4). Level 1, the default, is faster;
  measure it before choosing the default for large runs.
- Chunk method (2026-10-26): `chunkMethod` 1 (uniform,
  default) splits the cut dimension into equal parts, so the last chunk is
  (almost) full; 2 (max) keeps the largest chunk that fits. HDF5 stores and
  compresses whole chunks, so the padding of a partly empty last chunk
  costs CPU (and space when not compressed). Same number of chunks, chunk
  never larger:

  | Dataset (Real64, 1 MiB) | max | padding | uniform | padding |
  | --- | --- | --- | --- | --- |
  | 1000 x 1000 | 1000 x 131 | +4.8 % | 1000 x 125 | 0 |
  | 200000 | 131072 | +31 % | 100000 | 0 |
  | 400 x 400 | 400 x 327 | +64 % | 400 x 200 | 0 |

  Measured (Storage_test_1 and Compression_test_1, zlibOpt=4): the
  1000 x 1000 fields give 21727846 B, write 0.363 s, read 0.030 s with
  uniform chunks, against 21727414 B, 0.349 s, 0.029 s with max. The
  difference is within the timing noise: the padding is 5 % of the data
  and compresses to almost nothing. Compression_test_1 (400 x 400, padding
  64 % with max) shrinks from 10081 B to 9000 B. Full suite: 44 tests, 41
  pass; not passing are A3, A5 and A9 (known, open).

- **Change of behaviour:** with the default policy every file is now
  compressed (gzip level 1); before, compression needed `zlibOpt`. Use
  `zlibOpt=-1` or `chunkPolicy%deflateLevel = -1` to turn it off.
- The DEBUG check of the overwrite branch of `preWrite` now compares the
  number of elements of the old dataspace with the new data, not the
  storage size (which is the compressed size for a compressed dataset).

Problem (before the fix):

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

### B2. No shuffle filter

- Fixed 2026-10-26 (with B1). `preWrite` sets `H5Pset_shuffle` before
  deflate when `chunkPolicy%shuffle` is true and an element has more than
  one byte.

Problem (before the fix):

- Only deflate is set (Constructor:1234). For floating-point and integer
  arrays, `H5Pset_shuffle` before deflate usually gives a much better
  ratio at almost no CPU cost. It is a standard HDF5 setting and needs no
  extra library.
- Proposal: `H5Pset_shuffle_f` and then `H5Pset_deflate_f`, for numeric
  types.

### B3. Old file format

- Fixed 2026-10-26. New private component `libverLow` of `HDF5File_`
  (optional `Initiate` argument): 110 (HDF5 1.10, default), 18, 112, 114
  or 200; 0 does not set the bounds, so the default of the HDF5 library is
  used. `Open` sets `H5Pset_libver_bounds(libverLow, latest)` only when it
  creates the file (mode NEW); existing files keep their format. Files
  written with the default need HDF5 >= 1.10 to read.
- Note: the default lower bound of HDF5 2.2 is already V18 (checked with
  `H5Pget_libver_bounds`), not the earliest format; so before this change
  the files used the 1.8 format, not the 1.6 format assumed below.
- Tests: Layout_test_1 (new) passes.
- Result: Compression_test_1 (two chunked datasets) 9000 B -> 5002 B; the
  1000 x 1000 fields 21727846 B -> 21720000 B; 2000 scalars 646993 B ->
  646607 B (with B4).

Problem (before the fix):



- The file access list (Constructor:180) sets only the close degree.
  Without `H5Pset_libver_bounds`, HDF5 writes the earliest format: groups
  use symbol tables (a B-tree and a local heap per group, about 1 KB or
  more even for an empty group). Attributes are limited to 64 KB in the object
  header, and there is no compact or dense link storage.
- Proposal: `H5Pset_libver_bounds_f(fapl, H5F_LIBVER_V18_F,
  H5F_LIBVER_LATEST_F)` (or V110). Files then need HDF5 >= 1.8 to read;
  ParaView, h5py and current HDF5 tools are fine with that.

### A5. A String attribute keeps its old length

- Fixed 2026-10-26.
  - `createAttribute`: an existing attribute is deleted (`H5Adelete`)
    and created again when overwriting is allowed, so a value of another
    length or type replaces it. When overwriting is not allowed (mode
    WRITE) it is left unchanged, nothing is written (DEBUG error), and
    `attr_id` is -1; the writers write and close only a valid id. Before,
    `H5Acreate` failed and the writers used the invalid id.
  - `obj_WriteAttribute_st0` set the type size from `LEN(valss)` but
    wrote `TRIM(valss)`, so HDF5 read past the buffer for a String with
    trailing blanks; now the buffer and the type have the same length. An
    empty String is stored as one NUL character (size 0 is not allowed),
    and `obj_ReadAttribute_st0` removes it.
  - New method `AttributeExists(obj_name, attr_name)`: false, without
    an error, if the object or the attribute does not exist.
- Tests: WriteAttribute_test_2, WriteAttribute_test_3 (new),
  AttributeExists_test_1 (new) pass. Full suite: 47 tests, 45 pass; not
  passing are A3 (Overwrite_test_1) and A9 (Ls_test_1).

Problem (before the fix):


- `createAttribute` (WriteAttribute:121) opens an existing attribute
  with its old type. A longer value is converted to the old size and cut
  without any message.
- Proposal: delete the attribute (`H5Adelete`) and create it again when the
  size differs.
- Test: WriteAttribute_test_2.

## Open

### A. Correctness

#### A2. Errors are checked only in DEBUG mode

- Decision (2026-10-26): error checks stay DEBUG-only. Part 1 (correct
  status bookkeeping) is done in the source, not built yet:
  - `preWrite` and `preRead` keep the first failure in `error` (each call
    writes to a local `ierr`), skip the steps that need the dataset, and
    set the ids that are not created to -1. Before, `preWrite` returned
    the status of its last call (`H5Pcreate`), so `H5Dwrite` ran on an
    invalid dataset after a failed `H5Dcreate`.
  - `postWrite` and `postRead` close only valid ids and do not overwrite
    `error`.
  - `Open` sets the open status (and clears the new status) only when
    `H5Fcreate`/`H5Fopen` succeeds. `Close` keeps the id and the open
    status when `H5Fclose` fails.
  - The type-size checks of the overwrite branch of `preWrite` are now
    only compiled in DEBUG mode, since only DEBUG mode uses them.
  - The "unrecognized access mode" error of `Open` is now DEBUG-only.

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
- Test: Write_test_8. It passes: `H5Tset_size(0)` fails, but the type keeps
  the size 1 of `H5T_NATIVE_CHARACTER`, so a 1-character value is written
  and read back as empty. HDF5 prints "size must be positive" for each call,
  and the stored size is not the one the code meant to set.

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

#### A9. IsGroup("/") is false, so Ls("/") returns nothing

- `obj_PathExists` (Constructor) splits the path at `/` and calls
  `H5Lexists` on each prefix. For the root `"/"` the prefixes built from
  the empty segments are most likely not valid links (not checked yet), so
  `PathExists("/")` is false; `IsGroup("/")` then returns false, and `Ls`
  returns `objs` unallocated for the root group. Found by Ls_test_1
  (`IsGroup("/")` is F while `PathExists("/a")` is T); not caused by A1 or
  A2, which do not change these routines.
- Proposal: return true for `"/"` (the root group always exists) before
  the loop, and skip empty segments.

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

#### B4. Many small datasets

- Status (2026-10-26): compact layout done, but **no measurable gain** for
  scalars. New private component `compactBytes` of `HDF5File_` (optional
  `Initiate` argument, default 4096, at most 64000); `preWrite` uses the
  compact layout for a dataset with less data, scalars included, unless it
  is chunked. Layout_test_1 checks the layouts and passes.
- Result: 2000 scalar datasets 646993 B -> 646607 B (-0.06 %), still
  about 323 B per scalar. The compact layout saves only the separate data
  block (8 B); the cost is the object header of each dataset (dataspace,
  datatype, layout, fill value and time messages) and its link in the
  group. Not measured yet, possible next steps:
  - `H5Pset_obj_track_times(dcpl, .FALSE.)`: no time stamps in headers;
  - `H5Pset_dset_no_attrs_hint` (HDF5 >= 1.10.5): smaller headers for
    datasets without attributes;
  - in the callers, store many scalars as attributes of one group or as
    one array instead of one dataset each.

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
- The String read helpers (`obj_Read_st*_helper`) open the dataset to get
  the type size and the rank, close it, and then `preRead` opens it
  again: two metadata lookups per String read.
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
