! DATA_RANK is a preprocessor macro (#define DATA_RANK n in the including
! file); it is not named rank, because the formatter uppercases the
! intrinsic name RANK and the preprocessor is case sensitive
#ifdef mem_type_bool
#if DATA_RANK==0
CHARACTER(LEN=1) :: charvals
#endif

! The character copy of a logical array is allocatable: an automatic
! array is on the stack, which large arrays can overflow
#if DATA_RANK==1
CHARACTER(LEN=1), ALLOCATABLE :: charvals(:)
#endif

#if DATA_RANK==2
CHARACTER(LEN=1), ALLOCATABLE :: charvals(:, :)
#endif

#if DATA_RANK==3
CHARACTER(LEN=1), ALLOCATABLE :: charvals(:, :, :)
#endif
#endif

CHARACTER(LEN=LEN(dsetname) + 1) :: path
#if DATA_RANK==0
INTEGER(HSIZE_T), DIMENSION(1) :: ldims, gdims, offset, cnt
#else
INTEGER(HSIZE_T), DIMENSION(DATA_RANK) :: ldims, gdims, offset, cnt
#endif
INTEGER(HID_T) :: mem, dspace_id, dset_id, gspace_id, plist_id
INTEGER(I4B) :: ii, jj, kk, ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                        '[START] ')
#endif

! stash offset
DO ii = 1, DATA_RANK
#if DATA_RANK==0
  offset(ii) = 0
#else
  offset(ii) = LBOUND(vals, ii) - 1
#endif
END DO
IF (PRESENT(offset_in)) offset = offset_in
path = dsetname
#if DATA_RANK==0
ldims = 1
#else
ldims = SHAPE(vals)
#endif
IF (PRESENT(gdims_in)) THEN
  gdims = gdims_in
ELSE
  gdims = ldims
END IF
cnt = gdims
IF (PRESENT(cnt_in)) cnt = cnt_in

#ifdef mem_type_bool
#if DATA_RANK==0
IF (vals) THEN
  charvals = 'T'
ELSE
  charvals = 'F'
END IF
#endif

#if DATA_RANK==1
ALLOCATE(charvals(SIZE(vals)))
charvals = 'F'
DO CONCURRENT(ii=1:SIZE(vals), vals(ii))
  charvals(ii) = 'T'
END DO
#endif

#if DATA_RANK==2
ALLOCATE(charvals(SIZE(vals, 1), SIZE(vals, 2)))
charvals(:, :) = 'F'
DO CONCURRENT(ii=1:SIZE(vals, 1), jj=1:SIZE(vals, 2), vals(ii, jj))
  charvals(ii, jj) = 'T'
END DO
#endif

#if DATA_RANK==3
ALLOCATE(charvals(SIZE(vals, 1), SIZE(vals, 2), SIZE(vals, 3)))
charvals(:, :, :) = 'F'
DO CONCURRENT(ii=1:SIZE(vals, 1), jj=1:SIZE(vals, 2), &
              kk=1:SIZE(vals, 3), vals(ii, jj, kk))
  charvals(ii, jj, kk) = 'T'
END DO
#endif
#endif

mem = mem_type
CALL preWrite(obj, DATA_RANK, gdims, ldims, path, mem, dset_id, dspace_id, &
              gspace_id, plist_id, ierr, cnt, offset)

#ifdef mem_type_bool
IF (ierr == 0) CALL H5DWRITE_F(dset_id, mem, charvals, gdims, ierr, &
                               dspace_id, gspace_id, plist_id)
#else
IF (ierr == 0) CALL H5DWRITE_F(dset_id, mem, vals, gdims, ierr, dspace_id, &
                               gspace_id, plist_id)
#endif

CALL postWrite(Obj, ierr, dset_id, dspace_id, gspace_id, plist_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                        '[END] ')
#endif
