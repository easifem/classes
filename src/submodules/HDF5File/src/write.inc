#ifdef mem_type_bool
#if rank==0
  CHARACTER(LEN=1) :: charvals
#endif

#if rank==1
  CHARACTER(LEN=1) :: charvals(1:SIZE(vals))
#endif

#if rank==2
  CHARACTER :: charvals(SIZE(vals, 1), SIZE(vals, 2))
#endif

#if rank==3
  CHARACTER :: charvals(SIZE(vals, 1), SIZE(vals, DIM=2), SIZE(vals, 3))
#endif
#endif

  CHARACTER(LEN=LEN(dsetname) + 1) :: path
#if rank==0
  INTEGER(HSIZE_T), DIMENSION(1) :: ldims, gdims, offset, cnt
#else
  INTEGER(HSIZE_T), DIMENSION(rank) :: ldims, gdims, offset, cnt
#endif
  INTEGER(HID_T) :: mem, dspace_id, dset_id, gspace_id, plist_id
  INTEGER(I4B) :: ii, jj, kk
! stash offset
  DO ii = 1, rank
#if rank==0
    offset(ii) = 0
#else
    offset(ii) = LBOUND(vals, ii) - 1
#endif
  END DO
  IF (PRESENT(offset_in)) offset = offset_in
  path = dsetname
#if rank==0
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
#if rank==0
  IF (vals) THEN
    charvals = 'T'
  ELSE
    charvals = 'F'
  END IF
#endif

#if rank==1
  charvals = 'F'
  DO CONCURRENT(ii=1:SIZE(vals), vals(ii))
    charvals(ii) = 'T'
  END DO
#endif

#if rank==2
  charvals(:, :) = 'F'
  DO CONCURRENT(ii=1:SIZE(vals, 1), jj=1:SIZE(vals, 2), vals(ii, jj))
    charvals(ii, jj) = 'T'
  END DO
#endif

#if rank==3
  charvals(:, :, :) = 'F'
    do CONCURRENT ( ii = 1:SIZE( vals, 1 ), jj = 1:SIZE( vals, 2 ), kk=1:SIZE( vals, 3 ), vals( ii, jj, kk ) )
    charvals(ii, jj, kk) = 'T'
  END DO
#endif
#endif

  mem = mem_type
  CALL preWrite(obj, rank, gdims, ldims, path, mem, dset_id, dspace_id, &
    & gspace_id, plist_id, ierr, cnt, offset)

#ifdef mem_type_bool
  IF (ierr == 0) &
 CALL h5dwrite_f(dset_id,mem,charvals,gdims,ierr,dspace_id,gspace_id,plist_id)
#else
  IF (ierr == 0) &
    CALL h5dwrite_f(dset_id,mem,vals,gdims,ierr,dspace_id,gspace_id,plist_id)
#endif

  CALL postWrite(Obj, ierr, dset_id, dspace_id, gspace_id, plist_id)
