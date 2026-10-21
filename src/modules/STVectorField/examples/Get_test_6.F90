!> author: Vikas Sharma, Ph. D.
! summary: Get_test_6
!
! Results:
!
!```txt
! set value(:,:,1:3) =( :, :, 1 ) =
! ----------------------------------
!         0.180902  0.420299
!         0.983944  0.386861
! set value(:,:,1:3) =( :, :, 2 ) =
! ----------------------------------
!         0.248634  0.276885
!         0.320403  0.242269
! set value(:,:,1:3) =( :, :, 3 ) =
! ----------------------------------
!         0.026450  0.434365
!         0.303477  0.002217
! get value =( :, :, 1 ) =
! -------------------------
!    0.180902  0.420299
!    0.983944  0.386861
! get value =( :, :, 2 ) =
! -------------------------
!    0.248634  0.276885
!    0.320403  0.242269
! get value =( :, :, 3 ) =
! -------------------------
!    0.026450  0.434365
!    0.303477  0.002217
!```

PROGRAM main
  USE easifemBase
  USE easifemClasses
  TYPE( Domain_ ) :: dom
  TYPE( STVectorField_ ) :: obj
  TYPE( HDF5File_ ) :: meshfile
  TYPE( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr
  INTEGER( I4B ), PARAMETER :: spaceCompo=2_I4B
  INTEGER( I4B ), PARAMETER :: timeCompo=2_I4B
  REAL( DFP ), ALLOCATABLE :: svalue(:,:,:)
  REAL( DFP ), ALLOCATABLE :: value(:, :, :)
  CHARACTER(*), PARAMETER :: engine = "NATIVE_SERIAL"
  INTEGER( I4B ) :: tnodes

  ! import domain
  CALL meshfile%initiate( filename="./mesh.h5", mode="READ" )
  CALL meshfile%open()
  CALL dom%initiate( hdf5=meshfile, group="" )
  CALL meshfile%Deallocate()


  ! set parameters for VectorField_
  CALL FPL_INIT()
  CALL param%initiate()

  CALL SetSTVectorFieldParam( &
    & param=param, &
    & fieldType=FIELD_TYPE_NORMAL, &
    & name="U", &
    & engine=engine, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo)
  CALL obj%initiate( param, dom )

  tnodes = dom%getTotalNodes()

  call reallocate(svalue, spaceCompo, timeCompo, tnodes)
  call random_number(svalue)
  call obj%set(value=svalue)

  call obj%get(value=value, istart=1, iend=3, stride=1)
  call display(svalue(:,:,1:3), "set value(:,:,1:3) = ")
  call display(value(:,:,1:3), "get value = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
