!> author: Vikas Sharma, Ph. D.
! summary: In this example we test the [Get](./Get.md) method.
!
! Results:
!
!```txt
! set value(:,:,3) =( :, :, 1 ) =
! --------------------------------
!        0.189357  0.579115
!        0.412881  0.543243
! set value(:,:,3) =( :, :, 2 ) =
! --------------------------------
!        0.860663  0.715234
!        0.946888  0.539013
! set value(:,:,3) =( :, :, 3 ) =
! --------------------------------
!        0.240968  0.771520
!        0.793377  0.683370
! get value =
! RANK :: 1 (VECTOR)
! # VarType: SPACE & TIME
!    # VALUE:( :, :, 1 ) =
! ----------------------------
! 0.189357  0.860663  0.240968
! 0.412881  0.946888  0.793377
!    # VALUE:( :, :, 2 ) =
! ----------------------------
! 0.579115  0.715234  0.771520
! 0.543243  0.539013  0.683370
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
  REAL( DFP ), ALLOCATABLE :: value(:, :)
  CHARACTER(*), PARAMETER :: engine = "NATIVE_SERIAL"
  INTEGER( I4B ) :: tnodes
  type(FEVariable_) :: fevar

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

  call obj%get(value=fevar, globalNode=arange(1,3,1))
  call display(svalue(:,:,1:3), "set value(:,:,3) = ")
  call display(fevar, "get value = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
