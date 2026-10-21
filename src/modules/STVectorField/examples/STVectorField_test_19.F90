!> author: Vikas Sharma, Ph. D.
! summary: In this example we test the [Get](./Get.md) method.
!
! Results:
!
!```txt
! set value(:,:,1:3) =( :, :, 1 ) =
! ----------------------------------
!         0.756550  0.362901
!         0.014583  0.965337
! set value(:,:,1:3) =( :, :, 2 ) =
! ----------------------------------
!         0.640156  0.578452
!         0.030592  0.802883
! set value(:,:,1:3) =( :, :, 3 ) =
! ----------------------------------
!         0.661545  0.874983
!         0.684608  0.628923
! get value =( :, :, 1 ) =
! -------------------------
!    0.756550  0.362901
!    0.014583  0.965337
! get value =( :, :, 2 ) =
! -------------------------
!    0.640156  0.578452
!    0.030592  0.802883
! get value =( :, :, 3 ) =
! -------------------------
!    0.661545  0.874983
!    0.684608  0.628923
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

  call obj%get(value=value, globalNode=arange(1,3,1))
  call display(svalue(:,:,1:3), "set value(:,:,1:3) = ")
  call display(value(:,:,1:3), "get value = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
