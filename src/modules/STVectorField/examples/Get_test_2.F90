!> author: Vikas Sharma, Ph. D.
! summary: Get_test_2
!
! Results:
!
!```txt
! set value(:,:,3) =
! -------------------
! 0.131827  0.736131
! 0.861203  0.200003
!
!    get value =
! ------------------
! 0.131827  0.736131
! 0.861203  0.200003
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

  call obj%get(value=value)
  call display(svalue(:,:,3), "set value(:,:,3) = ")
  call display(value(:,:,3), "get value = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
