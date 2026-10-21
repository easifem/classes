!> author: Vikas Sharma, Ph. D.
! summary: Set_test_10
!
! Results:
!
!```txt
! # VAR :U
!  DOF-1 ,   DOF-2 ,   DOF-3 ,   DOF-4 ,
! -------,  -------,  -------,  -------,
! 0.37009,  0.06559,  0.60917,  0.68589,
! 0.00000,  0.00000,  0.00000,  0.00000,
!
! # spaceCompo = 2
! # timeCompo = 2
!
!      value =
! ------------------
! 0.370091  0.609166
! 0.065591  0.685886
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
  REAL( DFP ), allocatable :: value(:,:)
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
  call reallocate(value, spaceCompo, timeCompo)
  call random_number(value)

  call obj%set(value=value(1,1), globalNode=1, spaceCompo=1, timeCompo=1)
  call obj%set(value=value(1,2), globalNode=1, spaceCompo=1, timeCompo=2)
  call obj%set(value=value(2,1), globalNode=1, spaceCompo=2, timeCompo=1)
  call obj%set(value=value(2,2), globalNode=1, spaceCompo=2, timeCompo=2)
  CALL obj%Display("obj = ")
  call display(value, "value = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
