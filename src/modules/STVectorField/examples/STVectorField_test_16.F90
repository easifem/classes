!> author: Vikas Sharma, Ph. D.
! summary: In this example we test the [Set](./Set.md) method.
!
! Results:
!
!```txt
! # VAR :U
!   DOF-1 ,    DOF-2 ,    DOF-3 ,    DOF-4 ,
! --------,  --------,  --------,  --------,
! 0.720954,  0.720954,  0.720954,  0.720954,
! 0.720954,  0.720954,  0.720954,  0.720954,
! 0.720954,  0.720954,  0.720954,  0.720954,
!
! # spaceCompo = 2
! # timeCompo = 2
!
! value = 0.720954
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
  REAL( DFP ) :: value
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

  call random_number(value)
  call obj%set(value=value)

  CALL obj%Display("obj = ")
  call display(value, "value = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
