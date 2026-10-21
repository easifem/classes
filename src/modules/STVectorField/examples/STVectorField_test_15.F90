!> author: Vikas Sharma, Ph. D.
! summary: In this example we test the [Set](./Set.md) method.
!
! Results:
!
!```txt
!  DOF-1 ,   DOF-2 ,   DOF-3 ,   DOF-4 ,
! -------,  -------,  -------,  -------,
! 0.90416,  0.77363,  0.66872,  0.70278,
! 0.70275,  0.77907,  0.21098,  0.01168,
! 0.82073,  0.79908,  0.09018,  0.52359,
!
! # spaceCompo = 2
! # timeCompo = 2
!
! value =( :, :, 1 ) =
! ---------------------
!  0.904156  0.668723
!  0.773634  0.702783
! value =( :, :, 2 ) =
! ---------------------
!  0.702748  0.210977
!  0.779073  0.011683
! value =( :, :, 3 ) =
! ---------------------
!  0.820733  0.090183
!  0.799077  0.523587
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
  REAL( DFP ), allocatable :: value(:,:,:)
  CHARACTER(*), PARAMETER :: engine = "NATIVE_SERIAL"
  INTEGER( I4B ) :: tnodes
  TYPE(FEVariable_) :: fevar

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
  call reallocate(value, spaceCompo, timeCompo, 3)
  call random_number(value)

  fevar = NodalVariable(value, TypeFEVariableVector, TypeFEVariableSpaceTime)

  call obj%set(value=fevar, globalNode=arange(1,3,1))
  CALL obj%Display("obj = ")
  call display(value, "value = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
