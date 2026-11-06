This example shows the use of `getMeshID` method.

```fortran
PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE
TYPE( MeshSelection_ ) :: obj

CALL obj%Initiate( isSelectionByMeshID=.TRUE. )

CALL obj%Add( dim=0, meshID=[1,2,3,4,5,6])
CALL obj%Add( dim=1, meshID=[2,5,6,7])
CALL obj%Add( dim=2, meshID=[1,3,5,6,8])
CALL obj%Add( dim=3, meshID=[1,8])

CALL obj%Set()

CALL OK( ALL([1,2,3,4,5,6] .EQ. obj%getMeshID(0)), "getMeshID" )
CALL OK( ALL([2,5,6,7] .EQ. obj%getMeshID(1)), "getMeshID" )
CALL OK( ALL([1,3,5,6,8] .EQ. obj%getMeshID(2)), "getMeshID" )
CALL OK( ALL([1,8] .EQ. obj%getMeshID(3)), "getMeshID" )

CALL obj%Deallocate()
END PROGRAM main
```
