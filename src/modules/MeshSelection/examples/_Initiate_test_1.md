This example shows how to use `MeshSelection_` with `meshSelectionByID` option.

```fortran
PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE( MeshSelection_ ) :: obj
  type(ParameterList_) :: param
```

Let us initiate an instance of `MeshSelection_` wherein we will select the mesh by using mesh-ids.

```fortran
CALL FPL_INIT(); call param%Initiate()
CALL SetMeshSelectionParam(param=param, isSelectionByMeshID=.TRUE., prefix=obj%GetPrefix())
CALL obj%Initiate( param=param )
```

Adding mesh regions:

```fortran
CALL obj%Add( dim=0, meshID=[1,2,3,4,5,6])
CALL obj%Add( dim=1, meshID=[2,5,6,7])
CALL obj%Add( dim=2, meshID=[1,3,5,6,8])
CALL obj%Add( dim=3, meshID=[1,8])
```

After adding the regions in the `MeshSelection_`, we should call `set()` method. This call will do all the necessary steps.

```fortran
CALL obj%Set()
```

Display the content.

```fortran
CALL obj%Display( "" )
```

cleanup

```fortran
  CALL obj%Deallocate()
  CALL param%Deallocate()
  CALL FPL_FINALIZE
END PROGRAM main
```

<details>
<summary>Click here to see the results</summary>
<div>

```txt
IsInitiated : TRUE
IsSelectionByMeshID : TRUE
IsSelectionByElemNum : FALSE
IsSelectionByNodeNum : FALSE
IsSelectionByBox : FALSE
PointMeshID ALLOCATED : TRUE
CurveMeshID ALLOCATED : TRUE
SurfaceMeshID ALLOCATED : TRUE
VolumeMeshID ALLOCATED : TRUE
PointElemNum ALLOCATED : FALSE
CurveElemNum ALLOCATED : FALSE
SurfaceElemNum ALLOCATED : FALSE
VolumeElemNum ALLOCATED : FALSE
PointNodeNum ALLOCATED : FALSE
CurveNodeNum ALLOCATED : FALSE
SurfaceNodeNum ALLOCATED : FALSE
VolumeNodeNum ALLOCATED : FALSE
PointBox ALLOCATED : FALSE
CurveBox ALLOCATED : FALSE
SurfaceBox ALLOCATED : FALSE
VolumeBox ALLOCATED : FALSE
 
# PointMeshID :
# size : 6
1
2
3
4
5
6
 
# CurveMeshID :
# size : 4
2
5
6
7
 
# SurfaceMeshID :
# size : 5
1
3
5
6
8
 
# VolumeMeshID :
# size : 2
1
8
```

</div>
</details>
