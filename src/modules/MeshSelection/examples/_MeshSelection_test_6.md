This example shows how to USE [MeshSelection_](MeshSelection_.md) WITH `meshSelectionByID` option. It also demonstrates the usage of `getNodeNum()` method.

The mesh used in this example is given below.

![](../Domain/figures/mesh.png)

IMPORT modules and define variables

```fortran
PROGRAM main
    USE easifemBase
    USE easifemClasses
    IMPLICIT NONE
    TYPE( MeshSelection_ ) :: obj
    TYPE(Domain_) :: dom
    TYPE(HDF5File_) :: meshfile
    TYPE(IntVector_) :: intvec
```

Initiates the [HDF5File_](../HDF5File/HDF5File_.md) and OPEN it

```fortran
CALL meshfile%Initiate("./mesh.h5", mode="READ")
CALL meshfile%Open()
```

Initiates an instance of [Domain_](../Domain/Domain_.md)

```fortran
CALL dom%Initiate(meshfile, '')
```

Initiates an instance of [MeshSelection_](MeshSelection_.md) WITH mesh slection by mesh ID.

```fortran
CALL obj%Initiate( isSelectionByMeshID=.TRUE. )
```

Add left region of the mesh.

```fortran
CALL obj%Add( dim=2, meshID=[1])
```

Now that we have added the region, we should CALL set FUNCTION.

```fortran
CALL obj%Set()
```

The following code BLOCK shows how to USE `getNodeNum` to get the element numbers from surface mesh id (dim=2).

```fortran
intvec = obj%getNodeNum(domain=dom, dim=2)
CALL Display(intvec, "getNodeNum =", orient='row')
intvec = obj%getNodeNum(domain=dom)
CALL Display(intvec, "getNodeNum =", orient='row')
```

Let us not clean the obj and add some new regions, we will add bottom edges, that is, dim=1, and `entityNum=[1,2]`.

```fortran
CALL obj%Deallocate()
CALL obj%Add(dim=1, meshID=[1,2])
CALL obj%Set()
intvec = obj%getNodeNum(domain=dom)
CALL Display(intvec, "getNodeNum =", orient='row')
```

Cleanup.

```fortran
    CALL obj%Deallocate()
END PROGRAM main
```
