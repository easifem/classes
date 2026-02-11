---
date:  2023-11-08
summary:  Initiate kernel from the toml file

---
 
# obj_ImportFromToml2
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
printToml, elemType, nsd)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

CHARACTER(*), INTENT(IN) :: tomlName

TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile

CHARACTER(*), OPTIONAL, INTENT(IN) :: filename

LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml

INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
!! element type
INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
!! Number of spatial dimension
END SUBROUTINE obj_ImportFromToml2
END INTERFACE
```
 
