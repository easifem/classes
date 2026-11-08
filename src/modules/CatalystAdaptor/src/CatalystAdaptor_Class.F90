! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

MODULE CatalystAdaptor_Class
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP, I4B, LGT
USE ScalarField_Class, ONLY: ScalarField_
USE String_Class, ONLY: String
USE VectorField_Class, ONLY: VectorField_
USE tomlf, ONLY: toml_table
IMPLICIT NONE
PRIVATE

PUBLIC :: CatalystAdaptor_

!----------------------------------------------------------------------------
!                                                           CatalystAdaptor_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-19
! summary: Adaptor for ParaView Catalyst (in situ visualization)
!
!# Introduction
!
! The mesh and nodal vector or scalar fields are sent to ParaView
! Catalyst 2 as Conduit Mesh Blueprint (default channel name: grid).
!
! - coordsets/coords: explicit coordinates (x, y, z)
! - topologies/mesh: unstructured, linear tri, quad, tet, hex
! - fields/<name>: vertex association, 3 components (x, y, z) for
!   vector fields, single component for scalar fields
!
! The coordinates, connectivity, and field values are passed as
! external (zero copy) arrays. Therefore, these arrays are kept in
! this object.
!
! Usage:
!
!```fortran
! CALL obj%ImportFromToml(table)          ! reads [<table>.catalyst]
! CALL obj%Initiate(mesh)                 ! if obj%isActive
! ! every time step
! CALL obj%SetField(name="displacement", field=u)
! CALL obj%SetField(name="velocity", field=v)
! CALL obj%Execute(timestep=step, time=t)
! ! end of simulation
! CALL obj%Finalize()
!```
!
! Catalyst is enabled only if the library is built with USE_CATALYST.
! Otherwise all the methods do nothing.

TYPE :: CatalystAdaptor_
  LOGICAL(LGT) :: isActive = math%no, isInit = math%no
  !! isActive: catalyst is requested in the config
  !! isInit: catalyst is initialized
  INTEGER(I4B) :: frequency = math%one_i, nPoints = math%zero_i, &
                  nCells = math%zero_i, nne = math%zero_i, &
                  nFields = math%zero_i
  !! frequency: execute catalyst every frequency time steps
  !! nPoints: number of points (vertex nodes) in the mesh
  !! nCells: number of cells in the mesh
  !! nne: number of nodes in a cell (homogeneous mesh)
  !! nFields: number of fields set by SetField
  TYPE(String) :: implementation, searchPath, channelName, shape
  !! implementation: name of catalyst implementation (paraview)
  !! searchPath: directory of libcatalyst-<implementation>.so
  !! channelName: name of the channel (registrationName in python script)
  !! shape: blueprint shape name of cells
  TYPE(String), ALLOCATABLE :: scripts(:)
  !! catalyst python scripts
  TYPE(String), ALLOCATABLE :: envNames(:), envValues(:)
  !! environment variables set before catalyst is initialized,
  !! they are used to pass options to the python scripts
  TYPE(String), ALLOCATABLE :: fieldNames(:)
  !! names of fields set by SetField
  INTEGER(I4B), ALLOCATABLE :: fieldNComp(:)
  !! number of components of fields set by SetField
  !! 3 for vector fields, 1 for scalar fields
  REAL(DFP), ALLOCATABLE :: x(:), y(:), z(:)
  !! coordinates of points
  INTEGER(I4B), ALLOCATABLE :: conn(:)
  !! connectivity (zero based, VTK node ordering)
  REAL(DFP), ALLOCATABLE :: fieldValue(:, :, :)
  !! field values, shape = (nPoints, 3, nFields)
  !! only the first component is used for scalar fields
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml => obj_ImportFromToml
  !! Import options from toml table [<table>.catalyst]
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Build mesh data and initialize catalyst
  PROCEDURE, PASS(obj) :: SetField1 => obj_SetField1
  !! Set (copy) the vertex values of a nodal vector field
  PROCEDURE, PASS(obj) :: SetField2 => obj_SetField2
  !! Set (copy) the vertex values of a nodal scalar field
  GENERIC, PUBLIC :: SetField => SetField1, SetField2
  PROCEDURE, PUBLIC, PASS(obj) :: Execute => obj_Execute
  !! Send the mesh and fields to catalyst
  PROCEDURE, PUBLIC, PASS(obj) :: Finalize => obj_Finalize
  !! Finalize catalyst
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Finalize catalyst (if needed) and deallocate data
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content
END TYPE CatalystAdaptor_

!----------------------------------------------------------------------------
!                                                    ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-19
! summary: Import catalyst options from toml
!
!# Introduction
!
! table is the parent table (e.g. kernel), and options are read from
! the subtable `catalyst`:
!
!```toml
![kernel.catalyst]
!isActive = true
!scripts = ["./catalyst_live.py"]
!implementation = "paraview"
!searchPath = "/usr/lib/catalyst"
!frequency = 1
!channelName = "grid"
!
!# optional, environment variables for the python scripts
![kernel.catalyst.env]
!CATALYST_PROBES = "0.5,30,0;0.5,15,0"
!CATALYST_EXPORT_DIR = "./results/catalyst"
!```
!
! The values in `env` should be strings. They are set in the environment
! before catalyst is initialized, and they overwrite the existing values.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml(obj, table)
    CLASS(CatalystAdaptor_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-19
! summary: Build coordinates and connectivity, and initialize catalyst
!
!# Introduction
!
! The environment variables given in `env` are set, then
! catalyst is initialized (the python scripts are loaded).

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, mesh)
    CLASS(CatalystAdaptor_), INTENT(INOUT) :: obj
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetField@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-22
! summary: Set the vertex values of a nodal vector field
!
!# Introduction
!
! The vertex values (first nPoints nodes) of field are copied, and they
! are sent with the given name in the next Execute.
! A new name is added at the first call, the values are overwritten
! after that. Nothing is done if catalyst is not initialized.

INTERFACE
  MODULE SUBROUTINE obj_SetField1(obj, name, field)
    CLASS(CatalystAdaptor_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    CLASS(VectorField_), INTENT(INOUT) :: field
  END SUBROUTINE obj_SetField1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetField@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-23
! summary: Set the vertex values of a nodal scalar field
!
!# Introduction
!
! Same as SetField1, but for a nodal scalar field. The field is sent
! as a single component field.

INTERFACE
  MODULE SUBROUTINE obj_SetField2(obj, name, field)
    CLASS(CatalystAdaptor_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    CLASS(ScalarField_), INTENT(INOUT) :: field
  END SUBROUTINE obj_SetField2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Execute@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-19
! summary: Send mesh and fields to catalyst
!
!# Introduction
!
! The mesh and all the fields set by SetField are sent.
! Catalyst is executed only if MOD(timestep, frequency) == 0.
! Nothing is done if catalyst is not initialized.

INTERFACE
  MODULE SUBROUTINE obj_Execute(obj, timestep, time)
    CLASS(CatalystAdaptor_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timestep
    REAL(DFP), INTENT(IN) :: time
  END SUBROUTINE obj_Execute
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Finalize@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Finalize(obj)
    CLASS(CatalystAdaptor_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Finalize
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(CatalystAdaptor_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Display@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(CatalystAdaptor_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

END MODULE CatalystAdaptor_Class
