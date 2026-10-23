# Bug report: FEDomain_

- Module: `src/modules/FEDomain/src/FEDomain_Class.F90`
- Submodules: `src/submodules/FEDomain/src/`
- Tests: `src/modules/FEDomain/examples/`
- Date: 2026-10-22

Status: **Fixed** means the source is changed and the library is rebuilt;
the listed test passes. **Open** means not changed.

## Fixed

### 1. Initiate... methods segfault for 2D domains

- Where: `obj_InitiateNodeToElements`, `obj_InitiateNodeToNodes`,
  `obj_InitiateElementToElements`, `obj_InitiateBoundaryData`,
  `obj_InitiateFacetElements`, `obj_InitiateExtraNodeToNodes` in
  `FEDomain_Class@MeshDataMethods.F90`
- Symptom: `SIGSEGV` on the first call for a 2D mesh.
- Cause: they call `obj%meshVolume%...`, `obj%meshSurface%...`, etc.
  without checking `ASSOCIATED`; `meshVolume` is null when nsd < 3.
- Fix: each call is guarded by `IF (ASSOCIATED(obj%mesh...))`.
- Test: `InitiateNodeToElements_test_1`, `InitiateNodeToNodes_test_1`,
  `InitiateElementToElements_test_1`, `InitiateExtraNodeToNodes_test_1`

## Open

### 2. InitiateBoundaryData crashes (removed)

- Where: `obj_InitiateBoundaryData` (FEDomain_ and AbstractDomain_)
- Cause: after the meshes it called `obj%SetFacetElementType()`, which
  writes to the never allocated `facetElementType` of the mesh.
- Resolution: `InitiateBoundaryData` was removed from AbstractDomain_ and
  FEDomain_ (binding, interface, implementation). No method in the library
  called it. Call `InitiateBoundaryData` of a mesh (from GetMeshPointer)
  instead.

### 3. InitiateFacetElements fails for 2D and 3D domains (fixed)

- Where: `obj_InitiateFacetElements` in `FEDomain_Class@MeshDataMethods.F90`
- Cause: `InitiateFacetElements` of the curve mesh raised an error
  because 1D meshes have no faces.
- Fix: the mesh method now returns early for `xidim <= 1` (see
  `AbstractMesh.md`, bug 11).
- Test: `InitiateFacetElements_test_1`

### 4. GetMeshPointer ignores entityNum and globalElement (fixed)

- Where: `obj_GetMeshPointer1` in `FEDomain_Class@GetMethods.F90`
- The mesh of a dimension holds all entities of that dimension, so
  `entityNum`, `globalElement` and `isLocal` were not used. They are
  removed: `GetMeshPointer(dim)` (see `AbstractDomain.md`, bug 4).
- If `dim` is larger than nsd the returned pointer is not associated
  (documented); callers must check it.

### 5. Deallocate does not free the meshes (fixed)

- Where: `obj_Deallocate` in `FEDomain_Class@ConstructorMethods.F90`
- Cause: the meshes are allocated with `FEMesh_Pointer()` in `Import`;
  `Deallocate` called `Deallocate` on each mesh and nullified the pointer,
  but never freed the mesh objects.
- Fix: the four meshes are freed with `DEALLOCATE (obj%meshVolume)` etc.
  The domain owns them; pointers from `GetMeshPointer` (for example in
  FEDOF_) are dangling after the domain is deallocated.
- Test: `Deallocate_test_1`

### 6. GetOrder is not available (removed)

- `AbstractDomain_%GetOrder` raised an error and FEDomain_ did not
  override it. It was removed from AbstractDomain_ (see
  `AbstractDomain.md`, bug 3). Use `GetMeshPointer(dim)` and the mesh
  method `GetOrder()`.
