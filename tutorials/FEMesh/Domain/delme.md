Cleaning *.o *.out
Compiling test-1.f90
building main.out
executing main.out
## Mesh Format
4.100000 0 8
## Physical Names

| Sr.No. |  NSD  | Physical Tag | Physical Name |  NumElem | NumNodes |
| :---   | :---: | :---:        | :---:          |  :---: | ---: |
| 1 | 1           | 1           | Gamma | 32          | 32 | 
| 2 | 1           | 2           | Gamma_g_x | 20          | 21 | 
| 3 | 1           | 3           | Gamma_g_y | 20          | 21 | 
| 4 | 1           | 4           | Gamma_m | 4           | 5 | 
| 5 | 2           | 1           | Omega_l | 40          | 29 | 
| 6 | 2           | 2           | Omega_c | 40          | 29 | 
| 7 | 2           | 3           | Omega_r | 40          | 29 | 

Physical Tag to Entities Tag
| Physical Tag | PhysicalName | Entities Tag |
| :--- | :---: | ---: |
|    1| Gamma |    1,   2,   3,   4,   5,   6,   7,   8, |
|    2| Gamma_g_x |    1,   2,   3,   4,   8, |
|    3| Gamma_g_y |    1,   2,   3,   4,   8, |
|    4| Gamma_m |    6, |
|    1| Omega_l |    1, |
|    2| Omega_c |    2, |
|    3| Omega_r |    3, |
## Nodes

| Property | Value |
| :----    | ---:  |
| Total Nodes    |   77 | 
| Total Entities |   21 | 
| Min Node Tag   |    1 | 
| Max Node Tag   |   77 | 
| isSparse       |     F | 
## Elements

| Property | Value |
| :----    | ---:  |
| Total Elements    |  152 | 
| Total Entities |   11 | 
| Min Element Tag   |    1 | 
| Max Element Tag   |  152 | 
| isSparse       |     F | 
 
CREATING DOMAIN OBJECT FROM MSH4
=================================================
 
  Total Omega :: 3
  Total Boundary :: 4
  Total Edge :: 0
  Reading elements in
    Omega( 1 ) :: Omega_l
    Setting material properties1
  Reading elements in
    Omega( 2 ) :: Omega_c
    Setting material properties2
  Reading elements in
    Omega( 3 ) :: Omega_r
    Setting material properties3
  Reading elements in
    Boundary( 1 ) :: Gamma
    Setting material properties1
  Reading elements in
    Boundary( 2 ) :: Gamma_g_x
    Setting material properties2
  Reading elements in
    Boundary( 3 ) :: Gamma_g_y
    Setting material properties3
  Reading elements in
    Boundary( 4 ) :: Gamma_m
    Setting material properties4
  Reading nodes
------------------------------------nodes :: -----------------------------------
0.0000, 4.0000, 8.0000, 12.0000, 12.0000, 8.0000, 4.0000, 0.0000, 1.0000, 2.0000
0.0000, 0.0000, 0.0000,  0.0000,  4.0000, 4.0000, 4.0000, 4.0000, 0.0000, 0.0000
0.0000, 0.0000, 0.0000,  0.0000,  0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000

size of omega 1 == 40
