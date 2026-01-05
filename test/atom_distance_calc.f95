PROGRAM atom_distance_calc
 IMPLICIT NONE

 REAL :: x1,y1,z1
 REAL :: x2,y2,z2
 REAL :: dist

 x1 = 5.0
 y1 = 3.0
 z1 = 9.0

 x2 = 11.0
 y2 = 8.0
 z2 = 16.0

 dist = SQRT ((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)

 PRINT *, "The distance is:", dist

 END PROGRAM atom_distance_calc

