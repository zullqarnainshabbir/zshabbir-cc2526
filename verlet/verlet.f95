PROGRAM verlet
  IMPLICIT NONE
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300)
  INTEGER :: i, j, k                                                 
  INTEGER :: l, m                                               
  CHARACTER (LEN=72) :: str1, str2, str3       
  REAL (KIND=wp) :: a, b, c                                          
  REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: x
  REAL (KIND=wp), DIMENSION(:,:), ALLOCATABLE :: xmat
  
  ALLOCATE ( x (300) )
  ALLOCATE ( xmat (200,300) )
  
  a = b * c
  
  a = a + ( b * c)
  
  a = 3.0_wp
  i = 1
  j = 1

  x(i) = 3.5_wp
  
  x(3) = 1.0E-12_wp
  
  xmat(i,j) = 9.2E6_wp
  
  PRINT *, x(3)

END PROGRAM verlet
