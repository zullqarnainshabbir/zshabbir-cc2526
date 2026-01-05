PROGRAM energy_sum
  IMPLICIT NONE

  INTEGER :: i                 
  REAL :: energies(3)          
  REAL :: total_energy         

  energies(1) = -10.5
  energies(2) = -12.0
  energies(3) = -10.5

  total_energy = 0.0

  PRINT *, "Starting Calculation..."

  DO i = 1, 3
     
     total_energy = total_energy + energies(i)
     PRINT *, "Added atom", i, "Current Total is:", total_energy
  END DO

  PRINT *, "Final Total Energy of Molecule:", total_energy

END PROGRAM energy_sum
