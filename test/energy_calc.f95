PROGRAM energy_calc
 IMPLICIT NONE
 
 REAL :: energy
 REAL :: mass
 REAL :: speed_of_light

 mass = 2.0
 speed_of_light = 30000000

 energy = mass * speed_of_light**2

 PRINT *, "The Energy is:", energy, "Joules"

 END PROGRAM energy_calc
