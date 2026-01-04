PROGRAM ideal_gas_calc
  IMPLICIT NONE  

  REAL :: pressure      
  REAL :: moles         
  REAL :: temp          
  REAL :: volume        
  REAL :: R_constant    

  
  moles = 2.0
  temp = 298.15         
  volume = 10.0
  R_constant = 0.08206  


  pressure = (moles * R_constant * temp) / volume

  PRINT *, "For volume:", volume, "L"
  PRINT *, "The Pressure is:", pressure, "Atm"

END PROGRAM ideal_gas_calc
