PROGRAM velocity_verlet
  IMPLICIT NONE

  INTEGER :: k, steps
  REAL :: tau, mass, t_current
  REAL :: x, y, z
  REAL :: x_next, y_next, z_next
  REAL :: vx, vy, vz
  REAL :: vx_next, vy_next, vz_next
  REAL :: fx_current, fy_current, fz_current
  REAL :: fx_next, fy_next, fz_next

  steps = 600
  tau = 0.2        
  mass = 1.0       

  x = 0.0
  y = 0.0
  z = 0.0
  
  vx = 0.0
  vy = 0.0
  vz = 0.0

  fx_current = 0.0
  fy_current = 0.1
  fz_current = 0.0

  PRINT *, "Steps","Time(s)","Y","VY" 
  
  DO k = 1, steps

     x_next = x + (vx * tau) + (tau**2 / (2.0 * mass)) * fx_current
     y_next = y + (vy * tau) + (tau**2 / (2.0 * mass)) * fy_current
     z_next = z + (vz * tau) + (tau**2 / (2.0 * mass)) * fz_current

     fx_next = 0.0
     fy_next = 0.1
     fz_next = 0.0

     vx_next = vx + (tau / (2.0 * mass)) * (fx_current + fx_next)
     vy_next = vy + (tau / (2.0 * mass)) * (fy_current + fy_next)
     vz_next = vz + (tau / (2.0 * mass)) * (fz_current + fz_next)

     
     t_current = real(k) * tau
    
     PRINT *, real(k), t_current, y_next, vy_next

  END DO

END PROGRAM velocity_verlet
