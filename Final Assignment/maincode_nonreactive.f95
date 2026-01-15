      Program my_simulation_non_reactive
      implicit real*8 (a-h,o-z)

      dimension pos(3,3), vel(3,3), force(3,3)
      dimension mass(3)
      dimension s_vec(3), d_vec(3)
      
      integer i, j, step, nsteps

      open(10, file='traj.xyz')

      dt = 10.0d0
      nsteps = 2000

      rm = 1837.15d0
      do i=1,3
          mass(i) = rm
      end do

      pos(1,1) = -10.0d0
      pos(1,2) = 4.0d0    
      pos(1,3) = 0.0d0

      pos(2,1) = 0.0d0
      pos(2,2) = 0.0d0
      pos(2,3) = 0.0d0

      pos(3,1) = 1.4d0
      pos(3,2) = 0.0d0
      pos(3,3) = 0.0d0

      do i=1,3
          do j=1,3
              vel(i,j) = 0.0d0
              force(i,j) = 0.0d0
          end do
      end do

      vel(1,1) = 0.1d0

      call get_force_vals(pos, force, pot)

      print *, 'Starting simulation now...'

      do step = 1, nsteps
          
!         Formula: x = x + v*dt + 0.5*a*dt^2
          do i=1,3
              do j=1,3
                  acc = force(i,j) / mass(i)
                  pos(i,j) = pos(i,j) + vel(i,j)*dt + 0.5d0*acc*dt**2
              end do
          end do

!         Formula: v = v + 0.5*a*dt
          do i=1,3
              do j=1,3
                  acc = force(i,j) / mass(i)
                  vel(i,j) = vel(i,j) + 0.5d0*acc*dt
              end do
          end do

          call get_force_vals(pos, force, pot)

          do i=1,3
              do j=1,3
                  acc = force(i,j) / mass(i)
                  vel(i,j) = vel(i,j) + 0.5d0*acc*dt
              end do
          end do

          if (mod(step, 10) .eq. 0) then
              write(10, *) 3
              write(10, *) 'Step: ', step, ' E: ', pot
              write(10, 100) 'H ', pos(1,1), pos(1,2), pos(1,3)
              write(10, 100) 'H ', pos(2,1), pos(2,2), pos(2,3)
              write(10, 100) 'H ', pos(3,1), pos(3,2), pos(3,3)
          end if

      end do

      close(10)
      print *, 'Finished. Data saved to traj.xyz'

100   format(A,1x,3F15.6)
      
      end

      subroutine get_force_vals(p, f, energy)
      implicit real*8 (a-h,o-z)
      dimension p(3,3), f(3,3)
      dimension rvec(3), dvec(3)
      
      d12 = sqrt((p(1,1)-p(2,1))**2+(p(1,2)-p(2,2))**2+(p(1,3)-p(2,3))**2)

      d13 = sqrt((p(1,1)-p(3,1))**2+(p(1,2)-p(3,2))**2+(p(1,3)-p(3,3))**2)

      d23 = sqrt((p(2,1)-p(3,1))**2+(p(2,2)-p(3,2))**2+(p(2,3)-p(3,3))**2)

      rvec(1) = d12
      rvec(2) = d13
      rvec(3) = d23

      call jpca15(rvec, energy, dvec)

      do i=1,3
          do j=1,3
              f(i,j) = 0.0d0
          end do
      end do

      f(1,1) = -(dvec(1)*(p(1,1)-p(2,1))/d12+dvec(2)*(p(1,1)-p(3,1))/d13)

      f(2,1) = -(dvec(1)*(p(2,1)-p(1,1))/d12+dvec(3)*(p(2,1)-p(3,1))/d23)

      f(3,1) = -(dvec(2)*(p(3,1)-p(1,1))/d13+dvec(3)*(p(3,1)-p(2,1))/d23)


      f(1,2) = -(dvec(1)*(p(1,2)-p(2,2))/d12+dvec(2)*(p(1,2)-p(3,2))/d13)

      f(2,2) = -(dvec(1)*(p(2,2)-p(1,2))/d12+dvec(3)*(p(2,2)-p(3,2))/d23)

      f(3,2) = -(dvec(2)*(p(3,2)-p(1,2))/d13+dvec(3)*(p(3,2)-p(2,2))/d23)


      f(1,3) = -(dvec(1)*(p(1,3)-p(2,3))/d12+dvec(2)*(p(1,3)-p(3,3))/d13)

      f(2,3) = -(dvec(1)*(p(2,3)-p(1,3))/d12+dvec(3)*(p(2,3)-p(3,3))/d23)

      f(3,3) = -(dvec(2)*(p(3,3)-p(1,3))/d13+dvec(3)*(p(3,3)-p(2,3))/d23)

      return
      end
