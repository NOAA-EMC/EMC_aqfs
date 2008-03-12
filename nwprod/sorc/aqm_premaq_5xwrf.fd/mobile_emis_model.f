C Code revision:
C   01/24/05 David Wong
C     -- reduced number of floating point operations

      real function voc_nonlinear_model(n,X,T,iflag)
        integer n, iflag
        real X(n), T, T0, temp       
        T0 = 297.038888889 !! 75 degrees F
        temp = T-T0
        select case (iflag)
        case (0)
c          voc_nonlinear_model =+X(1)*temp**2+X(2)*temp+X(3)
           voc_nonlinear_model =(+X(1)*temp+X(2))*temp+X(3)
        case (1,2,3)
           voc_nonlinear_model = temp**(n-iflag)

        end select
        return
      end
