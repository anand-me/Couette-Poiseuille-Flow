!==================================================================
!COMPILE USING gfortran cpfinal.f90 -o cpfinal.exe
!Author: Akshay Anand 
!RUN USING     ./cpfinal.exe
!================================================================== 
PROGRAM CP_FLOW
IMPLICIT NONE 
REAL(8):: GRAD_PRES,DPDX,H,MU,DELX,X0,ZETA,BETA,ALPHA,F_M
REAL(8):: CASE_NUMBER,V_B1,V_B2,U_TAU1,U_TAU2,U_BULK
INTEGER :: N,ITTER,K,I,J,R,M,CAS
REAL(8),DIMENSION(:),ALLOCATABLE::L_O,L_M,V,DU_N,DU_S,DU,VP1UN,YPAD,Y_PLUS,U_PLUS,VP1,UN,A,B,C,D,Y1,Y2,MUT
character :: FMT
PRINT*, 'ENTER ANY CASE'                                                               
READ*, CAS                                                                                                 
!DO CASE_NUMBER=1, 18
!WRITE(X1,FMT) (CASE_NUMBER)
 ! SELECT CASE(CASE_NUMBER)
 ! CASE(1)
!GRAD_PRES = 0     
! V_B = 12.84 
 !H = 0.066
! CASE(2)
 !GRAD_PRES = -0.808
 !V_B =12.84  
 !H = 0.066
 !CASE(3)
 !GRAD_PRES = -1.486
 !V_B =12.84  
 !H = 0.066
 !CASE(4)
 !GRAD_PRES = -1.51 
 !V_B = 12.84 
 !H = 0.066
 !CASE(5)
!GRAD_PRES = -1.96 
 !V_B = 12.84 
 !H = 0.066
 !CASE(6)
! GRAD_PRES = -1.430
 !V_B =8.59   
! H = 0.066
!CASE(7)
!GRAD_PRES = -3.548
 !V_B =17.08  
 !H = 0.101
!CASE(8)
!GRAD_PRES = -2.323
 !V_B = 12.84 
 !H = 0.101
!CASE(9)
 !GRAD_PRES = -1.212
 !V_B =  8.59 
! H = 0.101
!CASE(10)
 !GRAD_PRES = -4.83 
 !V_B = 12.84 
 !H = 0.066
 !CASE(11)
 !GRAD_PRES = -7.5  
 !V_B = 12.84 
 ! H = 0.066
!CASE(12)
!GRAD_PRES = -14.3 
!V_B = 12.84 
 !H = 0.066
!CASE(13)
!GRAD_PRES = -18.5 
!V_B = 12.84 
!H = 0.066
!CASE(14)
!GRAD_PRES = -20.8 
!V_B = 8.59  
 !H = 0.066
!CASE(15)
!GRAD_PRES = -13.14
!V_B = 0     
 !H = 0.066
 !CASE(16)
 !GRAD_PRES = 0     
 !V_B = 3.75  
 !H = 0.0296
! CASE(17)
 !GRAD_PRES = -0.582
 !V_B = 3.09  
 !H = 0.0296     
 !CASE(18)
 !GRAD_PRES = -1.23 
!V_B = 1.8  
!H = 0.0296
!END SELECT
!END DO 

IF (CAS==1) THEN
N=1001
V_B2=12.84
DPDX=0.D0
ALPHA=0.05
OPEN(UNIT=101, FILE='case1.dat')
END IF

IF (CAS==2) THEN
DPDX=-.808
N=1001
V_B2=12.84
ALPHA=0.05
OPEN(UNIT=101, FILE='case2.dat')
END IF

IF (CAS==3) THEN
DPDX=-1.486
N=1001
V_B2=12.84
ALPHA=0.05
OPEN(UNIT=101, FILE='case3.dat')
END IF

IF (CAS==4) THEN
DPDX=-1.51
N=71
V_B2=12.84
ALPHA=0.05
OPEN(UNIT=101, FILE='case4.dat')
END IF

IF (CAS==5) THEN
N=1001
V_B2=12.84
DPDX=-1.96
ALPHA=0.05
OPEN(UNIT=101, FILE='case5.dat')
END IF

IF (CAS==6) THEN
N=1001
V_B2=8.59
DPDX=-1.43
ALPHA=0.05
OPEN(UNIT=101, FILE='case6.dat')
END IF

IF (CAS==7) THEN
DPDX=-3.548
V_B2=17.08
N=45
ALPHA=0.06
OPEN(UNIT=101, FILE='case7.dat')
END IF

IF (CAS==8) THEN
DPDX=-2.323
V_B2=12.84
N=45
ALPHA=0.06
OPEN(UNIT=101, FILE='case8.dat')
END IF

IF (CAS==9) THEN
DPDX=-1.212
V_B2=8.59
N=45
ALPHA=0.05
OPEN(UNIT=101, FILE='case9.dat')
END IF

IF (CAS==10) THEN
V_B2=12.84
DPDX=-4.83
N=1001
ALPHA=0.05
OPEN(UNIT=101, FILE='case10.dat')
END IF

IF (CAS==11) THEN
V_B2=12.84
DPDX=-7.5
N=1001
ALPHA=0.05
OPEN(UNIT=101, FILE='case11.dat')
END IF

IF (CAS==12) THEN
V_B2=12.84
DPDX=-14.3
N=1001
ALPHA=0.05
OPEN(UNIT=101, FILE='case12.dat')
END IF

IF (CAS==13) THEN
V_B2=12.84
DPDX=-18.5
N=1001
ALPHA=0.05
OPEN(UNIT=101, FILE='case13.dat')
END IF

IF (CAS==14) THEN
V_B2=8.59
DPDX=-20.8
N=101
ALPHA=0.05
OPEN(UNIT=101, FILE='case14.dat')
END IF

IF (CAS==15) THEN
V_B2=0
DPDX=-13.14
N=1001
ALPHA=0.05
OPEN(UNIT=101, FILE='case15.dat')
!==================================================================
IF (CAS==16) THEN
V_B2=0.d0
DPDX=-0.218
N=1001
ALPHA=0.05
V_B1=3.75
OPEN(UNIT=101, FILE='case16.dat')

IF (CAS==17) THEN
V_B2=0.d0
DPDX=-0.272
N=101
ALPHA=0.05
V_B1=3.09
OPEN(UNIT=1001, FILE='case17.dat')

IF (CAS==18) THEN
V_B2=0.d0
DPDX=-0.303
N=101
ALPHA=0.05
V_B1=1.8
OPEN(UNIT=101, FILE='case18.dat')
END IF
V_B1       =0.D0                                                                              
MU       =0.000015                                                                                             
H         =0.066                                                                                                 
ITTER=1000
ALLOCATE(A(N))                                                                                                   
ALLOCATE(B(N))                                                                                                   
ALLOCATE(C(N))                                                                                                   
ALLOCATE(D(N))                                                                                                   
ALLOCATE(Y1(1:N))                                                                                                  
ALLOCATE(Y2(1:N))                                                                                                  
ALLOCATE(MUT(N))                                                                                                 
ALLOCATE(V(N))                                                                                                   
ALLOCATE(UN(N))                                                                                                  
ALLOCATE(YPAD(1:N))                                                                                                
ALLOCATE(VP1(N))                                                                                                  
ALLOCATE(DU_N(N))                                                                                                 
ALLOCATE(DU_S(N))                                                                                                 
ALLOCATE(DU(N))                                                                                                 
ALLOCATE(L_O(N))                                                                                                  
ALLOCATE(L_M(N)) 
R=((N-1)/2)         
DO J=1,N                                                                                                            
YPAD(J)=0.D0                                   
END DO                                                                                                              
CALL SGRID_GENERATOR(N,ALPHA,YPAD)                                                                                 
DO J=1,N                                                                                                             
 Y1(J)=YPAD(J)*H                                        
END DO                                                                                                               
DO J=1,N                                                                                                            
Y2(J)=Y1(N+1-J)                                  
END DO                                                                                                             
DO I=1,N                                                                                                            
MUT(I)=0.D0                                                             
DU(I)  =0.D0                                
DU_N(I) =0.D0                                                 
DU_S(I) =0.D0                                                
V(I)   =0.D0                                                        
L_O(I)  =0.D0                                                              
L_M(I)  =0.D0                                                      
END DO                                                                                                               
U_TAU1=0.D0 
U_TAU2=0.D0 
DO K=1,ITTER     
 C(1)=1                                                                                                
 A(1)=0.D0                                                                                            
 B(1)=1                                                                                                
 D(1)=2*V_B1                                                                                           
                                                                                                     
 C(N)=0.D0                                                                                             
 A(N)=1                                                                                                
 B(N)=1                                                                                               
 D(N)=2*V_B2                                                                                            
DO I=2,N-1
 C(I)=(MU+((MUT(I+1)+MUT(I))/2.0))/(Y1(I+1)-Y1(I))
 B(I)=-1.0*(MU+((MUT(I)+MUT(I+1))/2.0))*(1/(Y1(I+1)-Y1(I)))-1.0*(MU+((MUT(I)+MUT(I-1))/2.0))*(1.0/(Y1(I)-Y1(I-1)))
 A(I)=(MU+((MUT(I)+MUT(I-1))/2.0))/(Y1(I)-Y1(I-1)) 
 D(I)=DPDX*(Y1(I+1)-Y1(I-1))/2.0
 END DO 
CALL THOMAS_ALGO(A,B,C,D,VP1,N)
 DO J=2,N-1
 DU_N(J)=(VP1(J+1)-VP1(J))/(Y1(J+1)-Y1(J))
 DU_S(J)=(VP1(J)-VP1(J-1))/(Y1(J)-Y1(J-1))
 END DO 
DO J=2,N-1
    DU(J)=(DU_N(J)+DU_S(J))/2
 END DO 
 U_TAU1=SQRT(MU*(ABS(DU_S(2))))
 U_TAU2=SQRT(MU*(ABS(DU_N(N-1))))
 DO J=2,N-1
 IF(J<R) THEN
 L_O(J)=0.5*H*(0.21-0.43*((1-2*(Y1(J)/H))**4)+(0.22*((1-2*(Y1(J)/H))**6)))
 L_M(J)=L_O(J)*(1-EXP((-Y1(J))/(26*MU/U_TAU1)))
 MUT(J)=((L_M(J))**2)*ABS(DU(J))
END IF
IF (J >= R) THEN 
L_O(J)=0.5*H*(0.21-0.43*((1-2*(Y2(J)/H))**4)+0.22*((1-2*(Y2(J)/H))**6))
L_M(J)=L_O(J)*(1-EXP((-Y2(J))/(26*MU/U_TAU2)))
MUT(J)=((L_M(J))**2)*ABS(DU(J))
END IF  
END DO 
MUT(1)=-1.0*MUT(2)                
MUT(N)=-1.0*MUT(N-1)               
UN=VP1/V_B2 
END DO 
F_M=0;
DO I=2,N-1
F_M=F_M+VP1(I)*(Y1(I+1)-Y1(I-1))/2
END DO
U_BULK=F_M/H
PRINT*, U_TAU1, U_TAU2
OPEN(UNIT=5, FILE='alpha.dat' )
DO  I=1, N
WRITE(5,*) (VP1(I)/MAXVAL(VP1)),YPAD(I)
 END DO  
 CLOSE (UNIT=5)
IF (CAS==15) THEN                                                                                                               
DO I=2,N-1                                                                                                                      
 WRITE(101,*) VP1(I)/MAXVAL(VP1),Y1(I)/H                                                                                        
 END DO  
ELSE IF (CASE==16) THEN 
DO I=2,N-1
WRITE(101,*) VP1(I)/U_BULK,Y1(I)/H    
ELSE IF (CASE==17) THEN 
DO I=2,N-1
WRITE(101,*) VP1(I)/U_BULK,Y1(I)/H  
  ELSE IF (CASE==18) THEN 
DO I=2,N-1
WRITE(101,*) VP1(I)/U_BULK,Y1(I)/H    

                                                                                                                      
ELSE
DO I=2,N-1   
WRITE(101,*) VP1(I)/V_B2,Y1(I)/H    
 END DO
END IF
END PROGRAM
!==================================================================
!I USED THOMAS ALGORITHM HERE
!==================================================================
SUBROUTINE THOMAS_ALGO(AS,BS,CS,DS,XS,NS)  
IMPLICIT NONE 
REAL(8), DIMENSION(1:1001):: DS, BS,AS,CS,XS
INTEGER :: I,J,NS
! FORWARD ELEMINATION 
 DO I=2, NS
 BS(I)=BS(I)-((AS(I)/BS(I-1))*CS(I-1))
 DS(I)=DS(I)-((AS(I)/BS(I-1))*DS(I-1))
END DO 
XS(NS)= DS(NS)/BS(NS)
!BACKWARD SUBSTITUTION
DO J= NS-1,1,-1
XS(J)=(DS(J)-CS(J)*XS(J+1))/BS(J)
END DO 
RETURN
END SUBROUTINE
!==================================================================
! SUBROUTINE TO GENERATE 1D STRETCHED GRID WITH N1 POINTS
! This subroutine basically divides the domain of length 1 unit from 0 to 1 in N1 number points
!According to the requirement, the discretized points have to be multiplied with length H to have a grid of size H. If this is   not multiplied, it wil simply create the mesh from 0 to 1 
! Original code : http://imp-turbulence.ec-lille.fr/Webpage/Laval/Lectures/TD-Programs.tar
!==================================================================
SUBROUTINE SGRID_GENERATOR(N1,ALPHA,YPAD)
IMPLICIT NONE 
REAL(8),INTENT(IN)				::ALPHA
INTEGER,INTENT(IN)				::N1
REAL(8),DIMENSION(N1),INTENT(INOUT)		::YPAD
INTEGER						::J
REAL(8)						::ALPHA1,ALPHA2
INTEGER						::N05,N06
REAL(8)						::COEF
WRITE(*,3)N1,ALPHA
3 FORMAT('GRID: NOMBRE DE POINTS: ',I3,' COEFFICIENT ALPHA: ',F7.3)
ALPHA1=ALPHA
ALPHA2=ALPHA
N05=(N1+1)/2
N06=N05+1
COEF=DEXP(ALPHA1*(1.-N05))
DO J=2,N05
YPAD(J)=.5*(DEXP(ALPHA1*(J-N05))-COEF)/(1.-COEF)
ENDDO
COEF=DEXP(ALPHA2*(N05-N1))
DO J=N06,N1-1
  YPAD(J)=(1.-.5*(DEXP(ALPHA2*(N05-J))-COEF)/(1.-COEF))
ENDDO
YPAD(1)=-YPAD(2)
YPAD(N1)=2.-YPAD(N1-1)
END SUBROUTINE

