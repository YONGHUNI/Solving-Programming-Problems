INTEGER::A
READ*,N
DO A=1,N
READ*,I,J 
M=I*J
do while(J/=0)
K=J
J=mod(I,J)
I=K
enddo
L=abs(I)
PRINT'(I0)',M/L
ENDDO
END