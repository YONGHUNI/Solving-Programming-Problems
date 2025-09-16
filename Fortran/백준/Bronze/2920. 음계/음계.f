INTEGER::A(8)
READ(*,*)(A(K),K=1,8)
I=0
DO L=1,7
IF(A(L)>A(L+1)) EXIT
IF(L==7) I=1
ENDDO
DO L=1,7
IF(A(L)<A(L+1)) EXIT
IF(L==7) I=2
ENDDO
IF (I==1) PRINT'(A)',"ascending"
IF (I==2) PRINT'(A)',"descending"
IF (I==0) PRINT'(A)',"mixed"
END