INTEGER::A(10),B(10)
READ(*,*)(A(K),K=1,10)
READ(*,*)(B(K),K=1,10)
I=0
J=0
L=0
DO K=1,10
IF(A(K)==B(K)) THEN
I=I+1
J=J+1
ELSEIF(A(K)>B(K))THEN
I=I+3
L=1
ELSEIF(A(K)<B(K))THEN
J=J+3
L=2
ENDIF
ENDDO
PRINT'(I0," ",I0)',I,J
IF(I>J.OR.(I==J.AND.L==1))PRINT'(A)',"A"
IF(I<J.OR.(I==J.AND.L==2))PRINT'(A)',"B"
IF(I==J.AND.L==0)PRINT'(A)',"D"
END