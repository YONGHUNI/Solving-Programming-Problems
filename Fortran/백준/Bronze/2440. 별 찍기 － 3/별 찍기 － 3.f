READ*,I
DO L=I,1,-1
DO M=1,L
WRITE(*, fmt="(a)", advance="no") '*'
ENDDO
PRINT*,''
ENDDO
END