      PROGRAM DAY01_1
      IMPLICIT NONE

      CHARACTER line*100
      INTEGER c, cal, can(100)
      INTEGER lline, i, j, ios

      cal = 0

      OPEN(11, FILE='input.txt', STATUS='OLD')
      DO
        can = 0
        j = 1

        READ(11, '(A)', END=100) line
        lline=LEN(line)
        DO i=1,lline

          IF (line(i:i).EQ.' ') THEN
            GOTO 10
          END IF

          READ(line(i:i), '(I1)', IOSTAT=ios) c
          IF (ios.EQ.0) THEN
C           WRITE(6, *) c
            can(j) = c
            j = j + 1
          END IF
          
10      CONTINUE
        END DO

        cal = cal + can(1)*10 + can(j-1) * 1
      END DO

100   CLOSE (11)

      WRITE(6,*) cal
      END PROGRAM
