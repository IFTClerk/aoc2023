      PROGRAM DAY01_2
      IMPLICIT NONE

      CHARACTER line*100
      INTEGER c, cal, can(100)
      INTEGER lline, i, j, ios
      CHARACTER dict(9)*5

      COMMON dict
      DATA dict /'one','two','three',
     +           'four','five','six',
     +           'seven','eight','nine'/
      cal = 0

      OPEN(11, FILE='input.txt', STATUS='OLD')
      DO
        can = 0
        j = 1

        READ(11, '(A)', END=100) line
        lline=LEN(line)
        DO i=1,lline

          c = -1

          IF (line(i:i).EQ.' ') GOTO 20
          IF (line(i:i).EQ.'o') THEN
            CALL LOOKUP(line(i:i+2),1,c)
          ELSE IF (line(i:i).EQ.'t') THEN
            CALL LOOKUP(line(i:i+2),2,c)
            CALL LOOKUP(line(i:i+4),3,c)
          ELSE IF (line(i:i).EQ.'f') THEN
            CALL LOOKUP(line(i:i+3),4,c)
            CALL LOOKUP(line(i:i+3),5,c)
          ELSE IF (line(i:i).EQ.'s') THEN
            CALL LOOKUP(line(i:i+2),6,c)
            CALL LOOKUP(line(i:i+4),7,c)
          ELSE IF (line(i:i).EQ.'e') THEN
            CALL LOOKUP(line(i:i+4),8,c)
          ELSE IF (line(i:i).EQ.'n') THEN
            CALL LOOKUP(line(i:i+3),9,c)
          ELSE
            READ(line(i:i), '(I1)', IOSTAT=ios) c
            IF (ios.GT.0) GOTO 20
          END IF

          IF (c.GE.0) THEN
            can(j) = c
            j = j + 1
          END IF

20      CONTINUE
        END DO

C       WRITE(6,*) can(1)*10 + can(j-1) * 1
        cal = cal + can(1)*10 + can(j-1) * 1
      END DO

100   CLOSE (11)

      WRITE(6,*) cal
      END PROGRAM

      SUBROUTINE LOOKUP(word,i,c)
      IMPLICIT NONE

      CHARACTER word*(*)
      INTEGER i,c
      CHARACTER dict(9)*5

      COMMON dict

      IF (TRIM(dict(i)).EQ.word) THEN
C       WRITE(6,*) word
        c = i
      END IF
      END SUBROUTINE
