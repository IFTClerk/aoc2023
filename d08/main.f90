MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  INTEGER, PARAMETER :: np = 200
  INTEGER, DIMENSION(np) :: primes

  TYPE node
     CHARACTER(LEN=3) :: name
     CHARACTER(LEN=3) :: left
     CHARACTER(LEN=3) :: right
  END TYPE node

CONTAINS

SUBROUTINE READMAP(map, nl)
  CHARACTER l*256
  TYPE(node), DIMENSION(:), ALLOCATABLE :: map
  INTEGER ios, nl, i, j

  ! read in map
  DO i=1,nl
     READ(10, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT

     ! replace stupid symbols
     DO j=1,LEN_TRIM(l)
        IF (l(j:j).EQ.'=' .OR. l(j:j).EQ.'(' .OR. l(j:j).EQ.')') &
             l(j:j)=''
     END DO

     READ(l, *) map(i)
  END DO
END SUBROUTINE READMAP

ELEMENTAL LOGICAL FUNCTION ENDWITH(l, c)
  CHARACTER(LEN=*), INTENT(IN) :: l, c
  endwith = INDEX(l, c, BACK=.TRUE.).EQ.LEN(l)
END FUNCTION ENDWITH

SUBROUTINE READPRIMES(primes)
  INTEGER, DIMENSION(np) :: primes

  OPEN(20, FILE='primes.txt', status='OLD')
  READ(20, *) primes
  CLOSE(20)
END SUBROUTINE READPRIMES

! Fear me
INTEGER(KIND=INT64) FUNCTION LCM(a, b)
  INTEGER, DIMENSION(:), ALLOCATABLE :: fs
  INTEGER(KIND=INT64) a, b, ta, tb
  INTEGER p, i, nf

  ! how else would you take log base 2?
  ta = a
  tb = b
  nf = CEILING(LOG(MAX(REAL(ta), REAL(tb))) / LOG(2.))
  ALLOCATE(fs(nf))
  nf = 0
  fs = 1
  DO i=1,np
     p = primes(i)
     IF (p.GT.ta .OR. p.GT.tb) EXIT
     DO
        IF (MODULO(ta, p).EQ.0 .AND. MODULO(tb, p).EQ.0) THEN
           ta = ta / p
           tb = tb / p
           nf = nf + 1
           fs(nf) = p
        ELSE
           EXIT
        END IF
     END DO
  END DO
  lcm = PRODUCT(fs) * ta * tb
END FUNCTION LCM

INTEGER(KIND=INT64) FUNCTION LCMARR(arr)
  INTEGER(KIND=INT64), DIMENSION(:) :: arr
  INTEGER na, i

  na = SIZE(arr)

  lcmarr = arr(1)
  DO i=1,na-1
     lcmarr = LCM(lcmarr, arr(i+1))
  END DO
END FUNCTION LCMARR

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER l*400 ! PAIN
  CHARACTER(LEN=3) :: nn
  CHARACTER, DIMENSION(:), ALLOCATABLE :: ins
  INTEGER ios, s, nl, ni, i, j
  LOGICAL, DIMENSION(:), ALLOCATABLE :: inn
  TYPE(node), DIMENSION(:), ALLOCATABLE :: map
  TYPE(node) cnode

  s = 0
  nl = NLINES(fin) - 2
  OPEN(10, FILE=fin, STATUS='OLD')
  READ(10, "(A)", IOSTAT=ios) l
  BACKSPACE(10)

  ! back in my day I had to allocate all my ram
  ni = LEN_TRIM(l)
  ! uphill both ways
  ALLOCATE(ins(ni))
  ALLOCATE(inn(ni))
  READ(10, "(*(A))", IOSTAT=ios) ins
  inn = ins.EQ.'L'
  ! and deallocate them
  DEALLOCATE(ins)

  ! skip newline
  READ(10, *)

  ALLOCATE(map(nl))
  CALL READMAP(map, nl)
  CLOSE(10)

  cnode = map(FINDLOC([(map(i)%name, i=1,nl)], 'AAA', DIM=1))
  i = 1
  DO
     s = s + 1

     IF (inn(1)) THEN
        nn  = cnode%left
     ELSE IF (.NOT.inn(1)) THEN
        nn = cnode%right
     END IF

     DO j=1,nl
        IF (map(j)%name.EQ.nn) THEN
           i=j
           EXIT
        ELSE
           CONTINUE
        END IF
     END DO

     cnode = map(i)

     ! are we at the end?
     IF (cnode%name.EQ.'ZZZ') EXIT
     ! otherwise get next instruction
     inn = CSHIFT(inn, 1)
  END DO

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER l*400 ! PAIN
  CHARACTER(LEN=3) :: nn
  CHARACTER, DIMENSION(:), ALLOCATABLE :: ins
  ! INTEGER, DIMENSION(:), ALLOCATABLE:: inn, oinn
  LOGICAL, DIMENSION(:), ALLOCATABLE :: inn, oinn
  INTEGER(KIND=INT64), DIMENSION(:), ALLOCATABLE :: pathl
  INTEGER ios, nl, ni, i, j, k, na
  INTEGER(KIND=INT64) s
  LOGICAL, DIMENSION(:), ALLOCATABLE :: msk
  TYPE(node), DIMENSION(:), ALLOCATABLE :: map
  TYPE(node), DIMENSION(:), ALLOCATABLE :: cnodes
  TYPE(node) cnode

  s = 0
  nl = NLINES(fin) - 2
  OPEN(10, FILE=fin, STATUS='OLD')
  READ(10, "(A)", IOSTAT=ios) l
  BACKSPACE(10)

  ! back in my day I had to allocate all my ram
  ni = LEN_TRIM(l)
  ! uphill both ways
  ALLOCATE(ins(ni))
  ALLOCATE(inn(ni))
  READ(10, "(*(A))", IOSTAT=ios) ins
  inn = ins.EQ.'L'
  ! and deallocate them
  DEALLOCATE(ins)

  ! skip newline
  READ(10, *)

  ALLOCATE(map(nl))
  CALL READMAP(map, nl)
  CLOSE(10)

  ! this block means
  ! list(filter(lambda x: x.name.endswith('A'), map))
  ALLOCATE(msk(nl))
  msk = ENDWITH([(map(i)%name, i=1,nl)], 'A')
  na = COUNT(msk)
  ALLOCATE(cnodes(na))
  cnodes = PACK(map, msk, cnodes)
  DEALLOCATE(msk)
  ! PRINT*, [(cnodes(i)%name, i=1,na)]

  ALLOCATE(pathl(na))
  ALLOCATE(oinn(ni))
  oinn = inn
  DO k=1,na
     cnode = cnodes(k)
     pathl(k) = 0
     ! reset instruction at every loop
     inn = oinn
     i = 1
     DO
        pathl(k) = pathl(k) + 1

        IF (inn(1)) THEN
           nn  = cnode%left
        ELSE IF (.NOT.inn(1)) THEN
           nn = cnode%right
        END IF

        DO j=1,nl
           IF (map(j)%name.EQ.nn) THEN
              i=j
              EXIT
           ELSE
              CONTINUE
           END IF
        END DO

        cnode = map(i)
        ! PRINT *, cnode%name

        ! are we at the end?
        IF (ENDWITH(cnode%name, 'Z')) EXIT
        ! otherwise get next instruction
        inn = CSHIFT(inn, 1)
     END DO
  END DO

  CALL READPRIMES(primes)
  s = LCMARR(pathl)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2
END MODULE MOD

PROGRAM MAIN
  USE MOD

  ! CALL PART1()
  CALL PART2()
END PROGRAM MAIN
