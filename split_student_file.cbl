IDENTIFICATION DIVISION.
PROGRAM-ID. F08-SPLIT-STUDENT-FILE.
*> Splits student master into UG and PG/PHD output files.
*> Record layout: ID(4) NAME(15) DEGREE(3) DEPT(4).

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT STUDENT-FILE ASSIGN TO "students.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS WS-IN-STAT.
    SELECT UG-FILE ASSIGN TO "undergrad.dat"
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT PG-FILE ASSIGN TO "graduate.dat"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD  STUDENT-FILE.
01  STU-REC.
    05  STU-ID          PIC X(4).
    05  STU-NAME        PIC X(15).
    05  STU-DEGREE      PIC X(3).
    05  STU-DEPT        PIC X(4).

FD  UG-FILE.
01  UG-REC              PIC X(26).

FD  PG-FILE.
01  PG-REC              PIC X(26).

WORKING-STORAGE SECTION.
01  WS-IN-STAT          PIC X(2) VALUE SPACES.
01  WS-EOF              PIC X(1) VALUE "N".
01  WS-UG-CNT           PIC 9(5) VALUE 0.
01  WS-PG-CNT           PIC 9(5) VALUE 0.
01  WS-BAD-CNT          PIC 9(5) VALUE 0.

PROCEDURE DIVISION.
0000-MAIN.
    OPEN INPUT STUDENT-FILE
    IF WS-IN-STAT NOT = "00"
        DISPLAY "CANNOT OPEN STUDENT FILE: " WS-IN-STAT
        STOP RUN
    END-IF

    OPEN OUTPUT UG-FILE PG-FILE

    PERFORM UNTIL WS-EOF = "Y"
        READ STUDENT-FILE
            AT END
                MOVE "Y" TO WS-EOF
            NOT AT END
                EVALUATE STU-DEGREE
                    WHEN "UG "
                        WRITE UG-REC FROM STU-REC
                        ADD 1 TO WS-UG-CNT
                    WHEN "PG "
                    WHEN "PHD"
                        WRITE PG-REC FROM STU-REC
                        ADD 1 TO WS-PG-CNT
                    WHEN OTHER
                        ADD 1 TO WS-BAD-CNT
                END-EVALUATE
        END-READ
    END-PERFORM

    CLOSE STUDENT-FILE UG-FILE PG-FILE
    DISPLAY "UG: " WS-UG-CNT
            " PG/PHD: " WS-PG-CNT
            " INVALID: " WS-BAD-CNT
    STOP RUN.
