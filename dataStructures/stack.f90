 MODULE STACKMOD
    IMPLICIT NONE
    PRIVATE

    ! DERRIVED TYPES
    PUBLIC::STACK
    
    ! PUBLIC SUBROUTINES
    PUBLIC::PUSH, DESTROY, PRINT, POP 

    ! PUBLIC FUNCTIONS
    PUBLIC::TOP, EMPTY

    ! TYPES
    TYPE NODE
        INTEGER, ALLOCATABLE::DATA
        CLASS(NODE), POINTER::NEXT => NULL()
    END TYPE NODE

    TYPE STACK
        CLASS(NODE), POINTER::HEAD => NULL()
        INTEGER::SIZE
    END TYPE STACK

    ! INTERFACES
    INTERFACE PUSH
        PROCEDURE::PUSH_NODE
    END INTERFACE PUSH

    INTERFACE EMPTY
        PROCEDURE::CHECK_IF_EMPTY
    END INTERFACE EMPTY

    INTERFACE TOP
        PROCEDURE::GET_TOP
    END INTERFACE TOP

    INTERFACE POP
        PROCEDURE::POP_NODE
    END INTERFACE POP

    INTERFACE PRINT
        PROCEDURE::PRINT_STACK
    END INTERFACE PRINT

    INTERFACE DESTROY
        PROCEDURE::DESTROY_STACK
    END INTERFACE DESTROY

    INTERFACE NODE
        PROCEDURE N_CONSTR
    END INTERFACE NODE

CONTAINS

    ! PUBLIC SUBROUTINES
    PURE SUBROUTINE PUSH_NODE(THIS, DATA)
        IMPLICIT NONE
        CLASS(STACK), INTENT(INOUT)::THIS
        INTEGER, INTENT(IN)::DATA
        CLASS(NODE), POINTER::TEMP 
        IF(.NOT. ASSOCIATED(THIS%HEAD)) THEN
            ALLOCATE(THIS%HEAD, SOURCE = N_CONSTR(DATA))
        ELSE
            TEMP => THIS%HEAD
            ALLOCATE(THIS%HEAD, SOURCE = N_CONSTR(DATA))
            THIS%HEAD%NEXT => TEMP 
        END IF
    END SUBROUTINE PUSH_NODE

    PURE SUBROUTINE POP_NODE(THIS)
        IMPLICIT NONE
        CLASS(STACK), INTENT(INOUT)::THIS
        CLASS(NODE), POINTER::TEMP
        IF(ASSOCIATED(THIS%HEAD)) THEN
            TEMP => THIS%HEAD
            THIS%HEAD => THIS%HEAD%NEXT
            DEALLOCATE(TEMP%DATA)
        END IF
    END SUBROUTINE POP_NODE

    SUBROUTINE PRINT_STACK(THIS)
        CLASS(STACK), INTENT(IN)::THIS
        CLASS(NODE), POINTER::NAV
        NAV => THIS%HEAD
        DO WHILE(ASSOCIATED(NAV))
            PRINT*, NAV%DATA
            NAV => NAV%NEXT
        END DO
    END SUBROUTINE PRINT_STACK

    PURE SUBROUTINE DESTROY_STACK(THIS)
        IMPLICIT NONE
        CLASS(STACK), INTENT(INOUT)::THIS
        CLASS(NODE), POINTER::TEMP 
        DO WHILE(ASSOCIATED(THIS%HEAD))
            TEMP => THIS%HEAD
            THIS%HEAD => THIS%HEAD%NEXT
            DEALLOCATE(TEMP%DATA)
        END DO
    END SUBROUTINE DESTROY_STACK

    ! PUBLIC FUNCTIONS
    LOGICAL FUNCTION CHECK_IF_EMPTY(THIS)
        IMPLICIT NONE
        CLASS(STACK), INTENT(IN)::THIS
        CHECK_IF_EMPTY = .TRUE.
        IF(ASSOCIATED(THIS%HEAD)) THEN
            CHECK_IF_EMPTY = .FALSE.
        END IF 
    END FUNCTION CHECK_IF_EMPTY

    INTEGER FUNCTION GET_TOP(THIS)
        IMPLICIT NONE
        CLASS(STACK), INTENT(IN)::THIS
        GET_TOP = -999 
        IF(ASSOCIATED(THIS%HEAD)) THEN
            GET_TOP = THIS%HEAD%DATA
        END IF
    END FUNCTION GET_TOP

    ! PRIVATE SUBROUTINES

    ! PRIVATE FUNCTIONS
    PURE FUNCTION N_CONSTR(DATA)
        INTEGER, INTENT(IN)::DATA
        TYPE(NODE)::N_CONSTR
        ALLOCATE(N_CONSTR%DATA, SOURCE = DATA) 
    END FUNCTION N_CONSTR

END MODULE STACKMOD
