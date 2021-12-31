module c_graphmod
    implicit none
    private

    ! derrived types
    public::graph

    ! public subroutines
    public::newvertex, print
    
    ! public functions

    ! types
    type vertex
        character, allocatable::value
        class(vertex), pointer::next => null()
    end type vertex

    type graph
        class(vertex), pointer::head => null()
        class(vertex), pointer::tail => null()
    end type graph

    ! interfaces

    interface newvertex
        module procedure::new_vert
    end interface newvertex

    interface print
        module procedure::print_graph
    end interface print

contains

    ! public subroutines
    
    pure subroutine new_vert(self, value)
        implicit none
        class(graph), intent(inout)::self
        character, intent(in)::value
        class(vertex), allocatable::new_v
        if(.not. associated(self%head)) then
            ! allocate(self%head, source = v_constr(value))
            allocate(new_v%value, source = value)
            allocate(self%head, source = new_v)
            self%tail => self%head
        ! else
        !     allocate(self%tail%next, source = v_constr(value))
        !     self%tail => self%tail%next
        end if
    end subroutine new_vert

    subroutine print_graph(self)
        implicit none
        class(graph), intent(inout)::self
        class(vertex), pointer::nav
        nav => self%head
        do while(associated(nav))
            print *, nav%value
            nav => nav%next
        end do
    end subroutine print_graph

    ! pure function v_constr(item)
    !     character, intent(in)::item
    !     type(vertex)::v_constr
    !     allocate(v_constr%value, source = item)
    ! end function v_constr

    ! public functions

end module c_graphmod
