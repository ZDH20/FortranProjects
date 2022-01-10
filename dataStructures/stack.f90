!------------------------------------------------------------------------------
! Dynamic Stack 
!------------------------------------------------------------------------------
!
! MODULE: stackmod
!
!> @author
!> Zachary Haskins}
!
! DESCRIPTION: 
!> Provides support for a dynamic stack
!
! REVISION HISTORY:
! 1/1/2022  - Initial Version       - 1.0
! 1/10/2022 - Fixed memory leaks    - 1.0.1
!------------------------------------------------------------------------------
module stackmod
    implicit none
    private

    ! derrived types
    public::stack
    
    ! public subroutines
    public::push, destroy, pop 

    ! public functions
    public::top, empty, size

    ! types
    type node
        integer, allocatable::data
        class(node), pointer::next => null()
    end type node

    type stack
        class(node), pointer::head => null()
        integer::size
    end type stack

    ! interfaces
    interface push
        procedure::push_node
    end interface push

    interface empty
        procedure::check_if_empty
    end interface empty

    interface size
        procedure::get_size
    end interface size

    interface top
        procedure::get_top
    end interface top

    interface pop
        procedure::pop_node
    end interface pop

    interface destroy
        procedure::destroy_stack
    end interface destroy

    interface node
        procedure n_constr
    end interface node

contains

    ! public subroutines
    pure subroutine push_node(this, data)
        implicit none
        class(stack), intent(inout)::this
        integer, intent(in)::data
        class(node), pointer::temp 
        if(.not. associated(this%head)) then
            allocate(this%head, source = n_constr(data))
        else
            temp => this%head
            allocate(this%head, source = n_constr(data))
            this%head%next => temp 
        end if
        this%size = this%size + 1
    end subroutine push_node

    pure subroutine pop_node(this)
        implicit none
        class(stack), intent(inout)::this
        class(node), pointer::temp
        if(associated(this%head)) then
            temp => this%head
            this%head => this%head%next
            deallocate(temp)
            this%size = this%size - 1
        end if
    end subroutine pop_node

    pure subroutine destroy_stack(this)
        implicit none
        class(stack), intent(inout)::this
        class(node), pointer::temp 
        do while(associated(this%head))
            temp => this%head
            this%head => this%head%next
            deallocate(temp)
        end do
        this%size = 0
    end subroutine destroy_stack

    ! public functions
    logical function check_if_empty(this)
        implicit none
        class(stack), intent(in)::this
        check_if_empty = .true.
        if(associated(this%head)) then
            check_if_empty = .false.
        end if 
    end function check_if_empty

    integer function get_top(this)
        implicit none
        class(stack), intent(in)::this
        get_top = -999 
        if(associated(this%head)) then
            get_top = this%head%data
        end if
    end function get_top

    integer function get_size(this)
        implicit none
        class(stack), intent(in)::this
        get_size = this%size
    end function get_size

    ! private subroutines

    ! private functions
    pure function n_constr(data)
        integer, intent(in)::data
        type(node)::n_constr
        allocate(n_constr%data, source = data) 
    end function n_constr

end module stackmod
