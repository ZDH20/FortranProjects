!------------------------------------------------------------------------------
! Adjacency List Graph
!------------------------------------------------------------------------------
!
! MODULE: graphmod
!
!> @author
!> zachary haskins}
!
! DESCRIPTION: 
!> provides support for creating graphs using the adjacency list method
!
! REVISION HISTORY:
! 1/1/2022 - initial version
! 1/2/2022 - added support for adding undirected edges. cleaned up code. - 1.0.1
!------------------------------------------------------------------------------
module graphmod
    implicit none
    private

    ! public derrived types
    public::graph

    ! private derrived types
    private::edge
    private::vertex

    ! public subroutines
    public::addvertex
    public::addedge
    public::print
    public::destroy

    ! public functions
    public::size
    public::costof

    ! type definitions
    type edge
        integer, allocatable::data
        real, allocatable::cost
        class(edge), pointer::next => null()
    end type edge

    type vertex
        integer, allocatable::data
        class(edge), pointer::neighbors => null()
        class(vertex), pointer::next => null()
    end type vertex

    type graph
        class(vertex), pointer::head => null()
        class(vertex), pointer::tail => null()
        integer::size = 0
    end type graph

    ! public interfaces
    interface addvertex
        procedure::add_vertex_to_graph
    end interface addvertex

    interface addedge
        procedure::add_edge_to_graph
    end interface addedge

    interface print
        procedure::print_graph
    end interface print

    interface destroy
        procedure::destroy_graph
    end interface destroy

    interface size
        procedure::get_graph_size
    end interface size

    interface costof
        procedure::get_cost_of_edge
    end interface costof

     ! private interfaces
     interface v_constr
         procedure::vertex_constructor
     end interface v_constr

     interface e_constr
         procedure::edge_constructor
     end interface e_constr

contains

    ! public subroutines
    pure subroutine add_vertex_to_graph(this, data)

        implicit none

        class(graph), intent(inout)::this
        integer, intent(in)::data

        if(.not. associated(this%head)) then
            allocate(this%head, source = v_constr(data))
            this%tail => this%head
        else
            allocate(this%tail%next, source = v_constr(data))
            this%tail => this%tail%next
        end if

        this%size = this%size + 1

    end subroutine add_vertex_to_graph

    recursive pure subroutine add_edge_to_graph(this, source, destination, cost, undirected)

        implicit none

        class(graph), intent(inout)::this
        integer, intent(in)::source, destination
        real, intent(in)::cost
        logical, optional, intent(in)::undirected

        class(vertex), pointer::nav
        class(edge), pointer::temp
        class(vertex), pointer::save
        logical::founddestination

        founddestination = .false.
        save => null()
        nav => this%head

        do while(associated(nav))
            if(nav%data .eq. source) save => nav
            if(nav%data .eq. destination) founddestination = .true.
            nav => nav%next
        end do

        if(.not. associated(save)) then
            call add_vertex_to_graph(this, source)
            save => this%tail
        end if

        if(.not. founddestination) call add_vertex_to_graph(this, destination)

        if(associated(save%neighbors)) then
            temp => save%neighbors
            do while(associated(temp%next))
                temp => temp%next
            end do
            allocate(temp%next, source = e_constr(destination, cost))
        else
            allocate(save%neighbors, source = e_constr(destination, cost))
        end if

        if(present(undirected)) then
            if(undirected .eqv. .true.) call addedge(this, destination, source, cost, .false.)
        end if


    end subroutine add_edge_to_graph

    subroutine print_graph(this)

        implicit none

        class(graph), intent(in)::this
        class(vertex), pointer::nav
        class(edge), pointer::temp

        nav => this%head

        do while(associated(nav))
            print*, nav%data, '---'
            temp => nav%neighbors
            do while(associated(temp))
                print*, '               ', '->', temp%data, '(', temp%cost, ')'
                temp => temp%next
            end do
            nav => nav%next
        end do

    end subroutine print_graph

    pure subroutine destroy_graph(this)

        implicit none

        class(graph), intent(inout)::this
        class(vertex), pointer::temp
        class(edge), pointer::temp2
        class(edge), pointer::temp3

        do while(associated(this%head))
            temp => this%head
            temp2 => this%head%neighbors
            do while(associated(temp2))
                temp3 => temp2
                temp2 => temp2%next
                deallocate(temp3%data)
                deallocate(temp3%cost)
            end do
            this%head => this%head%next
            deallocate(temp%data)
        end do

    end subroutine destroy_graph

    ! public functions
    integer function get_graph_size(this)

        implicit none
    
        class(graph), intent(in)::this
        get_graph_size = this%size
    
    end function get_graph_size

    real function get_cost_of_edge(this, source, destination)

    implicit none

        class(graph), intent(in)::this
        integer, intent(in)::source, destination
        class(vertex), pointer::nav
        class(edge), pointer::p => null()

        get_cost_of_edge = -999                     ! if anything fails

        if(.not. associated(this%head)) return      ! graph is empty

        nav => this%head

        do while(associated(nav))
            if(nav%data .eq. source) exit
            nav => nav%next
        end do

        if(.not. associated(nav)) return

        p => nav%neighbors

        do while(associated(p))
            if(p%data .eq. destination) then
                get_cost_of_edge = p%cost
                return
            end if
            p => p%next
        end do

    end function get_cost_of_edge

    ! private subroutines

    ! private functions
    pure function vertex_constructor(data)

        integer, intent(in)::data
        type(vertex)::vertex_constructor

        allocate(vertex_constructor%data, source = data)

    end function vertex_constructor

    pure function edge_constructor(destination, cost)
        
        integer, intent(in)::destination
        real, intent(in)::cost
        type(edge)::edge_constructor
        
        allocate(edge_constructor%data, source = destination)
        allocate(edge_constructor%cost, source = cost)
    
    end function edge_constructor

end module graphmod
