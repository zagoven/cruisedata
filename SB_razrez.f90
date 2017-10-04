program razrez
implicit none
    character(50):: str ! string
    !character(20):: s1,s2,s3,s4 ! spaces in file names
    integer:: i,j,n ! counters for cycles
    integer,dimension(20):: m 
    real,dimension(20,5000):: p,d,t,s ! pressure,depth,
    !temperature and salinyty arrays
    real(8),dimension(40):: x,y,gd,ga,l 
    !x-longitute,y-latitude,gd-geodesic distance,ga - geodesic azimut,
    !l-distance from the first station

  open(10,file='razrez.cfg')
  n=0
  m=0
  l=0
  do while(.not.eof(10))
   n=n+1
   read(10,*) str  
   open(20,file=trim(str))
  ! print*,str
 
      do i=1,2 
         read(20,'(1x)') !miss the head of the file
      end do
      
      read(20,*) y(n)
      read(20,*) x(n) 
      
      do i=1,4 !miss again
         read(20,'(1x)')
      end do
      
      do while(.not.eof(20))
       m(n)=m(n)+1
       read(20,*) p(n,m(n)),d(n,m(n)),t(n,m(n)),s(n,m(n))
      !print*, d(n,m(n)),t(n,m(n)),s(n,m(n))       
      end do
      close(20)
  end do  
  
    do i=2,n
        call geodesic_dist (x(i),y(i),x(i-1),y(i-1),1,gd(i),ga(i))
        !calculation the distance between two points from coordinates 
        l(i)=l(i-1)+gd(i)
    print*,gd(i),ga(i),l(i)
    end do
    l(i)=l(i)/1000
    
    open(15,file='raz_coor.dat') !the file with coordinates of stations
    do i=1,n
        write(15,*) x(i),y(i),i
    end do

    open(20,file='raz_tz.bln')!create the base map (bottom blank) for Surfer 
    write(20,*) n+3,0
    write(20,*) 0,-d(1,1)
    write(20,*) 0,-d(1,m(1))
    do i=2,n
     write(20,*) l(i)/1000,-d(i,m(i))
    end do
    write(20,*) l(n)/1000,-d(n,1)
    write(20,*) 0,-d(1,1)
    
    
    open(30,file='raz_ts.dat') ! the main file with output data
    do i=1,n
    do j=1,m(i)
        write(30,*) l(i)/1000,-d(i,j),t(i,j),s(i,j)
    end do
    end do
    end program razrez
