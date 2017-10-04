
    program gorizont_map
    implicit none
     
    character(50):: str
     character(20):: s1,s2,s3
     integer:: i,j,n
     real,dimension(200):: p,d,t,cond1,s
     real:: z,x,y,tz,sz
     
     open(10,file='cast.cfg')
     read(10,*) z
     open(30,file='map_05_ts_gel.dat') !05- the nessesary depth for creating map
     !Remember to create basemap for this isobath to extrapolate this data

     do while(.not.eof(10))
      read(10,*) str
       
      open(20,file=trim(str))
      print*,str
 
      do i=1,9
         read(20,'(1x)')
      end do
      
      read(20,*) s1,s2,s3,y
      read(20,*) s1,s2,s3,x 
      
      do i=1,18
         read(20,'(1x)')
      end do
      
      n=0
     
      do while(.not.eof(20))
         n=n+1
         read(20,*) p(n),d(n),t(n),cond1(n),cond2(n),s(n)
      end do

      do i=1,n-1
       if(z>=d(i).and.z<d(i+1)) then
           tz=t(i)+(t(i+1)-t(i))/(d(i+1)-d(i))*(z-d(i))
           sz=s(i)+(s(i+1)-s(i))/(d(i+1)-d(i))*(z-d(i))           
       end if
      end do
      
      if(z<d(n)) then
       write(30,'(2f9.5,2f6.2)') x,y,tz,sz
      end if
!    write(30,'(f5.2,1x,f5.2,1x,f5.2,1x,f5.2)') p,d,t,s
!      end do
      close(20)
     end do
     
    end program gorizont_map

