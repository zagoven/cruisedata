    program cast

     implicit none
     character(50):: str ! creating strings type
     character(2):: stp,st ! creating symbol type
     integer:: i,j,n
     real:: p,d,t,cond1,cond2,s 
     !p-pressure, d - depth, t-temperature,cond1 - conductivity1 
     !from CTD, cond2 - conductivity2 fromCTD
     
     open(10,file='cast.cfg') !reading the names of files for analysis
     read(10,*) stp,st !readind the symbol type
     open(40,file='tz'//st//'.dat')

     do while(.not.eof(10)) !reading till end of the file
      read(10,*) str

      i=index(str,'_')
      j=index(str,'.')
      
      
      open(20,file=trim(str))
      open(30,file=str(i+1:j-1)//'.dat')
     
      do i=1,29 
         read(20,'(1x)')! miss the head of CSV file
      end do
     
      n=0
     
      do while(.not.eof(20))
         n=n+1
         read(20,*) p,d,t,cond1,cond2,s
         write(30,'(f5.2,1x,f5.2,1x,f5.2,1x,f5.2)') p,d,t,s
         if(str(20:21)==st.or.str(20:21)==stp) then
            write(40,'(a,1x,f6.2,1x,f5.2,1x,f5.2)') str(17:18),-p,t,s 
         end if
         
      end do
      
      close(20)
      close(30)
      
     end do
     
    end program cast

