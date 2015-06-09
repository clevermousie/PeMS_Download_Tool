!  refined_pems_download.f90 
!
!  FUNCTIONS:
!  refined_pems_download - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: refined_pems_download
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
module file_variables
    integer ::fileid_par=11
    integer ::fileid_sta=13
    integer ::fileid_output=15
    integer ::fileid_log=7
    character(len=60) ::filename_par="parameter.txt"
    character(len=60) ::filename_sta="STA_ID.txt"
    character(len=60) ::filename_output="batch_file.bat"
    character(len=60) ::filename_log="log.txt"
    
end module
module variables
    integer i,j,k
    integer status_1,status_2,essen_avail
    character(len=100) ::input_info
    integer command_ind
    character(len=30) ::par(20)
    integer ::par_value(20)
    integer ::par_avail(20)=0
    integer par_type
    integer year, start_month,start_day, end_month,end_day
    logical ::day_on(0:7)
    character day_of_week
    character(len=100) ::day_of_week_str
    character(len=255) ::download_path
    integer ::sta_id(100)
    real ::mp(100)
    integer sta_num
    character(len=8) ::sta_id_str(100)
    character(len=30) ::second1_str,second2_str
    integer(kind=4) ::base_second,second1,second2
    integer collect_per_val
    integer collect_times
    integer day1,day2
    integer output_s_month,output_s_day,output_e_month,output_e_day
    character(len=30) ::tt_seg_id
    integer max_duration
    character(len=10) ::output_file_num_str
end module
    program refined_pems_download
    use file_variables
    use variables
    implicit none
    integer, external ::cal_day_of_year
    integer, external ::collect_separate
    
    open (fileid_log,file=filename_log)
    !call output("READING_PARAMETERS")
!********************************************READ IN PARAMETER FILE****************************   
    write(*,*) " IF PARAMETER.TXT FILE IS AVAILABLE, PRESS ANY KEY TO GO ON"
    read(*,*)
    open (fileid_par,file=filename_par)
    status_1=0
    do while (.true.)
        read(fileid_par,*,iostat=status_1) input_info
        if (status_1.lt.0) exit
        if (input_info(1:1).ne."#") then
            command_ind=scan(input_info,"=")
            select case (input_info(1:command_ind-1))
            case ("FILE_PATH")
                par_type=1
            case ("DATA_TYPE")
                par_type=2
            case ("STATION_CHAR")  
                par_type=3
            case ("YEAR")
                par_type=4
            case ("START_MONTH")
                par_type=5
            case ("START_DAY")
                par_type=6
            CASE ("END_MONTH")
                par_type=7
            CASE ("END_DAY")
                par_type=8
            case ("GRAN")
                par_type=9
            case ("DAYS")
                par_type=10
            case ("STATION_ID")
                PAR_TYPE=11
            case ("COLLECT_PERIOD")
                par_type=12
            case ("SEGMENT_ID")
                par_type=13
            case default 
                par_type=100
                
            end select
            if (par_type==100) then
                write(*,*) "ERROR IN PARAMETER FILE", input_info(1:command_ind-1)
                stop
            end if
            par_avail(par_type)=1
            par(par_type)=input_info(command_ind+1:len_trim(input_info))
            
            write(*,*) par(par_type)
        end if
    end do
    write (*,*) "***************PARAMETER FILE IS DONE************************"
!******************************************* PROCESS GENERAL INFORMATION ****************************************
!******************************************* START & END TIME ***************************************************
    essen_avail=par_avail(1)*par_avail(2)*par_avail(4)*par_avail(5)*par_avail(6)*par_avail(7)*par_avail(8)*par_avail(9)*par_avail(10)&
    *par_avail(12)
    if (essen_avail==0) then
        write(*,*) "REQUIRED PARAMETER INFORMATION MISSING"
        stop
    endif
    read(par(4),"(I4)") year
    read(par(5),"(I2)") start_month
    read(par(6),"(I2)") start_day
    read(par(7),"(I2)") end_month
    read(par(8),"(I2)") end_day
    WRITE (*,*) year,start_month,start_day,end_month,end_day
    select case (year)
    case (2012)
        base_second=56998400-366*86400
    case (2013)
        base_second=56998400
    case (2014) 
        base_second=56998400+365*86400
    end select
    read (par(12)(2:3),"(I2)") collect_per_val
    write (*,*) "collect period value:", collect_per_val
!********************************************DAY OF WEEK**********************************************************
    day_of_week_str=" "
    do i = 0 ,7 
        write(day_of_week,"(I1)") i
        
        j=scan(trim(par(10)),trim(day_of_week))
        if (j.gt.0) then
        if (i.lt.7) then
        day_of_week_str=trim(adjustl(day_of_week_str))//"&dow_"//day_of_week//"=on"
        else 
        day_of_week_str=trim(adjustl(day_of_week_str))//"&holidays=on"
        endif
       end if
        
    enddo
    
!*******************************************START TO PROCESS PARAMETER INFORMATION*******************************
    open(fileid_output,file=filename_output)
    if (trim(adjustl(par(2)))=="STATION") then
        if (par_avail(3)==1) then
        write(*,*) "IF STA_ID.TXT IS AVAILABLE, PLEASE PRESS ANY KEY TO CONTINUE"
        read(*,*)
        open (fileid_sta,file=filename_sta)
        read (fileid_sta,*) sta_num
        do i = 1, sta_num
            read (fileid_sta,*) mp(i),sta_id(i)
            write (sta_id_str(i),"(I8)") sta_id(i)
        end do
        
        else 
            write (*,*) "STATION CHARACTER IS NOT SPECIFIED"
            stop
        endif

    endif
!***************************************PROCESS PARAMETER INFORMATION FOR TRAVEL TIME*****************************
if (trim(adjustl(par(2)))=="TRAVEL_TIME") then
    if (par_avail(13)==1) then
        tt_seg_id=par(13)
    endif
endif
!**********************************************************************************************
if (trim(adjustl(par(2)))=="TRAVEL_TIME")then
    collect_times=collect_separate(start_month,start_day,end_month,end_day,par(12)(1:1),collect_per_val,year)
    output_s_month=start_month
    output_s_day=start_day
    day1=0
    do j = 1, collect_times
        max_duration=cal_day_of_year(end_month,end_day,year)
        if (par(12)(1:1)=="M") then
                if ((cal_day_of_year(output_s_month+collect_per_val,output_s_day,year)).le.max_duration) then
                    
                output_e_month=output_s_month+collect_per_val
                output_e_day=output_s_day
                else
                    !write(*,*) "hello2",cal_day_of_year(output_s_month+collect_per_val,output_s_day,year),output_s_month+collect_per_val,output_s_day
                    output_e_month=end_month
                    output_e_day=end_day+1
                endif
                day1=cal_day_of_year(output_s_month,output_s_day,year)
                day2=cal_day_of_year(output_e_month,output_e_day,year)
                write(*,*) "collect time period (by month)", output_s_month,output_s_day,output_e_month,output_e_day
                second1=base_second+(day1-1)*86400
                second2=base_second+(day2-1)*86400-1
                if (second1<100000000) then
                    write(second1_str,"(I8)") second1
                    second1_str="13"//second1_str
                else
                    write(second1_str,"(I8)") second1-100000000
                    second1_str="14"//second1_str
                end if
                if (second2<100000000) then
                    write(second2_str,"(I8)") second2
                    second2_str="13"//second2_str
                else
                    write(second2_str,"(I8)") second2-100000000
                    second2_str="14"//second2_str
                endif
                output_s_month=output_e_month
                output_s_day=output_e_day
            else
                if (par(12)(1:1)=="D")then
                day1= max(cal_day_of_year(output_s_month,output_s_day,year),day1)
                day2= min(day1+collect_per_val,cal_day_of_year(end_month,end_day,year)+1)
                write(*,*) "collect time period (by day)",day1,day2
                second1=base_second+(day1-1)*86400
                second2=base_second+(day2-1)*86400-1
                if (second1<100000000) then
                    write(second1_str,"(I8)") second1
                    second1_str=repeat('0',8-len_trim(adjustl(second1_str)))//trim(adjustl(second1_str))
                    second1_str="13"//second1_str
                else
                    write(second1_str,"(I8)") second1-100000000
                    second1_str=repeat('0',8-len_trim(adjustl(second1_str)))//trim(adjustl(second1_str))
                    second1_str="14"//second1_str
                end if
                if (second2<100000000) then
                    write(second2_str,"(I8)") second2
                    second2_str=repeat('0',8-len_trim(adjustl(second2_str)))//trim(adjustl(second2_str))
                    second2_str="13"//second2_str
                else
                    write(second2_str,"(I8)") second2-100000000
                    second2_str=repeat('0',8-len_trim(adjustl(second2_str)))//trim(adjustl(second2_str))
                    second2_str="14"//second2_str
                endif
                day1=day2

                endif
            endif
            download_path="http://udot.bt-systems.com/?report_form=1&dnode=Route&content=tt&tab=tt_compare&export=xls&route_id="//trim(adjustl(tt_seg_id))&
            //"&s_time_id="//trim(adjustl(second1_str))//"&e_time_id="//trim(adjustl(second2_str))//trim(adjustl(day_of_week_str))//"&gn="//trim(adjustl(par(9)))//"&agg=on"
            write(fileid_output,"(A255)",advance='no') 'explorer "'//trim(adjustl(download_path))//'"'
            write(fileid_output,*)
	    write(fileid_output,*)
	    write(output_file_num_str,"(I8)") j
	write(fileid_output,"(A9)") "@echo off"
        write(fileid_output,*) ":looptt"//trim(adjustl(tt_seg_id))//"_f"//trim(adjustl(output_file_num_str))//'l1'
        write(fileid_output,*) "	if exist "//trim(adjustl(par(1)))//"pems_output.xls ("
        write(fileid_output,*) "	echo rename station file"//trim(adjustl(tt_seg_id))//'    time    '//trim(adjustl(output_file_num_str))
        write(fileid_output,*) "	ren "//trim(par(1))//"pems_output.xls "//trim(adjustl(tt_seg_id))//"_"//trim(adjustl(output_file_num_str))//'.xls'
        write(fileid_output,*) ") else ("
        write(fileid_output,*) "	ping -n 3 127.1>nul"
        write(fileid_output,*) "	goto :looptt"//trim(adjustl(tt_seg_id))//'_f'//trim(adjustl(output_file_num_str))//'l1'
        write(fileid_output,*) ")"
        write(fileid_output,*)
        write(fileid_output,*) "@echo off"
        write(fileid_output,*) ":looptt"//trim(adjustl(tt_seg_id))//"_f"//trim(adjustl(output_file_num_str))//"l2"
        write(fileid_output,*) "	if not exist "//trim(adjustl(par(1)))//trim(adjustl(tt_seg_id))//'_'//trim(adjustl(output_file_num_str))//".xls ("
	write(fileid_output,*) "	echo waiting for"//trim(adjustl(tt_seg_id))//'	'//trim(adjustl(output_file_num_str))
        write(fileid_output,*) "	ping -n 2 127.1>nul"
        write(fileid_output,*) "	goto :looptt"//trim(adjustl(tt_seg_id))//'_f'//trim(adjustl(output_file_num_str))//'l2'
        write(fileid_output,*) ")"
        write(fileid_output,*)
    end do
    close(fileid_output)
    endif
    
!**********************************************************************************************        
    If (trim(adjustl(par(2)))=="STATION") then    
    do i = 1, sta_num
        collect_times=collect_separate(start_month,start_day,end_month,end_day,par(12)(1:1),collect_per_val,year)
        write(*,*) "for station", i, '/',sta_num,", there are ",collect_times,"files."
        output_s_month=start_month
        output_s_day=start_day
        day1=0
        do j = 1, collect_times
            max_duration=cal_day_of_year(end_month,end_day,year)
            !write(*,*) "the max duration is",j,output_s_month,output_s_day
            !write(*,*) par(12)(1:1)
            if (par(12)(1:1)=="M") then
                    write(*,*) "station",i
                    
                if ((cal_day_of_year(output_s_month+collect_per_val,output_s_day,year)).le.max_duration) then
                    !write(*,*) "hello",cal_day_of_year(output_s_month+collect_per_val,output_s_day,year)
                output_e_month=output_s_month+collect_per_val
                output_e_day=output_s_day
                
                else
                    !write(*,*) "hello2",cal_day_of_year(output_s_month+collect_per_val,output_s_day,year),output_s_month+collect_per_val,output_s_day
                    output_e_month=end_month
                    output_e_day=end_day+1
                endif
                write(*,*) "collect time period (by month)",output_s_month,output_s_day,output_e_month,output_e_day
                day1=cal_day_of_year(output_s_month,output_s_day,year)
                day2=cal_day_of_year(output_e_month,output_e_day,year)
                !write(*,*) "check",output_s_month,output_s_day,output_e_month,output_e_day
                !write(*,*) "monthly test", day1,day2
                second1=base_second+(day1-1)*86400
                second2=base_second+(day2-1)*86400-1
                if (second1<100000000) then
                    write(second1_str,"(I8)") second1
                    second1_str="13"//second1_str
                else
                    write(second1_str,"(I8)") second1-100000000
                    second1_str="14"//second1_str
                end if
                if (second2<100000000) then
                    write(second2_str,"(I8)") second2
                    second2_str="13"//second2_str
                else
                    write(second2_str,"(I8)") second2-100000000
                    second2_str="14"//second2_str
                endif
                output_s_month=output_e_month
                output_s_day=output_e_day
            else
                if (par(12)(1:1)=="D")then
                day1= max(cal_day_of_year(output_s_month,output_s_day,year),day1)
                day2=min(day1+collect_per_val,cal_day_of_year(end_month,end_day,year)+1)
                write(*,*) "Station:",i
                write(*,*) " collect time period (by day)", day1,day2
                second1=base_second+(day1-1)*86400
                second2=base_second+(day2-1)*86400-1
                if (second1<100000000) then
                    write(second1_str,"(I8)") second1
                    second1_str=repeat('0',8-len_trim(adjustl(second1_str)))//trim(adjustl(second1_str))
                    second1_str="13"//second1_str
                else
                    write(second1_str,"(I8)") second1-100000000
                    second1_str=repeat('0',8-len_trim(adjustl(second1_str)))//trim(adjustl(second1_str))
                    second1_str="14"//second1_str
                end if
                if (second2<100000000) then
                    write(second2_str,"(I8)") second2
                    second2_str=repeat('0',8-len_trim(adjustl(second2_str)))//trim(adjustl(second2_str))
                    second2_str="13"//second2_str
                else
                    write(second2_str,"(I8)") second2-100000000
                    second2_str=repeat('0',8-len_trim(adjustl(second2_str)))//trim(adjustl(second2_str))
                    second2_str="14"//second2_str
                endif
                day1=day2

                endif
            endif
            write(*,*)
        download_path="http://udot.bt-systems.com/?report_form=1&dnode=VDS&content=loops&export=xls&station_id="//trim(adjustl(sta_id_str(i)))//&
        "&s_time_id="//trim(adjustl(second1_str))//"&e_time_id="//trim(adjustl(second2_str))//trim(adjustl(day_of_week_str))//"&q="//trim((par(3)))//"&gn="//trim(adjustl(par(9)))//"&agg=on"
        write(fileid_output,"(A245)",advance='no') 'explorer "'//trim(adjustl(download_path))//'"'
        write(fileid_output,*)
        write(output_file_num_str,"(I8)") j
	    write(fileid_output,"(A9)") "@echo off"
        write(fileid_output,*) ":loops"//trim(adjustl(sta_id_str(i)))//"_f"//trim(adjustl(output_file_num_str))//'l1'
        write(fileid_output,*) "	if exist "//trim(adjustl(par(1)))//"pems_output.xls ("
        write(fileid_output,*) "	echo rename station file"//trim(adjustl(sta_id_str(i)))//'    time    '//trim(adjustl(output_file_num_str))
        write(fileid_output,*) "	ren "//trim(par(1))//"pems_output.xls "//trim(adjustl(sta_id_str(i)))//"_"//trim(adjustl(output_file_num_str))//'.xls'
        write(fileid_output,*) ") else ("
        write(fileid_output,*) "	ping -n 3 127.1>nul"
        write(fileid_output,*) "	goto :loops"//trim(adjustl(sta_id_str(i)))//'_f'//trim(adjustl(output_file_num_str))//'l1'
        write(fileid_output,*) ")"
        write(fileid_output,*)
        write(fileid_output,*) "@echo off"
        write(fileid_output,*) ":loops"//trim(adjustl(sta_id_str(i)))//"_f"//trim(adjustl(output_file_num_str))//"l2"
        write(fileid_output,*) "	if not exist "//trim(adjustl(par(1)))//trim(adjustl(sta_id_str(i)))//'_'//trim(adjustl(output_file_num_str))//".xls ("
	write(fileid_output,*) "	echo waiting for"//trim(adjustl(sta_id_str(i)))//'	'//trim(adjustl(output_file_num_str))
        write(fileid_output,*) "	ping -n 2 127.1>nul"
        write(fileid_output,*) "	goto :loops"//trim(adjustl(sta_id_str(i)))//'_f'//trim(adjustl(output_file_num_str))//'l2'
        write(fileid_output,*) ")"
        write(fileid_output,*)
        !write(*,*) trim(adjustl(day_of_week_str))
        enddo  
    enddo
    close(fileid_output)
    endif
    
!****************************************TRAVEL TIME*************************************************
    write(*,*) "PRESS ANY KEY TO CONTINUE"
    read(*,*)
!***************************************THE END******************************************************
    end program refined_pems_download


!****************************************FUNCTION CALCULATE THE DAY OF THE YEAR
integer function cal_day_of_year(a,b,c)
implicit none
integer a,b,c
integer ::day_of_month(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
integer i
integer ::days=0
days=0
if (c==2012) then
	day_of_month(2)=29
endif
if (a.gt.1) then
    do i =1, a-1
        days=days+day_of_month(i)
    enddo
endif

days=days+b
cal_day_of_year=days
return
end function
!***********************************FUNCTION CALCULATE THE DAY***********************
integer function collect_separate(a,b,c,d,e,f,g)
implicit none
integer a,b,c,d,f,times,g
character e
integer,external ::cal_day_of_year
if (e=="M") then
    if (d<b) then
        times=(c-a)/f
    else
        times=(c-a)/f+1
    endif
endif
if (e=="D") then
    if (mod(cal_day_of_year(c,d,g)-cal_day_of_year(a,b,g),f)==0) then
    times=(cal_day_of_year(c,d,g)-cal_day_of_year(a,b,g))/f
    else
    times=(cal_day_of_year(c,d,g)-cal_day_of_year(a,b,g))/f+1
    endif
endif
collect_separate=times
end function