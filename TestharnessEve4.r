source("./ProcessEveGrowthCurve.R")

allfnames <- c('TestData/Eve15186_E21_FI.txt',  #1
               'TestData/Eve15186_E21_AB.txt',  #2
               'TestData/Eve15186_G21_AB.txt',  #3
               'TestData/Eve15186_G21_FI.txt',  #4
               'TestData/Eve15186_H12_AB.txt',  #5
               'TestData/Eve15186_H12_FI.txt',  #6
               'TestData/Eve15187_C23_AB.txt',  #7
               'TestData/Eve15187_C23_FI.txt',  #8
               'TestData/Eve15187_D2_AB.txt',   #9
               'TestData/Eve15187_D2_FI.txt',   #10
               'TestData/Eve15188_A20_AB.txt',  #11
               'TestData/Eve15188_A20_FI.txt',  #12
               'TestData/Eve15188_H15_AB.txt',  #13
               'TestData/Eve15188_H15_FI.txt',  #14
               'TestData/Eve15188_I6_AB.txt',   #15
               'TestData/Eve15188_I6_FI.txt',   #16
               'TestData/Eve15188_M24_AB.txt',  #17
               'TestData/Eve15188_M24_FI.txt',  #18
               'TestData/Eve15188_M3_AB.txt',   #19
               'TestData/Eve15188_M3_FI.txt',   #20
               'TestData/Eve1874_I11_Chry.txt', #21
               'TestData/Eve1874_I11_Sapp.txt', #22
               'TestData/Eve1874_I4_Sapp.txt',  #23
               'TestData/Eve1875_D21_Sapp.txt', #24
               'TestData/Eve1877_B22_Sapp.txt', #25
               'TestData/EveTest-DualStrainDHFR 5_Plate010_1944_B_24.txt', #26
               'TestData/growthwithdecay.txt',   #27
               'TestData/Eve0023790_B2_Ch.txt', #28
               'TestData/EveDHFR5_17_P13.txt',  #29
               'TestData/EveDHFR5_23_B21.txt', #30
               'TestData/EveDHFR5_25_A18.txt', #31
               'TestData/EveDHFR5_25_K1.txt', #32
               'TestData/EveDHFR5_25_O3.txt', #33
               'TestData/EveDHFR5_31_D5.txt', #34
               'TestData/EveDHFR5_35_C1.txt', #35
               'TestData/EveDHFR5_35_N12.txt', #36
               'TestData/EveDHFR5_36_P20.txt', #37
               'TestData/EveDHFR5_37_K5.txt', #38
               'TestData/EveDHFR5_41_F20.txt', #39
               'TestData/EveDHFR5_41_G14.txt', #40
               'TestData/EveDHFR5_43_B8.txt', #41
               'TestData/EveDHFR5_43_N23.txt', #42
               'TestData/EveDHFR5_44_B4.txt', #43
               'TestData/EveDHFR5_44_F5.txt', #44
               'TestData/EveDHFR5_44_G20.txt', #45
               'TestData/EveDHFR5_44_J21.txt', #46
               'TestData/EveDHFR5_44_K11.txt', #47
               'TestData/EveDHFR5_44_M21.txt', #48
               'TestData/EveDHFR5_44_M3.txt', #49
               'TestData/EveDHFR5_44_P13.txt', #50
               'TestData/EveDHFR5_45_H1.txt', #51
               'TestData/EveDHFR5_45_P1.txt', #52
               'TestData/EveDHFR5_45_P5.txt', #53
               'TestData/EveDHFR5_8_B15.txt', #54
               'TestData/EveDHFR5_8_B2.txt', #55
               'TestData/EveDHFR5_8_E22.txt', #56
               'TestData/EveDHFR5_8_F20.txt', #57
               'TestData/EveDHFR5_8_I23.txt', #58
               'TestData/EveDHFR5_8_J1.txt', #59
               'TestData/EveDHFR5_8_L7.txt', #60
               'TestData/EveDHFR5_8_M23.txt', #61
               'TestData/EveDHFR5_8_P10.txt', #62
               'TestData/EveDHFR5_8_P9.txt' #63
               );

	 
	 {
	 #print("Enter index number of test curve to run:")
	 #i <- as.integer(readline()) 	
	 
	 for(i in 1:length(allfnames))
	 {
	 print(i)  #just to track progress

      fname = allfnames[i]
      dat <- read.table(fname, header=F)
      curveVals <- t(dat[1]) #transpose
      curveVals[0]
      curveT <- t(dat[2])
      title <- strsplit(fname,".txt")
      
      dev.new()
      plot(curveT,curveVals)
      lines(curveT,curveVals)
      outparams <- ProcessEveGrowthCurve(curveVals,curveT,title,"PLOTSON")
      print(c("OUTPUT",outparams))
      print(c("End of data set",i))
      readline()}
      }
     
