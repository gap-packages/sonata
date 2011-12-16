GROUP_PATH_NAME := DirectoriesPackageLibrary( "sonata", "grp" );

AutoReadTW := function ( name )
    Read( Filename( GROUP_PATH_NAME, Concatenation(name,".def") ) );
end;

AUTO( AutoReadTW,"gprime","GTW2_1","GTW3_1","GTW5_1","GTW7_1","GTW11_1",
                              "GTW13_1","GTW17_1","GTW19_1","GTW23_1","GTW29_1",
                              "GTW31_1"    );

AUTO( AutoReadTW,"g4","GTW4_1","GTW4_2" ); 

AUTO( AutoReadTW,"g6","GTW6_1","GTW6_2" ); 

AUTO( AutoReadTW,"g8","GTW8_1","GTW8_2","GTW8_3","GTW8_4","GTW8_5" ); 

AUTO( AutoReadTW,"g9","GTW9_1","GTW9_2" ); 

AUTO( AutoReadTW,"g10","GTW10_1","GTW10_2" );

AUTO( AutoReadTW,"g12","GTW12_1","GTW12_2","GTW12_3","GTW12_4","GTW12_5" ); 

AUTO( AutoReadTW,"g14","GTW14_1","GTW14_2" ); 

AUTO( AutoReadTW,"g15","GTW15_1"  ); 

AUTO( AutoReadTW,"g16","GTW16_1","GTW16_2","GTW16_3","GTW16_4","GTW16_5","GTW16_6",
                          "GTW16_7","GTW16_8","GTW16_9","GTW16_10","GTW16_11","GTW16_12",
                          "GTW16_13","GTW16_14"  ); 

AUTO( AutoReadTW,"g18","GTW18_1","GTW18_2","GTW18_3","GTW18_4","GTW18_5" ); 

AUTO( AutoReadTW,"g20","GTW20_1","GTW20_2","GTW20_3","GTW20_4","GTW20_5" ); 

AUTO( AutoReadTW,"g21","GTW21_1","GTW21_2" ); 

AUTO( AutoReadTW,"g22","GTW22_1","GTW22_2" ); 

AUTO( AutoReadTW,"g24","GTW24_1","GTW24_2","GTW24_3","GTW24_4","GTW24_5","GTW24_6",
                          "GTW24_7","GTW24_8","GTW24_9","GTW24_10","GTW24_11","GTW24_12",
                          "GTW24_13","GTW24_14","GTW24_15" ); 

AUTO( AutoReadTW,"g25","GTW25_1","GTW25_2" ); 

AUTO( AutoReadTW,"g26","GTW26_1","GTW26_2" ); 

AUTO( AutoReadTW,"g27","GTW27_1","GTW27_2","GTW27_3","GTW27_4","GTW27_5" ); 

AUTO( AutoReadTW,"g28","GTW28_1","GTW28_2","GTW28_3","GTW28_4" ); 

AUTO( AutoReadTW,"g30","GTW30_1","GTW30_2","GTW30_3","GTW30_4" ); 

AUTO( AutoReadTW,"g32","GTW32_1","GTW32_2","GTW32_3","GTW32_4","GTW32_5" );
AUTO( AutoReadTW,"g32","GTW32_6","GTW32_7","GTW32_8","GTW32_9","GTW32_10" );
AUTO( AutoReadTW,"g32","GTW32_11","GTW32_12","GTW32_13","GTW32_14","GTW32_15" );
AUTO( AutoReadTW,"g32","GTW32_16","GTW32_17","GTW32_18","GTW32_19","GTW32_20" );
AUTO( AutoReadTW,"g32","GTW32_21","GTW32_22","GTW32_23","GTW32_24","GTW32_25" );
AUTO( AutoReadTW,"g32","GTW32_26","GTW32_27","GTW32_28","GTW32_29","GTW32_30" );
AUTO( AutoReadTW,"g32","GTW32_31","GTW32_32","GTW32_33","GTW32_34","GTW32_35" );
AUTO( AutoReadTW,"g32","GTW32_36","GTW32_37","GTW32_38","GTW32_39","GTW32_40" );
AUTO( AutoReadTW,"g32","GTW32_41","GTW32_42","GTW32_43","GTW32_44","GTW32_45" );
AUTO( AutoReadTW,"g32","GTW32_46","GTW32_47","GTW32_48","GTW32_49","GTW32_50" );
AUTO( AutoReadTW,"g32","GTW32_51" ); 

AUTO( AutoReadTW,"grplist","GroupList");
