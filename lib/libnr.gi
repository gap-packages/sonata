################################################################################
##
#W  libnr.gi             library near-rings                   Christof N"obauer
##
#Y  Copyright (C)
##
##
##  13.01.00  N!.multiplication ersetzt, PM
##

##############################################################################
##
  InstallValue( NEARRING_PATH_NAME, 
		DirectoriesPackageLibrary( "sonata", "nr" ) );
  InstallValue( NEARRING_WITH_IDENTITY_PATH_NAME,
		DirectoriesPackageLibrary( "sonata", "nri" ) );

BindGlobal( "AutoReadNR",
  function ( filename )
    Read( Filename( NEARRING_PATH_NAME, Concatenation(filename,".nr") ) );
end );

BindGlobal( "AutoReadNRI",
  function ( filename )
    Read( Filename( NEARRING_WITH_IDENTITY_PATH_NAME, Concatenation(filename,".nr") ) );
end );

AUTO( AutoReadNR, "nr_2-7" ,  "NR_C2", "NR_C3", "NR_C4", "NR_V4", "NR_C5", 
                                 "NR_C6", "NR_S3", "NR_C7"                  );

AUTO( AutoReadNR, "nr8_1" ,   "NR_C8"       ); 
AUTO( AutoReadNR, "nr8_2" ,   "NR_C2xC4"    ); 
AUTO( AutoReadNR, "nr8_3" ,   "NR_C2xC2xC2" ); 
AUTO( AutoReadNR, "nr8_4" ,   "NR_D8"       ); 
AUTO( AutoReadNR, "nr8_5" ,   "NR_Q8"       ); 

AUTO( AutoReadNR, "nr9_1" ,   "NR_C9"       ); 
AUTO( AutoReadNR, "nr9_2" ,   "NR_C3xC3"    ); 

AUTO( AutoReadNR, "nr10_1" ,   "NR_C10"     ); 
AUTO( AutoReadNR, "nr10_2" ,   "NR_D10"     ); 

AUTO( AutoReadNR, "nr11_1" ,   "NR_C11"     ); 

AUTO( AutoReadNR, "nr12_1" ,   "NR_C12"     ); 
AUTO( AutoReadNR, "nr12_2" ,   "NR_C2xC6"   ); 

AUTO( AutoReadNR, "nr12_3.1" ,   "NR_D12_1"     ); 
AUTO( AutoReadNR, "nr12_3.2" ,   "NR_D12_2"     ); 
AUTO( AutoReadNR, "nr12_3.3" ,   "NR_D12_3"     ); 
AUTO( AutoReadNR, "nr12_3.4" ,   "NR_D12_4"     ); 
AUTO( AutoReadNR, "nr12_3.5" ,   "NR_D12_5"     ); 
AUTO( AutoReadNR, "nr12_3.6" ,   "NR_D12_6"     ); 
AUTO( AutoReadNR, "nr12_3.7" ,   "NR_D12_7"     ); 
AUTO( AutoReadNR, "nr12_3.8" ,   "NR_D12_8"     ); 
AUTO( AutoReadNR, "nr12_3.9" ,   "NR_D12_9"     ); 

AUTO( AutoReadNR, "nr12_4" ,   "NR_A4"      ); 
AUTO( AutoReadNR, "nr12_5" ,   "NR_T"       ); 

AUTO( AutoReadNR, "nr13_1" ,   "NR_C13"     ); 

AUTO( AutoReadNR, "nr14_1" ,   "NR_C14"     ); 
AUTO( AutoReadNR, "nr14_2" ,   "NR_D14"     ); 

AUTO( AutoReadNR, "nr15_1" ,   "NR_C15"     );

########################################################

AUTO( AutoReadNRI, "nri12_3" ,   "NI12_3"      ); 

AUTO( AutoReadNRI, "nri16_1" ,   "NI16_1"      ); 
AUTO( AutoReadNRI, "nri16_2" ,   "NI16_2"      ); 
AUTO( AutoReadNRI, "nri16_3" ,   "NI16_3"      ); 
AUTO( AutoReadNRI, "nri16_4" ,   "NI16_4"      ); 
AUTO( AutoReadNRI, "nri16_5" ,   "NI16_5"      ); 
AUTO( AutoReadNRI, "nri16_6" ,   "NI16_6"      ); 
AUTO( AutoReadNRI, "nri16_7" ,   "NI16_7"      ); 
AUTO( AutoReadNRI, "nri16_9" ,   "NI16_9"      ); 
AUTO( AutoReadNRI, "nri16_10" ,  "NI16_10"     ); 
AUTO( AutoReadNRI, "nri16_11" ,  "NI16_11"     );

AUTO( AutoReadNRI, "nri17_1" ,   "NI17_1"      );

AUTO( AutoReadNRI, "nri18_1" ,   "NI18_1"      );
AUTO( AutoReadNRI, "nri18_2" ,   "NI18_2"      );  
AUTO( AutoReadNRI, "nri18_3" ,   "NI18_3"      );

AUTO( AutoReadNRI, "nri19_1" ,   "NI19_1"      );

AUTO( AutoReadNRI, "nri20_1" ,   "NI20_1"      );
AUTO( AutoReadNRI, "nri20_2" ,   "NI20_2"      ); 
AUTO( AutoReadNRI, "nri20_3" ,   "NI20_3"      );

AUTO( AutoReadNRI, "nri21_1" ,   "NI21_1"      );

AUTO( AutoReadNRI, "nri22_1" ,   "NI22_1"      );

AUTO( AutoReadNRI, "nri23_1" ,   "NI23_1"      );

AUTO( AutoReadNRI, "nri24_1" ,   "NI24_1"      );
AUTO( AutoReadNRI, "nri24_2" ,   "NI24_2"      );  
AUTO( AutoReadNRI, "nri24_3" ,   "NI24_3"      );  
AUTO( AutoReadNRI, "nri24_4" ,   "NI24_4"      ); 
AUTO( AutoReadNRI, "nri24_5" ,   "NI24_5"      ); 
AUTO( AutoReadNRI, "nri24_7" ,   "NI24_7"      ); 
AUTO( AutoReadNRI, "nri24_9" ,   "NI24_9"      ); 

AUTO( AutoReadNRI, "nri25_1" ,   "NI25_1"      );
AUTO( AutoReadNRI, "nri25_2" ,   "NI25_2"      ); 

AUTO( AutoReadNRI, "nri26_1" ,   "NI26_1"      );

AUTO( AutoReadNRI, "nri27_1" ,   "NI27_1"      );
AUTO( AutoReadNRI, "nri27_2" ,   "NI27_2"      ); 
AUTO( AutoReadNRI, "nri27_3" ,   "NI27_3"      ); 
AUTO( AutoReadNRI, "nri27_4" ,   "NI27_4"      ); 
AUTO( AutoReadNRI, "nri27_5" ,   "NI27_5"      ); 

AUTO( AutoReadNRI, "nri28_1" ,   "NI28_1"      );
AUTO( AutoReadNRI, "nri28_2" ,   "NI28_2"      ); 
AUTO( AutoReadNRI, "nri28_3" ,   "NI28_3"      );

AUTO( AutoReadNRI, "nri29_1" ,   "NI29_1"      );

AUTO( AutoReadNRI, "nri30_1" ,   "NI30_1"      );

AUTO( AutoReadNRI, "nri31_1" ,   "NI31_1"      );

############################################################################
##
#F  NumberLibraryNearRings
##
NumberLibraryNearRings := function( group )

  local grpname_number;

  grpname_number := [  [1,1] ,     1,
		       [2,1] ,     3,
                       [3,1] ,     5,
                       [4,1] ,    12,
                       [4,2] ,    23,
                       [5,1] ,    10,
                       [6,1] ,    60,
                       [6,2] ,    39,
                       [7,1] ,    24,
                       [8,1] ,   135,
                       [8,2] ,  1159,
                       [8,3] ,   834,
                       [8,4] ,  1447,
                       [8,5] ,   281,
                       [9,1] ,   222,
                       [9,2] ,   264,
                      [10,1] ,   329,
                      [10,2] ,   206,
                      [11,1] ,   139,
                      [12,1] ,  1749,
                      [12,2] ,  3501,
                      [12,3] , 48137,
                      [12,4] ,   483,
                      [12,5] ,   824,
                      [13,1] ,   454,
                      [14,1] ,  2716,
                      [14,2] ,  1821,
                      [15,1] ,  3817     ];
  
  if Size( group ) < 16 then
    return grpname_number[ Position( grpname_number, IdTWGroup( group ) ) + 1 ];
  else
    Error( "group not recognized" );
  fi;

end;

############################################################################
##
#F  NumberLibraryNearRingsWithOne
##
NumberLibraryNearRingsWithOne := function( group )

  local grpname_number, none;


  none := [ [6,2], [8,5], [10,2], [12,4], [12,5], [14,2], [16,8],
            [16,12], [16,13], [16,14], [18,4], [18,5], [20,4], [20,5],
            [21,2], [22,2], [24,6], [24,8], [24,10], [24,11], [24,12], 
            [24,13], [24,14], [24,15], [26,2], [28,4], [30,2], [30,3], [30,4] ];

  grpname_number := [  [1,1] ,     1,

		       [2,1] ,     1,

                       [3,1] ,     1,

                       [4,1] ,     1,
                       [4,2] ,     5,

                       [5,1] ,     1,

                       [6,1] ,     1,

                       [7,1] ,     1,

                       [8,1] ,     1,
                       [8,2] ,    10,
                       [8,3] ,    35,
                       [8,4] ,     7,

                       [9,1] ,     1,
                       [9,2] ,    10,

                      [10,1] ,     1,

                      [11,1] ,     1,

                      [12,1] ,     1,
                      [12,2] ,     9,
                      [12,3] ,     1,

                      [13,1] ,     1,

                      [14,1] ,     1,

                      [15,1] ,     1,

                      [16,1] ,     1,
                      [16,2] ,    37,
                      [16,3] ,    51,
                      [16,4] ,   470,
                      [16,5] ,  2798,
                      [16,6] ,   708,
                      [16,7] ,     4,
                      [16,9] ,   132,
                      [16,10],    40,
                      [16,11],    33,

                      [17,1] ,     1,

                      [18,1] ,     1,
                      [18,2] ,    17,
                      [18,3] ,     8,

                      [19,1] ,     1,

                      [20,1] ,     1,
                      [20,2] ,     9,
                      [20,3] ,     1,

                      [21,1] ,     1,
          
                      [22,1] ,     1,

                      [23,1] ,     1, 

                      [24,1] ,     1, 
                      [24,2] ,    14, 
                      [24,3] ,   136, 
                      [24,4] ,    10, 
                      [24,5] ,     8, 
                      [24,7] ,     7, 
                      [24,9] ,     1, 

                      [25,1] ,     1, 
                      [25,2] ,    16, 

                      [26,1] ,     1,

                      [27,1] ,     1, 
                      [27,2] ,    20,
                      [27,3] ,   202, 
                      [27,4] ,    22, 
                      [27,5] ,     4,

                      [28,1] ,     1, 
                      [28,2] ,     9, 
                      [28,3] ,     1, 
 
                      [29,1] ,     1, 
 
                      [30,1] ,     1,

                      [31,1] ,     1 ];
  
  if Size( group ) < 32 then
    if IdTWGroup( group ) in none then
      return 0;
    else
      return grpname_number[ Position( grpname_number, IdTWGroup( group ) ) + 1 ];
    fi;
  else
    Error( "group not recognized" );
  fi;

end;

############################################################################
##
#F  AllLibraryNearRings
##
AllLibraryNearRings := function( group )
  local infolevel, lnrs, tmp;
  tmp := LibraryNearRing( group, 1 );		# print a warning?
  infolevel := InfoLevel(InfoWarning);		# remember InfoLevel
  SetInfoLevel( InfoWarning, 0 ); 		# no warnings
  lnrs := List( [1..NumberLibraryNearRings( group )], i ->
    LibraryNearRing( group, i ) );
  SetInfoLevel( InfoWarning, infolevel );	# reset InfoLevel
  return lnrs;
end;

############################################################################
##
#F  AllLibraryNearRingsWithOne
##
AllLibraryNearRingsWithOne := function( group )

  local position_list, tmp, lnrs, infolevel;

  position_list := [  
	[2,1] ,   [ 2 ],
	[3,1] ,   [ 3 ],
	[4,1] ,   [ 8 ],
	[4,2] ,   [ 12, 13, 15, 16, 22 ],
	[5,1] ,   [ 7 ],
	[6,1] ,   [ 28 ],
	[6,2] ,   [ ],
	[7,1] ,   [ 19 ],
	[8,1] ,   [ 95 ],
	[8,2] ,   [ 814, 815, 840, 849, 857, 970, 971, 974, 975, 1121 ],
	[8,3] ,   [ 634, 638, 639, 640, 644, 645, 647, 648, 657, 658, 
                    660, 661, 668, 669, 672, 674, 677, 678, 693, 694, 
		    696, 698, 707, 708, 709, 711, 713, 714, 715, 717,
		    718, 796, 801, 812, 833 ],
	[8,4] ,   [ 842, 844, 848, 849, 1094, 1096, 1097 ],
	[8,5] ,   [ ],
	[9,1] ,   [ 185 ],
	[9,2] ,   [ 208, 209, 211, 215, 216, 219, 222, 225, 226, 255 ], 
	[10,1] ,   [ 168 ],
	[10,2] ,   [ ],
	[11,1] ,   [ 130 ],
	[12,1] ,   [ 1117 ],
	[12,2] ,   [ 2096, 2105, 2118, 2120, 2180, 2250, 2255, 2355, 2952 ],
	[12,4] ,   [ ],
	[12,5] ,   [ ],
	[13,1] ,   [ 429 ],
	[14,1] ,   [ 1632 ],
	[14,2] ,   [ ],
	[15,1] ,   [ 2618 ]    ];

  if Size( group ) < 16 and IdTWGroup( group ) <> [12,3] then
##PM: has to be changed to obtain near-rings in category IsNearRingWithOne 
    return AllLibraryNearRings( group ){position_list[ Position( position_list, IdTWGroup( group ) ) + 1 ]};
  else
    if NumberLibraryNearRingsWithOne( group ) = 0 then return []; fi;
    tmp := LibraryNearRingWithOne( group, 1 );		# print a warning?
    infolevel := InfoLevel(InfoWarning);		# remember InfoLevel
    SetInfoLevel( InfoWarning, 0 ); 			# no warnings
    lnrs := List( [1..NumberLibraryNearRingsWithOne( group )], i ->
    					LibraryNearRingWithOne( group, i ) );
    SetInfoLevel( InfoWarning, infolevel );		# reset InfoLevel
    return lnrs;
  fi;

end;


#############################################################################
##
#M  LibraryNearRing
##  This function 'extracts' a nearring from the nearring library files.
##

InstallMethod(
	LibraryNearRing,
	"small groups",
	true,
	[IsGroup, IsInt and IsPosRat],
	0,
  function( group, num )
  local idtw;
    # check the arguments 
    if Size(group) > 15 then
    Error( "Usage: LibraryNearRing( <group>, <num> ) where <group> must be ", 
           "\na group from GroupList of order ", 
           "less or equal to 15 and \n<num> must be a positive ",
           "integer which determines an isomorphism class" );
    fi;

    Info( InfoWarning, 1, "using isomorphic copy of the group" );
    idtw := IdTWGroup( group );
    return LibraryNearRing( TWGroup(idtw[1],idtw[2]), num );
  end );

InstallMethod(
	LibraryNearRing,
	"TW groups",
	true,
	[IsGroup and HasName, IsInt and IsPosRat],
	10,
  function( group, num )
  local n,     # help var: a nearring
        clmax, # the maximal number of equivalence classes of nearrings
        NR,    # the nearring to be returned
        elms,  # help var: the elements of G
        i,     # help var: a loop variable
        tfle,  # help var: the record that holds the tfl's of the group endos
        f,     # help var: a valid function that represents a class of nr's
        vf,endos,g,a,a_inv,h,compute_all, ns,
        mul,   # local function: the multiplication of the nearring
	gens,  # generators of group
	imgs;  # images of gens under endomorphisms

  # check the arguments 
  if Size(group) > 15 then
    Error( "Usage: LibraryNearRing( <group>, <num> ) where <group> must be ", 
           "\na group from GroupList of order ", 
           "less or equal to 15 and \n<num> must be a positive ",
           "integer which determines an isomorphism class" );
  fi;
  
  n := rec(); n.classes := rec();

  if   ( Name( group ) = "2/1" ) then  
    n := NR_C2; ns := "2/1";
  elif ( Name( group ) = "3/1" )  then
    n := NR_C3; ns := "3/1";
  elif ( Name( group ) = "4/1" )  then
    n := NR_C4;  ns := "4/1";
  elif ( Name( group ) = "4/2" )  then
    n := NR_V4;  ns := "4/2";
  elif ( Name( group ) = "5/1" )  then
    n := NR_C5;  ns := "5/1";
  elif ( Name( group ) = "6/1" )  then
    n := NR_C6;  ns := "6/1";
  elif ( Name( group ) = "6/2" )  then
    n := NR_S3;  ns := "6/2";
  elif ( Name( group ) = "7/1" )  then
    n := NR_C7;  ns := "7/1";
  elif ( Name( group ) = "8/1" )  then
    n := NR_C8;  ns := "8/1";
  elif ( Name( group ) = "8/2" )  then
    n := NR_C2xC4;  ns := "8/2";
  elif ( Name( group ) = "8/3" )  then
    n := NR_C2xC2xC2;  ns := "8/3";
  elif ( Name( group ) = "8/4" )  then
    n := NR_D8;  ns := "8/4";
  elif ( Name( group ) = "8/5" )  then
    n := NR_Q8;  ns := "8/5";
  elif ( Name( group ) = "9/1" )  then
    n := NR_C9; ns := "9/1";
  elif ( Name( group ) = "9/2" )  then
    n := NR_C3xC3;  ns := "9/2";
  elif ( Name( group ) = "10/1" )  then
    n := NR_C10;  ns := "10/1";
  elif ( Name( group ) = "10/2" )  then
    n := NR_D10;  ns := "10/2";
  elif ( Name( group ) = "11/1" )  then
    n := NR_C11;  ns := "11/1";
  elif ( Name( group ) = "12/1" )  then
    n := NR_C12;  ns := "12/1";
  elif ( Name( group ) = "12/2" )  then
    n := NR_C2xC6;  ns := "12/2";

  elif ( Name( group ) = "12/3" )  then
    if num in [1..5000] then
      n := NR_D12_1;
    elif num in [5001..10000] then
      n := NR_D12_2;
    elif num in [10001..15000] then
      n := NR_D12_3;
    elif num in [15001..20000] then
      n := NR_D12_4;
    elif num in [20001..25000] then
      n := NR_D12_5;
    elif num in [25001..30000] then
      n := NR_D12_6;
    elif num in [30001..35000] then
      n := NR_D12_7;
    elif num in [35001..40000] then
      n := NR_D12_8;
    elif num in [40001..48137] then
      n := NR_D12_9;
    fi;
    ns := "12/3";

  elif ( Name( group ) = "12/4" )  then
    n := NR_A4;  ns := "12/4";
  elif ( Name( group ) = "12/5" )  then
    n := NR_T;  ns := "12/5";
  elif ( Name( group ) = "13/1" )  then
    n := NR_C13;  ns := "13/1";
  elif ( Name( group ) = "14/1" )  then
    n := NR_C14;  ns := "14/1";
  elif ( Name( group ) = "14/2" )  then
    n := NR_D14;  ns := "14/2";
  elif ( Name( group ) = "15/1" )  then
    n := NR_C15;  ns := "15/1";
  else
    TryNextMethod();
#    Print( "There is no group name '", group, 
#           "' in the nearrings library.\n" );
#    return;
  fi;

  clmax := NumberLibraryNearRings( group );
  if num > clmax then
    Print( "There are only ", clmax, " isomorphism classes of nearrings ",
           "on the group ", group, ".\n" );
    return;
  fi;
    
  # put the group of the nearring together and define a few help variables
  elms   := AsSSortedList( group );
  tfle   := n!.group_endomorphisms;
  f      := n!.classes!.(num)!.phi;
  group!.phi  := f;
  group!.a_y_i_nrs := n!.classes!.(num)!.autos_yielding_iso_nrs;

  # retrieve the group endomorphisms from the Nearrings record
  if not HasEndomorphisms( group ) then
    # convert the endomorphism record into a list of endomorphisms
    i := 1; gens := GeneratorsOfGroup( group ); endos := [];
    while IsBound( tfle.(i) ) do
      imgs := List( gens, gen -> elms[tfle.(i)[Position(elms,gen)]] );
      Add( endos, GroupGeneralMappingByImages( group, group, gens, imgs ) );   
      i := i + 1;
    od;
    SetEndomorphisms( group, endos );
  fi;
  
  # define a LEFT distributive multiplication  
  mul := function( y, x )
    return elms[ tfle!.(f[ Position( elms, y ) ]) [ Position( elms, x ) ] ];
  end;
  
  # put the nearring together
  NR  := ExplicitMultiplicationNearRingNC( group, mul );

  SetLibraryNearRingFlag( NR, true );
  SetIdLibraryNearRing( NR, [ group, num ] );
  SetName( NR, Concatenation( "LibraryNearRing(", Name( group ), ", ", 
                            String( num ), ")" ) );
  
  return NR;

  end );


#############################################################################
##
#M  LibraryNearRingWithOne
##  This function 'extracts' a nearring from the nearring library files.
##

InstallMethod(
	LibraryNearRingWithOne,
	"small groups",
	true,
	[IsGroup, IsInt and IsPosRat],
	0,
  function( group, num )
  local idtw;
    # check the arguments 
    if Size(group) > 32 then
    Error( "Usage: LibraryNearRing( <group>, <num> ) where <group> must be ", 
           "\na group from GroupList of order ", 
           "less or equal to 15 and \n<num> must be a positive ",
           "integer which determines an isomorphism class" );
    fi;

    Info( InfoWarning, 1, "using isomorphic copy of the group" );
    idtw := IdTWGroup( group );
    return LibraryNearRingWithOne( TWGroup(idtw[1],idtw[2]), num );
  end );

InstallMethod(
	LibraryNearRingWithOne,
	"TW groups",
	true,
	[IsGroup and HasName, IsInt and IsPosRat],
	10,
  function( group, num )

  local n,     # help var: a nearring
        clmax, # the maximal number of equivalence classes of nearrings
        NR,    # the nearring to be returned
        elms,  # help var: the elements of G
        i,     # help var: a loop variable
        tfle,  # help var: the record that holds the tfl's of the group endos
        f,     # help var: a valid function that represents a class of nr's
        vf,endos,g,a,a_inv,h,compute_all, ns,
        mul,   # local function: the multiplication of the nearring
	gens,  # generators of group
	imgs,  # images of gens under endomorphisms
	id;    # identity

  if Size( group ) < 16 and Name( group ) <> "12/3" then
    if num > NumberLibraryNearRingsWithOne( group ) then
      Print( "There are only ", NumberLibraryNearRingsWithOne( group ), 
             " isomorphism classes of nearrings with One ",
             "on the group ", group, ".\n" );
      return;
    else
##PM: has to be changed to obtain near-rings in category IsNearRingWithOne 
      return AllLibraryNearRingsWithOne( group )[ num ];
    fi;
  fi;


  if   ( Name( group ) = "12/3" ) then  
    n := NI12_3; ns := "12/3";
  elif ( Name( group ) = "16/1" ) then  
    n := NI16_1; ns := "16/1";
  elif ( Name( group ) = "16/2" )  then
    n := NI16_2; ns := "16/2";
  elif ( Name( group ) = "16/3" )  then
    n := NI16_3; ns := "16/3";
  elif ( Name( group ) = "16/4" )  then
    n := NI16_4; ns := "16/4";
  elif ( Name( group ) = "16/5" )  then
    n := NI16_5; ns := "16/5";
  elif ( Name( group ) = "16/6" )  then
    n := NI16_6; ns := "16/6";
  elif ( Name( group ) = "16/7" )  then
    n := NI16_7; ns := "16/7";
  elif ( Name( group ) = "16/9" )  then
    n := NI16_9; ns := "16/9";
  elif ( Name( group ) = "16/10" )  then
    n := NI16_10; ns := "16/10";
  elif ( Name( group ) = "16/11" )  then
    n := NI16_11; ns := "16/11";

  elif ( Name( group ) = "17/1" ) then  
    n := NI17_1; ns := "17/1";

  elif ( Name( group ) = "18/1" ) then  
    n := NI18_1; ns := "18/1";
  elif ( Name( group ) = "18/2" )  then
    n := NI18_2; ns := "18/2";
  elif ( Name( group ) = "18/3" )  then
    n := NI18_3; ns := "18/3";

  elif ( Name( group ) = "19/1" ) then  
    n := NI19_1; ns := "19/1";

  elif ( Name( group ) = "20/1" ) then  
    n := NI20_1; ns := "20/1";
  elif ( Name( group ) = "20/2" )  then
    n := NI20_2; ns := "20/2";
  elif ( Name( group ) = "20/3" ) then  
    n := NI20_3; ns := "20/3";

  elif ( Name( group ) = "21/1" ) then  
    n := NI21_1; ns := "21/1";

  elif ( Name( group ) = "22/1" ) then  
    n := NI22_1; ns := "22/1";

  elif ( Name( group ) = "23/1" ) then  
    n := NI23_1; ns := "23/1";

  elif ( Name( group ) = "24/1" ) then  
    n := NI24_1; ns := "24/1";
  elif ( Name( group ) = "24/2" )  then
    n := NI24_2; ns := "24/2";
  elif ( Name( group ) = "24/3" )  then
    n := NI24_3; ns := "24/3";
  elif ( Name( group ) = "24/4" )  then
    n := NI24_4; ns := "24/4";
  elif ( Name( group ) = "24/5" )  then
    n := NI24_5; ns := "24/5";
  elif ( Name( group ) = "24/7" )  then
    n := NI24_7; ns := "24/7";
  elif ( Name( group ) = "24/9" )  then
    n := NI24_9; ns := "24/9";

  elif ( Name( group ) = "25/1" ) then  
    n := NI25_1; ns := "25/1";
  elif ( Name( group ) = "25/2" ) then  
    n := NI25_2; ns := "25/2";

  elif ( Name( group ) = "26/1" ) then  
    n := NI26_1; ns := "26/1";

  elif ( Name( group ) = "27/1" ) then  
    n := NI27_1; ns := "27/1";
  elif ( Name( group ) = "27/2" ) then  
    n := NI27_2; ns := "27/2";
  elif ( Name( group ) = "27/3" ) then  
    n := NI27_3; ns := "27/3";
  elif ( Name( group ) = "27/4" )  then
    n := NI27_4; ns := "27/4";
  elif ( Name( group ) = "27/5" )  then
    n := NI27_5; ns := "27/5";

  elif ( Name( group ) = "28/1" ) then  
    n := NI28_1; ns := "28/1";
  elif ( Name( group ) = "28/2" )  then
    n := NI28_2; ns := "28/2";
  elif ( Name( group ) = "28/3" ) then  
    n := NI28_3; ns := "28/3";

  elif ( Name( group ) = "29/1" ) then  
    n := NI29_1; ns := "29/1";

  elif ( Name( group ) = "30/1" ) then  
    n := NI30_1; ns := "30/1";

  elif ( Name( group ) = "31/1" ) then  
    n := NI31_1; ns := "31/1";

  else
    Print( "There is no group name '", group,
           "' in the nearrings library.\n" );
    return;
  fi;
  
  clmax := Length( RecNames( n.classes ) );
  if num > clmax then
    Print( "There are only ", clmax, " isomorphism classes of nearrings with One",
           "on the group ", group, ".\n" );
    return;
  fi;
    
  # put the group of the nearring together and define a few help variables
  elms   := AsSSortedList( group );
  tfle   := n!.group_endomorphisms;
  f      := n!.classes!.(num)!.phi;
  group!.phi  := f;
  group!.a_y_i_nrs := n!.classes!.(num)!.autos_yielding_iso_nrs;

  # retrieve the group endomorphisms from the Nearrings record
  if not HasEndomorphisms( group ) then

    if not ( Name( group ) in [ "16/5", "27/3" ] ) then

      # convert the endomorphism record into a list of endomorphisms
      i := 1; gens := GeneratorsOfGroup( group ); endos := [];
      while IsBound( tfle.(i) ) do
        imgs := List( gens, gen -> elms[tfle.(i)[Position(elms,gen)]] );
        Add( endos, GroupGeneralMappingByImages( group, group, gens, imgs ) );   
        i := i + 1;
      od;
      SetEndomorphisms( group, endos );

    fi;

  fi;
  
  # define a LEFT distributive multiplication  
  mul := function( y, x )
    return elms[ tfle!.(f[ Position( elms, y ) ]) [ Position( elms, x ) ] ];
  end;

  # put the nearring together
##PM: has to be changed to obtain near-rings in category IsNearRingWithOne 
  NR  := ExplicitMultiplicationNearRingNC( group, mul );

  SetLibraryNearRingFlag( NR, true );
  SetIdLibraryNearRingWithOne( NR, [ group, num ] );
  SetName( NR, Concatenation( "LibraryNearRingWithOne(", Name( group ), ", ", 
                            String( num ), ")" ) );

  id := One(NR);

  return NR;

  end );


#############################################################################
##
#F  LibraryNearRingInfo( <name>, <list> ). . . . info about library nearrings
##

LibraryNearRingInfo := function( arg )
  local N, elms, grpelms, n, 
	symbols, help, i, k, 
	name, list, string, 
	letters, addGroup, PRINTLISTOFSTRINGS;

  if not ( Length( arg ) in [ 2, 3 ]
	   and ( IsString( arg[1] ) or IsGroup( arg[1] ) )
	   and IsList( arg[2] )
	   and ForAll( arg[2], l -> IsInt( l ) ) ) then
   Error( "Usage: LibraryNearRingInfo( <name>, <list> ) where <name>",
          " must be a\ngroup name or a group from _GroupList_ of order le 15 ",
          " and <list> must be\na list of numbers",
          " of classes" );
  fi;

  PRINTLISTOFSTRINGS := function ( list )
  local i;
    Print( "[ ",String(list[1]) );
    for i in [2..Length(list)] do
      Print( ", ",String(list[i]) );
    od;
    Print( " ]");
  end;

  name := arg[1]; list := arg[2];
  if IsBound( arg[3] ) then letters := arg[3]; else letters := ""; fi;
  if 'C' in letters or 'c' in letters then
    Print( "A ... abstract affine\n" );
    Print( "B ... boolean\n" );
    Print( "C ... commutative\n" );
    Print( "D ... distributive\n" );
    Print( "F ... nearfield\n" );
    Print( "G ... distributively generated\n" );
    Print( "I ... integral\n" );
    Print( "N ... nilpotent\n" );
    Print( "O ... planar\n" );
    Print( "P ... prime\n" );
    Print( "Q ... quasiregular\n" );
    Print( "R ... regular\n" );
    Print( "W ... without non-zero nilpotent elements\n" );
    Print( "---------------------------------------",
           "---------------------------------------\n" );
  fi;
  
  n       := Size( name );
  if n >= 16 then
    N := LibraryNearRingWithOne( name, list[ 1 ] );
  else
    N := LibraryNearRing( name, list[ 1 ] );
  fi;
  addGroup := GroupReduct(N);
  elms    := AsList( N ); 
  grpelms := List( elms, GroupElementRepOfNearRingElement );

  symbols := Symbols(N);

  Print( "---------------------------------------",
         "---------------------------------------" );
  Print( "\n>>> GROUP: ", Name(addGroup), "\nelements: " );
  PRINTLISTOFSTRINGS( symbols ); Print( "\n" );
  
  Print( "\naddition table:\n" );
  PrintTable( N, "ea" );
  
  if not ( Name( addGroup ) in [ "16/5", "27/3" ] ) then
    Print( "\ngroup endomorphisms:\n" );
    for i in [1..Length( Endomorphisms(addGroup) )] do
      if i < 10 then 
        Print( i, ":   " ); 
      else
        Print( i, ":  " ); 
      fi;
      PRINTLISTOFSTRINGS( List( grpelms, x -> symbols[ Position( grpelms, 
	     	Image( Endomorphisms( addGroup )[i], x ) ) ] ) );
      Print( "\n" );
    od;
  fi;

  Print( "\nNEARRINGS:\n" );
  Print( "---------------------------------------",
         "---------------------------------------" );
  
  for k in list do  
    if n >= 16 then
      N := LibraryNearRingWithOne( name, k );
    else
     N := LibraryNearRing( name, k );
   fi;
    elms := AsList( N ); 
    grpelms := List( elms, GroupElementRepOfNearRingElement );

    Print( "\n\n", k, ")  phi: ", addGroup!.phi, ";  " );
    for i in addGroup!.a_y_i_nrs do Print( i, ";" ); od;
    string := [];
    if IsAbstractAffineNearRing( N ) then Add( string, 'A' ); 
    else Add( string, '-' ); fi;
    if IsBooleanNearRing( N ) then Add( string, 'B' ); 
    else Add( string, '-' ); fi;
    if IsCommutative( N ) then Add( string, 'C' ); 
    else Add( string, '-' ); fi;
    if IsDistributiveNearRing( N ) then Add( string, 'D' ); 
    else Add( string, '-' ); fi;
    if IsNearField( N ) then Add( string, 'F' ); 
    else Add( string, '-' ); fi;
    if IsDgNearRing( N ) then Add( string, 'G' ); 
    else Add( string, '-' ); fi;
    if IsIntegralNearRing( N ) then Add( string, 'I' ); 
    else Add( string, '-' ); fi;
    if IsNilpotentNearRing( N ) then Add( string, 'N' ); 
    else Add( string, '-' ); fi;
    if IsPlanarNearRing( N ) then Add( string, 'O' ); 
    else Add( string, '-' ); fi;
    if IsPrimeNearRing( N ) then Add( string, 'P' ); 
    else Add( string, '-' ); fi;
    if IsQuasiregularNearRing( N ) then Add( string, 'Q' ); 
    else Add( string, '-' ); fi;
    if IsRegularNearRing( N ) then Add( string, 'R' ); 
    else Add( string, '-' ); fi;
    if IsNilpotentFreeNearRing( N ) then Add( string, 'W' ); 
    else Add( string, '-' ); fi;
    Print( "  ", string );
    if One( N ) <> fail then Print( ";  I = ", 
      symbols[ Position( elms, One(N) ) ], "\n" );  
    else
      Print( "\n" );
    fi;

    if 'M' in letters or 'm' in letters then
      Print("\nmultiplication table:\n");
      PrintTable( N, "m" );
    fi;

    if 'I' in letters or 'i' in letters then
      Print( "\nideals:\n" );
      help := AsSSortedList( NearRingIdeals( N ) );
      for i in [1..Length(help)] do
	Print( i,". " );
        PRINTLISTOFSTRINGS(
		List( AsSSortedList( help[i] ), 
                  elm -> symbols[Position(elms,elm)] ) );
	Print( "\n" );
      od;
    fi;
    if 'L' in letters or 'l' in letters then
      Print( "\nleft ideals:\n" );
      help := AsSSortedList( NearRingLeftIdeals( N ) );
      for i in [1..Length(help)] do
	Print( i,". " );
        PRINTLISTOFSTRINGS(
		List( AsSSortedList( help[i] ), 
                  elm -> symbols[Position(elms,elm)] ) );
	Print( "\n" );
      od;
    fi;
    if 'R' in letters or 'r' in letters then
      Print( "\nright ideals:\n" );
      help := AsSSortedList( NearRingRightIdeals( N ) );
      for i in [1..Length(help)] do
	Print( i,". " );
        PRINTLISTOFSTRINGS(
		List( AsSSortedList( help[i] ), 
                  elm -> symbols[Position(elms,elm)] ) );
	Print( "\n" );
      od;
    fi;
    if 'V' in letters or 'v' in letters then
      Print( "\ninvariant subnearrings:\n" );
      help := InvariantSubNearRings( N );
      for i in [1..Length(help)] do
	Print( i,". " );
        PRINTLISTOFSTRINGS(
		List( help[i], elm -> symbols[Position(elms,elm)] ) );
	Print( "\n" );
      od;
    fi;
    if 'S' in letters or 's' in letters then
      Print( "\nsubnearrings:\n" );
      help := SubNearRings( N );
      for i in [1..Length(help)] do
	Print( i,". ");
        PRINTLISTOFSTRINGS(
		List( help[i], elm -> symbols[Position(elms,elm)] ) );
	Print( "\n" );
      od;
    fi;
    if 'E' in letters or 'e' in letters then
      Print( "\nnearring endomorphisms: " );
      help := Endomorphisms( N );
      for i in List( help, e-> Position(Endomorphisms(addGroup), e )) do 
        Print( i, "; " );
      od; Print( "\n" );
    fi;
    if 'A' in letters or 'a' in letters then
      Print( "\nnearring automorphisms: " );
      help := Automorphisms( N );
      for i in List( help, e -> Position(Endomorphisms(addGroup), e )) do 
        Print( i, "; " );
      od; Print( "\n" );
    fi;
    Print( "\n---------------------------------------",
           "---------------------------------------" );
  
  od;
  Print( "\n" );
  return;
end;

#####################################################################
##
#F  SetSymbolsSupervised( <N>, <L> )	use the symbols in the list <L>
##				when printing operation tables
##

InstallGlobalFunction(
	SetSymbolsSupervised,
  function ( nr, symb )
  local symbols;
    if not IsHomogeneousList( symb ) then
	Error("<symb> must be a list without holes");
    elif not ForAll( symb, IsString ) then
	Error("<symb> must be a list of strings");
    elif Length(Set(symb)) < Length(symb) then
	Error("<symb> must be a list without repetitions");
    fi;

    if Length(symb) < Size(nr) then
	Print("Warning: not enough symbols ...",
	      "extending list automatically\n");
	symbols := Symbols(nr);
	symbols := Concatenation( symb,
				  symbols{[Length(symb)+1..Length(symbols)]}
				);
    else
	if Length(symb) > Size(nr) then
	    Print("Warning: too many symbols ...",
		  "ignoring the last ",Length(symb)-Size(nr)," symbols\n");
	fi;
	symbols := symb{[1..Size(nr)]};
    fi;

    SetSymbols( nr, symbols );

    return;
end );

#####################################################################
##
#M  Symbols
##

InstallMethod(
	Symbols,
	"standard",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return List( [1..Size(nr)], i -> 
			String( Concatenation( "n", String(i-1) ) ) );
  end );

#############################################################################
##
#M  MagicNumber
##

BindGlobal( "MAGIC_SUM",
  function ( base, list )
  local sum, i, limit;
    sum := 0; limit := Length(list);
    for i in [1..limit] do
      sum := sum + base^(limit-i)*list[i];
    od;

    return sum;
end );

InstallMethod(
	MagicNumber,
	"default",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local elms, ml, zeros, mml, e, zero;
    elms := AsSSortedList( nr ); zero := elms[1]-elms[1];

    ml := []; zeros := [];
    for e in elms do
      mml := List( elms, f -> f * e);
      Add( zeros, Number( mml, x -> x = zero ) );
      mml := List( elms, e -> Number( mml, x -> x = e ) );
      Sort( mml );
      Add( ml, mml );
    od;
    Sort( zeros );
    Sort( ml );

    return [zeros, ml];
  end );


#############################################################################
##
#M  IdLibraryNearRing
##

InstallMethod(
	IdLibraryNearRing,
	"nearring with One",
	true,
	[IsNearRing and IsNearRingWithOne],
	10,
  function ( nr )
  local position_list, wrongNumber, groupName, positions;

    if Size( nr ) > 15 then
	Error( "Only nearrings up to order 15 are in the library" );
    fi;

    position_list := [
	"2/1" ,   [ 2 ],
	"3/1" ,   [ 3 ],
	"4/1" ,   [ 8 ],
	"4/2" ,   [ 12, 13, 15, 16, 22 ],
	"5/1" ,   [ 7 ],
	"6/1" ,   [ 28 ],
	"6/2" ,   [ ],
	"7/1" ,   [ 19 ],
	"8/1" ,   [ 95 ],
	"8/2" ,   [ 814, 815, 840, 849, 857, 970, 971, 974, 975, 1121 ],
	"8/3" ,   [ 634, 638, 639, 640, 644, 645, 647, 648, 657, 658, 
                    660, 661, 668, 669, 672, 674, 677, 678, 693, 694, 
		    696, 698, 707, 708, 709, 711, 713, 714, 715, 717,
		    718, 796, 801, 812, 833 ],
	"8/4" ,   [ 842, 844, 848, 849, 1094, 1096, 1097 ],
	"8/5" ,   [ ],
	"9/1" ,   [ 185 ],
	"9/2" ,   [ 208, 209, 211, 215, 216, 219, 222, 225, 226, 255 ], 
	"10/1" ,   [ 168 ],
	"10/2" ,   [ ],
	"11/1" ,   [ 130 ],
	"12/1" ,   [ 1117 ],
	"12/2" ,   [ 2096, 2105, 2118, 2120, 2180, 2250, 2255, 2355, 2952 ],
	"12/3" ,   [ 37984 ],
	"12/4" ,   [ ],
	"12/5" ,   [ ],
	"13/1" ,   [ 429 ],
	"14/1" ,   [ 1632 ],
	"14/2" ,   [ ],
	"15/1" ,   [ 2618 ]    ];

    wrongNumber := IdLibraryNearRingWithOne( nr );
    groupName := Name( wrongNumber[1] );
    positions := position_list[ Position( position_list, groupName ) + 1 ];

    return [ wrongNumber[1], positions[wrongNumber[2]] ];
  end );
 
#############################################################################
##
#M  IdLibraryNearRing
##

InstallMethod(
	IdLibraryNearRing,
	"many classes",
	true,
	[IsNearRing],
	2,
  function ( nr )
  local addGroup, TWNumber, isoGroup, iso, autos, isos, n, isonr, candidates,
	nrCandidates, i, numbers, magicNumber;

  if Size( nr ) > 15 then
	Error( "Only nearrings up to order 15 are in the library" );
  else

   # find the isomorphic additive group

    addGroup := GroupReduct( nr );
    TWNumber := IdTWGroup( addGroup );
    isoGroup := TWGroup( TWNumber[1], TWNumber[2] );

    if NumberLibraryNearRings( isoGroup ) < 1500 then
	TryNextMethod();
    fi;

    candidates := []; numbers := [];
    magicNumber := MagicNumber( nr );

    iso := IsomorphismGroups( addGroup, isoGroup );
    autos := Automorphisms( isoGroup );

    isos := List( autos, aut -> iso * aut );

   # filter out possible candidates
   # possible candidates have similar multiplication table statistics

    for n in [1..NumberLibraryNearRings( isoGroup )] do
      isonr := LibraryNearRing( isoGroup, n );
      if MagicNumber( isonr ) = magicNumber then
        for iso in isos do
          if IsMultiplicationRespectingHomomorphism( iso, nr, isonr ) then
	      return [ isoGroup, n ];
          fi;
        od;
      fi;
    od;
  fi;

  end );

InstallMethod(
	IdLibraryNearRing,
	"default",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local addGroup, TWNumber, isoGroup, iso, autos, isos, n, isonr,
	nrCandidates, i, numbers, magicNumber;

  if Size( nr ) > 15 then
	Error( "Only nearrings up to order 15 are in the library" );
  elif Size( nr ) = 1 then
	return [ TWGroup(1,1), 1 ];
  else

   # find the isomorphic additive group

    addGroup := GroupReduct( nr );
    TWNumber := IdTWGroup( addGroup );
    isoGroup := TWGroup( TWNumber[1], TWNumber[2] );

    numbers := [];
    magicNumber := MagicNumber( nr );

   # filter out possible candidates
   # possible candidates have similar multiplication table statistics

    for n in [1..NumberLibraryNearRings( isoGroup )] do
      isonr := LibraryNearRing( isoGroup, n );
      if MagicNumber( isonr ) = magicNumber then
          Add( numbers, n );
      fi;
    od;

    nrCandidates := Length( numbers );

   # if there is only 1 candidate, nothing has to be checked

    if nrCandidates=1 then
	return [isoGroup, numbers[1]];
    fi;

   # otherwise for all group isomorphisms it has to be checked
   # if one of them is a nerring isomorphism

    iso := IsomorphismGroups( addGroup, isoGroup );
    autos := Automorphisms( isoGroup );

    isos := List( autos, aut -> iso * aut );

    for i in [1..nrCandidates-1] do
      isonr := LibraryNearRing( isoGroup, numbers[i] );
      for iso in isos do
        if IsMultiplicationRespectingHomomorphism( iso, nr, isonr ) then
	  return [ isoGroup, numbers[i] ];
        fi;
      od;
    od;

   # if the right nearring has not occured yet, it is the last one

    return [isoGroup, numbers[nrCandidates]];

  fi;
  end );

#############################################################################
##
#M  IdLibraryNearRingWithOne
##

InstallMethod(
	IdLibraryNearRingWithOne,
	"default",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local addGroup, TWNumber, isoGroup, iso, autos, isos, n, isonr, candidates,
	nrCandidates, i, numbers, magicNumber;

  if Size( nr ) > 31 then
    Error( "Only nearrings with One up to order 31 are in the library" );
  elif Identity( nr ) = fail then
    return fail;
  else

   # find the isomorphic additive group

    addGroup := GroupReduct( nr );
    TWNumber := IdTWGroup( addGroup );
    isoGroup := TWGroup( TWNumber[1], TWNumber[2] );

    if TWNumber in [ [16,5], [27,3] ] then
      Print( "Sorry, not all nearrings with this additive group are known\n");
      return fail;
    fi;
 
    candidates := []; numbers := [];
    magicNumber := MagicNumber( nr );

   # filter out possible candidates
   # possible candidates have similar multiplication table statistics

    for n in [1..NumberLibraryNearRingsWithOne( isoGroup )] do
      isonr := LibraryNearRingWithOne( isoGroup, n );
      if MagicNumber( isonr ) = magicNumber then
          Add( candidates, isonr );
          Add( numbers, n );
      fi;
    od;

    nrCandidates := Length( candidates );

   # if there is only 1 candidate, nothing has to be checked

    if nrCandidates=1 then
	return [isoGroup, numbers[1]];
    fi;

   # otherwise for all group isomorphisms it has to be checked
   # if one of them is a nerring isomorphism

    iso := IsomorphismGroups( addGroup, isoGroup );
    autos := Automorphisms( isoGroup );

    isos := List( autos, aut -> iso * aut );

    for i in [1..nrCandidates-1] do
      isonr := candidates[i];
      for iso in isos do
        if IsMultiplicationRespectingHomomorphism( iso, nr, isonr ) then
	  return [ isoGroup, numbers[i] ];
        fi;
      od;
    od;

   # if the right nearring has not occured yet, it is the last one

    return [isoGroup, numbers[nrCandidates]];

  fi;
  end );

#############################################################################
##
#M  IsIsomorphicNearRing
##

InstallMethod(
	IsIsomorphicNearRing,
	"unequal size",
	true,
	[IsNearRing, IsNearRing],
	100,
  function ( nr1, nr2 )
    if Size( nr1 ) = Size( nr2 ) then
	TryNextMethod();
    else
	return false;
    fi;
  end );

InstallMethod(
	IsIsomorphicNearRing,
	"additive groups not isomorphic",
	true,
	[IsNearRing, IsNearRing],
	80,
  function ( nr1, nr2 )
    if IsIsomorphicGroup( GroupReduct( nr1 ), GroupReduct( nr2 ) ) then
	TryNextMethod();
    else
	return false;
    fi;
  end );

InstallMethod(
	IsIsomorphicNearRing,
	"just one One",
	true,
	[IsNearRing, IsNearRing],
	70,
  function ( nr1, nr2 )
    if ( Identity( nr1 ) = fail and Identity( nr2 ) = fail ) or
       ( Identity( nr1 ) <> fail and Identity( nr2 ) <> fail ) then
	TryNextMethod();
    else
	return false;
    fi;
  end );

InstallMethod(
	IsIsomorphicNearRing,
	"unequal magic numbers",
	true,
	[IsNearRing, IsNearRing],
	50,
  function ( nr1, nr2 )
    if  ( Size( nr1 ) > 31 ) or
	( Size(nr1) > 15 and Identity( nr1 ) = fail ) then
		TryNextMethod();
    fi;

    if MagicNumber( nr1 ) = MagicNumber( nr2 ) then
      if Identity( nr1 ) = fail then
 	return IdLibraryNearRing( nr1 ) = IdLibraryNearRing( nr2 );
      else
	return IdLibraryNearRingWithOne( nr1 ) = IdLibraryNearRingWithOne( nr2 );
      fi;
    else
	return false;
    fi;
  end );

InstallMethod(
	IsIsomorphicNearRing,
	"default",
	true,
	[IsNearRing, IsNearRing],
	0,
  function ( nr1, nr2 )
  local iso, autgrp;
    iso := IsomorphismGroups( GroupReduct(nr1), GroupReduct(nr2) );
    autgrp := AutomorphismGroup( GroupReduct(nr2) );
    return ForAny( autgrp, 
	aut -> IsMultiplicationRespectingHomomorphism( iso*aut, nr1, nr2 ) );
end);




