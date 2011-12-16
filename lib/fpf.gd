
############################################################################
##
#O  IsFpfAutomorphismGroup( <phi>, <G>  )
##

DeclareOperation( "IsFpfAutomorphismGroup", [IsGroup, IsGroup] );


############################################################################
##
#O  FpfAutomorphismGroupsMaxSize( <G> )
##

DeclareOperation( "FpfAutomorphismGroupsMaxSize", [IsGroup] );


############################################################################
##
#O  IsFpfRepresentation( <matrices>, <F>  )
##

DeclareOperation( "IsFpfRepresentation", [IsList, IsDomain] );


############################################################################
##
#O  FpfRepresentationsCyclic( <p>, <size> )
##

DeclareOperation( "FpfRepresentationsCyclic",
			[IsInt, IsInt] );


############################################################################
##
#O  FpfRepresentationsMetacyclic( <p>, <m>, <r> )
##

DeclareOperation( "FpfRepresentationsMetacyclic",
			[IsInt, IsInt, IsInt] );


############################################################################
##
#O  FpfRepresentations2( <p>, <m>, <r>, <k> )
##

DeclareOperation( "FpfRepresentations2",
			[IsInt, IsInt, IsInt, IsInt] );


############################################################################
##
#O  FpfRepresentations3( <p>, <m>, <r> )
##

DeclareOperation( "FpfRepresentations3",
			[IsInt, IsInt, IsInt] );


############################################################################
##
#O  FpfRepresentations4( <p>, <m>, <r>, <k> )
##

DeclareOperation( "FpfRepresentations4",
			[IsInt, IsInt, IsInt, IsInt] );


############################################################################
##
#O  FpfAutGrpsC( <primepowers>, <dimensions>, <m> )
##

DeclareOperation( "FpfAutGrpsC", [IsList, IsList, IsInt] );


############################################################################
##
#O  FpfAutGrpsMC( <primepowers>, <dimensions>, <m>, <r> )
##

DeclareOperation( "FpfAutGrpsMC", [IsList, IsList, IsInt, IsInt] );


############################################################################
##
#O  FpfAutGrps2( <primepowers>, <dimensions>, <m>, <r>, <k> )
##

DeclareOperation( "FpfAutGrps2", [IsList, IsList, IsInt, IsInt, IsInt] );


############################################################################
##
#O  FpfAutGrps3( <primepowers>, <dimensions>, <m>, <r> )
##

DeclareOperation( "FpfAutGrps3", [IsList, IsList, IsInt, IsInt] );


############################################################################
##
#O  FpfAutGrps4( <primepowers>, <dimensions>, <m>, <r>, <k> )
##

DeclareOperation( "FpfAutGrps4", [IsList, IsList, IsInt, IsInt, IsInt] );


############################################################################
##
#O  FpfAutomorphismGroupsCyclic( <ints>, <m> )
##

DeclareOperation( "FpfAutomorphismGroupsCyclic", [IsList, IsInt] );


############################################################################
##
#O  FpfAutomorphismGroupsMetacyclic( <ints>, <m>, <r> )
##

DeclareOperation( "FpfAutomorphismGroupsMetacyclic", [IsList, IsInt, IsInt] );


############################################################################
##
#O  FpfAutomorphismGroups2( <ints>, <m>, <r>, <k> )
##

DeclareOperation( "FpfAutomorphismGroups2", [IsList, IsInt, IsInt, IsInt] );


############################################################################
##
#O  FpfAutomorphismGroups3( <ints>, <m>, <r> )
##

DeclareOperation( "FpfAutomorphismGroups3", [IsList, IsInt, IsInt] );


############################################################################
##
#O  FpfAutomorphismGroups4( <ints>, <m>, <r>, <k> )
##

DeclareOperation( "FpfAutomorphismGroups4", [IsList, IsInt, IsInt, IsInt] );


############################################################################
##
#O  FrobeniusGroup( <Phi>, <G> )
##
##

DeclareOperation( "FrobeniusGroup", [IsGroup, IsGroup] );

