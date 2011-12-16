############################################################################
##
#O  IsPairOfDicksonNumbers( <q>, <n> )
##

DeclareOperation( "IsPairOfDicksonNumbers",
		[IsInt, IsInt] );

############################################################################
##
#O  NumberOfDicksonNearFields( <q>, <n> )
##

DeclareOperation( "NumberOfDicksonNearFields", [IsInt, IsInt] );

############################################################################
##
#O  DicksonNearFields( <q>, <n> )
##

DeclareOperation( "DicksonNearFields", [IsInt, IsInt] );

############################################################################
##
#O  ExceptionalNearFields( <q> )
##

DeclareOperation( "ExceptionalNearFields", [IsInt] );

############################################################################
##
#O  AllExceptionalNearFields( )
##

DeclareOperation( "AllExceptionalNearFields", [] );

############################################################################
##
#O  PlanarNearRing( <G>, <phi>, <reps> )
##

DeclareOperation( "PlanarNearRing",
		[IsGroup, IsGroup, IsList] );

############################################################################
##
#O  OrbitRepresentativesForPlanarNearRing( <G>, <phi>, <I> )
##

DeclareOperation( "OrbitRepresentativesForPlanarNearRing",
		[IsGroup, IsGroup, IsInt] );

############################################################################
##
#O  WdNearRing( <G>, <psi>, <phi>, <reps> )
##

DeclareOperation( "WdNearRing",
		[IsGroup, IsMapping, IsGroup, IsList] );


###########################################################################
##
#P  IsWdNearRing( <nr> )
##

DeclareProperty( "IsWdNearRing", IsNearRing );




