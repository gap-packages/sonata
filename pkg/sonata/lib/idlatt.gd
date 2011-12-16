############################################################################
##
#O  ClosureNearRingIdeal( <I>, <J> ) ... compute the ideal I+J

DeclareOperation( "ClosureNearRingIdeal", [IsNRI,IsObject] );

#############################################################################
##
#O  ClosureNearRingLeftIdeal( <I>, <J> ) ... compute the left ideal I+J

DeclareOperation( "ClosureNearRingLeftIdeal", [IsNRI,IsObject] );

#############################################################################
##
#O  ClosureNearRingRightIdeal( <I>, <J> ) ... compute the right ideal I+J

DeclareOperation( "ClosureNearRingRightIdeal", [IsNRI,IsObject] );

#############################################################################
##
#P  IsSimpleNearRing ( <N> )		<N> is simple
##

DeclareProperty( "IsSimpleNearRing", IsNearRing );

#############################################################################
##
#O  NearRingCommutator( <I>, <J> )   returns the commutator of the ideals
##				     <I> and <J>

DeclareOperation( "NearRingCommutator", [IsNRI, IsNRI] );

#############################################################################
##
#A  NearRingCommutatorsTable( <N> )
##

DeclareAttribute( "NearRingCommutatorsTable", IsNearRing );

#############################################################################
##
#O  PrintNearRingCommutatorsTable( <N> )
##

DeclareOperation( "PrintNearRingCommutatorsTable", [IsNearRing] );







