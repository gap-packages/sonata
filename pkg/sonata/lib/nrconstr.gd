#############################################################################
##
#P  DirectProductNearRingFlag
##

DeclareProperty( "DirectProductNearRingFlag", IsNearRing );

DeclareSynonym( "IsDirectProductNearRing", HasDirectProductNearRingFlag );

#############################################################################
##
#O  DirectProductNearRing		the result is an
##					ExplicitMultiplicationNearRing

DeclareOperation( "DirectProductNearRing", [IsNearRing, IsNearRing] );

#############################################################################
##
#P  FactorNearRingFlag
##

DeclareProperty( "FactorNearRingFlag", IsNearRing );

DeclareSynonym( "IsFactorNearRing", HasFactorNearRingFlag );

#############################################################################
##
#O  FactorNearRing( <N>, <I> )
##

DeclareOperation( "FactorNearRing", [IsNearRing, IsNRI] );
