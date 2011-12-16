############################################################################
##
#C  IsDesign			category for designs
##

DeclareCategory( "IsDesign", IsObject );

############################################################################
##
#F  IsDesignFamily
##

DeclareCategoryFamily( "IsDesign" );

############################################################################
##
#O  DesignFamily( <t>, <v>, <k>, <lambda> )
##

DeclareOperation( "DesignFamily", [IsInt, IsInt, IsInt, IsInt] );

############################################################################
##
#R  IsDesignDefaultRep		def. rep. for designs
##

DeclareRepresentation( "IsDesignDefaultRep",
			IsComponentObjectRep and IsAttributeStoringRep,
			[] );

############################################################################
##
#A  PointsOfDesign( <D> )
##

DeclareAttribute( "PointsOfDesign", IsDesign );

############################################################################
##
#A  BlocksOfDesign( <D> )
##

DeclareAttribute( "BlocksOfDesign", IsDesign );

############################################################################
##
#O  IsPointIncidentBlock( <D>, <pointnr>, <blocknr> )
##

DeclareOperation( "IsPointIncidentBlock", [IsDesign, IsInt, IsInt] );

############################################################################
##
#O  PointsIncidentBlocks( <D>, <blocknrs> )
##

DeclareOperation( "PointsIncidentBlocks", [IsDesign, IsList] );

############################################################################
##
#O  BlocksIncidentPoints( <D>, <pointnrs> )
##

DeclareOperation( "BlocksIncidentPoints", [IsDesign, IsList] );

############################################################################
##
#A  DesignParameter( <D> )
##

DeclareAttribute( "DesignParameter", IsDesign );

############################################################################
##
#A  IncidenceMat( <D> )
##

DeclareAttribute( "IncidenceMat", IsDesign );

############################################################################
##
#O  PrintIncidenceMat( <D> )
##

DeclareOperation( "PrintIncidenceMat", [IsDesign] );

############################################################################
##
#O  BlockIntersectionNumbersK( <D>, <k> )
##

DeclareOperation( "BlockIntersectionNumbersK", [IsDesign, IsInt] );

############################################################################
##
#P  IsCircularDesign( <D> )
##

DeclareProperty( "IsCircularDesign", IsDesign );

############################################################################
##
#O  DesignFromPointsAndBlocks( <P>, <B> )
##

DeclareOperation( "DesignFromPointsAndBlocks", [IsList, IsList] );

############################################################################
##
#O  DesignFromIncidenceMat( <M> )
##

DeclareOperation( "DesignFromIncidenceMat", [IsMatrix] );

############################################################################
##
#O  DesignFromFerreroPairStar( <G>, <Phi> )
##

DeclareOperation( "DesignFromFerreroPairStar", [IsGroup, IsGroup] );

############################################################################
##
#O  DesignFromFerreroPairBlank( <G>, <Phi> )
##

DeclareOperation( "DesignFromFerreroPairBlank", [IsGroup, IsGroup] );

############################################################################
##
#F  DesignFromPlanarNearRing( <N> )
##

DeclareGlobalFunction( "DesignFromPlanarNearRing" );

############################################################################
##
#O  DesignFromPlanarNearRingStar( <N> )
##

DeclareOperation( "DesignFromPlanarNearRingStar", [IsNearRing] );

############################################################################
##
#O  DesignFromPlanarNearRingBlank( <N> )
##

DeclareOperation( "DesignFromPlanarNearRingBlank", [IsNearRing] );

############################################################################
##
#O  DesignFromWdNearRing( <N> )
##

DeclareOperation( "DesignFromWdNearRing", [IsNearRing] );

##########################################################################
##
#F  AutomorphismGroupOfCyclicGroup := function( G, k )
##
##	returns the automorphism group of size <k> of a cyclic group <G>
##	of prime power order if existing

DeclareGlobalFunction( "AutomorphismGroupOfCyclicGroup" );


############################################################################
##
#F  DesignFromPair( G, Phi )
##

DeclareGlobalFunction( "DesignFromPair" );

############################################################################
##
#F  PrintDesignParameterPBIBD( D )
##

DeclareGlobalFunction( "PrintDesignParameterPBIBD" );
