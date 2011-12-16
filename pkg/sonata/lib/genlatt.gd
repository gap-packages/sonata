############################################################################
##
#C  IsGeneralLattice			category for GeneralLattices
##

DeclareCategory( "IsGeneralLattice", IsCollection );

############################################################################
##
#F  IsGeneralLatticeFamily
##

DeclareCategoryFamily( "IsGeneralLattice" );

############################################################################
##
#R  IsGeneralLatticeDefaultRep		def. rep. for GeneralLattices
##

DeclareRepresentation( "IsGeneralLatticeDefaultRep",
			IsComponentObjectRep and IsAttributeStoringRep,
			[] );

############################################################################
##
#O  GeneralLattice( <coll>, <fct>, <string> )
##

DeclareOperation( "GeneralLattice", [IsCollection, IsFunction, IsString] );

############################################################################
##
#A  LessList( <linzlatt> )
##

DeclareAttribute( "LessList", IsGeneralLattice );

############################################################################
##
#O  Less( <L>, <x>, <y> )
##

DeclareOperation( "Less", [IsGeneralLattice, IsInt, IsInt] );

############################################################################
##
#O  SubCoverOfJI( <L>, <x> )
##

DeclareOperation( "SubCoverOfJI", [IsGeneralLattice, IsInt] );

############################################################################
##
#O  Join( <L>, <x>, <y> )
##

DeclareOperation( "Join", [IsGeneralLattice, IsInt, IsInt] );

############################################################################
##
#O  Meet( <L>, <x>, <y> )
##

DeclareOperation( "Meet", [IsGeneralLattice, IsInt, IsInt] );

############################################################################
##
#A  JoinIrreducibles( <linzlatt> )
##

DeclareAttribute( "JoinIrreducibles", IsGeneralLattice );

############################################################################
##
#O  IsJoinIrreducible( <linzlatt>, <int> )
##

DeclareOperation( "IsJoinIrreducible", [IsGeneralLattice, IsInt] );

############################################################################
##
#O  IsCoveringPair( <genlatt>, <pair> )
##

DeclareOperation( "IsCoveringPair", [IsGeneralLattice, IsList] );

############################################################################
##
#P  IsSC1Group( <G> )
##

DeclareProperty( "IsSC1Group", IsGroup );

############################################################################
##
#O  IsProjectivePairOfPairs( <genlatt>, <pair1>, <pair2> )
##

DeclareOperation( "IsProjectivePairOfPairs",
		[IsGeneralLattice, IsList, IsList] );

############################################################################
##
#A  AlphaBar( <G> )
##

DeclareAttribute( "AlphaBar", IsGroup and IsSC1Group );
