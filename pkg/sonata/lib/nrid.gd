############################################
##
##  functions for near-ring ideals
##

#############################################################################
##
#C  IsNRI			Every sort of ideal of a near ring

DeclareCategory( "IsNRI",
		IsCollection and
		IsNearAdditiveMagma and
		IsNearRingElementCollection );

#############################################################################
##
#R  IsNRIDefaultRep		Default rep for any sort of NR ideal

DeclareRepresentation( "IsNRIDefaultRep",
			IsNRI and IsComponentObjectRep and
			IsAttributeStoringRep,
			["elementsFam"] );

#############################################################################
##
#O  NRI			constructs an NRI-object

DeclareOperation( "NRI", [IsNearRing] );

#############################################################################
##
#P  IsNearRingLeftIdeal

DeclareProperty( "IsNearRingLeftIdeal", IsNRI );

#############################################################################
##
#P  IsNearRingRightIdeal

DeclareProperty( "IsNearRingRightIdeal", IsNRI );

#############################################################################
##
#P  IsNearRingIdeal

DeclareProperty( "IsNearRingIdeal", IsNRI );

#############################################################################
##
#O  NearRingIdealByGenerators

DeclareOperation( "NearRingIdealByGenerators", [IsNearRing,IsList] );

#############################################################################
##
#O  NearRingLeftIdealByGenerators

DeclareOperation( "NearRingLeftIdealByGenerators", [IsNearRing,IsList] );

#############################################################################
##
#O  NearRingRightIdealByGenerators

DeclareOperation( "NearRingRightIdealByGenerators", [IsNearRing,IsList] );

#############################################################################
##
#A  GeneratorsOfNearRingIdeal

DeclareAttribute( "GeneratorsOfNearRingIdeal", IsNRI );

#############################################################################
##
#A  GeneratorsOfNearRingLeftIdeal

DeclareAttribute( "GeneratorsOfNearRingLeftIdeal", IsNRI );

#############################################################################
##
#A  GeneratorsOfNearRingRightIdeal

DeclareAttribute( "GeneratorsOfNearRingRightIdeal", IsNRI );

#############################################################################
##
#O  SubNearRing

DeclareOperation( "SubNearRing", [IsNearRing,IsCollection] );

#############################################################################
##
#O  IsSubgroupNearRingLeftIdeal

DeclareOperation( "IsSubgroupNearRingLeftIdeal", [IsNearRing, IsGroup] );

#############################################################################
##
#O  IsSubgroupNearRingRightIdeal

DeclareOperation( "IsSubgroupNearRingRightIdeal", [IsNearRing, IsGroup] );

#############################################################################
##
#O  NearRingIdealClosureOfSubgroup

DeclareOperation( "NearRingIdealClosureOfSubgroup", [IsNearRing, IsGroup] );

#############################################################################
##
#O  NearRingLeftIdealClosureOfSubgroup

DeclareOperation( "NearRingLeftIdealClosureOfSubgroup",
		[IsNearRing, IsGroup] );

#############################################################################
##
#O  NearRingRightIdealClosureOfSubgroup

DeclareOperation( "NearRingRightIdealClosureOfSubgroup",
			[IsNearRing, IsGroup] );

#############################################################################
##
#O  AsSubNearRing

DeclareOperation( "AsSubNearRing",
		[IsNearRing, IsNearRingElementCollection] );

#############################################################################
##
#O  AsNearRingIdeal

DeclareOperation( "AsNearRingIdeal",
		[IsNearRing, IsNearRingElementCollection] );

#############################################################################
##
#O  AsNearRingLeftIdeal

DeclareOperation( "AsNearRingLeftIdeal",
		[IsNearRing, IsNearRingElementCollection] );

#############################################################################
##
#O  AsNearRingRightIdeal

DeclareOperation( "AsNearRingRightIdeal",
		[IsNearRing, IsNearRingElementCollection] );

#############################################################################
##
#A  NearRingRightIdeals

DeclareAttribute( "NearRingRightIdeals", IsNearRing );

#############################################################################
##
#A  NearRingLeftIdeals

DeclareAttribute( "NearRingLeftIdeals", IsNearRing );

#############################################################################
##
#A  NearRingIdeals

DeclareAttribute( "NearRingIdeals", IsNearRing );

#############################################################################
##
#P  IsPrimeNearRingIdeal

DeclareProperty( "IsPrimeNearRingIdeal", IsNRI and IsNearRingIdeal );

#############################################################################
##
#P  IsMaximalNearRingIdeal

DeclareProperty( "IsMaximalNearRingIdeal", 
			IsNRI and IsNearRingIdeal );

#############################################################################
##
#R  IsNearRingIdealEnumerator

DeclareRepresentation( "IsNearRingIdealEnumerator",
			IsAttributeStoringRep,
			["additiveGroup","elementsFam"] );

