##############################################################################
##
#W  nr.gd             Near-ring Library                   J"urgen Ecker
##
#Y  Copyright (C)
##
##  $Log: nr.gd,v $
##  Revision 1.4  2007-05-09 22:45:31  stein
##  added functions IsNearRingUnit, NearRingUnits
##
##  Revision 1.3  2002/01/17 18:25:52  juergen
##  two versions of IsultiplicationRespectingHomomorphism - both with errors
##  code cleaned
##
##  Revision 1.2  2001/03/21 14:41:07  juergen
##  erste korrekturen nach dem studium des tutorials
##
##  Revision 1.1.1.1  2000/02/21 15:59:03  hetzi
##  Sonata Project Start
##


## 13.01.00 PM: new attribute NRMultiplication
##		new attribute NRRowEndos


#############################################################################
##
#I  InfoNearRing

DeclareInfoClass( "InfoNearRing" );

#############################################################################
##
#O  NearRingElementByGroupRep		Element of the additive group -> nearring element

DeclareOperation( "NearRingElementByGroupRep", [IsObject,IsMultiplicativeElementWithInverse] );

#############################################################################
##
#O  IntRepOfObj		Nearring element -> element of the addtitive group
#
#DeclareOperation( "IntRepOfObj", [IsNearRingElement] );

#############################################################################
##
#O  AsTransformationNearRing( < N > )		compute an isomorphic
##						TfmNR

DeclareOperation( "AsTransformationNearRing", [IsNearRing] );

#############################################################################
##
#A  NearRingUnits( <R> )
##
##  `Units' returns the group of units of the near-ring <R>.
##  This may either be returned as a list or as a group.
##
##  An element $r$ is called a *unit* of a near-ring $R$, if $r$ has an
##  inverse in $R$.
##  It is easy to see that the set of units forms a multiplicative group.
##
##NECESSARY BECAUSE 'Units' IS DECLARED ONLY FOR RINGS IN THE GAP-FILE RING.GD
##SHOULD EVENTUALLY BE CHANGED TO NEARRINGS THERE  
##
DeclareAttribute( "NearRingUnits", IsNearRing );
DeclareOperation( "IsNearRingUnit", [ IsNearRing, IsNearRingElement ] );

############################################################################
##
#P  IsAbelianNearRing

DeclareProperty( "IsAbelianNearRing", IsNearRing );

############################################################################
##
#P  IsAbstractAffineNearRing
 
DeclareProperty( "IsAbstractAffineNearRing", IsNearRing );

############################################################################
##
#P  IsDistributiveNearRing

DeclareProperty( "IsDistributiveNearRing", IsNearRing );

############################################################################
##
#P  IsBooleanNearRing 

DeclareProperty( "IsBooleanNearRing", IsNearRing );

############################################################################
##
#P  IsDgNearRing 

DeclareProperty( "IsDgNearRing", IsNearRing );

############################################################################
##
#P  IsIntegralNearRing 

DeclareProperty( "IsIntegralNearRing", IsNearRing );

############################################################################
##
#P  IsNilNearRing 

DeclareProperty( "IsNilNearRing", IsNearRing );

############################################################################
##
#P  IsNilpotentNearRing 

DeclareProperty( "IsNilpotentNearRing", IsNearRing );

############################################################################
##
#P  IsPrimeNearRing 

DeclareProperty( "IsPrimeNearRing", IsNearRing );

############################################################################
##
#P  IsPMNearRing 

DeclareProperty( "IsPMNearRing", IsNearRing );

############################################################################
##
#P  IsQuasiregularNearRing 

DeclareProperty( "IsQuasiregularNearRing", IsNearRing );

############################################################################
##
#P  IsRegularNearRing 

DeclareProperty( "IsRegularNearRing", IsNearRing );

############################################################################
##
#P  IsNilpotentFreeNearRing 

DeclareProperty( "IsNilpotentFreeNearRing", IsNearRing );

############################################################################
##
#P  IsPlanarNearRing 

DeclareProperty( "IsPlanarNearRing", IsNearRing );

############################################################################
##
#P  IsNearField

DeclareProperty( "IsNearField", IsNearRing );

############################################################################
##
#A  Distributors

DeclareAttribute( "Distributors", IsNearRing );

############################################################################
##
#A  DistributiveElements

DeclareAttribute( "DistributiveElements", IsNearRing );

############################################################################
##
#A  ZeroSymmetricElements

DeclareAttribute( "ZeroSymmetricElements", IsNearRing );

############################################################################
##
#A  IdempotentElements

DeclareAttribute( "IdempotentElements", IsNearRing );

############################################################################
##
#A  NilpotentElements

DeclareAttribute( "NilpotentElements", IsNearRing );

############################################################################
##
#A  QuasiregularElements

DeclareAttribute( "QuasiregularElements", IsNearRing );

############################################################################
##
#A  RegularElements

DeclareAttribute( "RegularElements", IsNearRing );

############################################################################
##
#A  GroupReduct

DeclareAttribute( "GroupReduct", IsCollection );

#####################################################################
##
#O  IsMultiplicationRespectingHomomorphism ( <hom>, <nr>, <nr> )
##
##		check whether a homomorphism between the group reducts
##		respects the near ring multiplications also

DeclareOperation( "IsMultiplicationRespectingHomomorphism",
			[IsGeneralMapping, IsNearRing, IsNearRing] );

#############################################################################
##
#P  IsZeroSymmetricNearRing	returns true if 0x=x0 for all x
##

DeclareProperty( "IsZeroSymmetricNearRing", IsNearRing );

#############################################################################
##
#O  SubNearRingBySubgroupNC ( <nr>, <sg> ) .... the subnearring of <nr>
##						determined by the subgroup
##					<sg> of the additive group of <nr>. 
##  No Check!

DeclareOperation( "SubNearRingBySubgroupNC", [IsNearRing, IsGroup] );

#############################################################################
##
#A  InvariantSubNearRings( <N> ). . . . . . . compute all inv subnr's of <N>.
##

DeclareAttribute( "InvariantSubNearRings", IsNearRing );

#############################################################################
##
#A  SubNearRings( <N> ) . . . . . . . . . . . . . compute all subnr's of <N>.
##

DeclareAttribute( "SubNearRings", IsNearRing );

#############################################################################
##
#F  NoetherianQuotient
##

DeclareGlobalFunction( "NoetherianQuotient" );

#############################################################################
##
#O  AsGroupReductElement( <nrelm> )
##

DeclareOperation( "AsGroupReductElement", [IsNearRingElement] );

#############################################################################
##
#O  AsNearRingElement( <nr>, <grpelm> )
##

DeclareOperation( "AsNearRingElement",
		[IsNearRing, IsMultiplicativeElementWithInverse] );

#############################################################################
##
#A  GeneratorsOfNearRing
##

DeclareAttribute( "GeneratorsOfNearRing", IsNearRing );

#############################################################################
##
#A  AdditiveGenerators( <X> )	returns a set of additive generators of <X>
##

DeclareAttribute( "AdditiveGenerators", IsNearRingElementCollection );

#############################################################################
##
#F  NRClosureOfSubgroup( <G>, <gens>, <fam> )
##
##	<G>: the subgroup
##	<gens>: a set of generators of the nearring
##	<fam>: the family of the elements of the ExpMulNearRing
##	       the group the mappings act on for a TfmNR

DeclareGlobalFunction( "NRClosureOfSubgroup" );

#########################################################################
##
#O  IsDistributiveNearRingElement( <nr>, <elm> )
##
##  returns true if <elm> distributes over all elements of <nr>
##

DeclareOperation( "IsDistributiveNearRingElement",
		  [IsNearRing, IsNearRingElement] );
 

#############################################################################
##
#A  NRMultiplication( <N> )    returns the multiplication function of <N>   
##

DeclareAttribute( "NRMultiplication", IsNearRing );


#############################################################################
##
#A  NRRowEndos( <N> )    	returns the list of endomorphisms x -> n*x
##  				corresponding to the elements n of <N>
##

DeclareAttribute( "NRRowEndos", IsNearRing, "mutable" );
