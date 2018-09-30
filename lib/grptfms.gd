################################################################################
##
#W  grptfms.gd             Near-ring Library                   Christof N"obauer
##
#Y  Copyright (C)
##
##  $Log: grptfms.gd,v $
##  Revision 1.8  2008-11-13 14:18:17  stein
##  Replaced IsNearRingEnumerator by IsTransformationNearRingEnumerator
##
##  Revision 1.7  2002/01/18 07:20:50  erhard
##  I have added the function GraphOfMapping.
##
##  Revision 1.6  2001/06/28 18:56:43  erhard
##  I have added the functions EndoMappingByFunction, AsEndoMapping,
##  IdentityEndoMapping, IsIdentityEndoMapping, IsDistributiveEndoMapping.
##
##  Revision 1.5  2001/03/21 16:22:09  juergen
##  noch ein paar filter angepasst
##
##  Revision 1.4  2001/03/21 15:59:11  juergen
##  new operation EndoMappingByPositionList
##
##  Revision 1.3  2001/03/21 14:53:27  juergen
##  filter fuer isnearadditiveelementwithinverse angepasst
##
##  Revision 1.2  2001/03/21 14:41:07  juergen
##  erste korrekturen nach dem studium des tutorials
##
##  Revision 1.1.1.1  2000/02/21 15:59:03  hetzi
##  Sonata Project Start
##

DeclareOperation( "GroupGeneralMappingByGroupElement",
	[IsGroup,IsMultiplicativeElementWithInverse] ); 

InstallTrueMethod( IsNearRingElement, IsMapping );

InstallTrueMethod( IsNearAdditiveElementWithInverse, IsGeneralMapping );

DeclareAttribute( "GroupElementRepOfNearRingElement", IsNearRingElement );

DeclareAttribute( "EndoMappingFamily", IsGroup );

#############################################################################
##
#F  SizeFoldDirectProduct
##

DeclareAttribute( "SizeFoldDirectProduct", IsGroup );

#############################################################################
##
#C IsTransformationNearRing			Category for collections 
##						of FNR elements

DeclareSynonym( "IsTransformationNearRing",
	IsNearRing and IsGeneralMappingCollection );

#############################################################################
##
#R  IsTransformationNearRingRep		Representation for TfmNRs

DeclareRepresentation( "IsTransformationNearRingRep",
			IsTransformationNearRing and IsComponentObjectRep and
			IsAttributeStoringRep,
			["elementsFam"] );

#############################################################################
##
#O TransformationNearRing			over a group

DeclareOperation( "TransformationNearRing", [IsGroup] );

#############################################################################
##
#O TransformationNearRingByGenerators ( <G> , <gens> )
##						by GroupTransformations

DeclareOperation( "TransformationNearRingByGenerators", [IsGroup, IsList] );

#############################################################################
##
#O TransformationNearRingByAdditiveGenerators ( <G> , <gens> )
##						by GroupTransformations

DeclareOperation( "TransformationNearRingByAdditiveGenerators",
		[IsGroup, IsList] );

#############################################################################
##
#P  IsFullTransformationNearRing	nr = M(Gamma)

DeclareProperty( "IsFullTransformationNearRing", IsTransformationNearRing );

#############################################################################
##
#A  Gamma		the group the nearring acts on

DeclareAttribute( "Gamma", IsTransformationNearRing );

#############################################################################
##
#O  ConstantEndoMapping( <G>, <g> )	the constant mapping
##						on the group <G> that maps
##						every element to <g>

DeclareOperation( "ConstantEndoMapping",
		[IsGroup, IsMultiplicativeElementWithInverse] );


#############################################################################
##
#O  GraphOfMapping ( <m> )
##
##  returns the graph of the mapping m as a subset of <source> x <target>
##

DeclareOperation( "GraphOfMapping",
		   [IsMapping] );



#############################################################################
##
#O  AsExplicitMultiplicationNearRing( <nr> ) returns an
##		explicit multiplication nearring which is isomorphic to
##		the nearring <nr>

DeclareOperation( "AsExplicitMultiplicationNearRing", [IsNearRing] );

############################################################################
## MATRIX NEARRINGS                                                        #
############################################################################

#############################################################################
##
#P  MatrixNearRingFlag( <nr> )

DeclareProperty( "MatrixNearRingFlag", IsTransformationNearRing );

DeclareSynonym( "IsMatrixNearRing", HasMatrixNearRingFlag );

#############################################################################
##
#A  CoefficientRange( <matnr> )		the nearring of coefficients of
##					the matrix nearring <matnr>

DeclareAttribute( "CoefficientRange", IsNearRing and IsMatrixNearRing );

#############################################################################
##
#A  DimensionOfMatrixNearRing( <matnr> )	the dimension of the matrix
##						nearring <matnr>

DeclareAttribute( "DimensionOfMatrixNearRing",
		IsNearRing and IsMatrixNearRing );

#############################################################################
##
#O  MatrixNearRing( <ngroup>, <dim> )	construct the matrix nearring of
##					dimension <dim> over the N-group
##					<ngroup>.

DeclareOperation( "MatrixNearRing", [IsGroup, IsInt and IsPosRat] );

#JE eigentlich sollten wir IsGroup and IsNGroup fordern, N-Gruppen werden
#   aber erst spaeter definiert. Bei den Methoden steht dann aber beides.
#   Ergo: die Fehlermeldung, die man erhaelt, wenn man MatrixNearRing mit
#         einer ordinaeren Gruppe aufruft, ist NO METHOD FOUND und nicht,
#         wie man erwarten sollte OPERATION NOT APPLICABLE.
#   Kurz: es is eh wurscht

#########################################################################
##
#A  SomeInvariantSubgroupsOfGamma( <nr> )
##
##  returns the subset of the set of <nr>-invariant subgroups of Gamma(<nr>)
##  where all subgroups which are contained in another are deleted

DeclareAttribute( "SomeInvariantSubgroupsOfGamma",
		IsNearRing and IsTransformationNearRing );

#########################################################################
##
#P  IsConstantEndoMapping( <m> )
##

DeclareProperty( "IsConstantEndoMapping", IsMapping );

#########################################################################
##
#P  IsIdentityMapping( <m> )
##

DeclareProperty( "IsIdentityMapping", IsGroupGeneralMapping );

#############################################################################
##
#R  IsTransformationNearRingEnumerator	For nearrings with known additive group

DeclareRepresentation( "IsTransformationNearRingEnumerator",
			IsAttributeStoringRep,
			["additiveGroup", "elementsFam"] );

#############################################################################
##
#O  EndoMappingByPositionList( <group>, <list> )
##

DeclareOperation( "EndoMappingByPositionList", [IsGroup,IsList] );




#############################################################################
##
#O  EndoMappingByFunction( <group>, <fun> )
##

DeclareOperation( "EndoMappingByFunction", [IsGroup, IsFunction] );



#############################################################################
##
#O  AsEndoMapping( <map> )
##

DeclareOperation( "AsEndoMapping", [IsMapping] );



#############################################################################
##
#O  IdentityEndoMapping( <G> )
##

DeclareOperation( "IdentityEndoMapping", [IsGroup] );


#############################################################################
##
#O  IsIdentityEndoMapping( <endomap> )
##

DeclareOperation( "IsIdentityEndoMapping", [IsEndoMapping] );



#############################################################################
##
#O  IsDistributiveEndoMapping( <endomap> )
##

DeclareOperation( "IsDistributiveEndoMapping", [IsEndoMapping] );






