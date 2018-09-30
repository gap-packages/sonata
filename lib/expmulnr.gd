################################################################################
##
#W  expmulnr.gd             Near-ring Library                   J"urgen Ecker
##
#Y  Copyright (C)
##
##  $Log: expmulnr.gd,v $
##  Revision 1.2  2001/03/21 14:41:07  juergen
##  erste korrekturen nach dem studium des tutorials
##
##  Revision 1.1.1.1  2000/02/21 15:59:03  hetzi
##  Sonata Project Start
##

#############################################################################
##
#C  IsExplicitMultiplicationNearRingElement	Category for expl mult elements
##

DeclareCategory( "IsExplicitMultiplicationNearRingElement",
			IsNearRingElement );

#############################################################################
##
#C  IsExplicitMultiplicationNearRingElementFamily	category for families
##							of nearring elements

DeclareCategoryFamily( "IsExplicitMultiplicationNearRingElement" );

#############################################################################
##
#A  ExplicitMultiplicationNearRingElementFamilies	List of Exp.Mult.
##							families to one fun
##							over different groups

DeclareAttribute( "ExplicitMultiplicationNearRingElementFamilies",
			IsGroup, "mutable" );

#############################################################################
##
#O  ExplicitMultiplicationNearRingElementFamily
##

DeclareOperation( "ExplicitMultiplicationNearRingElementFamily",
		[IsGroup,IsFunction] );

#############################################################################
##
#R  IsExplicitMultiplicationNearRingElementDefaultRep

DeclareRepresentation( "IsExplicitMultiplicationNearRingElementDefaultRep",
			IsExplicitMultiplicationNearRingElement and
			IsPositionalObjectRep,
			[] );

#############################################################################
##
#C  IsExplicitMultiplicationNearRingElementCollection	Category for
##							collections of
##							expl mult elements
##

DeclareCategoryCollections( "IsExplicitMultiplicationNearRingElement" );

#############################################################################
##
#C  IsExplicitMultiplicationNearRing		Category for collections 
##						of EMNR elements

DeclareSynonym( "IsExplicitMultiplicationNearRing",
	IsNearRing and IsExplicitMultiplicationNearRingElementCollection );

#############################################################################
##
#R  IsExplicitMultiplicationNearRingDefaultRep

DeclareRepresentation( "IsExplicitMultiplicationNearRingDefaultRep",
			IsExplicitMultiplicationNearRing and
			IsComponentObjectRep and IsAttributeStoringRep,
			["additiveGroup","multiplication"] );

#############################################################################
##
#O  ExplicitMultiplicationNearRing
##

DeclareOperation( "ExplicitMultiplicationNearRing", [IsGroup,IsFunction] );

#############################################################################
##
#F  ExplicitMultiplicationNearRingNC
##

DeclareGlobalFunction( "ExplicitMultiplicationNearRingNC" );

#############################################################################
##
#R  IsExplicitMultiplicationNearRingEnumerator
##

DeclareRepresentation( "IsExplicitMultiplicationNearRingEnumerator",
			IsComponentObjectRep,
			["additiveGroup","multiplication"] );

#############################################################################
##
#O  NearRingMultiplicationByOperationTable
##

DeclareOperation( "NearRingMultiplicationByOperationTable",
		[IsGroup, IsMatrix, IsList] );

