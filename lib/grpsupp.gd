#############################################################################
##
#O  IsIsomorphicGroup( <G>, <H> ) . . Check if two groups G, H are isomorphic
##  V0.2 3.10.94
##  The return value is 'false' iff G and H are not isomorphic
##

DeclareOperation( "IsIsomorphicGroup", [IsGroup, IsGroup] );

##############################################################################
##
#A  OneGeneratedNormalSubgroups ( <G> ) 

DeclareAttribute( "OneGeneratedNormalSubgroups", IsGroup );

#####################################################################
##
#O  IsCharacteristic ( <G>, <U> )
##
## returns true iff the subgroup <U> is a characteristic
## subgroup of <G>.
##

#DeclareOperation( "IsCharacteristic", [IsGroup, IsGroup] );

#####################################################################
##
#A  IsCharacteristicInParent ( <U> )
##
## returns true iff the subgroup <U> is a characteristic
## subgroup of <G>.
##

DeclareAttribute( "IsCharacteristicInParent", IsGroup );

#####################################################################
##
#O  IsFullinvariant ( <G>, <U> )
##
## returns true iff the subgroup <U> is a fullinvariant
## subgroup of <G>.
##

DeclareOperation( "IsFullinvariant", [IsGroup, IsGroup] );

#####################################################################
##
#P  IsFullinvariantInParent ( <U> )
##
## returns true iff the subgroup <U> is a characteristic
## subgroup of <G>.
##

DeclareProperty( "IsFullinvariantInParent", IsGroup );

#####################################################################
##
#O  AsPermGroup ( <G> )		returns an isomorphic Permutation Group
##

DeclareOperation( "AsPermGroup", [IsGroup] );

#####################################################################
##
#F PrintTable	print a group or a near ring
##		this is only the dispatcher
##

DeclareGlobalFunction( "PrintTable" );

#####################################################################
##
#O  PrintTable2 ( <domain>, <mode> )	print a group or near ring
##					with the given mode
##

DeclareOperation( "PrintTable2", [IsDomain, IsString] );

#####################################################################
##
#O  RepresentativesModNormalSubgroup( <G>, <N> ) returns a list of
##						 representatives of the
##						 classes of <G>/<N> in <G>

DeclareOperation( "RepresentativesModNormalSubgroup", [IsGroup, IsGroup] );

#####################################################################
##
#O  NontrivialRepresentativesModNormalSubgroup( <G>, <N> )
##				returns a list of nontrivial representatives
##				of the classes of <G>/<N> in <G>

DeclareOperation( "NontrivialRepresentativesModNormalSubgroup",
		[IsGroup, IsGroup] );

#####################################################################
##
#F  ScottSigma		Scott's Sigma function 

DeclareGlobalFunction( "ScottSigma" );

#####################################################################
##
#A  ScottLength( <G> )		returns the Scott-length of the group <G>
##

DeclareAttribute( "ScottLength", IsGroup );

#####################################################################
##
#O  TWGroup( <size>, <number> )  returns the group <size>/<number>
##

DeclareOperation( "TWGroup", [IsInt and IsPosRat, IsInt and IsPosRat] );

#############################################################################
##
#O  FastImageOfProjection( <DP>, <dpElm>, <i> )
##
##	for a direct product of n copies of a group <G>, the function
##	returns the image of the element <dpElm> of <DP> under the
##	projection onto the <i>th component.
##	This function is especially important in the case, that the
##	projection itself can not be computed, because the group is too
##	large.

DeclareOperation( "FastImageOfProjection",
	[IsGroup, IsMultiplicativeElementWithInverse, IsInt and IsPosRat] );
