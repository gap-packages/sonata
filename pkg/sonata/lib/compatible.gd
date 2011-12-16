######################################################################
##
#O  GeneratorsOfIntervallInCongruenceLattice( <G>, <N> )
##  a set of generators for the intervall [0,<N>] in the
##  congruence lattice of <G>

DeclareOperation( "GeneratorsOfIntervallInCongruenceLattice",
			[IsGroup, IsGroup] );

DeclareSynonymAttr( "GeneratorsOfCongruenceLattice",
			OneGeneratedNormalSubgroups );

######################################################################
##
#P  IsCompatibleEndoMapping( <tfm> )
##

DeclareProperty( "IsCompatibleEndoMapping", IsEndoMapping );

######################################################################
##
#O  CompatibleFunctionNearRing( <G> )
##  compute the nearring of all compatible functions on <G>
##  as the intersection of the nearrings of functions compatible
##  with chains of normal subgroups of <G>

DeclareOperation( "CompatibleFunctionNearRing", [IsGroup] );
DeclareOperation( "RestrictedCompatibleFunctionNearRing", [IsGroup,IsGroup] );

######################################################################
##
#F  FunctionsCompatibleWithNormalSubgroupChain( <G>, <[N1..Ns]> )
##
##  compute the nearring of all functions on <G> compatible with
##  N1..Ns (where <N1> <= <N2> <= ... <= <Ns>)
##

DeclareOperation( "FunctionsCompatibleWithNormalSubgroupChain", 
	[IsGroup, IsCollection] );

DeclareOperation( "ZeroSymmetricCompatibleFunctionNearRing", [IsGroup] );

######################################################################
##
#O  IsEndoMappingCompatibleWithNormalSubgroup
##
##  test f(x+n) - f(x) \in N for all n in N
##

DeclareOperation( 
	"IsEndoMappingCompatibleWithNormalSubgroup", 
	[IsEndoMapping,IsGroup] );

######################################################################
##
#O  CompatibleFunctionModNormalSubgroupNC 

DeclareOperation( "CompatibleFunctionModNormalSubgroupNC",
	[IsEndoMapping,IsMapping] );

######################################################################
##
#O  ClosureSubgroups( <G>, <list> )
##
##  <list> is a list of subgroups of <G>. ClosureSubgroups computes 
##  the closure of all subgroups in <list>

DeclareOperation( "ClosureSubgroups",
	[IsGroup,IsCollection] );

######################################################################
##
#A  MinimalNormalSubgroups
##

#DeclareAttribute( "MinimalNormalSubgroups", IsGroup );

######################################################################
##
#F  PeakOfnAtg( <G>, <g>, <n> )
##
##  constructs the tfm with value <n> at <g> and 0 everywhere else
##

DeclareGlobalFunction( "PeakOfnAtg" );

######################################################################
##
#F  ConstGrpTfmOnRightCosetOfN( <G>, <N>, <g>, <h> )
##
##  constructs the tfm with value <g> on the coset <h>+<N> and
##  0 everywhere else
##

DeclareGlobalFunction( "ConstGrpTfmOnRightCosetOfN" );

######################################################################
##
#F  ProjectionsOntoDirectFactors
##

DeclareGlobalFunction( "ProjectionsOntoDirectFactors" );

######################################################################
##
#P  Is1AffineComplete
##

DeclareProperty( "Is1AffineComplete", IsGroup );

######################################################################
##
#F  DirectFactorisation
##

DeclareGlobalFunction( "DirectFactorisation" );

######################################################################
##
#F  DirectFactorisationRelativePrime
##

DeclareGlobalFunction( "DirectFactorisationRelativePrime" );

