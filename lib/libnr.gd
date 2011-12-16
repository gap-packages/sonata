DeclareGlobalVariable( "NEARRING_PATH_NAME",
	"path to the directory containing the library of small near rings" );

DeclareGlobalVariable( "NEARRING_WITH_IDENTITY_PATH_NAME",
	"path to the directory containing the library of small near rings with identity" );

#############################################################################
##
#O  LibraryNearRing( <group>, <num> ). . . . . . . . get a nr from the library
##
##  This function 'extracts' a nearring from the nearring library files.
##

DeclareOperation( "LibraryNearRing",[IsGroup,IsInt] );

#############################################################################
##
#O  LibraryNearRingWithOne( <group>, <num> ). .get a nr from the library
##
##  This function 'extracts' a nearring from the nearring library files.
##

DeclareOperation( "LibraryNearRingWithOne", [IsGroup,IsInt] );

#############################################################################
##
#P  LibraryNearRingFlag

DeclareProperty( "LibraryNearRingFlag", IsNearRing );

DeclareSynonym( "IsLibraryNearRing", HasLibraryNearRingFlag );

#############################################################################
##
#A  Symbols		user-defined symbols for the operation tables
##

DeclareAttribute( "Symbols", IsCollection, "mutable" );

#####################################################################
##
#F  SetSymbolsSupervised( <N>, <L> )	use the symbols in the list <L>
##				when printing operation tables
##

DeclareGlobalFunction( "SetSymbolsSupervised" );

#############################################################################
##
#A  MagicNumber( <nr> )		compute an identification number for the
##				nearring <nr>. This number is NOT unique.

DeclareAttribute( "MagicNumber", IsNearRing );

#############################################################################
##
#A  IdLibraryNearRing( <nr> )	the isomorphic nearring in the library
##

DeclareAttribute( "IdLibraryNearRing", IsNearRing );

#############################################################################
##
#A  IdLibraryNearRingWithOne( <nr> )	returns the number of the
##						isomorphic nearring in the
##			library of nearrings with identity

DeclareAttribute( "IdLibraryNearRingWithOne",
		IsNearRing );

#############################################################################
##
#O  IsIsomorphicNearRing( <nr1>, <nr2> )
##

DeclareOperation( "IsIsomorphicNearRing", [IsNearRing, IsNearRing] );
