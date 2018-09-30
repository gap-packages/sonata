################################################################################
##
#W  grptfms.gi             Near-ring Library                   Christof N"obauer
##
#Y  Copyright (C)
##
##  $Log: grptfms.gi,v $
##  Revision 1.15  2015/04/09 13:43:54  stein
##  Added condition to make the method "abelian near ring (Binder e.a.)" for IsDistributiveNearring only applicable to abelian nearrings.
##
##  Revision 1.14  2012-11-07 13:22:59  stein
##  Adapted filters in method for \in.
##
##  Revision 1.13  2011-11-23 20:01:17  stein
##  New methods for Zero for elements of a nearring.
##  New methods for multiplying a nearring element with an integer (These should
##  be integrated into GAP in a future release and then removed from nr.gi).
##
##  Revision 1.12  2011-06-17 08:48:13  stein
##  changed order of methods for GroupElementRepOfNearRingElement
##
##  Revision 1.11  2008-11-13 14:17:16  stein
##  Restricted the Enumerator - Method to IsTransformationNearRing
##  Replaced IsNearRingEnumerator by IsTransformationNearRingEnumerator
##
##  Revision 1.10  2007/07/19 22:40:33  stein
##  removed reference to IsNearRingWithOne
##
##  Revision 1.9  2007/05/09 21:26:16  stein
##  put the full TransformationNearRing into IsNearRingWithOne
##
##  Revision 1.8  2004/02/26 16:22:43  juergen
##  new filter for Random method
##
##  Revision 1.7  2003/03/07 13:48:20  juergen
##  new method for IsDistributiveNearRing
##
##  Revision 1.6  2002/01/18 07:20:30  erhard
##  I have added the function GraphOfMapping.
##
##  Revision 1.5  2001/06/28 18:57:08  erhard
##  I have added the functions EndoMappingByFunction, AsEndoMapping,
##  IdentityEndoMapping, IsIdentityEndoMapping, IsDistributiveEndoMapping.
##
##  Revision 1.4  2001/03/21 16:22:09  juergen
##  noch ein paar filter angepasst
##
##  Revision 1.2  2001/03/21 14:41:07  juergen
##  erste korrekturen nach dem studium des tutorials
##
##  Revision 1.1.1.1  2000/02/21 15:59:03  hetzi
##  Sonata Project Start
##

#############################################################################
##
#F  SizeFoldDirectProduct ( <G> ) ...  returns the direct 
##   product G^|G| for the group G.
##

InstallMethod(
	SizeFoldDirectProduct,
	"of a group",
	true,
	[IsGroup],
	0,
  G -> DirectProduct( List ([1..Size (G)], i -> G) )
);

###############################################################################
##
#M  \+
##

InstallOtherMethod(\+, "for IsTransformationRepOfEndo", IsIdenticalObj,
  [IsTransformationRepOfEndo and IsEndoGeneralMapping,
  IsTransformationRepOfEndo and IsEndoGeneralMapping], 0,
function(m, n)
  local mntrans, en;

  if Source(n) <> Source(m) then
    TryNextMethod();
  fi;

  en := EnumeratorSorted(Source(m));
  mntrans := Transformation(List([1 .. Length(en)],
    i->Position(en, en[i^m!.transformation] * en[i^n!.transformation])));

  return EndoMappingByTransformation(Source(m),FamilyObj(m), mntrans);
end);


InstallOtherMethod(\+, "for IsEndoGeneralMapping", IsIdenticalObj,
  [IsGeneralMapping, IsGeneralMapping], 0,
function(m, n)

  if Source(n) <> Source(m) then
    TryNextMethod();
  fi;

  return MappingByFunction(Source(m),Source(m), g->(g^m)*(g^n));
end);

#############################################################################
##
#M  Zero	For NR elements		
##

InstallOtherMethod(
	ZeroMutable,
	"for IsTransformationRepOfEndo",
	true,
	[IsTransformationRepOfEndo and IsEndoGeneralMapping],
	0,
  function ( m ) 
  local en, o, mntrans;

  en := EnumeratorSorted(Source(m));
  o := Position( en, en[1]^0 ); 
  mntrans := Transformation(List([1 .. Length(en)], i-> o) );

  return EndoMappingByTransformation(Source(m),FamilyObj(m), mntrans);
end );

## the following conflicts with GAP's method Zero for homomorphisms
#InstallOtherMethod(
#	ZeroMutable,
#	"for IsEndoGeneralMapping",
#	true,
#	[IsGeneralMapping], 0,
#  function(m)
#  local en, o; 
#
#    en := Source(m);
#    o := Identity( en );
#   
#    return MappingByFunction(Source(m),Source(m), g-> o );
#  end );


###############################################################################
##
#M  AdditiveInverse
##

InstallOtherMethod(
	AdditiveInverseOp,
	"for IsTransformationRepOfEndo",
	true,
  	[IsTransformationRepOfEndo and IsEndoGeneralMapping],
	0,
  function(m)
  local en, mntrans;

  en := EnumeratorSorted(Source(m));
  mntrans := Transformation(List([1 .. Length(en)],
    i->Position(en, en[i^m!.transformation]^(-1))));

  return EndoMappingByTransformation(Source(m),FamilyObj(m), mntrans);
end);

InstallOtherMethod(
	AdditiveInverseOp,
	"for IsEndoGeneralMapping",
	true,
	[IsGeneralMapping], 0,
  function(m)
    return MappingByFunction(Source(m),Source(m), g->(g^m)^(-1));
  end );

#############################################################################
##
#M  NearRingElementByGroupRep

InstallMethod(
	NearRingElementByGroupRep,
	"Group element -> vector",
	true,
	[IsGroup,IsMultiplicativeElementWithInverse],
	0,
  function ( G , dpElement )   
    return GroupGeneralMappingByGroupElement( G, dpElement );
  end ); 

InstallMethod(
	EndoMappingFamily,
	"stupid",
	true,
	[IsGroup],
	0,
  G -> FamilyObj( TransformationRepresentation( IdentityMapping( G ) ) )
  );

InstallMethod(
	GroupGeneralMappingByGroupElement,
	"Group element -> mapping",
	true,
	[IsGroup,IsMultiplicativeElementWithInverse],
	0,
  function( G, g )
  local DP, elms, trafo, m, fam;
    DP := SizeFoldDirectProduct( G );
    fam := EndoMappingFamily( G );
    elms := EnumeratorSorted(G);
    trafo := TransformationNC( 
	List( [1..Length (DirectProductInfo(DP).groups)],
             i -> Position( elms, FastImageOfProjection( DP, g, i ) )
	    ) );
    m := EndoMappingByTransformation( G, fam, trafo );
    SetGroupElementRepOfNearRingElement( m, g );

    return m;
  end );

#############################################################################
##
#M  GroupElementRepOfNearRingElement

InstallMethod(
	GroupElementRepOfNearRingElement,
	"mapping -> group element",
	true,
	[IsMapping],
	0,
  function ( t )

  local dpElement,    # the element of the direct product to be returned
	DP,	      # a direct product of groups
	elms,	      # elements of the group the transformation acts on
	vec;	      # the transformation as a vector

    DP := SizeFoldDirectProduct( Source( t ) );
    elms := AsSSortedList( Source( t ) );
    dpElement := Identity( DP );
    vec := List( elms, x -> Position( elms, ImageElm( t, x ) ) );

    dpElement := Product( [1..Length(vec)],
			i -> Image (Embedding (DP, i), elms[ vec[i] ])
		 );

    return dpElement;
  end );

InstallMethod(
	GroupElementRepOfNearRingElement,
	"GroupGeneralMappingTrafoRep -> group element",
	true,
	[IsGroupGeneralMapping and IsTransformationRepOfEndo],
	0,
  function ( t )

  local fam,	      # family of the transformation
	dpElement,    # the element of the direct product to be returned
        i,            # counter through all elements of the vector 
	DP,	      # a direct product of groups
	elms,	      # elements of the group the transformation acts on
	vec;	      # the transformation as a vector

    DP := SizeFoldDirectProduct( Source( t ) );
    elms := AsSSortedList( Source( t ) );
    dpElement := Identity( DP );
    vec := ImageListOfTransformation( t!.transformation );

    dpElement := Product( [1..Length(vec)],
			i -> Image (Embedding (DP, i), elms[ vec[i] ])
		 );

    return dpElement;
  end );


#############################################################################
##
#M  TransformationNearRing				The Complete M(G)
##
## secret components:
##	GtoG:		G^|G|
##	elementsInfo:	the group the mappings act on

InstallMethod(
	TransformationNearRing,
	"generic",
	true,
	[IsGroup],
	0,
  function ( group )
  local fam, TfmNR;
    fam := EndoMappingFamily( group );

    TfmNR := Objectify(
		NewType(CollectionsFamily(fam),
		IsTransformationNearRingRep),
		rec() );

    TfmNR!.elementsInfo := group;

    SetGamma( TfmNR, group );
    SetOne( TfmNR, IdentityMapping( group ) );
    SetGroupReduct( TfmNR, SizeFoldDirectProduct( group ) );
    SetIsFullTransformationNearRing( TfmNR, true );

    return TfmNR;
  end );

#############################################################################
##
#M  TransformationNearRingByGenerators		From a list of
##						GroupTransformations
##
## secret components:
##	elmentsFam:	family of its elements
##	GtoG:		G^|G|, where G is the group the tfms act on

InstallMethod(
	TransformationNearRingByGenerators,
	"generic",
	true,
	[IsGroup, IsList],
	0,
  function ( group, gens )
  local fam, TfmNR;
#    if not ForAll(gens,IsMapping) then 
#	Error("Generators have to be mappings\n");
#    fi;

    # in the case that no generators are given create the trivial near ring
    if gens = [] then
	gens := [EndoMappingByTransformation(group,EndoMappingFamily(group),
		TransformationNC(List([1..Size(group)],i->1)))];
    fi;

    fam := FamilyObj(gens[1]);

    if not ForAll( gens, g -> Source(g) = group ) then
	Error("generating transformations have to act on the same group\n");
    fi;

    TfmNR := Objectify(
		NewType(CollectionsFamily(fam),
		IsTransformationNearRingRep),
		rec() );

    TfmNR!.elementsInfo := group;

    SetGamma( TfmNR, group);
    SetGeneratorsOfNearRing( TfmNR, gens );

    return TfmNR;
  end );

#############################################################################
##
#M  TransformationNearRingByAdditiveGenerators		From a list of
##							GroupTransformations
##
## secret components:
##	elmentsFam:	family of its elements

#  constructs the nearring with the help of TransformationNearRingByGenerators
#  and sets the attribute GroupReduct

InstallMethod(
	TransformationNearRingByAdditiveGenerators,
	"generic",
	true,
	[IsGroup, IsList],
	0,
  function ( group, gens )
  local fam, TfmNR;
    TfmNR := TransformationNearRingByGenerators( group, gens );
    SetGroupReduct( TfmNR,
	SubgroupNC( SizeFoldDirectProduct(group),
			List(gens,GroupElementRepOfNearRingElement) ) );

    return TfmNR;
  end );

#############################################################################
##
#M \in			For nearrings with known additive group 

InstallMethod(
	\in,
	"nearrings with known additive group",
	IsElmsColls,
	[IsNearRingElement and HasGroupElementRepOfNearRingElement, IsNearRing],
	0,
  function ( e , nr )
    return GroupElementRepOfNearRingElement(e) in GroupReduct(nr);
  end );

InstallMethod(
	\in,
	"nearrings with known additive group",
	IsElmsColls,
	[IsExplicitMultiplicationNearRingElement, IsNearRing],
	0,
  function ( e , nr )
    return GroupElementRepOfNearRingElement(e) in GroupReduct(nr);
  end );

InstallMethod(
	\in,
	"one of the generators",
	IsElmsColls,
	[IsNearRingElement, IsNearRing and HasGeneratorsOfNearRing],
	0,
  function ( e, nr )
    if e in GeneratorsOfNearRing(nr) then
	return true;
    else
	TryNextMethod();
    fi;
  end );

#############################################################################
##
#M  Enumerator		For transformation nearrings

InstallMethod(
	Enumerator,
	true,
	[ IsTransformationNearRing ],
	0,
  function( nr )
    return Objectify( NewType( FamilyObj( nr ), 
			IsList and IsTransformationNearRingEnumerator ),
           rec( groupReduct := GroupReduct(nr),
		elementsInfo := nr!.elementsInfo ) 
	);
  end );

#####################################################################
##
#M  Length			for nearring enumerators

InstallMethod(
	Length,
	true,
	[ IsList and IsTransformationNearRingEnumerator ],
	0,
    e -> Size( e!.groupReduct ) );

#####################################################################
##
#M  \[\]			for nearring enumerators

InstallMethod(
	\[\],
	true,
	[ IsList and IsTransformationNearRingEnumerator, IsPosInt ],
	0,
  function( e, pos )
  local groupElement, group;
    group := e!.groupReduct;
    groupElement := Enumerator(group)[pos];

    return NearRingElementByGroupRep( e!.elementsInfo, groupElement );
  end );

#####################################################################
##
#M  Position			for nearring enumerators

InstallMethod(
	Position,
	true,
	[ IsList and IsTransformationNearRingEnumerator, IsGroupGeneralMapping, IsZeroCyc ],
	0,
  function( e, nrelm, zero )
  local groupElement;
    groupElement := GroupElementRepOfNearRingElement(nrelm);

    return Position( e!.groupReduct, groupElement, zero );
  end );

#####################################################################
##
#M  ViewObj			for nearring enumerators
#
InstallMethod(
	ViewObj,
	true,
	[ IsTransformationNearRingEnumerator ],
	0,
  function( G )
    Print( "<enumerator of near ring>" );
  end );

#############################################################################
##
#M  Random	  	For nearrings with known additive group

InstallMethod(
	Random,
	"Random for nearrings with known additive group",
	true,
	[IsNearRing and IsExplicitMultiplicationNearRing],
	0,
  function ( nr )
    return NearRingElementByGroupRep( nr!.elementsInfo, Random( GroupReduct(nr) ) );
  end );

InstallMethod(
	Random,
	"Random for nearrings with known additive group",
	true,
	[IsNearRing and IsTransformationNearRing],
	0,
  function ( nr )
    return GroupGeneralMappingByGroupElement( Gamma(nr), Random( GroupReduct(nr) ) );
  end );

#############################################################################
##
#M  IsFullTransformationNearRing		For TfmNRs

InstallMethod(
	IsFullTransformationNearRing,
	"TfmNRs",
	true,
	[IsTransformationNearRing],
	0,
  function ( TfmNR )
    return Size(TfmNR) = Size( Gamma(TfmNR) )^Size( Gamma(TfmNR) );
  end );

#############################################################################
##
#M  ViewObj		For transformation nearrings

InstallMethod(
	ViewObj,
	"for TfmNRs",
	true,
	[IsTransformationNearRing and HasGroupReduct],
	2,
  function ( TfmNR )
    Print("TransformationNearRingBySubgroup(");
    View( GroupReduct( TfmNR ) );
    Print(", ... )");
  end );

InstallMethod(
	PrintObj,
	"for TfmNRs",
	true,
	[IsTransformationNearRing and HasGroupReduct],
	2,
  function ( TfmNR )
    Print("TransformationNearRingBySubgroup(");
    Print( GroupReduct( TfmNR ) );
    Print(", ... )");
  end );

InstallMethod(
	ViewObj,
	"for TfmNRsByGenerators",
	true,
	[IsTransformationNearRing and HasGeneratorsOfNearRing],
	4,
  function ( TfmNR )
    if Length( GeneratorsOfNearRing(TfmNR) ) > 2 then
	Print("< transformation nearring with ",
	       Length( GeneratorsOfNearRing(TfmNR) ),
              " generators >" );
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	ViewObj,
	"for TfmNRsByGenerators",
	true,
	[IsTransformationNearRing and HasGeneratorsOfNearRing],
	3,
  function ( TfmNR )
    Print("TransformationNearRingByGenerators(");
    View( GeneratorsOfNearRing(TfmNR) );
    Print(")");
  end );

InstallMethod(
	PrintObj,
	"for TfmNRsByGenerators",
	true,
	[IsTransformationNearRing and HasGeneratorsOfNearRing],
	3,
  function ( TfmNR )
    Print("TransformationNearRingByGenerators(");
    Print( GeneratorsOfNearRing(TfmNR) );
    Print(")");
  end );

InstallMethod(
	ViewObj,
	"for full transformation NearRings",
	true,
	[IsTransformationNearRing and HasIsFullTransformationNearRing
		and IsFullTransformationNearRing],
	2,
  function ( TfmNR )
    Print( "TransformationNearRing(" );
    View( Gamma(TfmNR) );
    Print( ")" );
  end );

InstallMethod(
	PrintObj,
	"for full transformation nearrings",
	true,
	[IsTransformationNearRing and HasIsFullTransformationNearRing
		and IsFullTransformationNearRing],
	2,
  function ( TfmNR )
    Print("TransformationNearRing(",Gamma(TfmNR),")");
  end );

#############################################################################
##
#F  ClosureNearRing( <transformations> )
##
##  compute the closure of the <transformations> under addition and
##  composition. 

ClosureNearRing := function( mappings )

#JE diese Funktion sollte lieber mit Transformationen arbeiten!

  local closure, Closure, # the constructed closure
	group,		  # the group on which the tf's operate
        elms,             # the elements of group
        l,                # the number of the elements
        firstrun,         # help var: indicates if first loop run
        tfl,tfl1,tfl2,    # help vars: transformation lists
        elmset,           # help var: contains the elms curr. in the closure 
        changed_elmset,   # help var: indicates if elmset has changed
        done,             # help var: indicates, when it is time to stop
        tfs, gens, fam;    

    # perform a simple closure algorithm
    tfs      := Flat( [ mappings ] );
    gens     := Set( List( tfs, t -> 
	ImageListOfTransformation(
			TransformationRepresentation(t)!.transformation) ) );
    closure  := ShallowCopy( gens );
    group    := Source(tfs[1]);
    fam      := EndoMappingFamily( group );
    elms     := AsSSortedList(group);
    l        := Length( elms );
    firstrun := true;
#    Print( closure, "\n" );

    repeat
      # step 1: add all sums  
      Info( InfoNearRing,1,"Step 1: Building sums of full mappings" );
      elmset         := closure;
      changed_elmset := true;
      done           := true;
      while changed_elmset do
        closure        := ShallowCopy( elmset );
        changed_elmset := false;
        for tfl1 in closure do
          for tfl2 in     gens     do  
            tfl := List( [1..l], i -> 
                     Position( elms, elms[ tfl1[i] ] * elms[ tfl2[i] ] ) );
            if not tfl in elmset then 
              AddSet( elmset, tfl ); 
              Info( InfoNearRing, 3, "Adding", tfl ); 
              changed_elmset := true; 
              done           := false;
            fi;
          od;
        od;
      od; 
      if not done or firstrun then 
        # step 2 build the multiplicative closure    
        Info( InfoNearRing, 1, "Step 2: Multiplying full mappings" );
        firstrun       := false;
        elmset         := closure;
        changed_elmset := true;
        done           := true;
        while changed_elmset do
          closure        := ShallowCopy( elmset );
          changed_elmset := false;
          for tfl1 in    gens  do
            for tfl2 in     closure     do
              tfl := tfl1{ tfl2 };
              if not tfl in elmset then 
                AddSet( elmset, tfl ); 
                Info( InfoNearRing, 3, "Adding", tfl ); 
                changed_elmset := true; 
                done           := false;
              fi;
            od;
          od;
          Info( InfoNearRing, 2, "size:", Length( elmset ) );
        od;
      fi;
    until done;
    Closure := [];
    for tfl in closure do
      AddSet( Closure,
	EndoMappingByTransformation( group, fam, Transformation( tfl ) ) );
    od;
  
  return Closure;

end;

#############################################################################
##
#M  ConstantEndoMapping ( <G>, <g> )

InstallMethod(
	ConstantEndoMapping,
	"default",
	IsCollsElms,
	[IsGroup, IsMultiplicativeElementWithInverse],
	0,
  function ( G , g )
    return MappingByFunction( G, G, x -> g );
  end );


#############################################################################
##
#M  GraphOfMapping ( <m> )
##
InstallMethod(
	GraphOfMapping,
	"default",
	true,
	[IsMapping],
	0,
  function ( m )
    return List (Source (m), x -> [x, Image (m, x)]);
  end );



#############################################################################
##
#M  GroupReduct( <transformation nearring> )
##

InstallMethod(
	GroupReduct,
	"TfmNRs",
	true,
	[IsTransformationNearRing and HasGeneratorsOfNearRing],
	0, 
  function ( nr )
  local gensAsGroupElms, id;
    gensAsGroupElms := 
	List( GeneratorsOfNearRing( nr ), GroupElementRepOfNearRingElement );
    id := gensAsGroupElms[1]^0;

    return NRClosureOfSubgroup( Group( gensAsGroupElms, id ),
				gensAsGroupElms,
				Gamma(nr) );
  end );

#############################################################################
##
#M  SubNearRingBySubgroupNC
##
##  No Check!

InstallMethod(
	SubNearRingBySubgroupNC,
	"TfmNRs",
	true,
	[IsTransformationNearRing, IsGroup],
	0,
  function ( nr, sg )
  local addGens, subnr;
    addGens := List( GeneratorsOfGroup(sg),
			gen -> NearRingElementByGroupRep( Gamma(nr), gen ) );
    subnr := TransformationNearRingByAdditiveGenerators( Gamma(nr), addGens );
    SetParent( subnr, nr );

    return subnr;
  end );

InstallMethod(
	SubNearRingBySubgroupNC,
	"TfmNRs",
	true,
	[IsTransformationNearRing, IsGroup],
	2,
  function ( nr, sg )
  local addGens, subnr;
    subnr := Objectify(
		NewType( FamilyObj(nr), IsTransformationNearRingRep), rec() );

    subnr!.elementsInfo := nr!.elementsInfo;

    SetGamma( subnr, Gamma( nr ) );
    SetGroupReduct( subnr, sg );

    SetParent( subnr, nr );

    return subnr;
  end );

#############################################################################
##
#M  InvariantSubNearRings		for M(G)
##

InstallMethod(
	InvariantSubNearRings,
	"M(G)",
	true,
	[IsTransformationNearRing and IsFullTransformationNearRing],
	0,
  function ( nr )
  local gamma, gens, invsnr;
    gamma := Gamma( nr );
    gens := List( GeneratorsOfGroup(gamma),
			gen -> ConstantEndoMapping( gamma, gen ) );
    gens := List( gens, GroupElementRepOfNearRingElement );

    invsnr := SubgroupNC( GroupReduct(nr), gens );

    return [ nr, SubNearRingBySubgroupNC( nr, invsnr ),
	SubNearRingBySubgroupNC( nr, TrivialSubgroup( GroupReduct( nr) ) ) ];
  end );

#############################################################################
##
#M  IsConstantEndoMapping		generic method

InstallMethod(
	IsConstantEndoMapping,
	"generic",
	true,
	[IsMapping],
	0,
  m -> Size( Image( m ) ) = 1
);

#############################################################################
##
#M  AsExplicitMultiplicationNearRing
##

InstallMethod(
	AsExplicitMultiplicationNearRing,
	"transformation nearrings",
	true,
	[IsTransformationNearRing],
	0,
  function ( nr )
  local gamma, mul;
    gamma := Gamma(nr);
    mul := function ( a, b )
	return GroupElementRepOfNearRingElement( NearRingElementByGroupRep( gamma, a ) * NearRingElementByGroupRep( gamma, b ) );
    end;
    return ExplicitMultiplicationNearRingNC( GroupReduct( nr ), mul );
  end );

############################################################################
## MATRIX NEARRINGS                                                        #
############################################################################

#############################################################################
##
#M  MatrixNearRing			a la  K. Smith

InstallMethod(
	MatrixNearRing,
	"a la K. Smith",
	true,
	[IsGroup and IsNGroup, IsInt and IsPosRat],
	0,
  function( ng, n )
  local cr_i, fr_ij, r, i, j, nr, mat, gamma, action, i_i, pi_j, MvdWFct;
    nr := NearRingActingOnNGroup( ng );
    action := ActionOfNearRingOnNGroup( ng );
    gamma := DirectProduct( List( [1..n], i -> ng ) );
    fr_ij := [];
    for r in GeneratorsOfNearRing( nr ) do
      for i in [1..n] do
	i_i := Embedding( gamma, i ); 
        for j in [1..n] do
          pi_j := Projection( gamma, j );
	  MvdWFct := dpElm -> Image( i_i, action( r, dpElm^pi_j ) );
          Add( fr_ij, MappingByFunction( gamma, gamma, MvdWFct ) );
        od;
      od;
    od;

    mat := TransformationNearRingByGenerators( gamma, fr_ij );

    SetMatrixNearRingFlag( mat, true );
    SetDimensionOfMatrixNearRing( mat, n );
    SetCoefficientRange( mat, nr );

    return mat;
  end );

#############################################################################
##
#M  MatrixNearRing			for d.g. nearrings

InstallMethod(
	MatrixNearRing,
	"d.g. case",
	true,
	[IsGroup and IsNGroup, IsInt and IsPosRat],
	2,
  function( ng, n )
  local fr_ij, r, i, j, nr, mat, gamma, action, i_i, pi_j, MvdWFct;
    nr := NearRingActingOnNGroup( ng );
    if not( HasIsDgNearRing( nr ) and IsDgNearRing( nr ) ) then
	TryNextMethod();
    fi;
    action := ActionOfNearRingOnNGroup( ng );
    gamma := DirectProduct( List( [1..n], i -> ng ) );
    fr_ij := [];
    for r in DistributiveElements( nr ) do
      for i in [1..n] do
	i_i := Embedding( gamma, i ); 
        for j in [1..n] do
          pi_j := Projection( gamma, j );
	  MvdWFct := dpElm -> Image( i_i, action( r, dpElm^pi_j ) );
          Add( fr_ij, MappingByFunction( gamma, gamma, MvdWFct ) );
        od;
      od;
    od;

    mat := TransformationNearRingByAdditiveGenerators( gamma, fr_ij );

    SetMatrixNearRingFlag( mat, true );
    SetDimensionOfMatrixNearRing( mat, n );
    SetCoefficientRange( mat, nr );

    return mat;
  end );

#############################################################################
##
#M  ViewObj				for matrix nearrings

InstallMethod(
	ViewObj,
	"matrix nearrings",
	true,
	[IsNearRing and IsMatrixNearRing],
	5,
  function( nr )
    Print( "MatrixNearRing( ",CoefficientRange(nr),", ",
			DimensionOfMatrixNearRing(nr)," )" );
  end );

#############################################################################
##
#M  IsAbelianNearRing			for matrix nearrings

InstallMethod(
	IsAbelianNearRing,
	"matrix nearrings",
	true,
	[IsNearRing and IsMatrixNearRing],
	0,
  function( m )
    return IsAbelianNearRing( CoefficientRange( m ) );
  end );

#############################################################################
##
#M  IsZeroSymmetricNearRing		for matrix nearrings

InstallMethod(
	IsZeroSymmetricNearRing,
	"matrix nearrings",
	true,
	[IsNearRing and IsMatrixNearRing],
	0,
  function( m )
    return IsZeroSymmetricNearRing( CoefficientRange( m ) );
  end );

#############################################################################
##
#M  IsAbstractAffineNearRing		for matrix nearrings

InstallMethod(
	IsAbstractAffineNearRing,
	"matrix nearrings",
	true,
	[IsNearRing and IsMatrixNearRing],
	2,
  function( m )
    if IsAbstractAffineNearRing( CoefficientRange( m ) ) then
	return true;
    else
	TryNextMethod();
    fi;
  end );

#############################################################################
##
#F  Ntimesg( <nr>, <g> )
#F  IsDistributiveNearRingElement( <nr>, <n> )
##

Ntimesg := function( N, g )
local E, Ng, e, i, G, fine_so_far;

  E := GeneratorsOfNearRing( N );
  Ng := Group( Set( List( E, n -> ImageElm( n, g ) ) ), g^0 );

  e := 0;
  while e < Length(E) do
    e := e + 1;
    i := 0;
    G := Enumerator(Ng);
    fine_so_far := true;
    while fine_so_far and i < Size(Ng) do
      i := i + 1;
      if not ( ImageElm( E[e], G[i] ) in Ng ) then
	fine_so_far := false;
      fi;
    od;
    if not fine_so_far then
      Ng := ClosureGroup( Ng, ImageElm( E[e], G[i] ) );
      e := 0;
    fi;
  od;

  return Ng;
end;

#IsDistributiveNearRingElement := function ( nr, n )
#local gamma, Ng, Ngs, CheckedNgs, i, h;
#
#  gamma := Gamma( nr );
#  Ngs := List( gamma, g -> Ntimesg( nr, g ) );
#  CheckedNgs := [];
#  i := 0;
#
#  while i < Length(Ngs) do
#    i := i + 1;
#    Ng := Ngs[i];
#    if ForAll( CheckedNgs, BigOne -> not ( IsSubset( BigOne, Ng ) ) ) then
#      h := MappingByFunction( Ng, Ng, x -> Image( n, x ) );
#      if not IsGroupHomomorphism( h ) then
#	return false;
#      fi;
#    fi;
#  od;
#
#  return true;
#end;

#############################################################################
##
#M  DistributiveElements		for TfmNrs
##

InstallMethod(
	DistributiveElements,
	"TfmNrs",
	true,
	[IsNearRing and IsTransformationNearRing],
	0,
  function ( nr )
  local gamma, Ng, Ngs, CheckedNgs, i, j, h, distr, all_endos, n;

    gamma := Gamma( nr );
    Ngs := List( gamma, g -> Ntimesg( nr, g ) );
 
    for i in [1..Length(Ngs)] do
      if ForAny( [1..Length(Ngs)], j -> i<>j and
					IsBound(Ngs[j]) and
					IsSubset( Ngs[j], Ngs[i] )
		) then
	  Unbind( Ngs[i] );
      fi;
    od;
    Ngs := Flat(Ngs);

    distr := [];

    for n in nr do
      CheckedNgs := [];
      i := 0;
      all_endos := true;
      while all_endos and i < Length(Ngs) do
	i := i + 1;
	Ng := Ngs[i];
        h := MappingByFunction( Ng, Ng, x -> ImageElm( n, x ) );
        if not IsGroupHomomorphism( h ) then
	  all_endos := false;
        fi;
      od;
      if all_endos then
        Add( distr, n );
      fi;
    od;

    return distr;
  end );

#########################################################################
##
#M  IsDistributiveNearRing
##

InstallMethod(
	IsDistributiveNearRing,
	"abelian near ring (Binder e.a.)",
	true,
	[IsNearRing and IsTransformationNearRing],
	50,
  function( nr )
    local F, G, E, gamma;
    gamma := Gamma( nr );
    if not IsAbelian(gamma) then
      TryNextMethod();
    fi;
    F := AdditiveGenerators( nr );
    for gamma in Gamma( nr ) do
      G := Ntimesg( nr, gamma );
      E := GeneratorsOfGroup( G );
      if ForAny( F, f -> ForAny( G, g -> ForAny( E, e ->
		Image( f, g*e ) <> Image( f, g ) * Image( f, e ) ) ) ) then
	return false;
      fi;
    od;
    return true;   
  end);

#########################################################################
##
#M  SomeInvariantSubgroupsOfGamma
##

InstallMethod(
	SomeInvariantSubgroupsOfGamma,
	"default",
	true,
	[IsNearRing and IsTransformationNearRing],
	0,
  function( nr )
  local E, gamma, g, new, invsg;
    E := GeneratorsOfNearRing( nr );
    gamma := Gamma( nr );
    invsg := [];
    for g in gamma do
      if ForAll( invsg, I -> not (g in I) ) then
        new := Ntimesg( nr, g );
	invsg := Filtered( invsg, I -> not( IsSubgroup( new, I ) ) );
	Add( invsg, new );
      fi;
    od;

    return invsg;
  end );

#########################################################################
##
#M  IsDistributiveNearRingElement( <nr>, <elm> )
##
##  returns true if <elm> distributes over all elements of <nr>
##

InstallMethod(
	IsDistributiveNearRingElement,
	"TfmNRs",
	IsCollsElms,
	[IsNearRing and IsTransformationNearRing, IsNearRingElement],
	0,
  function ( nr, n )
  local Ngs, i, h, Ng, NgGens, g, e;

  Ngs := SomeInvariantSubgroupsOfGamma( nr );
  i := 0;

  while i < Length(Ngs) do
    i := i + 1;
    Ng := Ngs[i];
    NgGens := GeneratorsOfGroup( Ng );
    for g in Ng do
      for e in NgGens do
	if ImageElm( n, g * e ) <> ImageElm( n, g ) * ImageElm( n, e ) then
	  return false;
	fi;
      od;
    od;
  od;

  return true;
end );

#########################################################################
##
#M  DistributiveElements		for transformation nearrings
##

InstallMethod(
	DistributiveElements,
	"TfmNrs",
	true,
	[IsNearRing and IsTransformationNearRing],
	10,
  function ( nr )
  local gamma, Ng, NgGens, Ngs, i, h, distGens, distr, all_endos, n, g, e;

    gamma := Gamma( nr );
    Ngs := SomeInvariantSubgroupsOfGamma( nr );
    distr := [];

    for n in ZeroSymmetricPart(nr) do
     if not n in distr then
      i := 0;
      all_endos := true;
      while all_endos and i < Length(Ngs) do
	i := i + 1;
	Ng := Ngs[i];
	NgGens := GeneratorsOfGroup( Ng );
	for g in Ng do
	  for e in NgGens do
	    if ImageElm( n, g * e ) <> ImageElm( n, g ) * ImageElm( n, e ) then
		all_endos := false;
	    fi;
	  od;
	od;
      od;
      if all_endos then
	UniteSet( distr,
		AsSSortedList( Semigroup(TransformationRepresentation(n) ) ) );
      fi;
     fi;
    od;

    return distr;
  end );

#########################################################################
##
#M  GroupReduct	TfmNRs generated by distributive elements
##

InstallMethod(
	GroupReduct,
	"distributive generators",
	true,
	[IsNearRing and IsTransformationNearRing],
	50,
  function ( tfmnr )
  local gens;
    gens := GeneratorsOfNearRing( tfmnr );
	# distributivity can be checked without knowing all elements of the nr
    if not ForAll( gens, g -> IsDistributiveNearRingElement( tfmnr, g ) ) then
	TryNextMethod(); # the default closure algorithm
    fi;

    Info( InfoNearRing, 1, "computing multiplicative semigroup" );
    gens := List( gens, TransformationRepresentation );
    gens := AsList( Semigroup( gens ) );

    return Group( List( gens, GroupElementRepOfNearRingElement ) );
  end );

#########################################################################
##
#M  IsIdentityMapping
##

InstallMethod(
	IsIdentityMapping,
	"compare with identity mapping",
	true,
	[IsGroupGeneralMapping],
	0,
  m -> m = IdentityMapping( Source(m) )
);

#########################################################################
##
#M  AsList			for TfmNRs
##

InstallMethod(
	AsList,
	"for TfmNRs",
	true,
	[IsNearRing and IsTransformationNearRing],
	0,
  function( nr )
  local G;
    G := Gamma( nr );
    return List( AsList( GroupReduct(nr) ), 
		 t -> GroupGeneralMappingByGroupElement( G, t ) );
  end );



#########################################################################
##
#M  	EndoMappingByPositionList
##

InstallMethod(
	EndoMappingByPositionList,
	"list -> endomapping",
	true,
	[IsGroup,IsList],
	0,
  function( G, list )
    return EndoMappingByTransformation( G, 
				      EndoMappingFamily( G ), 
				      Transformation( list) );
  end );






#########################################################################
##
#M  	EndoMappingByFunction
##

InstallMethod(
	EndoMappingByFunction,
	"function -> endomapping",
	true,
	[IsGroup, IsFunction],
	0,
  function( G, fun )
  #
      local elementsOfG;
  #
    elementsOfG := AsSSortedList (G);
    return EndoMappingByPositionList (G,
                                      List ([1..Size (G)],
                                      i -> Position (
                                             elementsOfG,
                                             fun (elementsOfG [i])
                                                    )
                                           )
                                     );
  end );




#########################################################################
##
#M  	AsEndoMapping
##

InstallMethod(
        AsEndoMapping,
	"mapping -> endomapping",
	true,
	[IsMapping],
	0,
  function( map )
  #
      local G;
  #
  if not (IsGroup (Source (map)) and
          IsGroup (Range  (map)) and
          IsIdenticalObj (Source (map), Range (map))) then
     TryNextMethod ();
  fi;
  #
  G := Source (map);
  return EndoMappingByFunction (G, x -> Image ( map, x ));
  end);



#########################################################################
##
#M  	IdentityEndoMapping
##

InstallMethod(
        IdentityEndoMapping,
	"identity on G",
	true,
	[IsGroup],
	0,
  function( G )
     return  EndoMappingByFunction ( G, x -> x );    
  end);



#########################################################################
##
#M  	IsIdentityEndoMapping
##

InstallMethod(
        IsIdentityEndoMapping,
	"is identity on G",
	true,
	[IsEndoMapping],
	0,
  function( endomap )
     return IsIdentityMapping ( endomap ); 
  end);



#########################################################################
##
#M  	IsDistributiveEndoMapping
##

InstallMethod(
        IsDistributiveEndoMapping,
	"is a group homomorphism",
	true,
	[IsEndoMapping],
	0,
  function( endomap )
     return IsGroupHomomorphism ( endomap ); 
  end);










