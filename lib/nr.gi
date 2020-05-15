##############################################################################
##
#W  nr.gi             Near-ring Library                   J"urgen Ecker
##
#Y  Copyright (C)
##
##  $Log: nr.gi,v $
##  Revision 1.10  2011-11-23 20:01:17  stein
##  New methods for Zero for elements of a nearring.
##  New methods for multiplying a nearring element with an integer (These should
##  be integrated into GAP in a future release and then removed from nr.gi).
##
##  Revision 1.9  2007-07-19 22:19:07  stein
##  added a new method for One for TransformationNearRings
##
##  Revision 1.8  2007/05/09 22:44:14  stein
##  added functions Zero, Representative, IsNearRingUnit, NearRingUnits
##
##  Revision 1.7  2003/04/04 10:25:30  juergen
##  final revision 2.1: erhard and juergen
##
##  Revision 1.6  2003/04/02 09:39:26  juergen
##  eliminated InParent
##
##  Revision 1.5  2002/01/17 18:25:52  juergen
##  two versions of IsMultiplicationRespectingHomomorphism - both with errors
##  code cleaned
##
##  Revision 1.4  2002/01/03 14:12:33  juergen
##  binder/mayr part 2
##
##  Revision 1.3  2001/07/16 10:05:36  stein
##  documentation of IsWdNearRing and IsPlanarNearRing
##  examples were updated
##
##  Revision 1.2  2001/03/21 14:41:07  juergen
##  erste korrekturen nach dem studium des tutorials
##
##  Revision 1.1.1.1  2000/02/21 15:59:03  hetzi
##  Sonata Project Start
##

#############################################################################
##
#M  IsAdditivelyCommutative			For NRs

InstallMethod(
	IsAdditivelyCommutative,
	"NRs",
	true,
	[IsNearRing],
	0,
  N -> IsAbelianNearRing( N )
);

### DIESE METHODE MUSS UNBEDINGT WIEDER ENTFERNT WERDEN!
### NOW NECESSARY SO THAT OUR METHODS FOR ZERO AND ONE ARE NOT USED FOR
### SOME GAP-RINGS OR FIELDS
InstallMethod(
	IsLDistributive,
	"NRs",
	true,
	[IsNearRing],
	0,
  N -> false
);

#############################################################################
##
#M  \-			For NR elements

InstallOtherMethod(
	AdditiveInverseOp,
	"Nearring Elements",
	true,
	[IsNearRingElement],
	0,
  function ( a )
  local fam;
    fam := FamilyObj(a);
    return NearRingElementByGroupRep( fam, GroupElementRepOfNearRingElement(a)^(-1) );
  end );
 
InstallOtherMethod(
	\-,
	"2x IsNearAdditiveElementWithInverse",
	IsIdenticalObj,
	[IsNearAdditiveElementWithInverse,
		IsNearAdditiveElementWithInverse],
	0,
  function ( a , b )
    return a + (-b);
  end );


#############################################################################
##
#M  Zero		Return the Zero of the nearring
##

InstallMethod(
	Zero,
	"generic method for nearrings",
	true,
	[IsNearRing],
	0,
  function ( nr )    

    if IsAdditiveGroup( nr ) and IsLDistributive( nr ) then
	TryNextMethod();
    fi;

    return NearRingElementByGroupRep( nr!.elementsInfo,
					Identity(GroupReduct(nr)) );
  end );

#############################################################################
##
#M  One			Return the One of the nearring

# install as OtherMethod, the near ring need not have a 1

InstallOtherMethod(
	One,
	"generic method for transformation nearrings given by generators",
	true,
	[IsTransformationNearRing],
	0,
  function ( nr )

  local nrgens, 		# near-ring generators of nr
        G,			# group where nr acts on
   	gens,
        GN,  			# subgroup G*N of G
	GNE,
        one, found, xone,
        xnrgens,
  	x, y;
	
#Print( "generic method for transformation nearrings given by generators \n" );

##JE eigentlich gehoert hier die Prioritaet fuer die entsprechenden Methoden
##   bei Ringen und Koerpern erhoeht
    if IsAdditiveGroup( nr ) and IsLDistributive( nr ) then
	TryNextMethod();
    fi;
 
  nrgens := GeneratorsOfNearRing( nr );
  G := Gamma( nr );
  if ForAny( nrgens, IsBijective ) then
    return IdentityMapping( G );
  fi;
  gens := GeneratorsOfGroup( G );

## compute G*N

  GN := Subgroup( G, Concatenation( List( nrgens, n ->
						List( gens, x -> x^n ) ) ) );
  GNE := Union( List( nrgens, n -> Image( n, GN ) ) );
  while ForAny( GNE, x -> not x in GN ) do
#Print( "closure again for GNE \n" );
    GN := ClosureSubgroupNC( GN, GNE );
    GNE := Union( List( nrgens, n -> Image( n, GN ) ) );
  od;

  xone := [];
  for x in AsSSortedList(G) do
    xnrgens := List( nrgens, n -> x^n );
    found := false;
    for y in Enumerator(GN) do
      if ForAll( [1..Length(nrgens)], k -> y^nrgens[k] = xnrgens[k] ) then
        if found then
## there is no one in nr
#Print( "too many solutions for x,y = ", [x,y], "\n" );
          return fail;
        else
## the value for one on x has been found
#Print( " found y = ", y, "\n" );
          found := true;
          Add( xone, y ); 
        fi;
      fi;
    od;   
 
    if not found then
## there is no one in nr
#Print( "no solution found for x =", x, "\n" );
      return fail;
    fi;
  od;    

  one := EndoMappingByTransformation( G, EndoMappingFamily( G ),
	Transformation( List( xone, x -> Position( AsSSortedList(G), x ) ) ) );

## it still remains to check whether one is an element of nr

  if not one in nr then
#Print( "one is not contained \n" );
    return fail;
  fi;   

  return one;
  end );


InstallOtherMethod(
	One,
	"generic method for nearrings",
	true,
	[IsNearRing],
	0,
  function ( nr )
#JE eigentlich gehoert hier die Prioritaet fuer die entsprechenden Methoden
#   bei Ringen und Koerpern erhoeht
    if IsAdditiveGroup( nr ) and IsLDistributive( nr ) then
	TryNextMethod();
    fi;
    return First( Enumerator(nr), i ->
			ForAll( GeneratorsOfNearRing(nr), g -> i*g = g ) and
			ForAll( nr, e -> e*i = e )
		);
  end );

#############################################################################
##
#M  Representative( <R> ) . . . . . . . . . . . . one element of a near-ring
##
InstallMethod( Representative,
    "for a near-ring with generators",
    true,
    [ IsNearRing and HasGeneratorsOfNearRing ], 0,
    RepresentativeFromGenerators( GeneratorsOfNearRing ) );

#############################################################################
##
#M  IsUnit( <R>, <r> )  . . . . . . . . . . . .  test if an element is a unit
##
## ACTUALLY THE GAP-FUNCTION IsUnit SHOULD BE DECLARED FOR NEAR-RINGS 
## INSTEAD OF RINGS IN ring.gd AND IsNearRingUnit SHOULD BE RENAMED IsUnit
## SAME FOR NearRingUnits
##
InstallMethod( IsNearRingUnit,
    "for a near-ring with known units",
    IsCollsElms,
    [ IsNearRing and HasNearRingUnits, IsNearRingElement ], 0,
    function ( R, r )
    return r in NearRingUnits( R );
    end );

InstallMethod( IsNearRingUnit,
    "for a transformation near-ring with identity mapping",
    IsCollsElms,
    [ IsTransformationNearRingRep, IsNearRingElement ], 0,
    function ( R, r )
    local one, G;

    one:= One( R );
    G := Gamma( R );
    if one <> IdentityMapping( G ) or not IsFinite( G ) then
      TryNextMethod();
    else
# units are bijective functions
      return IsInjective( r );
    fi;
    end );

InstallMethod( IsNearRingUnit,
    "default",
    IsCollsElms,
    [ IsNearRing, IsNearRingElement ], 0,
    function ( R, r )
    local one;

    one:= One( R );
    if one = fail then
      return false;
    else
# simply try to find the inverse
      return r <> Zero( R ) and First( Enumerator(R), i -> i*r = one ) <> fail;
    fi;
    end );


#############################################################################
##
#M  Units( <R> )  . . . . . . . . . . . . . . . . . . . units of a near-ring
##

InstallMethod( NearRingUnits,
    "for a transformation near-ring with identity mapping",
    true,
    [ IsTransformationNearRingRep ], 0,
    function ( R )
    local one,
	  G,
          units,
          elm;

    one := One( R );
    G := Gamma( R );
    if one <> IdentityMapping( G ) or not IsFinite( G ) then
      TryNextMethod();
    fi;

    units:= GroupByGenerators( [], one );
    for elm in Enumerator( R ) do
      if IsNearRingUnit( R, elm ) and not elm in units then
        units:= ClosureGroupDefault( units, elm );
      fi;
    od;
    return units;
    end );

InstallMethod( NearRingUnits,
    "for a (finite) near-ring",
    true,
    [ IsNearRing ], 0,
    function ( R )
    local one,
          units,
          elm;

    one := One( R );

    if one = fail then
      return [];
    else
      return Filtered( R, n -> ForAny( R, m -> m * n = one ) );
    fi;

  end );


#############################################################################
##
#M  IsCommutative		For nearrings

InstallMethod(
	IsCommutative,
	"generic method for nearrings",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local id;
    return ForAll( nr, c -> ForAll( nr,e -> c * e = e * c ) );
  end );

############################################################################
##
#M  IsAbelianNearRing

InstallMethod(
	IsAbelianNearRing,
	"check additive group",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return IsAbelian(GroupReduct(nr));
  end );

############################################################################
##
#M  IsAbstractAffineNearRing

InstallMethod(
	IsAbstractAffineNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return IsAbelianNearRing( nr ) and
                 ( ZeroSymmetricElements( nr ) = DistributiveElements( nr ) );
  end );  

############################################################################
##
#M  IsDistributiveNearRing

InstallMethod(
	IsDistributiveNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return Length( DistributiveElements( nr ) ) = Size( nr );
  end );  

InstallMethod(
	IsDistributiveNearRing,
	"test all elements",
	true,
	[IsNearRing],
	1, # faster in the moment
  function ( nr )
    return ForAll( nr, d -> ForAll( nr, a -> ForAll( nr, b ->
              (a+b)*d = (a*d) + (b*d) ) ) );
  end );

############################################################################
##
#M  IsBooleanNearRing 

InstallMethod(
	IsBooleanNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return Length( IdempotentElements( nr ) ) = Size( nr );
  end );

InstallMethod(
	IsBooleanNearRing,
	"test all elements",
	true,
	[IsNearRing],
	1, # faster in the moment
  function ( nr )
    return ForAll( nr, i -> i^2 = i );
  end );  

############################################################################
##
#M  IsDgNearRing 

InstallMethod(
	IsDgNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local distrelm;
    distrelm := List( DistributiveElements( nr ), GroupElementRepOfNearRingElement );
    return Size( Subgroup( GroupReduct( nr ), distrelm ) ) = Size( nr );
  end );  

############################################################################
##
#M  IsIntegralNearRing 

InstallMethod(
	IsIntegralNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local zero;
    zero := 0 * Enumerator(nr)[1];

    return ForAll( nr, x -> (x = zero) or ForAll( nr, 
			y -> (y = zero) or ( y * x <> zero ) ) ); 
  end );

############################################################################
##
#M  IsNilNearRing 

InstallMethod(
	IsNilNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return Length( NilpotentElements( nr ) ) = Size( nr ); 
  end );  

InstallMethod(
	IsNilNearRing,
	"test all elements",
	true,
	[IsNearRing],
	1, # faster in the moment
  function ( nr )
  local zero;
    zero := 0 * Enumerator(nr)[1];  

    return ForAll( nr, n -> ForAny( [1..Size(nr)], k -> n^k = zero ) );
  end );

############################################################################
##
#M  IsNilpotentNearRing 

InstallMethod(
	IsNilpotentNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local prod, elms, previous_prod, m, n, forever;
    forever := false;
    elms := AsList( nr );
    previous_prod := ShallowCopy( elms );
        
    repeat
          
      prod := []; 
      for m in previous_prod do
	for n in elms do
 	  AddSet( prod, n*m );
	od;
      od;
      if prod = previous_prod then 
            return false;
      elif Size( prod ) = 1 then 
            return true;
      else 
            previous_prod := ShallowCopy( prod );
      fi;

    until forever;

  end );

############################################################################
##
#M  IsPrimeNearRing 

InstallTrueMethod( IsPrimeNearRing, IsNearRing and IsIntegralNearRing );

InstallMethod(
	IsPrimeNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local ideals, zero;
    ideals := Filtered( NearRingIdeals( nr ), I -> Size(I) > 1 ); 
    zero := NearRingElementByGroupRep(  nr!.elementsInfo,
					Identity(GroupReduct(nr))  );

    return ForAll( ideals, I -> ForAll( ideals, J ->
           ForAny( I, i -> ForAny( J, j -> i * j <> zero ) ) ) );  
  end );  

############################################################################
##
#M  IsPMNearRing   # every prime ideal maximal 

InstallMethod(
	IsPMNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )

  return ForAll( Filtered( NearRingIdeals( nr ), id -> 
                 IsPrimeNearRingIdeal( id ) ), i -> 
                 IsMaximalNearRingIdeal( i ) );

  end );    

############################################################################
##
#M  IsQuasiregularNearRing 

InstallMethod(
	IsQuasiregularNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return Length( QuasiregularElements( nr ) ) = Size( nr );
  end );  

############################################################################
##
#M  IsRegularNearRing 

InstallMethod(
	IsRegularNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return Length( RegularElements( nr ) ) = Size( nr );
  end );  

InstallMethod(
	IsRegularNearRing,
	"test all elements",
	true,
	[IsNearRing],
	1, # faster in the moment
  function ( nr )
    return ForAll( nr, x -> ForAny( nr, y -> ( x * y ) * x = x ) );
  end );  

############################################################################
##
#M  IsNilpotentFreeNearRing 

InstallMethod(
	IsNilpotentFreeNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return Length( NilpotentElements( nr ) ) = 1; 
  end );  

InstallMethod(
	IsNilpotentFreeNearRing,
	"test all elements",
	true,
	[IsNearRing],
	1, # faster in the moment
  function ( nr )
  local zero;
    zero := 0 * Enumerator(nr)[1];  

    return ForAll( nr, n -> n = zero or
			    ForAll( [1..Size(nr)], k -> n^k <> zero ) );
  end );  

############################################################################
##
#M  IsPlanarNearRing 

InstallMethod(
	IsPlanarNearRing,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( N )

  local	elms, phi, size, k, planar,
	x, p;
# endos, addGroup;

    elms := AsList( N );
    size := Size( N );

#  find the possible automorphisms as columns in the multiplication table
#  of <N>

    phi := Collected( List( elms, x -> x*elms ) );

#  check that all columns (except the zero)  occur the same number of times
#  and all possible automorphisms are fixedpointfree 


    k := Size( phi );
    planar := ( k >= 3 ) and
#  the zero function:
	Set( phi[1][1] ) = [elms[1]] and
#  all columns occur the same number of times
	Size( Set( List( phi{[2..k]}, p -> p[2] ) ) ) = 1 and	
#  automorphisms:
 	ForAll( phi{[2..k]}, p -> Size( Set( p[1] ) ) = size and
#  One mapping or fixedpointfree:
		( p[1] = elms or 
		  ForAll( [2..size], x -> p[1][x] <> elms[x]  ) ) );

#    SetIsPlanarNearRing( N, planar );

    return planar; 

#    addGroup := GroupReduct(N);
#    phi   := Set( addGroup!.phi );
#    size  := Size( addGroup );
#    endos := Endomorphisms( addGroup );


#    return Size( phi ) >= 3 and
#        ForAll( phi, p -> p = 1 or p = Length( endos ) or 
#          ( Size( Image(endos[p]) ) = size and
#            ForAll( addGroup, x -> x = () or Image( endos[p], x ) <> x ) ) );

  end );  

############################################################################
##
#M  IsNearField

InstallMethod(
	IsNearField,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local elms, id;

    if not IsElementaryAbelian( GroupReduct( nr ) ) then
      return false;
    fi;

    id := One(nr);
    if id=fail then return false; fi;

    return Size( Filtered( nr, e -> ForAny( nr, x -> x * e = id ) ) ) 
	 = Size( nr ) - 1; 
  end );  

############################################################################
##
#M  Distributors

InstallMethod(
	Distributors,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )  
  local dbs, a, b, c;
    dbs := [];
    for a in nr do
      for b in nr do
        for c in nr do
          AddSet( dbs, ((b+c)*a) - ((b*a)+(c*a)) ); 
        od;
      od;
    od;

    return dbs;
  end );  

############################################################################
##
#M  DistributiveElements

InstallMethod(
	DistributiveElements,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return Filtered( nr, d -> ForAll( nr, a -> ForAll( nr, b ->
              (a+b)*d = (a*d) + (b*d) ) ) );
  end );

############################################################################
##
#M  ZeroSymmetricElements
##  Note: this function works only for RIGHT nearrings, i.e. it computes
##        all elements n s.t n0 = 0. (Note that in a RIGHT nearring 
##        0n = 0 is always true).

InstallMethod(
	ZeroSymmetricElements,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local elms, zero;
    zero := 0 * Enumerator(nr)[1];
  
    return Filtered( nr, n -> zero * n = zero );
  end );

############################################################################
##
#M  IdempotentElements

InstallMethod(
	IdempotentElements,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return Filtered( nr, i -> i^2 = i );
  end );  

############################################################################
##
#M  NilpotentElements

InstallMethod(
	NilpotentElements,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local elm, size, zero, npelms, k, e, old_e;
    size   := Size( nr );
    zero := 0 * Enumerator(nr)[1];
    npelms := [ ];
  
    for elm in nr do
      k := 1; e := ShallowCopy( elm );
      old_e := zero;
      while e <> zero and e <> old_e and k < size do  
        k := k + 1;
        old_e := ShallowCopy( e );
        e := elm * e;
      od;
      if e = zero then Add( npelms, [ elm, k ] ); fi;
    od;
  
    return npelms;
  end );

############################################################################
##
#M  QuasiregularElements

InstallMethod(
	QuasiregularElements,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local elms, z, elms_to_test, qr_elms, A, li, Lz;
    elms    := AsList( nr );
    qr_elms := List( NilpotentElements( nr ), n -> n[1] ); # remove power info
    elms_to_test := ShallowCopy( elms );
    SubtractSet( elms_to_test, IdempotentElements( nr ) );
    SubtractSet( elms_to_test, qr_elms );

    for z in elms_to_test do
      A := Set( List( elms, n-> n - ( z * n ) ) );
      li := List( NearRingRightIdeals( nr ), i -> AsList( i ) );
  
      Lz := First( li, i -> IsSubset( i, A ) );
      if z in Lz then AddSet( qr_elms, z ); fi;
  od;
  
  return qr_elms;
  end );  

############################################################################
##
#M  RegularElements

InstallMethod(
	RegularElements,
	"generic",
	true,
	[IsNearRing],
	0,
  function ( nr )
    return Filtered( nr, x -> ForAny( nr, y -> ( x * y ) * x = x ) );
  end );  

#####################################################################
##
#M  PrintTable2
##

InstallMethod(
	PrintTable2,
	"groups",
	true,
	[IsNearRing, IsString],
	0,
  function ( N, mode )
    
  local elms,    # the elements of the nearring
        n,       # the size of the nearring
        symbols, # a list of the symbols for the elements of the nearring
        tw,      # the width of a table
        spc,     # local function which prints the right number of spaces
        bar,     # local function for printing the right length of the bar
        ind,     # help variable, an index
        print_addition, print_multiplication, # status variables
        i,j,     # loop variables
        max;     # length of the longest symbol string

    n := Size( N );

    symbols := Symbols( N );

    max := Maximum( List( symbols, Length ) );

    # compute the number of characters per line required for the table
    tw := (max+1)*(n+1) + 2;

    if SizeScreen()[1] - 3 < tw then
      Print( "The table of a group of order ", n, " will not ",
           "look\ngood on a screen with ", SizeScreen()[1], " characters per ", 
           "line.\nHowever, you may want to set your line length to a ",
           "greater\nvalue by using the GAP function 'SizeScreen'.\n" );
      return;
    fi;

    spc := function( i )
	   return String( 
		Concatenation( List( [Length(symbols[i])..max+1], j -> " " ) )
			);
    end;

    bar     := function()
               return String( Concatenation( List( [0..max+1], i -> "-" ) ) );
    end;

    elms    := AsSSortedList( N );

    if 'e' in mode then
      # info about the elements
      Print( "Let:\n" );
      for i in [1..n] do Print( symbols[i], " := ", elms[i], "\n" ); od;
    fi;

    if 'a' in mode then  
      # print the addition table
      Print("\n");
      for i in [1..max+1] do Print(" "); od;
      Print( "+  | " ); 
      for i in [1..n] do Print( symbols[i], spc(i) ); od;
      Print( "\n  ", bar() ); for i in [1..n] do Print( bar() ); od;
      for i in [1..n] do
        Print( "\n  ", symbols[i], spc(i), "| " );
        for j in [1..n] do
	  ind := Position( elms, elms[i] + elms[j] );
	  Print( symbols[ ind ], spc(ind) );  
        od;
      od;
      Print("\n");
    fi;

    if 'm' in mode then
      # print the multiplication table
      Print("\n");
      for i in [1..max+1] do Print(" "); od;
      Print( "*  | " ); 
      for i in [1..n] do Print( symbols[i], spc(i) ); od;
      Print( "\n  ", bar() ); for i in [1..n] do Print( bar() ); od;
      for i in [1..n] do
        Print( "\n  ", symbols[i], spc(i), "| " );
        for j in [1..n] do
	  ind := Position( elms, elms[i] * elms[j] );
	  Print( symbols[ ind ], spc(ind) );  
        od;
      od;
      Print("\n");
    fi;

  end );

#############################################################################
##
#M  IsMultiplicationRespectingHomomorphism( <hom>, <nr1>, <nr2> )
##		returns true if the group homomorphism <hom> from the additive
##		group of <nr1> to the additive group of <nr2> respects
##		nearring multiplication

InstallMethod(
	IsMultiplicationRespectingHomomorphism,
	"generic",
	true,
	[IsGeneralMapping, IsNearRing, IsExplicitMultiplicationNearRing],
	5,
  function ( hom, nr1, nr2 )
  local group, elms, gens, mul, fam;
    if not( IsGroupHomomorphism( hom ) ) then
	return false;
    fi;
    group := GroupReduct( nr1 );
    elms := Enumerator(group);
    gens := GeneratorsOfGroup(group);
    fam := nr1!.elementsInfo;
    mul := NRMultiplication( nr2 );

    return ForAll( elms, x -> ForAll( gens, y ->
	Image( hom, GroupElementRepOfNearRingElement( 
	NearRingElementByGroupRep(fam,x) * NearRingElementByGroupRep(fam,y) ) )
		= mul( Image( hom, x ), Image( hom, y ) )
	) );
end ); 

InstallMethod(
	IsMultiplicationRespectingHomomorphism,
	"generic",
	true,
	[IsGeneralMapping, IsNearRing, IsNearRing],
	0,
  function ( hom, nr1, nr2 )
  local group1, gens1, fam1, fam2;
    if not( IsGroupHomomorphism( hom ) ) then
	return false;
    fi;
    group1 := GroupReduct( nr1 );
    gens1 := GeneratorsOfGroup(group1);
    fam1 := nr1!.elementsInfo;
    fam2 := nr2!.elementsInfo;

    return ForAll( group1, x -> ForAll( gens1, y ->
      NearRingElementByGroupRep( fam2, Image( hom, x ) ) *
        NearRingElementByGroupRep( fam2, Image( hom, y ) ) 

	=

      NearRingElementByGroupRep( fam2, 
	Image( hom, GroupElementRepOfNearRingElement( 
	             NearRingElementByGroupRep( fam1, x ) *
		     NearRingElementByGroupRep( fam1, y ) ) ) ) 
      ) );

end ); 

#####################################################################
##
#M  Endomorphisms		for near rings
##

InstallMethod(
	Endomorphisms,
	"near rings",
	true,
	[IsNearRing],
	0,
  function( N )
  local addGroup;
    addGroup := GroupReduct(N);
    return Filtered( Endomorphisms(addGroup),
		e -> IsMultiplicationRespectingHomomorphism(e,N,N) );
  end );

#####################################################################
##
#M  Automorphisms		for near rings
##

InstallMethod(
	Automorphisms,
	"near rings",
	true,
	[IsNearRing],
	0,
  function( N )
  local addGroup;
    addGroup := GroupReduct(N);
    return Filtered( Automorphisms(addGroup),
		e -> IsMultiplicationRespectingHomomorphism(e,N,N) );
  end );

#############################################################################
##
#M  IsZeroSymmetricNearRing
##

InstallMethod(
	IsZeroSymmetricNearRing,
	"default",
	true,
	[IsNearRing],
	0,
  function(nr)
  local gens, zero;
    gens := GeneratorsOfNearRing(nr);
    zero := 0 * gens[1];
    return ForAll( gens, g -> zero * g = zero );
  end );

#############################################################################
##
#M  InvariantSubNearRings		default method
##

InstallMethod(
	InvariantSubNearRings,
	"default",
	true,
	[IsNearRing],
	0,
  function( N )
  local fam, sgps, g, m, invars, isinv, i;
    fam := N!.elementsInfo;
    sgps := Subgroups( GroupReduct(N) );
  
    invars := [];
    for g in sgps do
      isinv := true;
      i := 0;
      while i < Size(g) and isinv do
        i := i+1;
        m := NearRingElementByGroupRep( fam, Enumerator(g)[i] );
        if ForAny( N, n -> not ( GroupElementRepOfNearRingElement(m*n) in g ) 
				or 
			   not ( GroupElementRepOfNearRingElement(n*m) in g ) )
				then
	  isinv := false;
        fi;
      od;
      if isinv then Add( invars, g ); fi;
    od;

    return List( invars, inv -> SubNearRingBySubgroupNC( N, inv ) ); 
  end );

#############################################################################
##
#M  SubNearRings				default method
##

InstallMethod(
	SubNearRings,
	"default",
	true,
	[IsNearRing],
	0,
  function( N )
  local fam, sgps, g, m, subnrs, i, issub;
    fam := N!.elementsInfo;
    sgps := Subgroups( GroupReduct(N) );
  
    subnrs := [];
    for g in sgps do
      issub := true;
      i := 0;
      while i < Size(g) and issub do
        i := i+1;
        m := NearRingElementByGroupRep( fam, Enumerator(g)[i] );
        if ForAny( g, n -> not( GroupElementRepOfNearRingElement(NearRingElementByGroupRep(fam,n)*m) in g ) ) then
		issub := false;
        fi;
      od;
      if issub then Add( subnrs, g ); fi;
    od;

    return List( subnrs, sg -> SubNearRingBySubgroupNC( N, sg ) ); 
  end );

#############################################################################
##
#M  IsNearRingWithOne
##
## AS OF NOW, 18.7.2007, NO NEAR-RING IN SONATA IS GENERATED AS IsNearRingWithOne
## WE SIMPLY IGNORE THIS CATEGORY
##

#InstallImmediateMethod(
#	IsNearRingWithOne,
#	HasOne,			# One already computed
#	0,
#  function ( nr )
#    return ( One(nr) <> fail );
#  end );

#InstallMethod(
#	IsNearRingWithOne,
#	"compute One",
#	true,
#	[IsNearRing],
#	0,
#  function ( nr )
#  local id;
#    id := One( nr );
#    return ( id <> fail );
#  end );

#############################################################################
##
#M  AsGroupReductElement
##

InstallMethod(
	AsGroupReductElement,
	"default",
	true,
	[IsNearRingElement],
	0,
  function ( nrelm )
    return GroupElementRepOfNearRingElement( nrelm );
  end );

#############################################################################
##
#M  AsNearRingElement
##

InstallMethod(
	AsNearRingElement,
	"default",
	true,
	[IsNearRing, IsMultiplicativeElementWithInverse],
	0,
  function ( nr, grpelm )
    if not ( grpelm in GroupReduct(nr) ) then
	Error("<grpelm> must lie in the additive group of <nr>");
    fi;
    return NearRingElementByGroupRep( nr!.elementsInfo, grpelm );
  end );

#############################################################################
##
#M  GeneratorsOfNearRing
##

InstallMethod(
	GeneratorsOfNearRing,
	"default (additive generators)",
	true,
	[IsNearRing],
	0,
  function ( nr )
  local fam;
    fam := nr!.elementsInfo;

    return List( GeneratorsOfGroup( GroupReduct( nr ) ),
			gen -> NearRingElementByGroupRep( fam, gen ) );
  end );

#############################################################################
##
#M  IsSubset
##

InstallMethod(
	IsSubset,
	"both have additive groups",
	IsIdenticalObj,
	[IsNearRingElementCollection and HasGroupReduct,
		IsNearRingElementCollection and HasGroupReduct],
	0,
  function ( N, S )
  local addGroup;
    addGroup := GroupReduct( N );

    return ForAll( GeneratorsOfGroup( GroupReduct( S ) ),
		gen -> gen in addGroup );
  end );

InstallMethod(
	IsSubset,
	"two nearrings, second has generators",
	IsIdenticalObj,
	[IsNearRing, IsNearRing and HasGeneratorsOfNearRing],
	0,
  function ( N, S )
    return ForAll( GeneratorsOfNearRing( S ), gen -> gen in N );
  end );

#############################################################################
##
#M  Intersection2 ( <NR1>, <NR2> ) 
##    Returns the Intersection of <NR1> and <NR2>.
##

InstallMethod (
	Intersection2,
	"AddGenNearRings",
	IsIdenticalObj,
        [IsNearRing, IsNearRing],
	0,
  function ( NR1, NR2 )
  local addGroup;
    addGroup := Intersection2( GroupReduct( NR1 ), GroupReduct( NR2 ) );

    return SubNearRingBySubgroupNC( Parent(NR1), addGroup );
  end );

#############################################################################
##
#F  NoetherianQuotient		Dispatcher
##

InstallGlobalFunction( NoetherianQuotient, function ( arg )
    # NoetherianQuotient( NearRing, ngroup, target, source )
    if Length( arg ) = 4 then
      return NoetherianQuotient2( arg[1], arg[2], arg[3], arg[4] );

    # NoetherianQuotient( transformation nearring, target, source )
    elif Length( arg ) = 3 then
      return NoetherianQuotient2( arg[1], Gamma( arg[1] ), arg[2], arg[3] );

    # NoetherianQuotient( right ideal, exp mul nearring ) 
    elif Length( arg ) = 2 then
      return NoetherianQuotient2( arg[2], 
		NGroupByNearRingMultiplication( arg[2] ),
		GroupReduct( arg[1] ), GroupReduct( arg[2] ) );

    fi;
    Error( "Usage: NoetherianQuotient( <NR>, <NGroup>, <Target>, <Source> )",
	 "\n or: NoetherianQuotient( <TfmNR>, <Target>, <Source> )",
	 "\n or: NoetherianQuotient( <RightIdeal>, <ExpMulNr> )" );
  end );

#############################################################################
##
#M  AdditiveGenerators
##

InstallMethod(
	AdditiveGenerators,
	"from the additive group",
	true,
	[IsNearRing],
	0,
  function( N )
    return List( GeneratorsOfGroup( GroupReduct( N ) ),
			g -> NearRingElementByGroupRep( N!.elementsInfo, g ));
  end );

#############################################################################
##
#M  \*			integer * nearring element

#InstallMethod(
#	\*,
#	true,
#	[IsInt, IsNearRingElement],
#	0,
#  function( n, e )
#  local prod, i;
#    prod := e - e;
#    if n < 0 then
#      for i in [1..-n] do
#	prod := prod - e;
#      od;
#    else
#      for i in [1..n] do
#	prod := prod + e;
#      od;
#    fi;
#
#    return prod;
#  end );
#
## Peter: The following methods for \* should eventually be included in GAP (see arith.gi)
## and removed from Sonata: 

InstallOtherMethod( \*,
    "positive integer * additive element",
    [ IsPosInt, IsNearAdditiveElement ],
    PROD_INT_OBJ );

InstallOtherMethod( \*,
    "zero integer * additive element with zero",
    [ IsInt and IsZeroCyc, IsNearAdditiveElementWithZero ], SUM_FLAGS,
    PROD_INT_OBJ );

InstallOtherMethod( \*,
    "negative integer * additive element with inverse",
    [ IsInt and IsNegRat, IsNearAdditiveElementWithInverse ],
    PROD_INT_OBJ );

#############################################################################
##
#F  NRClosureOfSubgroup( <G>, <gens>, <fam> )
##
##	<G>: the subgroup
##	<gens>: a set of generators of the nearring
##	<fam>: the family of the elements of the ExpMulNearRing
##	       the group the mappings act on for a TfmNR

InstallGlobalFunction(
	NRClosureOfSubgroup,
  function ( group, gens, fam )
  local e, n, m;
    Info( InfoNearRing, 2, "closure has ", Size( group ), " elements" );
    for e in gens do
      for n in group do
        m := GroupElementRepOfNearRingElement(
	  	NearRingElementByGroupRep( fam, n ) * 
		NearRingElementByGroupRep( fam, e )
	  );
        if not m in group then
          Info( InfoNearRing, 3, 
		"new element: ", NearRingElementByGroupRep( fam, m ) ); 
	  return NRClosureOfSubgroup( ClosureGroup( group, m ), gens, fam );
        fi;
      od;
    od;

    return group;
end );

