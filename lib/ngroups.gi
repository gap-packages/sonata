#############################################################################
##
#M  NGroup
##

InstallMethod(
	NGroup,
	"default",
	true,
	[IsGroup, IsNearRing, IsFunction],
	0,
  function ( G, N, mu )
  local NG;
    NG := Subgroup( Parent( G ), GeneratorsOfGroup( G ) );
    SetIsNGroup( NG, true );
    SetNearRingActingOnNGroup( NG, N );
    SetActionOfNearRingOnNGroup( NG, mu );
    return NG;
  end );

#############################################################################
##
#M  ViewObj			for N-groups
##

InstallMethod(
	ViewObj,
	"N-groups",
	true,
	[IsGroup and IsNGroup],
	100,
  function ( NG )
    Print( "< N-group of " );
    View( NearRingActingOnNGroup(NG) );
    Print( " >" );
  end );

#############################################################################
##
#M  IsNGroup			set flag to false
##

InstallMethod(
	IsNGroup,
	"default (no)",
	true,
	[IsGroup],
	0,
  G -> false );

#############################################################################
##
#M  NGroupByNearRingMultiplication
##

InstallMethod(
	NGroupByNearRingMultiplication,
	"ExpMulNRs",
	true,
	[IsExplicitMultiplicationNearRing],
	0,
  function ( N )
  local ng, action;
    ng := GroupReduct(N);
    action := function ( g, n )
	return NRMultiplication(N)( g, GroupElementRepOfNearRingElement(n) );
    end;
    ng := NGroup( ng, N, action );

    return ng;
  end );

InstallMethod(
	NGroupByNearRingMultiplication,
	"TfmNRs",
	true,
	[IsTransformationNearRing],
	0,
  function ( N )
  local ng, action;
    ng := GroupReduct(N);
    action := function ( g, n )
	return AsGroupReductElement( AsNearRingElement( N, g ) * n ); 
    end;
    ng := NGroup( ng, N, action );

    return ng;
  end );

#############################################################################
##
#F NGroupByApplication
##

InstallMethod( 
	NGroupByApplication,
	"transformation nearring acts on its gamma",
	true,
	[IsNearRing and IsTransformationNearRing],
	0,
  function( T )
    return NGroup( Gamma( T ), 
		     T, 
		     function( g, t ) return Image( t, g ); end );
end );
        
#############################################################################
##
#M  PrintTable2			for N-groups
##

InstallMethod(
	PrintTable2,
	"N-groups",
	true,
	[IsNGroup, IsString],
	0,
  function( NG, mode )

  local N, elmsN, elmsG,    # the elements of the group
        nN, nG,             # the size of the group
        symbolsG, symbolsN, # a list of the symbols for the elements of the group
        tw,      # the width of a table
        spc,     # local function which prints the right number of spaces
	spcN,    # also
        bar,     # local function for printing the right length of the bar
	barN,    # also
        ind,     # help variable, an index
        i,j,     # loop variables
	max,     # length of the longest symbol of the N-group
	maxN;    # length of the longest symbol of the near ring

    N := NearRingActingOnNGroup( NG );

    elmsN    := AsSSortedList( N );
    elmsG    := AsSSortedList( NG );
    nN       := Length( elmsN );
    nG       := Length( elmsG );
    symbolsN := Symbols( N );
    symbolsG := List( [0..nG-1], i ->
			String( Concatenation( "g", String(i) ) ) );

    max := Maximum( List( symbolsG, Length ) );
    maxN := Maximum( List( symbolsN, Length ) );

    # compute the number of characters per line required for the table
    tw := (max+1)*(nG+1) + 2;

    if SizeScreen()[1] - 3 < tw then
      Print( "The table of an N-group of order ", nG, " will not ",
           "look\ngood on a screen with ", SizeScreen()[1], " characters per ", 
           "line.\nHowever, you may want to set your line length to a ",
           "greater\nvalue by using the GAP function 'SizeScreen'.\n" );
      return;
    fi;

    spc := function( i )
	   return String( 
		Concatenation( List( [Length(symbolsG[i])..max+1], j -> " " ) )
			);
    end;

    spcN := function( i )
	   return String( 
	       Concatenation( List( [Length(symbolsN[i])..maxN+1], j -> " " ) )
			);
    end;

    bar     := function()
               return String( Concatenation( List( [0..max+1], i -> "-" ) ) );
    end;

    barN     := function()
               return String( Concatenation( List( [0..maxN+1], i -> "-" ) ) );
    end;

    if 'e' in mode then
      # info about the elements
      Print( "Let:\n" );
      for i in [1..nN] do Print( symbolsN[i], " := ", elmsN[i], "\n" ); od;
      Print( "-------------------------------------",
		"-------------------------------\n");
      for i in [1..nG] do Print( symbolsG[i], " := ", elmsG[i], "\n" ); od;
    fi;

    if 'm' in mode then
      # print the action table
      Print( "\nN = ", NearRingActingOnNGroup( NG ), " acts on \nG = ", 
          NG, "\nfrom the right by the following action: \n" );
      Print("\n");
      for i in [1..maxN+1] do Print(" "); od;
      Print( "   | " ); 
      for i in [1..nG] do Print( symbolsG[i], spc(i) ); od;
      Print( "\n  ", barN() ); for i in [1..nG] do Print( bar() ); od;
      for i in [1..nN] do
	Print( "\n  ", symbolsN[i], spcN(i), "| " );
	for j in [1..nG] do
	  ind := Position( elmsG,
			ActionOfNearRingOnNGroup(NG)( elmsG[j], elmsN[i] ) );
          Print( symbolsG[ ind ], spc(ind) );
        od;
      od;
    fi;

    Print( "\n\n" );
  end );

#############################################################################
##
#M  IsCompatible
##

InstallMethod(
	IsCompatible,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( NGroup )
  local N, action;
    N := NearRingActingOnNGroup( NGroup );
    action := ActionOfNearRingOnNGroup( NGroup );

    return ForAll( NGroup, g ->
	     ForAll( N, n ->
	       ForAny( N, m ->
		 ForAll( NGroup, delta ->
	action( g*delta, n ) / ( action( g, n ) ) =
	action( delta, m )
	   ) ) ) );
  end );

##############################################################################
##
#M  IsTameNGroup			for N-groups
##

InstallMethod(
	IsTameNGroup,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( NGroup )
  local N, N0, action;
    N := NearRingActingOnNGroup( NGroup );
    N0 := ZeroSymmetricPart( N );
    action := ActionOfNearRingOnNGroup( NGroup );

    return ForAll( NGroup, delta ->
             ForAll( N0, n ->
               ForAll( NGroup, gamma ->
                 ForAny( N0, m ->
        action( gamma*delta, n ) / ( action( gamma, n ) ) = action( delta, m )
	   ) ) ) );
  end );                

##############################################################################
##
#M  Is2TameNGroup			for N-groups
##

InstallMethod(
	Is2TameNGroup,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( NGroup )
  local N, N0, action;

    N := NearRingActingOnNGroup( NGroup );
    N0 := ZeroSymmetricPart( N );
    action := ActionOfNearRingOnNGroup( NGroup );

    return ForAll( NGroup, delta1 ->
             ForAll( NGroup, delta2 ->
               ForAll( N0, n ->
                 ForAll( NGroup, gamma ->
                   ForAny( N0, m ->
        
       action( gamma*delta1, n ) / ( action( gamma, n ) ) = action( delta1, m )
 
                                  and

       action( gamma*delta2, n ) / ( action( gamma, n ) ) = action( delta2, m )

           ) ) ) ) );
  end );        

##############################################################################
##
#M  Is3TameNGroup			for N-groups
##

InstallMethod(
	Is3TameNGroup,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( NGroup )
  local N, N0, action;

    N := NearRingActingOnNGroup( NGroup );
    N0 := ZeroSymmetricPart( N );
    action := ActionOfNearRingOnNGroup( NGroup );

    return ForAll( NGroup, delta1 -> 
             ForAll( NGroup, delta2 ->
               ForAll( NGroup, delta3 ->
                 ForAll( N0, n ->
                   ForAll( NGroup, gamma ->
                     ForAny( N0, m ->
        
      action( gamma*delta1, n ) / ( action( gamma, n ) ) = action( delta1, m )
 
                                   and

      action( gamma*delta2, n ) / ( action( gamma, n ) ) = action( delta2, m )

                                   and

      action( gamma*delta3, n ) / ( action( gamma, n ) ) = action( delta3, m )

           ) ) ) ) ) );
  end );

###############################################################################
##
#M  NGroupByRightIdealFactor
##

InstallMethod(
	NGroupByRightIdealFactor,
	"default",
	true,
	[IsNearRing, IsNearRingRightIdeal],
	0,
  function( N, R )
  local addN, addR, f, factor, mu;
    addN := GroupReduct(N);
    addR := GroupReduct(R);
    f := NaturalHomomorphismByNormalSubgroup( addN, addR );
    factor := Image( f, addN );
    mu := function( x, n )
	return Image( f, NRMultiplication(N)( 
		PreImagesRepresentative( f, x ) ,
		GroupElementRepOfNearRingElement(n) ) );
    end;
    factor := NGroup( factor, N, mu );

    return factor;
  end );

###############################################################################
##
#M  IsNIdeal
##

InstallMethod(
	IsNIdeal,
	"BM01",
	true,
	[IsGroup and IsNGroup, IsGroup],
	0,
  function( G, D )
  local E, action;
    E := GeneratorsOfNearRing( NearRingActingOnNGroup( G ) );
    action := ActionOfNearRingOnNGroup( G );
    return IsSubgroup( G, D ) and IsNormal( G, D ) and
	   ForAll( G, gamma ->
	   ForAll( D, delta ->
	   ForAll( E, e -> action( gamma*delta, e ) / action( gamma, e ) in D
		) ) );
  end );

###############################################################################
##
#M  NIdeals
##

InstallMethod(
	NIdeals,
	"filter normal subgroups",
	true,
	[IsGroup and IsNGroup],
	0,
  function( G )
  local nsgps, N, action, ideals;
    nsgps := NormalSubgroups( G );
    N := NearRingActingOnNGroup( G );
    action := ActionOfNearRingOnNGroup( G );
    ideals := Filtered( nsgps, D -> IsNIdeal( G, D ) );

    return List( ideals, id -> NGroup( id, N, action ) );
  end );

InstallMethod(
	NIdeals,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( GAMMA )
  local nsgps, N, action, ideals;
    nsgps := NormalSubgroups( GAMMA );
    N := NearRingActingOnNGroup( GAMMA );
    action := ActionOfNearRingOnNGroup( GAMMA );

    ideals := Filtered( nsgps, DELTA -> 
		ForAll( N, n ->
    		  ForAll( GAMMA, gamma -> 
		    ForAll( DELTA, delta ->
      action( gamma*delta, n ) / action( gamma, n ) in DELTA 
	      ) ) ) );

    return List( ideals, id -> NGroup( id, N, action ) );
  end );

###############################################################################
##
#M  N0Subgroups
##

InstallMethod(
	N0Subgroups,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( ng )
  local subgroups, N, N0, action, n0Subgroups;
    subgroups := Subgroups( ng );
    N := NearRingActingOnNGroup( ng );
    N0 := ZeroSymmetricPart( N );
    action := ActionOfNearRingOnNGroup( ng );

    n0Subgroups := Filtered( subgroups, DELTA ->
			ForAll( N0, n ->
    			  ForAll( DELTA, delta -> 
      			action( delta, n ) in DELTA 
		   ) ) );

    return List( n0Subgroups, sg -> NGroup( sg, N0, action ) );
  end ); 

###############################################################################
##
#M  IsMonogenic
##

InstallMethod(
	IsMonogenic,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( ng )
  local s, N, action;
    s := Size( ng );
    N := NearRingActingOnNGroup( ng );
    action := ActionOfNearRingOnNGroup( ng );

    return ForAny( ng, delta ->
	Size( Set( List( N, n -> action( delta, n ) ) ) ) = s );
  end );

###############################################################################
##
#M  IsStronglyMonogenic
##

InstallMethod(
	IsStronglyMonogenic,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( ng )
  local s, N, action;
    s := Size( ng );
    N := NearRingActingOnNGroup( ng );
    action := ActionOfNearRingOnNGroup( ng );

    return ForAll( ng, delta ->
	Size( Set( List( N, n -> action( delta, n ) ) ) ) = s or
	Size( Set( List( N, n -> action( delta, n ) ) ) ) = 1 );
  end );

###############################################################################
##
#M  IsSimpleNGroup
##

InstallMethod(
	IsSimpleNGroup,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( ng )
    return Length( NIdeals( ng ) ) <= 2;
  end );

###############################################################################
##
#M  IsN0SimpleNGroup
##

InstallMethod(
	IsN0SimpleNGroup,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( ng )
    return Length( N0Subgroups( ng ) ) <= 2;
  end );

###############################################################################
##
#M  TypeOfNGroup
##

InstallMethod(
	TypeOfNGroup,
	"default",
	true,
	[IsGroup and IsNGroup],
	0,
  function( ng )
  local ismonogenic, isstronglymonogenic, issimple, isn0simple;
    ismonogenic := false;
    isstronglymonogenic := false;
    issimple := false;
    isn0simple := false;

    if Size( ng ) <= 1 then return fail; fi;

    if IsMonogenic( ng ) then
      ismonogenic := true;
      if IsStronglyMonogenic( ng ) then
	isstronglymonogenic := true;
      fi;
    else 
      return fail;
    fi;

    if IsN0SimpleNGroup( ng ) then 
      return 2;
    fi;

    if IsSimpleNGroup( ng ) then 
      if isstronglymonogenic then
	return 1;
      else
	return 0;
      fi;
    fi;

    return fail;
  end );

############################################################################
##
#M  NoetherianQuotient2		for N-groups
##

InstallMethod(
	NoetherianQuotient2,
	"N-groups (target is N-normal(N-ideal))",
	true,
	[IsNearRing, IsGroup and IsNGroup, 
	 IsMultiplicativeElementCollection, IsMultiplicativeElementCollection],
	5,
  function ( NR, NGroup, Target, Source )
  local action, NN, nq;
    if not ( IsNIdeal( NGroup, Target ) ) then
	TryNextMethod();
    fi;
    action := ActionOfNearRingOnNGroup( NGroup );
    NN := NGroupByNearRingMultiplication( NR );
    nq := Filtered( NR, n -> ForAll( Source, x -> action(x,n) in Target ) );
    nq := Subgroup( NN, List( nq, GroupElementRepOfNearRingElement ) );

    nq := NearRingIdealBySubgroupNC( NR, nq );

    return nq;
  end );

InstallMethod(
	NoetherianQuotient2,
	"N-groups (target is subgroup)",
	true,
	[IsNearRing, IsGroup and IsNGroup,
	 IsMultiplicativeElementCollection, IsMultiplicativeElementCollection],
	4,
  function ( NR, NGroup, Target, Source )
  local action, NN, nq;
    if not ( IsSubgroup( NGroup, Target ) ) then
	TryNextMethod();
    fi;
    action := ActionOfNearRingOnNGroup( NGroup );
    NN := NGroupByNearRingMultiplication( NR );
    nq := Filtered( NR, n -> ForAll( Source, x -> action(x,n) in Target ) );
    nq := Subgroup( NN, List( nq, GroupElementRepOfNearRingElement ) );

    nq := NearRingLeftIdealBySubgroupNC( NR, nq );

    return nq;
  end );

InstallMethod(
	NoetherianQuotient2,
	"N-groups (target is subset)",
	true,
	[IsNearRing, IsGroup and IsNGroup, IsMultiplicativeElementCollection,
		IsMultiplicativeElementCollection],
	0,
  function ( NR, NGroup, Target, Source )
  local action, NN, nq;
    action := ActionOfNearRingOnNGroup( NGroup );
    NN := NGroupByNearRingMultiplication( NR );
    nq := Filtered( NR, n -> ForAll( Source, x -> action(x,n) in Target ) );

    return nq;
  end );

############################################################################
##
#M  IsModularNearRingRightIdeal

InstallMethod(
	IsModularNearRingRightIdeal,
	"near rings with One",
	true,
	[IsNRI and IsNearRingRightIdeal],
	10,
  function( R )
  local NR;
    NR := Parent( R );
#    if IsNearRingWithOne(Parent(R)) then
    if One(Parent(R)) <> fail then
	return true;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	IsModularNearRingRightIdeal,
	"default",
	true,
	[IsNRI and IsNearRingRightIdeal],
	0,
  function ( R )
  local NR;
    NR := Parent( R );

    return ForAny( NR, e -> ForAll( NR, n ->
      ( n - ( e * n ) ) in R  ) );
  end );

############################################################################
##
#M  ModularityOfRightIdeal

InstallMethod(
	ModularityOfRightIdeal,
	"ideal is not modular",
	true,
	[IsNRI and IsNearRingRightIdeal],
	10,
  function( R )
    if not IsModularNearRingRightIdeal( R ) then 
	return fail;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	ModularityOfRightIdeal,
	"default",
	true,
	[IsNRI and IsNearRingRightIdeal],
	0,
  function ( R )	
  local type;
    return TypeOfNGroup( NGroupByRightIdealFactor( Parent(R), R ) );
  end );

############################################################################
##
#M  NuRadicals
##

InstallMethod(
	NuRadicals,
	"ExpMulNrs",
	true,
	[IsNearRing and IsExplicitMultiplicationNearRing],
	0,
  function ( N )
  local right_ideals, ri, m0, m1, m2, j0, jhalf, j1, j2;

    right_ideals := NearRingRightIdeals( N );

    m0 := [ NearRingRightIdealBySubgroupNC( N, GroupReduct(N) ) ];
    m1 := [ NearRingRightIdealBySubgroupNC( N, GroupReduct(N) ) ];
    m2 := [ NearRingRightIdealBySubgroupNC( N, GroupReduct(N) ) ];

    for ri in right_ideals do
      if ModularityOfRightIdeal( ri ) = 2 then
	Add( m2, ri ); Add( m1, ri ); Add( m0, ri );
      elif ModularityOfRightIdeal( ri ) = 1 then
        Add( m1, ri ); Add( m0, ri );
      elif ModularityOfRightIdeal( ri ) = 0 then
        Add( m0, ri );
      fi;
    od;

    j2 := Intersection( m2 );
    j1 := Intersection( m1 );
    jhalf := Intersection( m0 );
    j0 := NearRingIdealBySubgroupNC( N,	GroupReduct( Intersection(
	    List( m0, li -> NoetherianQuotient( li, N ) ) ) ) );

    SetIsNearRingIdeal( j1, true );
    SetIsNearRingIdeal( j2, true );

    return rec( J2   := j2,
                J1   := j1,
                J1_2 := jhalf,
                J0   := j0 );
  end );

############################################################################
##
#F  NuRadical( <NR>, <nu> )
##

NuRadical := function ( NR, nu )
  if   nu = 0 then
	return NuRadicals( NR ).J0;
  elif nu = 1/2 then
	return NuRadicals( NR ).J1_2;
  elif nu = 1 then
	return NuRadicals( NR ).J1;
  elif nu = 2 then
	return NuRadicals( NR ).J2;
  fi;

  Error( "<nu> must be one of 0, 1/2, 1 or 2" );
end;

############################################################################
##
#M  GroupKernelOfNearRingWithOne
##

InstallMethod(
	GroupKernelOfNearRingWithOne,
	"default",
	true,
	[IsNearRing and IsNearRingWithOne],
	0,
  function( N )
  local i;
    i := One( N );

    if i = fail then
      return fail;
    else
      return Filtered( N, n -> ForAny( N, m -> m * n = i ) );
    fi;

  end );

############################################################################
##
#M  IsNSubgroup
##

InstallMethod(
	IsNSubgroup,
	"BM01",
	true,
	[IsGroup and IsNGroup, IsGroup],
	0,
  function ( N, S )
  local NR, action;
    NR := NearRingActingOnNGroup(N);
    action := ActionOfNearRingOnNGroup(N);
    return IsSubgroup(N,S) and
	   ForAll(GeneratorsOfNearRing(NR),
			gen -> ForAll( S, s -> action( s, gen ) in S ) );
  end );

############################################################################
##
#M  NSubgroups
##

InstallMethod(
	NSubgroups,
	"filter subgroups",
	true,
	[IsGroup and IsNGroup],
	0,
  function ( ng )
    return Filtered( Subgroups( ng ), S -> IsNSubgroup( ng, S ) );
  end );

############################################################################
##
#M  NSubgroup
##

InstallMethod(
	NSubgroup,
	"BM01 Alg.1",
	true,
	[IsGroup and IsNGroup, IsMultiplicativeElementCollection],
	0,
  function ( G, F )
  local E, H, e, h, new, foundnew, mu;
    mu := ActionOfNearRingOnNGroup( G );
    E := GeneratorsOfNearRing( NearRingActingOnNGroup( G ) );
    H := Subgroup( G, F );
    foundnew := true;
    while foundnew do
      foundnew := false;
      for e in E do
        for h in H do
	  new := mu(h,e);
	  if not( new in H ) then
	    foundnew := true;
	    break;
          fi;
        od;
        if foundnew then break; fi;
      od;
      if foundnew then
	H := ClosureSubgroup( H, new );
      fi;
    od;
    return H;
end );

############################################################################
##
#M  NIdeal
##

InstallMethod(
	NIdeal,
	"BM01 Cor.5.3",
	true,
	[IsGroup and IsNGroup, IsMultiplicativeElementCollection],
	0,
  function ( G, F )
  local E, N, e, n, g, new, foundnew, mu;
    mu := ActionOfNearRingOnNGroup( G );
    E := GeneratorsOfNearRing( NearRingActingOnNGroup( G ) );
    N := NormalClosure( G, Subgroup( G, F ) );
    foundnew := true;
    while foundnew do
      foundnew := false;
      for e in E do
        for n in N do
          for g in G do
 	    new := mu(g*n,e)/mu(g,e);
	    if not( new in N ) then
	      foundnew := true;
	      break;
            fi;
	  od;
        od;
        if foundnew then break; fi;
      od;
      if foundnew then
	N := NormalClosure( G, ClosureSubgroup( N, new ) );
      fi;
    od;
    return N;
end );

############################################################################
##
#M  DirectProductNGroups
##

InstallMethod(
	DirectProductNGroups,
	"N-groups of the same nearring",
	true,
	[IsGroup and IsNGroup, IsGroup and IsNGroup],
	0,
  function ( G, H )
  local N, D, actionG, actionH, action, pG, pH, eG, eH;
    N := NearRingActingOnNGroup(G);
    if N <> NearRingActingOnNGroup(H) then
	Error( "The N-groups <G> and <H> are N-groups of different nearrings!" );
    fi;
    actionG := ActionOfNearRingOnNGroup( G );
    actionH := ActionOfNearRingOnNGroup( H );
    D := DirectProduct( G, H );
    pG := Projection( D, 1 );
    pH := Projection( D, 2 );
    eG := Embedding( D, 1 );
    eH := Embedding( D, 2 );
    action := function( g, n )
	return (actionG( g^pG, n ))^eG * (actionH( g^pH, n ))^eH;
    end;
    return NGroup( D, N, action );
end );

