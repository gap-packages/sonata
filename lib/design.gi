
DESIGN_FAMILIES := [];

############################################################################
##
#M  DesignFamily
##

InstallMethod(
	DesignFamily,
	"default",
	true,
	[IsInt, IsInt, IsInt, IsInt],
	0,
  function ( t, v, k, lambda )
  local fam;

    for fam in DESIGN_FAMILIES do
      if fam!.parameters = [t,v,k,lambda] then
	  return fam;
      fi;
    od;

    fam := NewFamily( "DesignFamily(...)",
			IsDesign,
			IsObject,
			IsDesignFamily );

    fam!.parameters := [t,v,k,lambda];

    fam!.defaultKind := NewType( fam, IsDesignDefaultRep );

    Add( DESIGN_FAMILIES, fam );

    return fam;

  end );


############################################################################
##
#M  IsPointIncidentBlock( <D>, <pointnr>, <blocknr> )
##

InstallMethod(
	IsPointIncidentBlock,
	"default",
	true,
	[IsDesign, IsInt, IsInt],
	0,
  function ( D, pointnr, blocknr )

  local	points, blocks;

    points := PointsOfDesign( D );
    blocks := BlocksOfDesign( D );

    if points[pointnr] in blocks[blocknr] then
      return true;
    fi;
      
    return false;
  end);

############################################################################
##
#M  PointsIncidentBlocks( <D>, <blocknrs> )
##

InstallMethod(
	PointsIncidentBlocks,
	"default",
	true,
	[IsDesign, IsList],
	0,
  function ( D, blocknrs )

  local points, blocks, pibnrs,
	x, y;

  points := PointsOfDesign( D );
  blocks := BlocksOfDesign( D );
  pibnrs := Filtered( [1..Size( points )],
	 x -> ForAll( blocknrs, y -> points[x] in blocks[y] ) );

  return pibnrs;
end );

############################################################################
##
#M  BlocksIncidentPoints( <D>, <pointnrs> )
##

InstallMethod(
	BlocksIncidentPoints,
	"default",
	true,
	[IsDesign, IsList],
	0,
  function ( D, pointnrs )

  local points, blocks, bipnrs,
	x, y;

  points := PointsOfDesign( D );
  blocks := BlocksOfDesign( D );
  bipnrs := Filtered( [1..Size( blocks )],
	 x -> ForAll( pointnrs, y -> points[y] in blocks[x] ) );

  return bipnrs;
end );

############################################################################
##
#A  DesignParameter( <D> )
##

InstallMethod(
	DesignParameter,
	"default",
	true,
	[IsDesign],
	0,
  function ( D )

  local	points,blocks,
	t, v, b, r, k, lambda, l, sets,
	i;

  points := PointsOfDesign( D );
  blocks := BlocksOfDesign( D );

  v := Size( points );
  b := Size( blocks );
  k := Size( blocks[1] );
  for i in [2..b]  do
    if Size( blocks[i] ) <> k then
      return ("Error: blocks are not of equal size"); 
    fi;
  od;
  r := Size( BlocksIncidentPoints( D, [1] ) );
  for i in [2..v] do
    if Size( BlocksIncidentPoints( D, [1] ) ) <> r then
      return 
	("Error: not all points are incident with the same number of blocks");
    fi;
  od;

  lambda := r; 

#  test for <t> and <lambda>

  for t in [2..k] do 
    l := Size( BlocksIncidentPoints( D, [1..t] ) );
    sets := Combinations( [1..v], t );
    if ForAll( sets, x -> Size( BlocksIncidentPoints( D, x ) ) = l ) then
      lambda := l;
    else
      return [t-1, v, b, r, k, lambda];
    fi;
  od;

  return [t, v, b, r, k, lambda];
end );

############################################################################
##
#A  IncidenceMat( <D> )
##

InstallMethod(
	IncidenceMat,
	"default",
	true,
	[IsDesign],
	0,
  function ( D )

  local	points, blocks, incmat,
	row, p, b;

  points := PointsOfDesign( D );
  blocks := BlocksOfDesign( D );
  incmat := [];
 
  for p in points do
    row := [];
    for b in blocks do
      if p in b then
	Append( row, [1] );
      else
	Append( row, [0] );
      fi; 
    od;
    Append( incmat, [row] );
  od;	
  
  SetIncidenceMat( D, incmat );
  
  return incmat;
end );

############################################################################
##
#M  PrintIncidenceMat( <D> )
##

InstallMethod(
	PrintIncidenceMat,
	"default",
	true,
	[IsDesign],
	0,
  function ( D )

  local	incmat,
	row, j;

  incmat := IncidenceMat( D );
  row := incmat[1];
#81
  if Length( row ) > 100 then
    return ( "Error : too many blocks to print incidence matrix" );
  fi;
 
  for row in incmat do
    for j in row do
      if j = 0 then
	Print( "." );
      elif j = 1 then
	Print( 1 );
      fi;
    od;
    Print( "\n" );
  od;

  return;
end ); 


############################################################################
##
#F  BlockIntersectionNumbers( <D> [, <k>] )
##

BlockIntersectionNumbers := function( arg )

  local	D,k,binrs;

  if Length( arg ) = 1 then
    D := arg[1];
    binrs := [];
    for k in [1..Size( BlocksOfDesign( D ) )] do
      Append( binrs, [BlockIntersectionNumbersK( D, k )] );
    od;
  elif Length( arg ) = 2 then
    D := arg[1];
    k := arg[2];
    binrs := BlockIntersectionNumbersK( D, k );
  else
    Error( "usage: BlockIntersectionNumbers( <D> [, <blocknr>] )" );
  fi;

  return binrs;
end;

############################################################################
##
#M  BlockIntersectionNumbersK( <D>, <k> )
##

InstallMethod(
	BlockIntersectionNumbersK,
	"default",
	true,
	[IsDesign, IsInt],
	0,
  function ( D, k )

  local	blocks,B,binrs,points,
	x;

  blocks := BlocksOfDesign( D );
  B := blocks[k];
#  binrs := List( blocks, x -> Size( IntersectionSet( B, x ) ) );
 binrs := List( blocks, x -> Size( Intersection( B, x ) ) );
  return binrs;
end );

############################################################################
##
#M  IsCircularDesign( <D> )
##
##  only for designs that are a result of DesignFromPlanarNearRing or
##  DesignFromFerreroPair
##
InstallMethod(
	IsCircularDesign,
	"nearring-designs",
	true,
	[IsDesign],
	0,
  function ( D )

  local v,b,t,k,circular,l,
	x;

  v := Size( PointsOfDesign( D ) );
  b := Size( BlocksOfDesign( D ) );
  t := QuoInt( b, v );
  k := Size( BlocksOfDesign( D )[1] );
 
  circular := ForAll( [1..t],
	 x -> IsSubset( [0,1,2,k], Set( BlockIntersectionNumbersK( D, x ) ) ) );
  SetIsCircularDesign( D, circular);

  return circular;
end );
    

############################################################################
##
#M  DesignFromPointsAndBlocks( <points>, <blocks> )
##

InstallMethod(
	DesignFromPointsAndBlocks,
	"default",
	true,
	[IsList, IsList],
	0, 
  function( points, blocks )

  local fam, D, lambda,
	x;

    lambda := Size( Filtered( blocks, x -> points[1] in x ) );

    fam := DesignFamily( 1, Size( points ), Size( blocks[1] ), lambda );
    D := Objectify( fam!.defaultKind, rec() );

    SetPointsOfDesign( D, points );
    SetBlocksOfDesign( D, blocks );    

    SetName( D, Concatenation( "<an incidence structure with ",
		 String( Size( points ) ), " points and ",
		String( Size( blocks ) ), " blocks>" ) );  
    return D;

  end );

 

############################################################################
##
#M  DesignFromIncidenceMat( M )
##

InstallMethod(
	DesignFromIncidenceMat,
	"default",
	true,
	[IsMatrix],
	0, 
  function( M )

  local	dim, points, blocks, b,
	i, x;

    dim := DimensionsMat( M );
    points := [1..dim[1]];
    blocks := [];
    for i in [1..dim[2]] do
      b := Filtered( points, x -> M[x][i] = 1 );
      Append( blocks, [Set( b )] );
    od;
    		
  return DesignFromPointsAndBlocks( Set( points ), Set( blocks ) );
end );


############################################################################
##
#F  DesignFromFerreroPair( <G>, <Phi>, <type> )
##

DesignFromFerreroPair := function( G, Phi, type )

  local	D;

  if type = "*" then
    D := DesignFromFerreroPairStar( G, Phi );
  elif type = " " then
    D := DesignFromFerreroPairBlank( G, Phi );	
  fi;
  return D;
end;

############################################################################
##
#M  DesignFromFerreroPairStar
##

InstallMethod(
	DesignFromFerreroPairStar,
	"default",
	true,
	[IsGroup, IsGroup],
	0,
  function ( G, Phi )

  local t,v,k,lambda,
	elms,basis,points,blocks,b,
	fam,D,
	i,j;

    elms := ShallowCopy( AsList( G ) );
    v := Size( G );
    k := Size( Phi );
    t := 2;
    lambda := k-1;
    elms := Difference( elms, [Identity( G )] );
    basis := Orbits( Phi, elms );
    blocks := ShallowCopy( basis );
    for b in elms do
      Append( blocks, basis*b );
    od;

    fam := DesignFamily( t, v, k, lambda );
    D := Objectify( fam!.defaultKind, rec() );

    points := AsList( G ); 
    SetPointsOfDesign( D, points );
    SetBlocksOfDesign( D, blocks );    

#    D!.group := G;
#    D!.Phi := Phi;

    SetName( D, Concatenation( "<a 2 - ( ", String( v ), ", ",
		String( k ), ", ", String ( k-1 ),
		 " ) nearring generated design>" ) );  
    return D;

  end );



############################################################################
##
#M  DesignFromFerreroPairBlank
##

InstallMethod(
	DesignFromFerreroPairBlank,
	"default",
	true,
	[IsGroup, IsGroup],
	0,
  function ( G, Phi )

  local t,v,k,lambda,
	elms,id,basis,points,blocks,b,block,
	fam,D,
	i,j;

    elms := ShallowCopy( AsList(G) );
    id := Identity( G );
    v := Size( G );
    k := Size( Phi )+1;
    elms := Difference( elms, [id] );
    basis := List( Orbits( Phi, elms ), x -> Concatenation( x, [id] ) );

    if v mod k = 0 and IsPrimePowerInt( k ) and
				ForAny( basis, x -> Size(Group(x)) = k ) then
## some basis blocks are subgroups of G, not all their translates are distinct
      Error( "some basis blocks form groups \n" ); 
    fi;

    t := 2;
    lambda := k;
    blocks := List( basis, Set );     
    for b in elms do
      Append( blocks, basis*b );
    od;

    fam := DesignFamily( t, v, k, lambda );
    D := Objectify( fam!.defaultKind, rec() );

    points := AsList( G ); 
    SetPointsOfDesign( D, points );
    SetBlocksOfDesign( D, blocks );    

#    D!.group := G;
#    D!.Phi := Phi;
    SetName( D, Concatenation( "<a 2 - ( ", String( v ), ", ",
		String( k ), ", ", String ( k ),
		 " ) nearring generated design>" ) );  
    return D;

  end );




############################################################################
##
#F  DesignFromPlanarNearRing( <N>, <type> )
##

InstallGlobalFunction( 
	DesignFromPlanarNearRing,
  function( N, type )
  local	D;

  if type = "*" then
    D := DesignFromPlanarNearRingStar( N );
  elif type = " " then
    D := DesignFromPlanarNearRingBlank( N );	
  fi;
  return D;
end );


############################################################################
##
#M  DesignFromPlanarNearRingStar
##

InstallMethod(
	DesignFromPlanarNearRingStar,
	"default",
	true,
	[IsNearRing],
	0,
  function ( nr )

  local t,v,k,lambda,
	elms,zero,a,l,basis,points,blocks,b,
	fam,D,
	i,j,all,B,block,transblock;

    elms := ShallowCopy( AsList( nr ) );
    zero := elms[1];
    elms := Difference( elms, [zero] );
    l := ShallowCopy( elms );
    basis := [];
    all := false;
    while not all do
      a := l[1];
      B := Difference( Set( elms*a ), [zero] );
      Append( basis, [B] );
      l := Difference( l, B );
      if l = [] then
	all := true;
      fi;
    od; 

    t := 2;
    v := Size( nr );
    k := Size( basis[1] );
    lambda := k-1;
    blocks := ShallowCopy( basis );
    for b in elms do
      for block in basis do
	transblock := [];
        for i in block do
	  Append( transblock, [i+b] );
        od;
        Append( blocks, [transblock] );
      od;
    od;

    fam := DesignFamily( t, v, k, lambda );
    D := Objectify( fam!.defaultKind, rec() );

    points := AsList( nr ); 
    SetPointsOfDesign( D, points );
    SetBlocksOfDesign( D, blocks );    

#    D!.nearring := nr;

    SetName( D, Concatenation( "<a 2 - ( ", String( v ), ", ",
		String( k ), ", ", String ( k-1 ),
		 " ) nearring generated design>" ) );  
    return D;

  end );



############################################################################
##
#M  DesignFromPlanarNearRingBlank
##

InstallMethod(
	DesignFromPlanarNearRingBlank,
	"default",
	true,
	[IsNearRing],
	0,
  function ( nr )

  local t,v,k,lambda,
	elms,zero,a,l,basis,points,blocks,b,
	fam,D,
	i,j,all,B,block,transblock;

    v := Size( nr );
    elms := ShallowCopy( AsList( nr ) );
    zero := 0*elms[1];
    l := ShallowCopy( elms );
    l := Difference( l, [zero] );
    basis := [];
    all := false;
    while not all do
      a := l[1];
      B := Set( elms*a );
      k := Size(B);
      if v mod k = 0 and IsPrimePowerInt( k ) and 
			Size(Group( List(B,AsGroupReductElement)) ) = k then
## some basis blocks are subgroups of the additive group of nr,
## not all their translates are distinct
        Error( "some basis blocks form groups \n" ); 
      fi;
      Append( basis, [B] );
      l := Difference( l, B );
      if l = [] then
	all := true;
      fi;
    od; 

    t := 2;
    k := Size( basis[1] );
    lambda := k;
    blocks := ShallowCopy( basis );
    elms := Difference( elms, [zero] );
    for b in elms do
      for block in basis do
	transblock := [];
        for i in block do
	  Append( transblock, [i+b] );
        od;
        Append( blocks, [transblock] );
      od;
    od;

    fam := DesignFamily( t, v, k, lambda );
    D := Objectify( fam!.defaultKind, rec() );

    points := AsList( nr ); 
    SetPointsOfDesign( D, points );
    SetBlocksOfDesign( D, blocks );    

#    D!.nearring := nr;
  

    SetName( D, Concatenation( "<a 2 - ( ", String( v ), ", ",
		String( k ), ", ", String ( k ),
		 " ) nearring generated design>" ) );  
  return D;

end );








##
##  functions for analyzing partial balanced incomplete block designs
##  from wd nearrings on cyclic groups of prime power order
##
##  experimental
##






############################################################################
##
#M  DesignFromWdNearRing
##
##
##  only for wd nearrings with cyclic additive group of prime power order
##


InstallMethod(
	DesignFromWdNearRing,
	"cyclic",
	true,
	[IsNearRing],
	0,
  function ( nr )

  local Q, C, elms,
	t,v,k,lambda,
	a,l,basis,points,blocks,b,
	fam,D,
	i,j,all,B,block,transblock,c, x;


    Q := List( NilpotentElements( nr ), x -> x[1] );
    C := Difference( ShallowCopy( AsList( nr ) ), Q );  
#    elms := AsList( nr );

    a := AdditiveGenerators( nr )[1];
    c := a;
#    points := elms;
    v := Size( nr );
    points :=[a];
    for i in [2..v] do
      c := c+a;
      Add( points, c );
    od;

    if Length( Set(points) ) <> v then
      return ("Error: additive group is not cyclic");
    fi;
#    c := c+a;
#    points := Concatenation( [c], points );

    l := ShallowCopy( C );
    basis := [];
    all := false;
    while not all do
      a := l[1];
      B := Set( points*a );
      Append( basis, [B] );
      l := Difference( l, B );
      if l = [] then
	all := true;
      fi;
    od; 

    t := 1;
    k := Size( basis[1] );
    
    blocks := ShallowCopy( basis );
# take all translates of the basis blocks
    for b in points{[1..v-1]} do
      for block in basis do
	transblock := [];
        for i in block do
	  Append( transblock, [i+b] );
        od;
        Append( blocks, [Set( transblock )] );
      od;
    od;
    b := Length( blocks );
    lambda := b*k/v;

    fam := DesignFamily( t, v, k, lambda );
    D := Objectify( fam!.defaultKind, rec() );

  
    SetPointsOfDesign( D, points );
    SetBlocksOfDesign( D, blocks );    

    SetName( D, Concatenation( "<a 1 - ( ", String( v ), ", ", String( k ),
		", ", String( lambda ), " ) nearring generated design>" ) ); 

  return D;
end);



##########################################################################
##
#F  AutomorphismGroupOfCyclicGroup := function( G, k )
##
##	returns the automorphism group of size <k> of a cyclic group <G>
##	of prime power order if existing
##

InstallGlobalFunction(
	AutomorphismGroupOfCyclicGroup,
  function( G, k )
  local	n, p, d, sizeAut, g, a, h, o,
        phi1, phi2, phi3;

    n := Size( G );
    p := SmallestRootInt( n );
    d := LogInt( n, p );
    sizeAut := (p-1)*p^(d-1);
    
    if not IsCyclic( G ) or not IsPrime( p ) then
      Error( "no cyclic group of prime power order" );
    elif RemInt( sizeAut, k ) <> 0 then
      Error( "<G> has no automorphism group of order <k>" );
    fi;

    g := GeneratorsOfGroup( G )[1];

    if p = 2 then
      h := LogInt( k, 2 );
# there are 3 possible types of automorphism groups of order 2^h on <G>
      o := 2^(d-h);
      phi3 := Group( [GroupHomomorphismByImages( G, G, [g], [g^(2*o+1)] ),
			GroupHomomorphismByImages( G, G, [g], [g^(n-1)] )] );
      if o = 2 then
        return phi3;
      fi;

      phi1 := Group( GroupHomomorphismByImages( G, G, [g], [g^(o+1)] ) );
      phi2 := Group( GroupHomomorphismByImages( G, G, [g], [g^(o-1)] ) );
      
      return [phi1, phi2, phi3];
    else
      a := PrimitiveRootMod( n );
      o := RemInt( a^QuoInt( sizeAut, k ), n );
 
      return Group( GroupHomomorphismByImages( G, G, [g], [g^o] ) );
    fi;

end );


############################################################################
##
#F  DesignFromPair( G, Phi )
##
##  for a cyclic group <G> of prime power order and an automorphism group 
##  <Phi>
##

InstallGlobalFunction(
	DesignFromPair,
  function ( G, Phi )
  local p, ppowers, n,
	t,v,k,lambda,
	elms,id,basis,points,blocks,b,block,
	fam,D, Q, C,
	i,j,x,a,c,

	o;


    id := Identity( G );
    a := GeneratorsOfGroup( G )[1];
    c := id;
    v := Size( G );
    p := SmallestRootInt( v );
    n := LogInt( v, p );
    elms :=[]; Q := []; C := [];
    for i in [1..v-1] do
      c := c*a;
      Add( elms, c );
      if RemInt( i, p ) = 0 then
	Add( Q, c );
      else 
	Add( C, c );
      fi;
    od;
 
    points := Concatenation( elms, [id] );

    basis := []; ppowers := List( [0..n-1], x -> p^x );
    while C <> [] do
      a := C[1];
      block := Union( Orbits( Phi, List( ppowers, x -> a^x ) ) );
      C := Difference( C, block );
      block := Concatenation( [id], block );
      Add( basis, block );
    od;

    k := Size( basis[1] );
    
    blocks := basis;
    
    for b in elms do
      for block in basis do
        AddSet( blocks, block*b );
      od;
    od;

    t := 1;
    b := Length( blocks );
    lambda := b*k/v; 
     
    fam := DesignFamily( t, v, k, lambda );
    D := Objectify( fam!.defaultKind, rec() );

    SetPointsOfDesign( D, points );
    SetBlocksOfDesign( D, blocks );    

#    D!.group := G;
#    D!.Phi := Phi;
    SetName( D, Concatenation( "<a 1 - ( ", String( v ), ", ",
		String( k ), ", ", String ( lambda ),
		 " ) nearring generated design>" ) );  
  return D;
end );


############################################################################
##
#F  ZeroAssociates( G, Phi )
##
##
##

BindGlobal( "ZeroAssociates",
  function( G, Phi )
  local n, a, elms, phis, perms, PPhi, delta, u, P, Ph,
	h, i, j, jay, x, y, d;

    a := GeneratorsOfGroup( G )[1];
    n := Size( G );
    elms := List( [1..n-1], x -> a^x );
    phis := GeneratorsOfGroup( Phi );
    perms := List( phis, x -> PermList( List( elms, y -> 
		Position( elms, Image( x, y ) ) ) ) );
    PPhi := Group( perms );  
    


    if RemInt( Size( PPhi ), 2 ) = 0 then
      delta := Orbits( PPhi, [1..n-1] );
    else
      elms := [1..n-1];
      delta := [];
      while elms <> [] do
	d := Set( Flat( Orbits( PPhi, [elms[1], n-elms[1]] ) ) );
	elms := Difference( elms, d );
	Add( delta, d );
      od;
    fi;

  return delta;
end );


############################################################################
##
#F  AssociationMatrices( D )
##
##  returns the association matrices for the wd nearring generated PBIB design
##  <D>
##
##

BindGlobal( "AssociationMatrices",
  function( D )
  local G, Phi, n, p, b, k, a, elms, f, fperm, PPhi, delta, u, P, Ph,
	h, i, j, jay, x, y, d;



    n := Size( PointsOfDesign( D ) );
    G := CyclicGroup( n );
    a := GeneratorsOfGroup( G )[1];
#    n := Size( G );
    elms := List( [1..n-1], x -> a^x );

    p := SmallestRootInt( n );
    b := Size( BlocksOfDesign( D ) );
    k := QuoInt( n^2*(p-1), p*b );
    Phi := AutomorphismGroupOfCyclicGroup( G, k );
    f := GeneratorsOfGroup( Phi )[1];
    fperm := PermList( List( elms, x -> Position( elms, Image( f, x ) ) ) );
    PPhi := Group( fperm );  
    

    if RemInt( Size( PPhi ), 2 ) = 0 then
      delta := Orbits( PPhi, [1..n-1] );
    else
      elms := [1..n-1];
      delta := [];
      while elms <> [] do
	d := Set( Flat( Orbits( PPhi, [elms[1], n-elms[1]] ) ) );
	elms := Difference( elms, d );
	Add( delta, d );
      od;
    fi;

    u := Length( delta );

    P := [];  
    for h in [1..u] do
      y := delta[h][1];
      Ph := [];
      for j in [1..u] do
	jay := List( delta[j], d -> (d+y) mod n );
        Add( Ph, List( [1..u], i -> Size( Intersection( delta[i], jay ) ) ) );
      od;
      Add( P, Ph );
    od;

  return P;
end );


############################################################################
##
#F  PrintDesignParameterPBIBD( D )
##
##  returns the association matrices for the wd nearring generated PBIB design
##  <D> and gives information on the association scheme
##

InstallGlobalFunction(
	PrintDesignParameterPBIBD,
  function( D )
  local n, G, p, b, k, a, delta, u, lambda, Ph, P,
	h, i, j, x, y, jay;



    n := Size( PointsOfDesign( D ) );
    G := CyclicGroup( n );
    a := GeneratorsOfGroup( G )[1];

    p := SmallestRootInt( n );
    b := Size( BlocksOfDesign( D ) );
    k := QuoInt( n^2*(p-1), p*b );
    Phi := AutomorphismGroupOfCyclicGroup( G, k );

    delta := ZeroAssociates( G, Phi );
    u := Length( delta );

    Print( "\n" );

    for i in [1..u] do
      Print( " xR", i, "y  :<=>  y-x in ", delta[i], "\n" ); 
    od;

    Print( "\n" );

    for i in [1..u] do
       Print( " n", i, " = ", Size( delta[i] ), "\n" );
    od;

    Print( "\n" );

    for i in [1..u] do
      lambda := Set( List( delta[i], x -> 
			Size( BlocksIncidentPoints( D, [n,x] ) ) ) );
      if Size( lambda ) > 1 then
	Print 
	(" Error: not all pairs of points associated by R", i, 
	 "\n are together incident with the same number of blocks \n lambda", 
	 i," = ", lambda, "\n" );
      else
        Print( " lambda", i, " = ", lambda[1], "\n" );
      fi; 
    od;

    Print( "\n\n" );

    P := [];
    for h in [1..u] do
      y := delta[h][1];
      Ph := [];
      for j in [1..u] do
	jay := List( delta[j], d -> (d+y) mod n );
        Add( Ph, List( [1..u], i -> Size( Intersection( delta[i], jay ) ) ) );
      od;
      Add( P, Ph );
      Print( " P", h, " = \n\n" );
      PrintArray( Ph );
      Print( "\n" );	
    od;	

  return P;
end );














