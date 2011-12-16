
##
## finite near-fields:
##


############################################################################
##
#M  IsPairOfDicksonNumbers( <q>, <n> )
##

InstallMethod( 
	IsPairOfDicksonNumbers,
	"default",
	true,
	[IsInt, IsInt],
	0,
  function( q, n )

  local primes, x;

    primes := Set( Factors( n ) );
    if n mod 4 = 0 then
      primes[1] := 4;
    fi;
	
  return (IsPrimePowerInt( q ) and ForAll( primes, x -> (q-1) mod x = 0 ));
end );   


############################################################################
##
#M  NumberOfDicksonNearFields( <q>, <n> )
##

InstallMethod( 
	NumberOfDicksonNearFields,
	"default",
	true,
	[IsInt, IsInt],
	0,
  function( q, n )

  return QuoInt( Phi( n ), Size( GroupByPrimeResidues( [SmallestRootInt( q )], n ) ) ); 
end );


############################################################################
##
#M  DicksonNearFields( <q>, <n> )
##
##  returns the list of all non-isomorphic near-fields determined by the
##  Dickson pair ( <q>, <n> ) 
##

InstallMethod( 
	DicksonNearFields,
	"default",
	true,
	[IsInt, IsInt],
	0,
  function( q, n )

  local primes,
 	F, basis, l, p, z,
	a, s, rems, ks, bs, b, powers,
	G, gens, g, imgs,
	alpha, beta, phi, endos, mul, Ns, N,
	x, y, e, f, i;

	
    primes := Set( Factors( n ) );
    if n mod 4 = 0 then
      primes[1] := 4;
    fi;
	
    if not IsPrimePowerInt( q ) or ForAny( primes, x -> (q-1) mod x <> 0 ) then
      Error( " (q, n) are not Dickson numbers \n" );
    fi;

    F := GF( q^n );
    basis := Basis( F );
    l := Length( basis );
    p := RootInt( q^n, l );
    z := PrimitiveElement( F );

## determine the non-isomorphic nearfields

    a := z^n;
    ks := List( RightCosets(Units(Integers mod n),GroupByPrimeResidues([p],n)),
				 x -> Int( Representative(x) ) );
    bs := List( ks, x -> z^x );

Info( InfoNearRing, 1, Length( bs ), " non-isomorphic nearfields \n" );

    G := ElementaryAbelianGroup( q^n );
    gens := GeneratorsOfGroup( G );
    g := gens[1];

    imgs := [];
    for i in List( basis, x -> Coefficients( basis, a*x ) ) do 
      Add( imgs, Product( List( [1..l], x -> gens[x]^Int(i[x]) ) ) );
    od;

    alpha := GroupHomomorphismByImagesNC( G, G, gens, imgs );
    Ns := [];
    for b in bs do
      imgs := [];
      for i in List( basis, x -> Coefficients( basis, b*x^q ) ) do 
        Add( imgs, Product( List( [1..l], x -> gens[x]^Int(i[x]) ) ) );
      od;

      beta := GroupHomomorphismByImagesNC( G, G, gens, imgs );

Info( InfoNearRing, 2, "autos are built \n" );

      Add( Ns, PlanarNearRing( G, Group( alpha, beta ), [g] ) );
Info( InfoNearRing, 1, "new near field \n" );
    od;

  return Ns;
end );



############################################################################
##
#M  ExceptionalNearFields( <q> )
##
##  returns the list of all non-isomorphic finite near-fields of size <q>
##  which are not Dickson near-fields 
##


InstallMethod( 
	ExceptionalNearFields,
	"default",
	true,
	[IsInt],
	0,
  function( q )

  local p, G, gens, a, b, alpha, phis,
	phi, endos, mul, Ns, N, 
	x, y, e, f, i;

	
    if not q in [25, 49, 121, 529, 841, 3481] then
      Error( "there is no exceptional near field of this order" );
    fi;

    p := RootInt( q );
    G := ElementaryAbelianGroup(q);
    gens := GeneratorsOfGroup( G ); 
    a := gens[1]; b := gens[2];
    alpha :=  GroupHomomorphismByImagesNC( G, G, gens, [b, a^-1] );   
   
    if q = 25 then
      phis := [Group( alpha,
	GroupHomomorphismByImagesNC( G, G, gens, [a*b^-1, a^-2*b^-2] ) )];
    elif q = 49 then
      phis := [Group( alpha,
	GroupHomomorphismByImagesNC( G, G, gens, [a*b^-1, a^3*b^-2] ) )];
    elif q = 121 then
      phis := [Group( alpha,
	GroupHomomorphismByImagesNC( G, G, gens, [a*b^-5, a^5*b^-2] ),
	GroupHomomorphismByImagesNC( G, G, gens, [a^4, b^4] ) ),
		Group( alpha,
	GroupHomomorphismByImagesNC( G, G, gens, [a^2*b, a^4*b^-3] ) )];
    elif q = 529 then
      phis := [Group( alpha,
	GroupHomomorphismByImagesNC( G, G, gens, [a*b^12, a^-6*b^-2] ),
	GroupHomomorphismByImagesNC( G, G, gens, [a^2, b^2] ) )];
    elif q = 841 then
      phis := [Group( alpha,
	GroupHomomorphismByImagesNC( G, G, gens, [a*b^-12, a^-7*b^-2] ),
	GroupHomomorphismByImagesNC( G, G, gens, [a^-13, b^-13] ) )];
    elif q = 3481 then
      phis := [Group( alpha,
	GroupHomomorphismByImagesNC( G, G, gens, [a^9*b^-10, a^15*b^-10] ),
	GroupHomomorphismByImagesNC( G, G, gens, [a^4, b^4] ) )];
    fi;

  return List( phis, phi -> PlanarNearRing( G, phi, [a] ) );   
end );




############################################################################
##
#M  AllExceptionalNearFields( <q> )
##
##  returns the list of all non-isomorphic finite near-fields
##  which are not Dickson near-fields 
##


InstallMethod( 
	AllExceptionalNearFields,
	"default",
	true,
	[],
	0,
  function( )
    return Concatenation( List(  [25, 49, 121, 529, 841, 3481],
						 ExceptionalNearFields ) );
 end);




##
##  planar near-rings
##



############################################################################
##
#M  PlanarNearRing( <G>, <phi>, <reps> )
##
##  returns the planar near-ring determined by the group <G> 
##  with a group of fixed-point-free automorphisms <phi> and
##  a set of orbit representatives <reps>
##

InstallMethod( 
	PlanarNearRing,
	"nearfield",
	true,
	[IsGroup, IsGroup, IsList],
	10,
  function( G, phi, reps )

  local p, e, autos, mul, a, i, N,
	f, j, x, y;
    
## assuming Size(phi) = Size(G) - 1, that is, nearring is a nearfield

    if Size(phi) <> Size(G) - 1 then
      TryNextMethod();
    fi;

    p := SmallestRootInt(Size(G));
    e := reps[1];
    autos := [];
    for f in phi do
      i := ExtRepOfObj( e^f );	
      autos[Sum( List( [1..QuoInt(Length(i),2)], j -> p^(i[2*j-1]-1)*i[2*j]) )] := f;
    od;
      
    mul := function( a, x )
      if a = Identity( G ) then
 	return a;
      else
## a = e^f, with f in autos
	i := ExtRepOfObj(a); 
       	return Image( autos[Sum( List( [1..QuoInt(Length( i ),2)],
		 	j -> p^(i[2*j-1]-1)*i[2*j]) )], x );
      fi;
    end;

    N := ExplicitMultiplicationNearRingNC( G, mul );
    SetIsNearField( N, true );

  return N;
end );



InstallMethod( 
	PlanarNearRing,
	"default",
	true,
	[IsGroup, IsGroup, IsList],
	0,
  function( G, phi, reps )

  local t, autos, orbits, mul, a, N,
	x, y;
    
    t := Length( reps );
    autos := AsList( phi );	
    orbits := List( reps, x -> List( autos, y -> x^y ) );
    
    mul := function( a, x )
    local 	z,
		i, j;

	i := 1;
	z := Identity( G );
        
   	while i in [1..t] do
          j := Position( orbits[i], a );
	  if IsInt( j ) then
            z := x^autos[j];
# stop search and return <z>     
	    i := t;	  
          fi;
	  i := i+1;
        od;
 
      return z;
    end;

    N := ExplicitMultiplicationNearRingNC( G, mul );
    SetIsPlanarNearRing( N, true );

  return N;
end );


############################################################################
##
#M  OrbitRepresentativesForPlanarNearRing( <G>, <phi>, <I> )
##
##
##  	returns all sets of <I> representatives of the orbits of <phi> on <G> 
##  	so that they generate all non isomorphic planar nearrings out of the 
##	Ferrero pair ( <G>, <phi> )


InstallMethod( 
	OrbitRepresentativesForPlanarNearRing,
	"default",
	true,
	[IsGroup, IsGroup, IsInt],
	0,
  function( G, phi, I )

  local A, N, S,
	orbs, combs, lanes, reps, tuples,
	o, So, t,
	i, j, x, y;

    t := (Size(G)-1)/Size(phi);

    if I > t then
      Error( "more representatives than orbits" );
    elif t = 1 then
      return [[AsList( G )[2]]];
    fi;
  
    A := AutomorphismGroup( G );
    N := Normalizer( A, phi );

    orbs := List( Orbits( phi, Difference( AsList( G ), [Identity(G)] ) ),
							 		Set );
## consider combinations of I orbits to choose one representative
## from each of these orbits
    combs := Combinations( orbs, I );
## determine the orbits of N acting on combs
    lanes := Orbits( N, combs, OnSetsDisjointSets );

    reps  := [];
    for i in [1..Length(lanes)] do
## choose one combination of I distinct orbits,
## ie, choose one element o from each lane
      o := lanes[i][1];
      So := Stabilizer( N, o, OnSetsDisjointSets );
## take one element of each orbit in o to obtain I representatives of distinct
## orbits
      tuples := List( Cartesian( o ), Set );
## choose representatives of the action of So (the stabilizer of o in N)
## on tuples 
      reps := Concatenation( reps,
		 List( Orbits( So, tuples, OnSets ), Representative ) );
    od;

  return reps;
end );




##
##  weakly divisible near-rings
##


############################################################################
##
#M  WdNearRing( <G>, <psi>, <phi>, <reps> )
##
##  returns the weakly divisible near-ring determined by the group <G> 
##  with a group of fixed-point-free automorphisms <phi>, a set of
##  orbit representatives <reps> and an endomorphism <psi> 
##

InstallMethod( 
	WdNearRing,
	"default",
	true,
	[IsGroup, IsMapping, IsGroup, IsList],
	0,
  function( G, psi, phi, reps )

  local ker_psi,		# kernel of <psi> 
	q,			# size of the kernel of <psi>
	r,			# smallest number such that psi^r = 0
	autos,  		# elements of <phi>
 	t, 			# number of right identities, <reps>
	psi_autos,      
	autos_psi, 
	elements,		# list of (sorted) group elements
	psi_i,			# psi^i
#	reps_psi,
	multendos,		# list of endormophisms determining the
				# multiplication, in 1 to 1 correspondence
				# with elements
	mul,			# multiplication function of the near-ring <nr> 		
	nr,			# near-ring
	x, y, e, a, i, j;
    

    ker_psi := Kernel( psi );
    q := Size( ker_psi );	
    r := LogInt( Size(G), q );
    t := Length( reps );

#    if q = Size( G ) then
#      Error( "ideal of nilpotent elements is trivial, planar near-ring \n" );
#    elif
    if q^r <> Size(G) or Image(psi^(r-1)) <> ker_psi or
	 				q^(r-1)*(q-1) <> t*Size(phi) then
      Error( "psi is not feasible \n" );
    fi;
    
    autos := AsList( phi );	
    psi_autos := List( autos, x -> psi*x );
    autos_psi := Set( List( autos, x -> x*psi ) );
    if not IsSubset( autos_psi, psi_autos ) then
# necessary condition for associativity is violated 
# (mappings operate from the right) 
      Error( "<phi> is not feasible \n" );
    fi; 

    elements := AsSSortedList( G );
    multendos := [];
# determine multiplication by the invertible elements, G - Image(psi):    
    for e in reps do
      for a in autos do
	j := Position( elements, e^a );      
	if IsBound( multendos[j] ) then
# phi is not fixed-point-free on G - Image(psi)
	  Error( "<phi> is not fixed-point-free on G - Image(psi) or <reps> are not feasible \n" );
        fi;
	multendos[j] := a;
      od;	
    od;    



# determine the multiplication by elements in Image(psi) - Image(psi^2),
# if they exist:
  if r > 1 then
    for e in reps do
      j := Position( elements, e^psi );   
      if not IsBound( multendos[j] ) then
	for a in autos_psi do
	  j := Position( elements, e^a );
	  multendos[j] := a;
        od;
      elif multendos[j] <> psi then
	Error( "<reps> are not feasible, Image(psi) - Image(psi^2 \n" );
      fi;
    od;
  fi;

    psi_i := psi;
#    reps_psi := Set( List( reps, x -> x^psi ) );
    for i in [2..r-1] do
# determine the multiplication by the elements Image(psi^i) - Image(psi^(i+1)):
      psi_i := psi_i*psi;
      autos_psi := Set( List( autos_psi, x -> x*psi ) );
#      reps_psi := Set( List( reps_psi, x -> x^psi ) );
      for e in reps do
        j := Position( elements, e^psi_i );   
        if not IsBound( multendos[j] ) then
	  for a in autos_psi do
	    j := Position( elements, e^a );
	    multendos[j] := a;
          od;
        elif multendos[j] <> psi_i then
Print( "elements = ", elements, "\n" );
Print( "multendos =", multendos, "\n" );
Print( " i = ", i, ", e = ", e, " j = ", j, "\n" ); 
	  Error( "<reps> are not feasible \n" );
        fi;
      od;
    od;

# multiplication by 0:
    j := Position( elements, Identity(G) );
    if IsBound(multendos[j]) then 
Print( multendos[j], "\n" );
      Error( "<reps> are not feasible \n" );
    fi;
    multendos[j] := GroupHomomorphismByFunction( G, G, x -> Identity(G) );


if ForAny( [1..Size(G)], j -> not IsBound( multendos[j] ) ) then    
  Error( "mul not defined \n" );
fi;

    mul := function( a, x )
      return Image( multendos[Position( elements, a )], x );
    end;    

    nr := ExplicitMultiplicationNearRingNC( G, mul );
    SetIsWdNearRing( nr, true );

  return nr;
end );





###########################################################################
##
#O  IsWdNearRing( <nr> )
##
##   	 wd for right nearrings means: for all <a>,<b> in <nr> there is an <x>
##	 such that <x>*<a> = <b> or <x>*<b> = <a>

InstallMethod(
	IsWdNearRing,
	"default",
	true,
	[IsNearRing],
	0,
  function( nr )

  local elms, n, a, B, b, wd,
	i, x;

    elms := AsList( nr );
    n := Size( nr );
# <B[i]> are the elements of <nr> which are obtained by a multiplication
# <elms>[i]*<elms>
    B := List( elms, a ->  Set( a*elms ) );
    wd := true; i := 1;
    while wd and i <= n do
      b := Filtered( [1..n], x -> x >= i and not elms[x] in B[i]  );
# for any element <elms[x]> not in <B[i]> there has to be <elms[i]> in 
# <B[x]> 
      wd := ForAll( b, x -> elms[i] in B[x] );
      i := i+1;
    od;
    
    SetIsWdNearRing( nr, wd );
  
  return wd;
end );
















