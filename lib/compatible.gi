######################################################################
##
#M  MinimalNormalSubgroups
##
#
#InstallMethod(
#	MinimalNormalSubgroups,
#	"filter OneGeneratedNormalSubgroups",
#	[IsGroup],
#	0,
#  function ( G )
#  local gens;
#    gens := OneGeneratedNormalSubgroups( G );
#    return Filtered( gens, 
#	N -> not ( ForAny( gens, M -> IsSubset( N, M ) and N<>M ) ) );
#  end );

######################################################################
##
#M  GeneratorsOfCongruenceLattice
##

InstallMethod(
	GeneratorsOfCongruenceLattice,
	"by conjugacy classes",
	true,
	[IsGroup],
	0,
  function( G )

  local cc, rep, reps, NList, lengthList, ord, i, j, k, deleted, gens;
    cc := ConjugacyClasses( G );
    reps := List( cc, Representative );
    NList := List( cc, AsList );
    lengthList := Length(reps);
    for i in [1..lengthList] do
      rep := reps[i]; ord := Order(rep); j := 1; deleted := false;
      while not deleted and j<ord-1 do
        rep := rep*reps[i];
        j := j+1;
        if Gcd(j,ord)=1 then
	  k := i;
	  while not deleted and k<lengthList do
	    k := k+1;
	    if Order(reps[k]) = ord and rep in NList[k] then
		Unbind(NList[i]);
		deleted := true;
	    fi;
          od;
        fi;
      od;
    od;
 
    gens := [];
    for i in [1..lengthList] do
      if IsBound(NList[i]) and reps[i]<>Identity(G) then
         Add( gens, Subgroup(G,NList[i]) );
#	 Add( gens, NormalClosure( Subgroup( G, [reps[i]] ) ) );
      fi;
    od;

    Sort( gens, function( x, y ) return Size(x) < Size(y); end );

    if Size( gens[Length(gens)] ) = Size(G) then
	return gens{[1..Length(gens)-1]};
    else
	return gens;
    fi;
  end );

######################################################################
##
#M  GeneratorsOfIntervallInCongruenceLattice
##

InstallMethod(
	GeneratorsOfIntervallInCongruenceLattice,
	"all elements",
	true,
	[IsGroup, IsGroup],
	10,
  function( G, N )
    return Filtered( NormalSubgroups( G ), 
	I -> Size(I)>1 and Size(I)<Size(N) and IsSubset( N, I ) );
  end );
	
######################################################################
##
#F  PeakOfnAtg( <G>, <g>, <n> )
##
##  constructs the tfm with value <n> at <g> and 0 everywhere else
##

InstallGlobalFunction( PeakOfnAtg,
  function( G, g, n )
    return EndoMappingByFunction( G,
		function( x )
		  if x = g then return n; else return Identity(G); fi;
                end );
  end );


######################################################################
##
#F  ConstGrpTfmOnRightCosetOfN( <G>, <N>, <g>, <h> )
##
##  constructs the tfm with value <g> on the coset <h>+<N> and
##  0 everywhere else
##

InstallGlobalFunction( ConstGrpTfmOnRightCosetOfN,
  function( G, N, g, h )
  local elms, posg, vec, i;
    elms := AsSSortedList( G );
    posg := Position( elms, g );
    vec := [];
    for i in [1..Size(G)] do
      if elms[i]/h in N then 
	Add( vec, posg );
      else
	Add( vec, 1 );
      fi;
    od;

  return EndoMappingByPositionList( G, vec );

  end );

######################################################################
##
#M  IsCompatibleEndoMapping
##
##  test f(x+n) - f(x) \in <n>
##  Aufwand: |G| * ? < |G|^2
##

InstallMethod(
	IsCompatibleEndoMapping,
	"on quotients",
	true,
	[IsEndoMapping],
	0,
  function( tfm )
  local G, lattgens, mins, others, minscontained, N, epi, J;
    G := Source( tfm );

    lattgens := GeneratorsOfCongruenceLattice( G );
    mins := MinimalNormalSubgroups( G );
    others := Difference( lattgens, mins );
    
    if not ( ForAll( mins, N -> ( IsEndoMappingCompatibleWithNormalSubgroup( tfm, N ) ) ) ) then
	return false;
    fi;

    for N in others do
      minscontained := Filtered( mins, M -> IsSubset(N,M) );
      J := ClosureSubgroups( G, minscontained );
      epi := NaturalHomomorphismByNormalSubgroup( G, J );
      if not ( IsEndoMappingCompatibleWithNormalSubgroup( CompatibleFunctionModNormalSubgroupNC(tfm,epi), Image(epi,N) ) ) then
	return false;
      fi;
    od;

    return true;
end );

######################################################################
##
#M  IsEndoMappingCompatibleWithNormalSubgroup
##
##  test f(x+n) - f(x) \in N for all n in N
##

InstallMethod(
	IsEndoMappingCompatibleWithNormalSubgroup,
	"default",
	[IsEndoMapping,IsGroup],
	0,
  function ( tfm, N )
  local x, n, G;
      G := Source(tfm);
      if Size(G)=Size(N) or Size(N)=1 then return true; fi;
      for x in G do
        for n in GeneratorsOfGroup( N ) do
          if not ( Image( tfm, x*n ) / Image( tfm, x ) in N ) then
	    return false;
          fi;
        od;
      od;
      return true;
  end );

######################################################################
##
#M  CompatibleFunctionModNormalSubgroupNC 

InstallMethod(
	CompatibleFunctionModNormalSubgroupNC,
	"default",
	[IsEndoMapping,IsMapping],
	0,
  function (phi,epi)
	Print("CFMNS: ", Source(phi),Range(phi),Source(epi),Range(epi),"\n");
    return EndoMappingByFunction( Image(epi),
	x -> Image( epi, Image( phi, PreImagesRepresentative( epi, x ) ) )
    );
  end );

######################################################################
##
#M  ClosureSubgroups
##

InstallMethod(
	ClosureSubgroups,
	"one by one",
	[IsGroup,IsCollection],
	0,
  function( G, list )
   local J, S;
     J := TrivialSubgroup( G );
     for S in list do
       J := ClosureGroup( J, S );
     od;
     return J;
  end );
  
######################################################################
##
#F  FunctionsCompatibleWithNormalSubgroupChain( <G>, <[N1..Ns]> )
##
##  compute the nearring of all functions on <G> compatible with
##  N1..Ns (where <N1> <= <N2> <= ... <= <Ns>)
##

InstallMethod(
	FunctionsCompatibleWithNormalSubgroupChain,
	"compute additive generators explicitely",
	true,
	[IsGroup, IsCollection],
	0,
	function( G, nsgps )
local reps, g, h, i, n, generators;
  nsgps := ShallowCopy( nsgps );
  Add( nsgps, G );
  generators := [];

  # functions from G to smallest subgroup
  for n in GeneratorsOfGroup( nsgps[1] ) do
    for g in G do
      Add( generators, PeakOfnAtg( G, g, n ) );
    od;
  od;

  # piecewise constant functions from h+nsgps[i] to nsgps[i+1]
  for i in [1..Length(nsgps)-1] do
    for g in GeneratorsOfGroup( nsgps[i+1] ) do
      reps := RepresentativesModNormalSubgroup( G, nsgps[i] );
      for h in reps do
        Add( generators, ConstGrpTfmOnRightCosetOfN( G, nsgps[i], g, h ) );
      od;
    od;
  od;

  return TransformationNearRingByAdditiveGenerators( G, generators );

  end);

######################################################################
##
#M  CompatibleFunctionNearRing
##

#InstallMethod(
#	CompatibleFunctionNearRing,
#	"as an intersection (use lattice generators only)",
#	true,
#	[IsGroup],
#	1,
#  function( G )
#  local gens, visited, n, chain, chains, C, compfcts, K, MakeChain, i;
#
#    gens := GeneratorsOfCongruenceLattice( G );
#    if gens = [] then
#	return MapNearRing(G);
#    fi;
#    visited := List( gens, x -> false );
#    n := Length(visited);
#
#    # MakeChain uses gens, visited, n and chain as global variables
#    # when recursing
#    MakeChain := function( j )
#    local K;
#      K := First( [j+1..n],
#	k -> not visited[k] and IsSubset(gens[k],gens[j]) );
#      if K=fail then
#	return;
#      else
#	visited[K] := true;
#	Add( chain, K );
#	MakeChain( K );
#	return;
#      fi;
#    end;
#
#    Info( InfoNearRing, 1, "making subgroup chains..." );
#    chains := [];
#    for i in [1..Length(visited)] do
#      if not visited[i] then
#        visited[i] := true;
#        chain := [i];
#        MakeChain(i);
#	Info( InfoNearRing, 2, "chain ", chain );
#	Add( chains, chain );
#      fi;
#    od;
#    # longer chains of subgroups will hopefully result in smaller
#    # nearrings for intersection, so we begin with the longest chains
#    Sort( chains, function( x, y ) return Length(x)>Length(y); end );
#
#    Info( InfoNearRing, 1, "computing additive generators" );
#    C := FunctionsCompatibleWithNormalSubgroupChain( G,
#					List( chains[1], i->gens[i] ) );
#    chains := chains{[2..Length(chains)]};
#
#    for chain in chains do
#      Info( InfoNearRing, 1, "computing additive generators" );
#      compfcts := FunctionsCompatibleWithNormalSubgroupChain( G,
#					List( chain, i->gens[i] ) );
#      Info( InfoNearRing, 1, "intersecting nearrings");
#      C := Intersection( C, compfcts );
#      Info( InfoNearRing, 3, "intersection has size ", Size(C) );
#    od;
#
#    return C;
#
#  end );

#InstallMethod(
#	CompatibleFunctionNearRing,
#	"as an intersection (use all normal subgroups)",
#	true,
#	[IsGroup],
#	10,
#  function( G )
#  local gens, visited, n, chain, chains, C, compfcts, K, MakeChain, i;
#
#    gens := ShallowCopy( NormalSubgroups( G ) );
#    RemoveElmList( gens, Position( gens, G ) );
#    RemoveElmList( gens, Position( gens, TrivialSubgroup( G ) ) );
#    if gens = [] then
#	return MapNearRing(G);
#    fi;
#    visited := List( gens, x -> false );
#    n := Length(visited);
#
#    # MakeChain uses gens, visited, n and chain as global variables
#    # when recursing
#    MakeChain := function( j )
#    local K;
#      K := First( [j+1..n],
#	k -> Size(gens[k])>Size(gens[j]) and IsSubset(gens[k],gens[j]) );
#      if K=fail then
#	return;
#      else
#	visited[K] := true;
#	Add( chain, K );
#	MakeChain( K );
#	return;
#      fi;
#    end;
#
#    Info( InfoNearRing, 1, "making subgroup chains..." );
#    chains := [];
#    for i in [1..Length(visited)] do
#      if not visited[i] then
#        visited[i] := true;
#        chain := [i];
#        MakeChain(i);
#	Info( InfoNearRing, 2, "chain ", chain );
#	Add( chains, chain );
#      fi;
#    od;
#    # longer chains of subgroups will hopefully result in smaller
#    # nearrings for intersection, so we begin with the longest chains
#    Sort( chains, function( x, y ) return Length(x)>Length(y); end );
#
#    Info( InfoNearRing, 1, "computing additive generators" );
#    C := FunctionsCompatibleWithNormalSubgroupChain( G,
#					List( chains[1], i->gens[i] ) );
#    chains := chains{[2..Length(chains)]};
#
#    for chain in chains do
#      Info( InfoNearRing, 1, "computing additive generators" );
#      compfcts := FunctionsCompatibleWithNormalSubgroupChain( G,
#					List( chain, i->gens[i] ) );
#      Info( InfoNearRing, 1, "intersecting nearrings");
#      C := Intersection( C, compfcts );
#      Info( InfoNearRing, 3, "intersection has size ", Size(C) );
#    od;
#
#    return C;
#
#  end );

InstallMethod(
	CompatibleFunctionNearRing,
	"compute the 0-symmetric part first",
	true,
	[IsGroup],
	0,
  function( G )
  local C0, c0gens, const;
    C0 := ZeroSymmetricCompatibleFunctionNearRing( G );
    c0gens := AdditiveGenerators( C0 );
    const := List( GeneratorsOfGroup( G ), g -> ConstantEndoMapping( G, g ) );
    
    return TransformationNearRingByAdditiveGenerators( G,
	Concatenation( c0gens, const ) );
  end );

######################################################################
##
#F  ZeroSymmetricFunctionsCompatibleWithNormalSubgroupChain( <G>, <[N1..Ns]> )
##
##  compute the nearring of all functions on <G> compatible with
##  N1..Ns (where <N1> <= <N2> <= ... <= <Ns>)
##

ZeroSymmetricFunctionsCompatibleWithNormalSubgroupChain := function( G, nsgps )
local reps, g, h, i, n, generators;
  nsgps := ShallowCopy( nsgps );
  Add( nsgps, G );
  generators := [];

  # functions from G to smallest subgroup
  for n in GeneratorsOfGroup( nsgps[1] ) do
    for g in G do
      if g<>Identity(G) then # 0-symmetry !
	Add( generators, PeakOfnAtg( G, g, n ) );
      fi;
    od;
  od;

  # piecewise constant functions from h+nsgps[i] to nsgps[i+1]
  for i in [1..Length(nsgps)-1] do
    for g in GeneratorsOfGroup( nsgps[i+1] ) do
      reps := NontrivialRepresentativesModNormalSubgroup( G, nsgps[i] );
		# 0-symmetry !
      for h in reps do
        Add( generators, ConstGrpTfmOnRightCosetOfN( G, nsgps[i], g, h ) );
      od;
    od;
  od;

  return TransformationNearRingByAdditiveGenerators( G, generators );

end;

######################################################################
##
#M  ZeroSymmetricCompatibleFunctionNearRing
##

InstallMethod(
	ZeroSymmetricCompatibleFunctionNearRing,
	"as an intersection (use all normal subgroups)",
	true,
	[IsGroup],
	1,
  function( G )
  local gens, visited, n, chain, chains, C, compfcts, K, MakeChain, i;

    gens := ShallowCopy( NormalSubgroups( G ) );
    RemoveElmList( gens, Position( gens, G ) );
    RemoveElmList( gens, Position( gens, TrivialSubgroup( G ) ) );
    if gens = [] then
	return ZeroSymmetricPart(MapNearRing(G));
    fi;
    visited := List( gens, x -> false );
    n := Length(visited);

    # MakeChain uses gens, visited, n and chain as global variables
    # when recursing
    MakeChain := function( j )
    local K;
      K := First( [j+1..n],
	k -> not visited[k] and IsSubset(gens[k],gens[j]) );
      if K=fail then
	return;
      else
	visited[K] := true;
	Add( chain, K );
	MakeChain( K );
	return;
      fi;
    end;

    Info( InfoNearRing, 1, "making subgroup chains..." );
    chains := [];
    for i in [1..Length(visited)] do
      if not visited[i] then
        visited[i] := true;
        chain := [i];
        MakeChain(i);
	Info( InfoNearRing, 2, "chain ", chain );
	Add( chains, chain );
      fi;
    od;
    # longer chains of subgroups will hopefully result in smaller
    # nearrings for intersection, so we begin with the longest chains
    Sort( chains, function( x, y ) return Length(x)>Length(y); end );

    Info( InfoNearRing, 1, "computing additive generators" );
    C := ZeroSymmetricFunctionsCompatibleWithNormalSubgroupChain( G,
					List( chains[1], i->gens[i] ) );
    chains := chains{[2..Length(chains)]};

    for chain in chains do
      Info( InfoNearRing, 1, "computing additive generators" );
      compfcts := ZeroSymmetricFunctionsCompatibleWithNormalSubgroupChain( G,
					List( chain, i->gens[i] ) );
      Info( InfoNearRing, 1, "intersecting nearrings");
      C := Intersection( C, compfcts );
      Info( InfoNearRing, 3, "intersection has size ", Size(C) );
    od;

    return C;

  end );

######################################################################
##
#M  RestrictedCompatibleFunctionNearRing( <S>, <G> )
##  the nearring of functions on <G> compatible with all subgroups
##  of <G> which are normal in <S> (<G> is normal in <S>) 

InstallMethod(
	RestrictedCompatibleFunctionNearRing,
	"as an intersection",
	true,
	[IsGroup,IsGroup],
	0,
  function( S, G )
  local gens, visited, n, chain, chains, C, compfcts, K, MakeChain, i;

    gens := GeneratorsOfIntervallInCongruenceLattice( S, G );
    if gens = [] then
	return MapNearRing(G);
    fi;
    visited := List( gens, x -> false );
    n := Length(visited);

    # MakeChain uses gens, visited, n and chain as global variables
    # when recursing
    MakeChain := function( j )
    local K;
      K := First( [j+1..n],
	k -> not visited[k] and IsSubset(gens[k],gens[j]) );
      if K=fail then
	return;
      else
	visited[K] := true;
	Add( chain, K );
	MakeChain( K );
	return;
      fi;
    end;

    Info( InfoNearRing, 1, "making subgroup chains..." );
    chains := [];
    for i in [1..Length(visited)] do
      if not visited[i] then
        visited[i] := true;
        chain := [i];
        MakeChain(i);
	Info( InfoNearRing, 2, "chain ", chain );
	Add( chains, chain );
      fi;
    od;
    # longer chains of subgroups will hopefully result in smaller
    # nearrings for intersection, so we begin with the longest chains
    Sort( chains, function( x, y ) return Length(x)>Length(y); end );

    Info( InfoNearRing, 1, "computing additive generators" );
    C := FunctionsCompatibleWithNormalSubgroupChain( G,
					List( chains[1], i->gens[i] ) );
    chains := chains{[2..Length(chains)]};

    for chain in chains do
      Info( InfoNearRing, 1, "computing additive generators" );
      compfcts := FunctionsCompatibleWithNormalSubgroupChain( G,
					List( chain, i->gens[i] ) );
      Info( InfoNearRing, 1, "intersecting nearrings");
      C := Intersection( C, compfcts );
      Info( InfoNearRing, 3, "intersection has size ", Size(C) );
    od;

    return C;

  end );


######################################################################
##
#M  CompatibleFunctionNearRing
##

InstallMethod(
	CompatibleFunctionNearRing,
	"cyclic p-groups",
	true,
	[IsGroup],
	20,
  function( G )
  local gens, visited, n, chain, chains, C, compfcts, K, MakeChain, i;
    if not ( IsCyclic(G) and IsPGroup(G) ) then
	TryNextMethod();
    fi;
    gens := Subgroups( G ); gens := gens{[2..Length(gens)-1]};
    if gens = [] then
	return MapNearRing(G);
    fi;

    return FunctionsCompatibleWithNormalSubgroupChain( G, gens );

  end );


######################################################################
##
#M  CompatibleFunctionNearRing
##

InstallMethod(
	CompatibleFunctionNearRing,
	"abelian groups",
	true,
	[IsGroup],
	10,
  function( G )
  local Ggens, exp, g1, exp2, hom, F, preImageList, p,
	UniquePreImagesRepresentativePowExp2, sigmaGens, generators, c;
  if not IsAbelian(G) then
	TryNextMethod();
  fi;

  if IsPGroup( G ) then
	Info( InfoNearRing, 1, "group is abelian p-group" );
        Ggens := GeneratorsOfGroup( G );
        exp := Exponent(G);
  	g1 := First( Ggens, g -> Order(g)=exp );
  	exp2 := Exponent( G/Subgroup(G,[g1]) );
  	if exp2 = Exponent(G) or 2*exp2 = exp then
		Info( InfoNearRing, 3, "Group is affine complete" );
		return PolynomialNearRing( G );
  	fi;

        hom := GroupHomomorphismByImages( G, G, 
			Ggens, List( Ggens, g -> g^exp2 ) );
        F := Image( hom, G );

        preImageList := List( AsSSortedList(F),
				f -> (PreImagesRepresentative(hom,f))^exp2 );
        UniquePreImagesRepresentativePowExp2 := function( f )
		return preImageList[ Position( AsSSortedList(F), f ) ];
	end;

        sigmaGens := AdditiveGenerators( CompatibleFunctionNearRing( F ) );

        generators := List( sigmaGens, sigma ->
		EndoMappingByFunction( G,
			x -> UniquePreImagesRepresentativePowExp2( 
				Image( sigma, Image( hom, x ) ) 
			     ) 
					     )
			  );

        for c in GeneratorsOfGroup( G ) do
	    Add( generators, ConstantEndoMapping( G, c ) );
        od;
        Add( generators, IdentityEndoMapping( G ) );

        return TransformationNearRingByAdditiveGenerators( G, generators );
  else
	TryNextMethod();
  fi;
end );

######################################################################
##
#M  ProjectionsOntoDirectFactors
##

InstallGlobalFunction( ProjectionsOntoDirectFactors,
  function( G, factors )
  local projections, S, actSize, gensS, sumSizes, 
	allGenerators, numberOfGenerators;

    allGenerators := Flat( List( factors, GeneratorsOfGroup ) );
    numberOfGenerators := Length( allGenerators );
    projections := []; sumSizes := 0;
    for S in factors do
	gensS := GeneratorsOfGroup(S);
	actSize := Length( gensS );
	Add( projections, 
	      GroupHomomorphismByImages( G, S, allGenerators, 
		  Concatenation( 
		     List( [1..sumSizes], i -> Identity(S) ), gensS,
		     List( [sumSizes+actSize+1..numberOfGenerators], 
			i -> Identity(S) )
		  )
	      )
	);
	sumSizes := sumSizes + actSize;
    od;

    return projections;
  end );

######################################################################
##
#M  CompatibleFunctionNearRing
##

InstallMethod(
	CompatibleFunctionNearRing,
	"nilpotent groups", 
	true,
	[IsGroup],
	5,
  function( G )
  local sylowSystem, proj, generatorsOfCompFunctNROnFactors,
	generators;
    if (not IsNilpotent( G ) ) or IsPGroup( G ) then
	TryNextMethod();
    fi;

    sylowSystem := SylowSystem( G );
    Info( InfoNearRing, 2, "computing nearrings of compatible functions" );
    Info( InfoNearRing, 2, "on groups of size ", List( sylowSystem, Size) );

    proj := ProjectionsOntoDirectFactors( G, sylowSystem );
    generatorsOfCompFunctNROnFactors :=
	List( sylowSystem, S -> 
		AdditiveGenerators( CompatibleFunctionNearRing( S ) ) );
    generators := Flat( 
	List( [1..Length(sylowSystem)],
		i -> List( generatorsOfCompFunctNROnFactors[i],
			tfm -> EndoMappingByFunction( G,
				x -> Image( tfm, Image( proj[i], x ) ) )
			) )
		  );
    return TransformationNearRingByAdditiveGenerators( G, generators );
  end);

######################################################################
##
#M  DirectFactorisation
##

InstallGlobalFunction( DirectFactorisation,
  function ( G )
   local factorList, rest, N12, c, C, factorsFound; 

   factorList := [];
   rest := G; 
   C := Combinations (NormalSubgroups (G), 2);
   for c in C do
      Sort (c, function (x,y) return Size (x) < Size (y); end);
   od;

   Sort (C, function (p,q) 
              return  Size (p[1]) < Size (q[1]) or 
                        (Size (p[1]) = Size (q[1]) and
                         Size (p[2])< Size (q[2]));
            end);

   while Size (rest) > 1 do
      factorsFound := false;
      for N12 in C do
         if (not factorsFound) and
               Size (N12 [1]) > 1 and Size (N12 [2]) > 1 and
               Size (N12 [1]) * Size (N12 [2]) = Size (rest) and
               Size (Intersection (N12 [1], N12 [2])) = 1 then
            Add (factorList, N12 [1]);
            rest := N12 [2];
            C := Filtered (C, x -> IsSubgroup (rest, x[1]) and
                                   IsSubgroup (rest, x[2]));    
            
            #  Print (List (C, c -> List (c, Size)));
            factorsFound := true;
         fi;
      od;

      if not factorsFound then 
         Add (factorList, rest);
         rest := TrivialSubgroup (G);
      fi;   
   od;
   return factorList;

  end );

######################################################################
##
#M  DirectFactorisationRelativePrime
##

InstallGlobalFunction( DirectFactorisationRelativePrime,
  function ( G )
   local factorList, rest, N12, c, C, factorsFound; 

   factorList := [];
   rest := G; 
   C := Combinations (NormalSubgroups (G), 2);
   for c in C do
      Sort (c, function (x,y) return Size (x) < Size (y); end);
   od;

   Sort (C, function (p,q) 
              return  Size (p[1]) < Size (q[1]) or 
                        (Size (p[1]) = Size (q[1]) and
                         Size (p[2])< Size (q[2]));
            end);

   while Size (rest) > 1 do
      factorsFound := false;
      for N12 in C do
         if (not factorsFound) and
               Size (N12 [1]) > 1 and Size (N12 [2]) > 1 and
	       Gcd( Size (N12 [1]), Size (N12 [2]) ) = 1 and
               Size (N12 [1]) * Size (N12 [2]) = Size (rest) and
               Size (Intersection (N12 [1], N12 [2])) = 1 then
            Add (factorList, N12 [1]);
            rest := N12 [2];
            C := Filtered (C, x -> IsSubgroup (rest, x[1]) and
                                   IsSubgroup (rest, x[2]));    
            
            #  Print (List (C, c -> List (c, Size)));
            factorsFound := true;
         fi;
      od;

      if not factorsFound then 
         Add (factorList, rest);
         rest := TrivialSubgroup (G);
      fi;   
   od;
   return factorList;

  end );

######################################################################
##
#M  CompatibleFunctionNearRing
##

InstallMethod(
	CompatibleFunctionNearRing,
	"direct products rel. prime order", 
	true,
	[IsGroup],
	4,
  function( G )
  local factors, proj, generatorsOfCompFunctNROnFactors,
	generators;
    if IsPGroup( G ) then
	TryNextMethod();
    fi;

    factors := DirectFactorisationRelativePrime( G );
    if Length(factors)=1 then
	TryNextMethod();
    fi;
    proj := ProjectionsOntoDirectFactors( G, factors );
    generatorsOfCompFunctNROnFactors :=
	List( factors, S -> 
		AdditiveGenerators( CompatibleFunctionNearRing( S ) ) );
    generators := Flat( 
	List( [1..Length(factors)],
		i -> List( generatorsOfCompFunctNROnFactors[i],
			tfm -> EndoMappingByFunction( G,
				x -> Image( tfm, Image( proj[i], x ) ) )
			) )
		  );
    return TransformationNearRingByAdditiveGenerators( G, generators );
  end);







ConjugateCompatible := function( f, h )
local G,H;
  G := Source(h);
  H := Image(h,G);
  return EndoMappingByFunction( H, x->Image(h,Image(f,
		PreImagesRepresentative(h,x))));
end;

RestrictToSubgroup := function( f, S )
  return EndoMappingByFunction( S, x -> Image( f, x ) );
end;

######################################################################
##
#M  Is1AffineComplete
##

InstallMethod(
	Is1AffineComplete,
	"small groups",
	true,
	[IsGroup],
	200,
  function( G )
  if Size(G)<=100 then return IdGroup(G) in 
        [ [ 1, 1 ],
          [ 2, 1 ],
          [ 4, 2 ],
	  [ 8, 2 ], [ 8, 5 ], 
	  [ 9, 2 ], 
	  [ 16, 2 ], [ 16, 10 ], [ 16, 11 ], [ 16, 12 ], [ 16, 14 ],
	  [ 18, 4 ], [ 18, 5 ],
  	  [ 25, 2 ], 
	  [ 27, 5 ], 
	  [ 32, 3 ], [ 32, 21 ], [ 32, 27 ], [ 32, 34 ], [ 32, 35 ], 
	  [ 32, 45 ], [ 32, 46 ], [ 32, 47 ], [ 32, 51 ],
 	  [ 36, 13 ], [ 36, 14 ],
 	  [ 49, 2 ],
	  [ 50, 4 ], [ 50, 5 ], 
	  [ 54, 14 ], [ 54, 15 ],
  	  [ 60, 5 ], 
	  [ 64, 2 ], [ 64, 55 ], [ 64, 73 ], [ 64, 76 ], [ 64, 83 ],
  	  [ 64, 173 ], [ 64, 174 ], [ 64, 175 ], [ 64, 179 ], [ 64, 181 ],
  	  [ 64, 192 ], [ 64, 202 ], [ 64, 211 ], [ 64, 212 ], [ 64, 260 ],
  	  [ 64, 261 ], [ 64, 262 ], [ 64, 267 ], 
	  [ 72, 32 ], [ 72, 34 ], [ 72, 36 ], [ 72, 49 ], [ 72, 50 ], 
	  [ 81, 2 ], [ 81, 15 ], 
	  [ 98, 4 ], [ 98, 5 ],
  	  [ 100, 15 ], [ 100, 16 ] ];
  else
	TryNextMethod();
  fi;
  end );

InstallMethod(
	Is1AffineComplete,
	"abelian groups",
	true,
	[IsGroup and IsAbelian],
	110,
  function( G )
  local inv, bases, primes, exponents, p, pexps, i;
    inv := List( AbelianInvariants( G ), Factors );
    bases := List( inv, i -> i[1] );
    exponents := List( inv, Length );
    primes := Set( bases );
    for p in primes do
      pexps := [];
      for i in [1..Length(bases)] do
        if p=bases[i] then
	  Add( pexps, exponents[i] );
        fi;
      od;
      Sort(pexps); pexps := Reversed(pexps);
      if p=2 then
	if (Length( pexps ) = 1 and pexps[1] > 1 ) or
           (Length( pexps ) <> 1 and 
	   pexps[1]-pexps[2]>1) then
		return false;
	fi;
      else
	if Length( pexps ) = 1 or
	   pexps[1]<>pexps[2] then
		return false;
	fi;
      fi;
    od;

    return true;
   
  end );

InstallMethod(
	Is1AffineComplete,
	"2-group",
	true,
	[IsGroup and IsPGroup],
	105,
  function( G )
    local complement;
    if Factors( Size( G ) )[1] <> 2 then
	TryNextMethod();
    fi;
    if Size( DerivedSubgroup( G ) ) <> 2 then
	TryNextMethod();
    fi;
    complement := function( G, I )
  	return ClosureSubgroups( G, 
	Filtered( NormalSubgroups(G), 
			n -> Size( Intersection( n, I ) ) = 1 ) );
    end;
    return Size(G)=4*Size(Centre(G)) and
	   Exponent(G)=4 and
	   Exponent(G/DerivedSubgroup(G))=2 and
	   Size(G)=4*Size(complement( G, DerivedSubgroup( G ) ));
end );

InstallMethod(
	Is1AffineComplete,
	"small centre",
	true,
	[IsGroup and IsPGroup],
	102,
  function ( G )
    if Factors( Size( G ) )[1] = 2 then
	TryNextMethod();
    fi;
    if Length( Factors( Size( Centre( G ) ) ) ) < 3 then
	return false;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	Is1AffineComplete,
	"small factor by centre",
	true,
	[IsGroup and IsPGroup],
	101,
  function ( G )
    if Factors( Size( G ) )[1] = 2 then
	TryNextMethod();
    fi;
    if Length( Factors( Size( G / Centre( G ) ) ) ) < 3 then
	return false;
    else
	TryNextMethod();
    fi;
  end );
 
InstallMethod(
	Is1AffineComplete,
	"cyclic centre",
	true,
	[IsGroup and IsPGroup],
	100,
  function ( G )
    if Factors( Size( G ) )[1] = 2 then
	TryNextMethod();
    fi;
    if IsCyclic( Centre( G ) ) then
	return false;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	Is1AffineComplete,
	"small commutator",
	true,
	[IsGroup and IsPGroup],
	97,
  function ( G )
    if Factors( Size( G ) )[1] = 2 then
	TryNextMethod();
    fi;
    if Length( Factors( Size( DerivedSubgroup( G ) ) ) ) < 3 then
	return false;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	Is1AffineComplete,
	"cyclic commutator",
	true,
	[IsGroup and IsPGroup],
	95,
  function ( G )
    if Factors( Size( G ) )[1] = 2 then
	TryNextMethod();
    fi;
    if IsCyclic( DerivedSubgroup( G ) ) then
	return false;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	Is1AffineComplete,
	"cyclic centre",
	true,
	[IsGroup and IsPGroup],
	90,
  function ( G )
    if Factors( Size( G ) )[1] = 2 then
	TryNextMethod();
    fi;
    if IsCyclic( Intersection( DerivedSubgroup(G), Centre( G ) ) ) then
	return false;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	Is1AffineComplete,
	"nilpotent group",
	true,
	[IsGroup],
	80,
  function ( G )
  if IsPGroup(G) or not( IsNilpotentGroup( G ) ) then
	TryNextMethod();
  fi;
  return ForAll( SylowSystem( G ), Is1AffineComplete );
end
);


InstallMethod(
	Is1AffineComplete,
	"default",
	true,
	[IsGroup],
	0,
  G -> Size( CompatibleFunctionNearRing(G) ) = Size( PolynomialNearRing(G) ) );

#CanBe1AffineComplete := function ( G )
#
# local p, normals, minimals, J, Jlist, canbeaffinecomplete, i, j;
 
#
#
# p := FactorsInt (Size(G)) [1];
# normals := NormalSubgroups (G);
# minimals := Filtered (normals, x -> Size(x) = p);
# canbeaffinecomplete := true;
# i := 1;
# while canbeaffinecomplete and i <= Length(minimals) do
##    Print (i);
#    Jlist := Filtered (normals, x -> not IsSubgroup (x, minimals [i]));
#    J := Jlist[1];
#    for j in [2..Length (Jlist)] do
#       J := ClosureGroup (J, Jlist[j]);
#    od;
#    canbeaffinecomplete := (Size(J) = Size (G));
#    if canbeaffinecomplete then
#       i := i + 1;
##       Print(i);
#    fi;
# od;
# return canbeaffinecomplete;
#end;

NarrowCongruences := function( G )
local gens, nsgs;
  gens := GeneratorsOfCongruenceLattice( G );
  nsgs := NormalSubgroups( G );
  return Filtered( nsgs, N -> 
	Size(N)>1 and Size(N)<Size(G) and
	ForAll( gens, I -> IsSubset(I,N) or IsSubset(N,I) ) );
end;


######################################################################
##
#F  PrintAsTerm( <poly> )
##
##  print a polynomial function as a term

DeclareGlobalFunction( "PrintAsTerm" );

InstallGlobalFunction( PrintAsTerm,

function ( p )
local names, P, Pgen, Pgroup, F, Fgen, hom, term,
	letter, exponent, first, i, len;
  P := PolynomialNearRing( Source(p) );
  if not p in P then
	Error("Transformation has to be a polynomial function");
  fi;
  Pgroup := GroupReduct( P );
  Pgen := GeneratorsOfGroup( Pgroup );
  names := List( [1..Length(Pgen)-1],
			i -> Concatenation( "g", String(i) ) );
  Add( names, "x" );
  F := FreeGroup( names );
  Fgen := GeneratorsOfGroup(F);
  hom := GroupHomomorphismByImagesNC( F, Pgroup, Fgen, Pgen );
  term := PreImagesRepresentative( hom, AsGroupReductElement(p) );

  len := Length(term);
  if term = Identity(F) then
	Print( " 0 " );
  else
	letter := Subword(term,1,1);
	exponent := 1;
	first := true;
	for i in [2..len] do
		if Subword(term,i,i)<>letter then
			if letter in Fgen then
				if not first then
					Print(" + ");
				else
					first := false;
				fi;
				if exponent>1 then
					Print(exponent," * ");
				fi;
				Print(letter);
			else
				Print(" - ");
				if exponent>1 then
					Print(exponent," * ");
				fi;
				Print(letter^-1);
				first := false;
			fi;
			letter := Subword(term,i,i);
			exponent := 1;
		else
			exponent := exponent + 1;
		fi;
	od;
	if letter in Fgen then
		if not first then
			Print(" + ");
		fi;
		if exponent>1 then
			Print(exponent," * ");
		fi;
		Print(letter);
	else
		Print(" - ");
		if exponent>1 then
			Print(exponent," * ");
		fi;
		Print(letter^-1);
	fi;
  fi;
  Print("\n");

 end );

LiftCompGenFromQuotByMinDistNormSgp :=
  function( G, A, K, phi )
# phi is a compatible function on G/A
# K is the sum of all ideals of G having trivial intersection with A  
  local R, S, epsA, epsK, E, listG, gens, phiA, iso,
	t, e, si, x, s, dx, cx, psix, pos;

    # coset representatives
    epsA := NaturalHomomorphismByNormalSubgroup( G, A );
    epsK := NaturalHomomorphismByNormalSubgroup( G, K );
    iso := IsomorphismGroups( Range(epsA), Source(phi) );
    epsA := epsA*iso;
    phiA := epsA*phi;

    R := List( Image(epsA), x -> PreImagesRepresentative( epsA, x ) );
    S := List( Image(epsK), x -> PreImagesRepresentative( epsK, x ) );

    E := Enumerator( A );
    listG := AsSSortedList( G );

    gens := [];

    for t in Tuples( [1..Size(A)], Length(S) ) do
      psix := [];
      # compute offset cx for every x in G
      for x in listG do
	s := PreImagesRepresentative( epsK, Image( epsK, x ) );
        dx := PreImagesRepresentative( epsA, Image( phiA, x ))^(-1)
            * PreImagesRepresentative( epsA, Image( phiA, s ));
        cx := Intersection( RightCoset( K, dx ), A )[1];
        Add( psix, PreImagesRepresentative( epsA, Image( phiA, x )) * cx*
	 E[ t[ Position( S, s) ] ] );
      od;
      pos := List( psix, y -> Position( listG, y ) );
      Add( gens, EndoMappingByPositionList( G, pos ) );
#       Add( gens, AsEndoMapping( GroupGeneralMappingByImages( G, G, listG, psix ) ) );
    od;
    Info( InfoNearRing, 3, "single generator lifted" );
    return gens;
end;

######################################################################
##
#M  DistributiveMinimalNormalSubgroupsAndComplements
##

#InstallMethod(
#	DistributiveMinimalNormalSubgroup,
#	"filter OneGeneratedNormalSubgroups",
#	[IsGroup],
#	0,
DistributiveMinimalNormalSubgroupsAndComplements :=
  function( G )
  local M, A, I, J, K, nondist, DMNSGC;
    M := MinimalNormalSubgroups( G );
    A := NormalSubgroups(G);
    DMNSGC := [];
    for J in M do
      nondist := false;
      K := TrivialSubgroup( G );
      for I in A do
        if not( IsSubset( I, J ) ) then
	  K := ClosureGroup( K, I );
	  if IsSubset( K, J ) then
	     nondist := true;
	     break;
	  fi; 
        fi;
      od;
      if not(nondist) then
	Add( DMNSGC, [J,K] );
      fi;
    od;
    return DMNSGC;
end;

InstallMethod(
	CompatibleFunctionNearRing,
	"distributive minimal normal subgroup",
	[IsGroup],
	3,
  function( G )
  local H, A, K, CGAgens, psi, CGgens, epsA, R, x, gen, e, g, numbersofgens;
    H := DistributiveMinimalNormalSubgroupsAndComplements( G );

    if H=[] then
	TryNextMethod();
    fi;
    
    # case 1: unique minimal normal subgroup

    if Length(H)=1 and Size(H[1][2])=1 then
      Info( InfoNearRing, 1, "group has a unique minimal normal subgroup" );
      A := H[1][1];
      CGAgens := AdditiveGenerators( CompatibleFunctionNearRing( G/A ) );
      epsA := NaturalHomomorphismByNormalSubgroup( G, A );
      R := List( Image(epsA), x -> PreImagesRepresentative( epsA, x ) );
      CGgens := List( CGAgens, gen ->
	EndoMappingByFunction( G, 
	x -> PreImagesRepresentative( epsA, Image( gen, Image( epsA, x ) ) ) ) );
      for g in G do
        for e in GeneratorsOfGroup( A ) do
	   Add( CGgens, PeakOfnAtg( G, g, e ) );
        od;
      od;
      return TransformationNearRingByAdditiveGenerators( G, CGgens );
    fi;

    # case 2: low index complement

    numbersofgens := List( H, AK -> Size(AK[1])^(Size(G)/Size(AK[2])));
    SortParallel( numbersofgens, H );
    
    if numbersofgens[1] < 500 then
      Info( InfoNearRing, 1, 
	"group has a distributive minimal normal subgroup" );
      Info( InfoNearRing,1,
	"with a low exponent complement" );
      A := H[1][1]; K := H[1][2];
      Info( InfoNearRing, 2, "computing nearring of compatible functions" );
      Info( InfoNearRing, 2, "on a quotient of size ", Size(G)/Size(A) );
      CGAgens := AdditiveGenerators( CompatibleFunctionNearRing( G/A ) );
      Info( InfoNearRing, 2, "lifting ", Length( CGAgens )," generators" );
      if Length( CGAgens ) * numbersofgens[1] > 1000 then
	Info( InfoNearRing, 1, "giving up, too many generators" );
	TryNextMethod();
      fi;
      CGgens := Flat( List( CGAgens, 
            psi -> LiftCompGenFromQuotByMinDistNormSgp( G, A, K, psi ) ) );
      return TransformationNearRingByAdditiveGenerators( G, CGgens );
    fi;

    # case 3: more than 1000 generators expected, give up

    TryNextMethod();

  end );    

#InstallMethod(
#	Is1AffineComplete,
#	"distributive minimal normal subgroup",
#	true,
#	[IsGroup],
#	70,
#  function( G )
#  if ForAny( DistributiveMinimalNormalSubgroupsAndComplements( G ),
#	P -> not( Is1AffineComplete( G/P[1] ) ) ) then
#    return false;
#  fi;
#  TryNextMethod();
#end
#);


SizeOfCentralizerOfJmodIinG := function(G, J, I )
  local e, c;
    e := NaturalHomomorphismByNormalSubgroup( G, I );
    c := Centralizer( Image(e,G), Image(e,J) );
    return Size(c)*Size(I);
end;

InstallMethod(
	Is1AffineComplete,
	"distributive minimal normal subgroup",
	true,
	[IsGroup],
	70,
  function( G )
  local P, p, a, astar, sizec;
    P := DistributiveMinimalNormalSubgroupsAndComplements( G );
    if P=[] then
	TryNextMethod();
    fi;
    if ForAny( P, p -> not( Is1AffineComplete( G/p[1] ) ) )
	then return false;
    fi;
    p:=P[1];
    a := p[1]; astar := p[2]; 
    sizec := SizeOfCentralizerOfJmodIinG( G, G, astar );
    return (sizec=Size(G) and sizec/Size(astar)=2) or
	   (sizec=Size(astar) and not (IsAbelian( a ) ) );
end
);  

InstallMethod(
	Is1AffineComplete,
	"minimal normal subgroup with 1ac quotient",
	true,
	[IsGroup],
	60,
  function( G )
  local M, Mstar;
    M := Filtered(
	MinimalNormalSubgroups( G ), m -> Is1AffineComplete( G/m ) );
    if M=[] then
	TryNextMethod();
    fi;
    if ForAny( M, m -> not IsAbelian(m) ) then
	return true;
    fi;
    M := M[1];
    Mstar := ClosureSubgroups( G, 
	Filtered( NormalSubgroups(G), I -> not( IsSubset(I,M) ) ) );
    if Size(Mstar)<Size(Centre(G)) and IsSubset( Centre(G), Mstar ) then 
	return false;
    else
    	return Size(PolynomialNearRing(G)) = 
		Size( PolynomialNearRing( G/M ) ) * 
		Size(M)^(Size(G)/Size(Mstar));
    fi;
end 
);

	

    





