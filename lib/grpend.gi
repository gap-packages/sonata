# file: endos.gi

                  
##############################################################################
##
#M  Automorphisms ( <G> )
##
## returns a list of all Automorphisms of the group <G>.
##

InstallMethod(
	Automorphisms,
        "default method for Automorphisms",
	true,
	[IsGroup],
	0,
  function ( G )
    return AsList (AutomorphismGroup (G));
  end );

##############################################################################
##
#M  InnerAutomorphisms ( <G> )
##
## returns a list of all InnerAutomorphisms of the group <G>.
##

InstallMethod(
	InnerAutomorphisms,
        "default method for InnerAutomorphisms",
	true,
	[IsGroup],
	0,
  function ( G )
   return List( RepresentativesModNormalSubgroup(G,Centre(G)), 
		g -> InnerAutomorphism( G, g ) );
  end );






## new methods for computing additive generators of 
## homomorphisms/endomorphisms into abelian groups
##							 by pm 12.05.00 






#############################################################################
##
#M  IndependentGeneratorsOfAbelianGroup( <G> )  . . . . . . . nice generators
##
#InstallMethod( IndependentGeneratorsOfAbelianGroup, "for pc group", true,
#        [ IsPcGroup and IsAbelian ], 0,
#    function ( G )
#
#  local gens,	     	# generators of G, not independent
# 	gensorders,  	# orders of the generators
#	qs,		# list of sizes of the p-Sylow subgroups of G
# 	p,		# prime factor of the size of G
#	pgens,		# generators of of the p-Sylow subgroups of G,
#			# not independent
#	indpgens,	# independent generators of a p-Sylow subgroup
#	Gp,		# p-Sylow subgroup by indpgens
#	indgens,	# independent generators of G as union of indpgens
# 	mingens,	# minimal set of independent generators of G
#	n,		# length of mingens
#			# maximal number of cyclic factors of all Gp's 
#	np,
#	g, i, j,  q, x, y;
#
#
#  gens := GeneratorsOfGroup( G );
#  qs := List( Collected( Factors( Size( G ) ) ), x -> x[1]^x[2] );
#  indgens := [];
#
##  split into p-groups
#
#  for q in qs do
#    p := SmallestRootInt( q );
#    pgens := [];
#    for g in gens do
#      if Order( g ) mod p = 0 then
#	i := 2;
#	while Order( g ) mod p^(i) = 0 do
#	  i := i+1;
#	od;
#	Add( pgens, g^QuoInt( Order( g ), p^(i-1) ) );
#      fi;
#    od;	
#    Sort( pgens, function( x, y ) return Order(x) > Order(y); end );
#
#    indpgens := [pgens[1]]; Gp := Group( indpgens ); i := 2;
#    while Size( Gp ) <> q do
#      g := SiftedPcElement( Pcgs( Gp ), pgens[i] );
#      if Order( g ) = Order( pgens[i] ) then       
##  g is independent from Gp
#	Add( indpgens, g );
#        Gp := Group( indpgens );
#      elif Order( g ) > 1 then
##  pgens[i] depends on indpgens, but is not in Gp
##  g is inserted in pgens according to its order	
#	np := Length( pgens );	
#	j := PositionProperty( pgens{[i+1..np]}, x ->
#		 Order(x) = Order(g) );
#	pgens := Concatenation( pgens{[1..j-1]}, [g], pgens{[j..np]} );
#      fi;
#      i := i+1;
#    od;
#    Add( indgens, indpgens );	
#  od; 
# 
## this new generating set can now be minimized
#
#  n := Maximum( List( indgens, Length ) );
#  mingens := [];
#  for i in [1..n] do
#    g := Identity( G ); 
#    for pgens in indgens do
#      if IsBound( pgens[i] ) then		
#	g := g*pgens[i];
#      fi;
#    od;
#    Add( mingens, g );
#  od;
#
#  return mingens;
#end );
#
#


#############################################################################
##
#M  AdditiveGeneratorsForHomomorphisms( <G>, <H> )
##
## returns a list of additive generators for Hom(G,H) with abelian H.
## compare elementary matrices for vectorspaces.
##
InstallMethod( AdditiveGeneratorsForHomomorphisms, "default", true,
        [ IsGroup, IsGroup and IsAbelian ], 0,
    function ( G, H )


  local homos,		# additive generators for Hom(G,H); result
	F, 		# F = G/G' ;  the maximal abelian factor of G
	h, 		# natural homo from G to f
	fomos,		# additive generators for Hom(F,H)
	gens, hens,
	fens, 		# generators for G, H, F
   	gensorders,
	hensorders,	# orders of the generators
	id, 		# identity of H
	idimgs,		# list of images of gens under the homo x -> id
			# from G to H
	imgs,		# list of images of gens under some elementary homo
			# from G to H
	n, m,  d,
   	i, j, f, x;

  if not IsAbelian( G ) then 
## Since H is abelian, homos from G to H are maps from G via G/G', abelian,
## to H. We find the additive generators for Hom( G/G', H ).
    h := NaturalHomomorphismByNormalSubgroupNC( G, DerivedSubgroup( G ) ); 
    F := Image( h, G );
    fomos := AdditiveGeneratorsForHomomorphisms( F, H );
    gens := GeneratorsOfGroup( G );
    fens := List( gens, x -> Image( h, x ) );
    homos := [];
## Now the homos from G to H are compositions from h and fomos
    for f in fomos do
      Add( homos, GroupHomomorphismByImagesNC( G, H, gens,
		 		List( fens, x -> Image( f, x ) ) ) );
    od;
    return homos;
  fi;

  gens := IndependentGeneratorsOfAbelianGroup( G );
  gensorders := List( gens, Order );
  if G = H then
    hens := gens; hensorders := gensorders;
  else
    hens := IndependentGeneratorsOfAbelianGroup( H );
    hensorders := List( hens, Order );
  fi;

  id := Identity( H );
  n := Length( gens ); m := Length( hens );
  idimgs := List( [1..n], x -> id ); 

  homos := [];
  for i in [1..n] do
    for j in [1..m] do
      d := Gcd( gensorders[i], hensorders[j] );
      if d > 1 then
	imgs := ShallowCopy( idimgs );
        imgs[i] := hens[j]^QuoInt( hensorders[j], d ); 
        Add( homos, GroupHomomorphismByImages( G, H, gens, imgs ) );
      fi;
    od;
  od;

  return homos;
end );   






#############################################################################
##
#M  AdditiveGeneratorsForEndomorphisms( <G> )
## 
## returns additive generators of E(G) for abelian G
##

InstallMethod( AdditiveGeneratorsForEndomorphisms, "default", true,
        [ IsGroup and IsAbelian ], 0,
    function ( G )

  return AdditiveGeneratorsForHomomorphisms( G, G );
 end );





#############################################################################
##
  ExtendHomo := function( bs, gens, i, H, As )
##
##  extends partial homomorphisms from Group( <gens> ) to <H> that are
##  defined via the images <bs[j]> on the first <i>-1 elements of <gens> to
##  partial homos on the first <i> generators whenever this is possible.
##  <As> is the correspondig list of automorphism of <H> that stabilize
##  the homos represented by <bs>
##  The extensions and the new stabilizers are returned.

  local n, 		# number of partial homos given by <bs>
	g, 		# gens[i]; generator whose possible images have to
			# be determined 
	o,		# its order 
	G,		# Group( gens{[1..i]} )
	b,		# list of images representing one homo bs[j]
	orbits,		# orbits of the stabilizer As[j] on H
	reps,		# representatives of these orbits
  	repsorders,	# their orders
	newbs, newstabs,# list of new image tupels and the corresponding
			# stabilizers    
	img,		# list of possible images for g
	j, x;

    n := Length( bs ); g := gens[i]; o := Order( g );
    G := Group( gens{[1..i]} );
    newbs := []; newstabs := [];
    for j in [1..n] do
##  for all tuples b of images in bs do
      b := bs[j];
      orbits := Orbits( As[j], H );
      reps := List( orbits, Representative );
      repsorders := List( reps, Order );
      img := reps{Filtered( [1..Length( reps )], x ->
				 o mod repsorders[x] = 0 )};
      img := Filtered( img, x ->
      	GroupHomomorphismByImages( G, H, gens{[1..i]},
					 Concatenation( b, [x] ) ) <> fail );
## Only feasible choices for the image of g remain.
      Append( newbs, List( img, x -> Concatenation( b, [x] ) ) );
      Append( newstabs, List( img, x -> Stabilizer( As[j], x ) ) );
## The stabilizer of the extended homo is found in the old stabilizer of the
## initial homo.
    od;

  return [newbs, newstabs];
end;






#############################################################################
##
#M  NearRingGeneratorsForHomomorphisms( <G>, <H> )
##
##  returns a list of homomorphisms h from G to H and a list
##  of the respective groups of automorphisms A of H which stabilize h
##  i.e., h = h*a for a in A. 
##  ( Homomorphisms operate from the left. )
##  Each homomorphism from G to H has a unique representation h*c for some
##  h and c some coset representative for A*c in Aut( H ).
## 
InstallMethod( NearRingGeneratorsForHomomorphisms, "default", true,
        [ IsGroup, IsGroup ], 0,
    function ( G, H )

  local gens,		# generators of G
	bs,		# list of images of gens under the (partial) 
			# homorphism h
 	As,		# list of stabilizers of the (partial) homorphism h
			# in Aut( H )
 	imgs,		# complete list of images of gens under h
	i, t;

    gens := GeneratorsOfGroup( G );
    As := [AutomorphismGroup( H )];
    bs := [[]];

    for i in [1..Length(gens)] do
##  Let h be defined on the first i-1 generators of G.
##  Now the possible images for the next generator gens[i] are determined.
##  If h can not be extended, then it is discarded.
##  Otherwise, the new stabilizers are for the partial homos defined on
##  the first i generators are determined.
##  Images and stabilizers are returned for the next iteration.  
      t := ExtendHomo( bs, gens, i, H, As );
      bs := t[1]; As := t[2];	
    od;

  return [List( bs, imgs -> GroupHomomorphismByImagesNC( G, H, gens, imgs ) ),
		As];
end );  





#############################################################################
##
#M  Homomorphisms( G, H )
##
## returns a list of all endomorphisms of the group <G> into the group <H>
InstallMethod( Homomorphisms, "default", true, [ IsGroup, IsGroup ], 0,
    function ( G, H )

  local homoreps,	# list of homomorphism representatives as by
			# NearRingGeneratorsForHomomorphisms( G, H )
 	As,		# list of automorphism groups stabilizing the homos
			# in homoreps 
 	A,		# Aut( G )
	homos,		# list of all homomorphisms from G to H, result
	n, t, U, h, i;

    t := NearRingGeneratorsForHomomorphisms( G, H );
    homoreps := t[1]; As := t[2];
    A := AutomorphismGroup( H );
    n := Length( homoreps );
    homos := [];
    for i in [1..n] do
      U := As[i]; h := homoreps[i];
      Append( homos, List( RightTransversal( A, As[i] ), x -> h*x ) );
    od;

  return homos;
end );

## for abelian <H> it is more efficient to use additive generators from
## AdditiveGeneratorsForEndomorphisms( <H> ) and take all sums





#############################################################################
##
#M  Endomorphisms( G )
##
## returns a list of all endomorphisms of the group <G>
InstallMethod( Endomorphisms, "default", true, [ IsGroup], 0,
    function ( G )
	return Homomorphisms( G, G );
end );







