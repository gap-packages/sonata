############################################################################
##
#F  GeneralLatticeFamily
##

GeneralLatticeFamily :=
    NewFamily( "GeneralLatticeFamily",
			IsGeneralLattice,
			IsObject,
			IsGeneralLatticeFamily );

GeneralLatticeFamily!.defaultKind :=
	NewType( GeneralLatticeFamily, IsGeneralLatticeDefaultRep );

############################################################################
##
#M  GeneralLattice
##

InstallMethod(
	GeneralLattice,
	"default",
	true,
	[IsCollection, IsOperation, IsString],
	0,
  function ( coll, less, string )
   local L, fam;
	  
    fam := GeneralLatticeFamily;
    L := Objectify( fam!.defaultKind, rec() );
    SetAsSSortedList( L, coll );
    SetAsList( L, coll );
    SetSize( L, Size( coll ) );
    L!.zero := 1;
    L!.one := Length (coll);
    L!.string := string;
    SetLessList( L, Filtered (Combinations ([1..Size(L)], 2),
                           c -> less (coll[c[2]], coll[c[1]])) );
    return L;
  end );
 
############################################################################
##
#M  ViewObj			for GeneralLattices
##

InstallMethod(
	ViewObj,
	"GeneralLattices",
	true,
	[IsGeneralLattice],
	0,
  function ( L )
    View("GeneralLattice( ",Size(L)," ",L!.string,"s )");
  end );

# a lattice is a record that consists of the following components:
# L.zero L.one L.LessList : the ordering, transitive, x less y => not
# y less x.  furthermore, we assume x less y => x < y.  L.elements:
# the elements.  #

############################################################################
##
#M  Less
##

InstallMethod(
	Less,
	"default",
	true,
	[IsGeneralLattice, IsInt, IsInt],
	0,
  function (L, x, y) 
    return [x,y] in LessList(L);
  end );  

############################################################################
##
#M  SubCoverOfJI
##

InstallMethod(
	SubCoverOfJI,
	"default",
	true,
	[IsGeneralLattice, IsInt],
	0,
  function (L, x)
    return First (Reversed ([1..Size(L)]), y -> Less(L, y, x));
  end );

############################################################################
##
#M  Join
##

InstallMethod(
	Join,
	"default",
	true,
	[IsGeneralLattice, IsInt, IsInt],
	0,
  function (L, x, y)
    if x > y then 
      return Join (L,y,x);
    fi; 
    if x = y or Less(L, x, y) then
      return y;
    else
      return First ([1..Size(L)],
                   z -> Less(L, x, z) and Less(L, y, z));
    fi;
  end );

############################################################################
##
#M  Meet
##

InstallMethod(
	Meet,
	"default",
	true,
	[IsGeneralLattice, IsInt, IsInt],
	0,
  function (L, x, y)
    if x > y then return
      Meet (L,y,x);
    fi;
    if x = y or Less(L, x, y) then
      return x;
    else
      return First (Reversed ([1..Size(L)]),
                   z -> Less(L, z, x) and Less(L, z, y));
    fi;
  end );

############################################################################
##
#M  IsJoinIrreducible
##

InstallMethod(
	IsJoinIrreducible,
	"all jis known",
	true,
	[IsGeneralLattice and HasJoinIrreducibles, IsInt],
	0,
  function ( L, i )
    return i in JoinIrreducibles( L );
  end );

InstallMethod(
	IsJoinIrreducible,
	"default",
	true,
	[IsGeneralLattice, IsInt],
	0,
  function ( L, i )
    return (i <> L!.zero) and not ForAny (Combinations(
			Filtered ([1..Size(L)], z -> Less(L, z, i)), 2),
                     c -> (Join (L, c[1], c[2]) = i));
  end );

############################################################################
##
#M  JoinIrreducibles
##

InstallMethod(
	JoinIrreducibles,
	"default",
	true,
	[IsGeneralLattice],
	0,
  function (L)
    return Filtered ([1..Size(L)], 
                       x -> IsJoinIrreducible (L, x));
  end );

############################################################################
##
#M  IsCoveringPair
##

InstallMethod(
	IsCoveringPair,
	"default",
	true,
	[IsGeneralLattice, IsList],
	0,
  function (L, pair)
  local x,y;
    x := pair [1];
    y := pair [2];

    return ( Less (L, x, y)) and not ForAny ([1..Size(L)],
                          z -> Less (L, x, z) and Less (L, z, y));
  end );


############################################################################
##
#M  IsSC1Group
##

InstallMethod(
	IsSC1Group,
	"default",
	true,
	[IsGroup],
	0,
  function ( G )
  local Ns, L, jis, jipairs, issc1group, i, alpha, beta, beta_;

    Ns := NormalSubgroups (G);
    L := GeneralLattice(Ns,IsSubgroup,"normal subgroup");

    jis := JoinIrreducibles (L);
    jipairs := Filtered (Combinations (jis, 2), c -> Less (L, c[1], c[2]));
   
    issc1group := true; 
    i := 1;
    while issc1group and i <= Length (jipairs) do
      alpha := jipairs [i] [2];
      beta  := jipairs [i] [1];
      beta_ := SubCoverOfJI (L, beta);
      issc1group := issc1group and 
                    (not IsSubgroup (Ns[beta_],
                                   CommutatorSubgroup (Ns[beta], Ns[alpha])));
      i := i + 1;
    od;

    return issc1group;
  end );


############################################################################
##
#M  IsProjectivePairOfPairs
##

InstallMethod(
	IsProjectivePairOfPairs,
	"default",
	true,
	[IsGeneralLattice, IsList, IsList],
	0,
  function (L, pair1, pair2)
  local result;
    result := (Join (L, pair2[1], pair1[2]) = pair2[2]) and
              (Meet (L, pair2[1], pair1[2]) = pair1[1]);

    return result;
  end );

############################################################################
##
#M  AlphaBar
##

InstallMethod(
	AlphaBar,
	"SC1 allready tested",
	true,
	[IsGroup and HasIsSC1Group and IsSC1Group],
	0,
  function ( G )

  local NL, CoveringPairs, ClassesList, p, i, j, k,
        waveclasses, number, added, MinimalClass,
        alphaBar, m;

    NL := GeneralLattice(NormalSubgroups(G), IsSubgroup, "normal subgroup");

    CoveringPairs := Filtered (LessList(NL), p -> IsCoveringPair (NL, p));

    ClassesList := []; i := 1;
    for p in CoveringPairs do 
      Add (ClassesList, rec (pair := p, class := i));
      i := i + 1;
    od;

    for i in [1..Length (ClassesList)] do
      for j in [i+1..Length (ClassesList)] do
        if ClassesList [i].class <> ClassesList [j].class and
           IsProjectivePairOfPairs (NL, ClassesList [i].pair,
                                   ClassesList [j].pair) then
           for k in [1..Length(ClassesList)] do
              if ClassesList [k].class = ClassesList [j].class then
                 ClassesList [k].class := ClassesList [i].class;
              fi;
           od;
        fi;
      od;
    od;
#   
#   Collect the join irreducibles.
#
    waveclasses := [];
#    
    for number in [1..Length (ClassesList)] do
      added := Filtered (ClassesList, c -> c.class = number);
      added := List (added, a -> a.pair [2]);
      added := Filtered (added, a -> IsJoinIrreducible (NL, a));
      if Length (added) > 0 then
        Add (waveclasses, added);
      fi;
    od;

    MinimalClass := First (waveclasses,
                         wc1 ->
                         not ForAny (waveclasses,
                                     wc2 -> 
                                     wc2 <> wc1 and
                                     ForAll (wc2,
                                             w2 ->
                                             ForAny (wc1,
                                                     w1 ->
                                                     Less (NL, w2, w1)))));

    alphaBar := TrivialSubgroup (G);
    for m in MinimalClass do
      alphaBar := ClosureSubgroup (alphaBar, NormalSubgroups (G) [m]);
    od;
  
    return alphaBar; 
  end );
                                     


