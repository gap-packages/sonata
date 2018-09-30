###########################################################
##
##  nearring ideals and related functions
##  (mainly for function nearrings)
##
##  v0.01 4.12.1996
##
##  Author: Juergen Ecker
##

#****************************************************************************
# ideals.g
#
# computes the ideals of a near-ring.
#
# Author: Erhard Aichinger
#
# Date : 06 June 1995
#
#****************************************************************************

#############################################################################
##
#I  Ideal <=> LeftIdeal & RightIdeal

InstallTrueMethod( IsNearRingIdeal, IsNearRingLeftIdeal and IsNearRingRightIdeal );
InstallTrueMethod( IsNearRingLeftIdeal, IsNearRingIdeal );
InstallTrueMethod( IsNearRingRightIdeal, IsNearRingIdeal );

#############################################################################
##
#M  ViewObj				For NRIdeals

InstallMethod(
	ViewObj,
	"ideal",
	true,
	[IsNearRingIdeal and HasSize],
	1,			# if it's 2-sided write it
  function ( i )
    Print("< nearring ideal of size ", Size(i), " >" );
  end );

InstallMethod(
	ViewObj,
	"ideal",
	true,
	[IsNearRingIdeal],
	1,			# if it's 2-sided write it
  function ( i )
    Print("< nearring ideal >");
  end );

InstallMethod(
	ViewObj,
	"ideal",
	true,
	[IsNearRingLeftIdeal and HasSize],
	0,
  function ( i )
    Print("< nearring left ideal of size ", Size(i), " >" );
  end );

InstallMethod(
	ViewObj,
	"ideal",
	true,
	[IsNearRingLeftIdeal],
	0,
  function ( i )
    Print("< nearring left ideal >" );
  end );

InstallMethod(
	ViewObj,
	"ideal",
	true,
	[IsNearRingRightIdeal and HasSize],
	0,
  function ( i )
    Print("< nearring right ideal of size ", Size(i), " >" );
  end );

InstallMethod(
	ViewObj,
	"ideal",
	true,
	[IsNearRingRightIdeal],
	0,
  function ( i )
    Print("< nearring right ideal >" );
  end );

#############################################################################
##
#M  PrintObj				For NRIdeals

InstallMethod(
	PrintObj,
	"ideal",
	true,
	[IsNearRingIdeal and HasSize],
	1,			# if it's 2-sided write it
  function ( i )
    Print("< nearring ideal of size ", Size(i), " >" );
  end );

InstallMethod(
	PrintObj,
	"ideal",
	true,
	[IsNearRingIdeal],
	1,			# if it's 2-sided write it
  function ( i )
    Print("< nearring ideal >");
  end );

InstallMethod(
	PrintObj,
	"ideal",
	true,
	[IsNearRingLeftIdeal and HasSize],
	0,
  function ( i )
    Print("< nearring left ideal of size ", Size(i), " >" );
  end );

InstallMethod(
	PrintObj,
	"ideal",
	true,
	[IsNearRingLeftIdeal],
	0,
  function ( i )
    Print("< nearring left ideal >" );
  end );

InstallMethod(
	PrintObj,
	"ideal",
	true,
	[IsNearRingRightIdeal and HasSize],
	0,
  function ( i )
    Print("< nearring right ideal of size ", Size(i), " >" );
  end );

InstallMethod(
	PrintObj,
	"ideal",
	true,
	[IsNearRingRightIdeal],
	0,
  function ( i )
    Print("< nearring right ideal >" );
  end );

#############################################################################
##
#M  \=				For nearring ideals

InstallMethod(
	\=,
	"different size",
	IsIdenticalObj,
	[IsNRI, IsNRI],
	100,
  function ( i, j )
    if Size(i) <> Size(j) then
	return false;
    else
	TryNextMethod();
    fi;
  end );

#BindGlobal( "xor", function ( a, b )
#	return ( ( a and ( not b ) ) or ( ( not a ) and b ) );
#end );

InstallMethod(
	\=,
	"left <-> not left",
	IsIdenticalObj,
	[IsNRI and HasIsNearRingLeftIdeal,
		IsNRI and HasIsNearRingLeftIdeal],
	100,
  function ( i, j )
#    if xor( IsNearRingLeftIdeal(i), IsNearRingLeftIdeal(j) )
    if IsNearRingLeftIdeal(i) <> IsNearRingLeftIdeal(j)
		then
		return false;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	\=,
	"right <-> not right",
	IsIdenticalObj,
	[IsNRI and HasIsNearRingRightIdeal,
		IsNRI and HasIsNearRingRightIdeal],
	100,
  function ( i, j )
#    if xor( IsNearRingRightIdeal(i), IsNearRingRightIdeal(j) )
    if IsNearRingRightIdeal(i) <> IsNearRingRightIdeal(j)
	then
		return false;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	\=,
	"ideal <-> not ideal",
	IsIdenticalObj,
	[IsNRI and HasIsNearRingIdeal,
		IsNRI and HasIsNearRingIdeal],
	100,
  function ( i, j )
#    if xor( IsNearRingIdeal(i), IsNearRingIdeal(j) )
    if IsNearRingIdeal(i) <> IsNearRingIdeal(j)
	then
		return false;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	\=,
	"with known additive group",
	IsIdenticalObj,
	[IsNRI , IsNRI ],
	0,
  function ( i , j )
    return AsSSortedList(GroupReduct(i)) = AsSSortedList(GroupReduct(j));
  end );

#############################################################################
##
#M  \<  compare sizes of near-ring ideals (required for AsSSortedList)

InstallMethod(
	\<,
	"compare group reducts",
	IsIdenticalObj,
	[IsNRI , IsNRI ],
	0,
  function ( i , j )
    return GroupReduct(i) < GroupReduct(j);
  end );

#############################################################################
##
#M  AsList			For nearring ideals

InstallMethod(
	AsList,
	"with known additive group",
	true,
	[IsNRI],
	0,
  function ( i )
    return List( GroupReduct(i), x -> NearRingElementByGroupRep( i!.elementsInfo, x ) );
  end );

#############################################################################
##
#M  Size			For nearring ideals

InstallMethod(
	Size,
	"with known additive group",
	true,
	[IsNRI ],
	0,
  function ( i )
    return Size( GroupReduct(i) );
  end );

#############################################################################
##
#M  \in				For nearring ideals

InstallMethod(
	\in,
	"with known additive group",
	IsElmsColls,
	[IsNearRingElement, IsNRI ],
	0,
  function ( x , i )
    return GroupElementRepOfNearRingElement(x) in GroupReduct(i);
  end );

#############################################################################
##
#M  Random			For nearring ideals

InstallMethod(
	Random,
	"with known additive group",
	true,
	[IsNRI ],
	0,
  function ( i )
    return NearRingElementByGroupRep( i!.elementsInfo, Random( GroupReduct(i) ) );
  end );

##########################################################################
## closure algorithms
##########################################################################

#############################################################################
##
#M  NearRingLeftIdealClosureOfSubgroup

NearRingLeftIdealClosureOfSubgroupDefault := function (
			F,
			S
		       )
#*********************************************************
# returns the smallest leftt ideal containing the elements
# of the subgroup S of the nearring F as a normal subgroup
# of the additive group of F
#*********************************************************
#
   local G, L, fam, g, g_add, f, leftProd;

   G := GroupReduct(F);
   L := ShallowCopy(S);
   fam := F!.elementsInfo;

   L := NormalClosure( G, L );
   if Size(L)=Size(G) then return G; fi;

   for g_add in GeneratorsOfGroup(L) do
	g := NearRingElementByGroupRep(fam,g_add);
	for f in G do
	    leftProd := GroupElementRepOfNearRingElement( NearRingElementByGroupRep(fam,f) * g );
	    if not leftProd in L then
		L := NormalClosure( G, ClosureGroup( L, leftProd ));
		if Size(L) = Size(G) then return G; fi;
	    fi;
	od;
   od;

   return L;
end;

InstallMethod(
	NearRingLeftIdealClosureOfSubgroup,
	"generic",
	true,
	[IsNearRing, IsGroup],
	0,
  NearRingLeftIdealClosureOfSubgroupDefault );

#############################################################################
##
#M  NearRingIdealClosureOfSubgroup

TransformationNearRingIdealClosureOfSubgroup := function(
                        F, 
                        S
                       )
#*********************************************************
# returns the smallest ideal containing the elements of the
# subgroup S of the additive group of the nearring F again
# as a subgroup of the additive group of F
#*********************************************************
# 
   local I, fam, a, f, i, generators, G, rightProd, pos, visited,
	 additivelyGeneratingTrafos, distributiveGeneratingTrafos,
	 distributiveGenerators, nonDistributiveGenerators,
	 newGens;
#
   fam := F!.elementsInfo;

   I := NormalClosure( GroupReduct(F), S );
   
#  subgroup is the whole additive group?
   if Size(I)=Size(F) then return GroupReduct(F); fi;

   G := GroupReduct(F);

   additivelyGeneratingTrafos :=
	List( GeneratorsOfGroup( G ), gen -> NearRingElementByGroupRep( fam, gen ) );

   # constant generators need not be tested

   additivelyGeneratingTrafos :=
	Filtered( additivelyGeneratingTrafos,
			tfm -> not IsConstantEndoMapping(tfm) );
   distributiveGeneratingTrafos :=
     Filtered( additivelyGeneratingTrafos, IsGroupHomomorphism );
   
   generators := Set( List( additivelyGeneratingTrafos, GroupElementRepOfNearRingElement ) );
   distributiveGenerators :=
		 Set( List( distributiveGeneratingTrafos, GroupElementRepOfNearRingElement ) );
   nonDistributiveGenerators :=
		 Difference( generators, distributiveGenerators );

   visited := []; pos := 1;
   while pos <= Size(I) do
     i := Enumerator(I)[pos];
     pos := pos + 1;
     newGens := [];
     if not i in visited then
       AddSet( visited, i );

       # case 1: distributive generators

       for a in distributiveGenerators do
	 # rightProd := i * a
	 rightProd := GroupElementRepOfNearRingElement( NearRingElementByGroupRep(fam,i) * NearRingElementByGroupRep(fam,a));
	 if not rightProd in I then
	   AddSet( newGens, rightProd );
	 fi;
       od;

       # case 2: nondistributive generators

       for f in G do
         for a in nonDistributiveGenerators do
	   # rightProd := ( f + i ) * a - f * a
	   rightProd := GroupElementRepOfNearRingElement( NearRingElementByGroupRep(fam,f*i) * a ) / 
		       GroupElementRepOfNearRingElement( NearRingElementByGroupRep(fam,f) * a );
	   if not rightProd in I then
	     AddSet( newGens, rightProd );
	   fi;
	 od;
       od;

       # is there a new element ?

       if newGens <> [] then
	 I := NearRingLeftIdealClosureOfSubgroup( F,
		ClosureGroup( I, SubgroupNC( G, newGens ) ) );
	 pos := 1;				# start anew
	 # the ideal is the whole nearring ?
	 if Size(I)=Size(G) then return G; fi;
       fi;

     fi;
   od;

   return I;
end;

ExpMultNearRingIdealClosureOfSubgroup := function(
                        F, 
                        S
                       )
#*********************************************************
# returns the smallest ideal containing the elements of the
# subgroup S of the additive group of the nearring F again
# as a subgroup of the additive group of F
#*********************************************************
# 
   local I, a, f, i, generators, G, rightProd, pos, visited, mult, newGens;

   mult := NRMultiplication(F);

   I := NormalClosure( GroupReduct(F), S );
   
#  subgroup is the whole additive group?
   if Size(I)=Size(F) then return GroupReduct(F); fi;

   G := GroupReduct(F);
   generators := GeneratorsOfGroup( G );

   visited := []; pos := 1;
   while pos <= Size(I) do
     i := Enumerator(I)[pos];
     pos := pos + 1;
     newGens := [];
     if not i in visited then
       AddSet( visited, i );
       for a in generators do
	 for f in G do
	   rightProd := mult( f*i, a ) / mult( f, a );
	   if not rightProd in I then
	     AddSet( newGens, rightProd );
	   fi;
	 od;
       od;
       if newGens <> [] then
	 I := NearRingLeftIdealClosureOfSubgroup( F, 
			ClosureGroup( I, SubgroupNC( G, newGens ) ) );
	 pos := 1;
	 # the ideal is the whole nearring ?
	 if Size(I)=Size(G) then return G; fi;
       fi;
     fi;
   od;

   return I;
end;

InstallMethod(
	NearRingIdealClosureOfSubgroup,
	"for TfmNRs",
	true,
	[IsTransformationNearRing, IsGroup],
	0,
  TransformationNearRingIdealClosureOfSubgroup );

InstallMethod(
	NearRingIdealClosureOfSubgroup,
	"for ExpMulNRs",
	true,
	[IsExplicitMultiplicationNearRing, IsGroup],
	0,
  ExpMultNearRingIdealClosureOfSubgroup );

# the following method replaces the 2 methods above and is much faster
# probably the 2 methods above will die sooner or later
InstallMethod(
	NearRingIdealClosureOfSubgroup,
	"for TfmNRs",
	true,
	[IsNearRing, IsGroup],
	50,
  function ( nr, group )
    # see Prop. 1.52 in Pilz, NearRings
    return NearRingRightIdealClosureOfSubgroup( nr,
		NearRingLeftIdealClosureOfSubgroup( nr, group ) );
  end );

#############################################################################
##
#M  NearRingRightIdealClosureOfSubgroup

TransformationNearRingRightIdealClosureOfSubgroup := function(
                        F, 
                        S
                       )
#*********************************************************
# returns the smallest right ideal containing the elements
# of the subgroup S of the additive group of the nearring F
# as a normal subgroup of the additive group of F
#*********************************************************
# 
   local I, fam, a, f, i, generators, G, rightProd, pos, visited,
	 additivelyGeneratingTrafos, distributiveGeneratingTrafos,
	 distributiveGenerators, nonDistributiveGenerators, idtfm,
	 newGens;
#
   fam := F!.elementsInfo;

   I := NormalClosure( GroupReduct(F), S );
   
#  subgroup is the whole additive group?
   if Size(I)=Size(F) then return GroupReduct(F); fi;

   G := GroupReduct(F);

   additivelyGeneratingTrafos :=
	List( GeneratorsOfGroup( G ), gen -> NearRingElementByGroupRep( fam, gen ) );

   # constant generators need not be tested

   additivelyGeneratingTrafos :=
	Filtered( additivelyGeneratingTrafos,
			tfm -> not IsConstantEndoMapping(tfm) );
   distributiveGeneratingTrafos :=
     Filtered( additivelyGeneratingTrafos, IsGroupHomomorphism );
   
   generators := Set( List( additivelyGeneratingTrafos, GroupElementRepOfNearRingElement ) );
   distributiveGenerators :=
		 Set( List( distributiveGeneratingTrafos, GroupElementRepOfNearRingElement ) );
   nonDistributiveGenerators :=
		 Difference( generators, distributiveGenerators );

   visited := []; pos := 1;
   while pos <= Size(I) do
     i := Enumerator(I)[pos];
     pos := pos + 1;
     newGens := [];
     if not i in visited then
       AddSet( visited, i );

       # case 1: distributive generators

       for a in distributiveGenerators do
	 # rightProd := i * a
	 rightProd := GroupElementRepOfNearRingElement( NearRingElementByGroupRep(fam,i) * NearRingElementByGroupRep(fam,a) );
	 if not rightProd in I then
	   AddSet( newGens, rightProd );
	 fi;
       od;

       # case 2: nondistributive generators

       for f in G do
         for a in nonDistributiveGenerators do
	   # rightProd := ( f + i ) * a - f * a
	   rightProd := GroupElementRepOfNearRingElement( NearRingElementByGroupRep(fam,f*i) * NearRingElementByGroupRep(fam,a) ) / 
		       GroupElementRepOfNearRingElement( NearRingElementByGroupRep(fam,f) * NearRingElementByGroupRep(fam,a) );
	   if not rightProd in I then
	     AddSet( newGens, rightProd );
	   fi;
	 od;
       od;

       # is there a new element ?

       if newGens <> [] then
	 I := NormalClosure( G, ClosureGroup( I, SubgroupNC( G, newGens ) ) );
	 pos := 1;				# start anew
	 # the ideal is the whole nearring ?
	 if Size(I)=Size(G) then return G; fi;
       fi;

     fi;
   od;

   return I;
end;

ExpMultNearRingRightIdealClosureOfSubgroup := function(
                        F, 
                        S
                       )
#*********************************************************
# returns the smallest right ideal containing the elements of the
# subgroup S of the additive group of the nearring F again
# as a subgroup of the additive group of F
#*********************************************************
# 
   local I, a, f, i, generators, G, rightProd, pos, visited, mult, newGens;

   mult := NRMultiplication(F);

   I := NormalClosure( GroupReduct(F), S );
   
#  subgroup is the whole additive group?
   if Size(I)=Size(F) then return GroupReduct(F); fi;

   G := GroupReduct(F);
   generators := GeneratorsOfGroup( G );

   visited := []; pos := 1;
   while pos <= Size(I) do
     i := Enumerator(I)[pos];
     pos := pos + 1;
     newGens := [];
     if not i in visited then
       AddSet( visited, i );
       for a in generators do
	 for f in G do
	   rightProd := mult( f*i, a ) / mult( f, a );
	   if not rightProd in I then
	     AddSet( newGens, rightProd );
	   fi;
	 od;
       od;
       if newGens <> [] then
	 I := NormalClosure( G, ClosureGroup( I, SubgroupNC( G, newGens ) ) );
	 pos := 1;
	 # the ideal is the whole nearring ?
	 if Size(I)=Size(G) then return G; fi;
       fi;
     fi;
   od;

   return I;
end;

InstallMethod(
	NearRingRightIdealClosureOfSubgroup,
	"for TfmNRs",
	true,
	[IsTransformationNearRing, IsGroup],
	0,
  TransformationNearRingRightIdealClosureOfSubgroup );

InstallMethod(
	NearRingRightIdealClosureOfSubgroup,
	"for ExpMulNRs",
	true,
	[IsExplicitMultiplicationNearRing, IsGroup],
	0,
  ExpMultNearRingRightIdealClosureOfSubgroup );

#############################################################################
##
#F  NRI constructs an object that can be a one or two sided ideal of a
##	nearring

InstallMethod(
	NRI,
	"for EMNRs",
	true,
	[IsNearRing and IsExplicitMultiplicationNearRing],
	0,
  function ( nr )
  local fam, nri;

    fam := nr!.elementsInfo;
    nri := Objectify(
		NewType(CollectionsFamily(fam),
		IsNRIDefaultRep),
		rec() );

    nri!.elementsInfo := fam;

    return nri;
end );

InstallMethod(
	NRI,
	"for TfmNRs",
	true,
	[IsNearRing and IsTransformationNearRing],
	0,
  function ( nr )
  local nri;
    nri := Objectify(
		NewType(CollectionsFamily(EndoMappingFamily(Gamma(nr))),
		IsNRIDefaultRep),
		rec() );

    nri!.elementsInfo := nr!.elementsInfo;

    return nri;
end );

#############################################################################
##
#M  NearRingIdealByGenerators		two sided ideal

InstallMethod(
	NearRingIdealByGenerators,
	"known ideals",
	true,
	[IsNearRing and HasNearRingIdeals, IsList],
	0,
  function ( nr , gens )
  local ideals;
    ideals := Filtered( NearRingIdeals(nr), ideal ->
				ForAll( gens, gen -> gen in ideal ) );

    return Intersection( ideals );
  end );

InstallMethod(
	NearRingIdealByGenerators,
	"two-sided ideal by gens",
	true,		# bad style
	[IsNearRing,IsList],
	0,
  function ( nr , gens )
  local groupgens, subgroup, ideal;

    ideal := NRI( nr );
    SetGeneratorsOfNearRingIdeal( ideal, gens );
    SetParent( ideal, nr );
    SetIsNearRingIdeal( ideal, true );

    return ideal;
  end );

#############################################################################
##
#M  NearRingRightIdealByGenerators		right ideal

InstallMethod(
	NearRingRightIdealByGenerators,
	"known right ideals",
	true,
	[IsNearRing and HasNearRingRightIdeals, IsList],
	0,
  function ( nr , gens )
  local ideals;
    ideals := Filtered( NearRingRightIdeals(nr), ideal ->
				ForAll( gens, gen -> gen in ideal ) );

    return Intersection( ideals );
  end );

InstallMethod(
	NearRingRightIdealByGenerators,
	"right ideal by gens",
	true,		# bad style
	[IsNearRing,IsList],
	0,
  function ( nr , gens )
  local groupgens, subgroup, ideal;

    ideal := NRI( nr );
    SetGeneratorsOfNearRingRightIdeal( ideal, gens );
    SetParent( ideal, nr );
    SetIsNearRingRightIdeal( ideal, true );

    return ideal;
  end );

#############################################################################
##
#M  NearRingLeftIdealByGenerators		right ideal

InstallMethod(
	NearRingLeftIdealByGenerators,
	"known left ideals",
	true,
	[IsNearRing and HasNearRingLeftIdeals, IsList],
	0,
  function ( nr , gens )
  local ideals;
    ideals := Filtered( NearRingLeftIdeals(nr), ideal ->
				ForAll( gens, gen -> gen in ideal ) );

    return Intersection( ideals );
  end );

InstallMethod(
	NearRingLeftIdealByGenerators,
	"left ideal by gens",
	true,		# bad style
	[IsNearRing,IsList],
	0,
  function ( nr , gens )
  local groupgens, subgroup, ideal;

    ideal := NRI( nr );
    SetGeneratorsOfNearRingLeftIdeal( ideal, gens );
    SetParent( ideal, nr );
    SetIsNearRingLeftIdeal( ideal, true );

    return ideal;
  end );

###########################################
##
## testing subgroups of the additive group
##

###########################################

#############################################################################
##
#M  IsSubgroupNearRingRightIdeal

IsSubgroupTransformationNearRingRightIdeal := function(
                        F, 
                        R
                       )
#*********************************************************
# returns true iff the normal subgroup R of F is a right
# ideal of F.
#*********************************************************

   local a, a_add, f, isIdeal, lengthAddGs, fam, actualGen, generators;

   if not IsSubgroup(GroupReduct(F),R) then
	Error("<R> has to be a subgroup of the additive group of <F>");
   fi;

   if not IsNormal(GroupReduct(F),R) then
	return false;
   fi;

   isIdeal := true;
   generators := GeneratorsOfGroup (GroupReduct(F));
   lengthAddGs := Length (generators);
   fam := F!.elementsInfo;

   actualGen := 0; 
   while isIdeal and (actualGen < lengthAddGs) do
#                            at this place, the condition is ok
#                            for all generators up to actualGen.
      actualGen := actualGen + 1;
      a_add := generators [actualGen];
      a := NearRingElementByGroupRep(fam, a_add);
      if IsGroupHomomorphism (a) then
         if IsIdentityMapping (a) then
#                            since R is a normal subgroup,
#                            we do already know that (f + R - f) \subseteq R
            isIdeal := true; 
         else
            isIdeal := ForAll (R,
                       r -> (GroupElementRepOfNearRingElement( NearRingElementByGroupRep(fam, r) * a ) in R)
                            );
         fi;
      elif IsConstantEndoMapping(a) then  # a-a=0 is the always in R
         isIdeal := true;
      else
         isIdeal := ForAll (F, f -> ForAll (R, r ->
				( GroupElementRepOfNearRingElement((f+NearRingElementByGroupRep(fam,r))*a)
				/
				(GroupElementRepOfNearRingElement(f*a)) ) in R
			));
      fi;
   od;

   return isIdeal;
end;

IsSubgroupNearRingRightIdealDefault := function(
                        F, 
                        R
                       )
#*********************************************************
# returns true iff the normal subgroup R of F is a right
# ideal of F.
#*********************************************************

   local a, a_add, f, l, isIdeal, lengthAddGs, fam, actualGen, generators;

   if not IsSubgroup(GroupReduct(F),R) then
	Error("<R> has to be a subgroup of the additive group of <F>");
   fi;

   if not IsNormal(GroupReduct(F),R) then
	return false;
   fi;

   isIdeal := true;
   generators := GeneratorsOfGroup (GroupReduct(F));
   lengthAddGs := Length (generators);
   fam := F!.elementsInfo;

   actualGen := 0; 
   while isIdeal and (actualGen < lengthAddGs) do
#                            at this place, the condition is ok
#                            for all generators up to actualGen.
      actualGen := actualGen + 1;

      a_add := generators [actualGen];
      a := NearRingElementByGroupRep(fam, a_add);

         isIdeal := ForAll (F,
                       f -> ForAll (R,
                            r -> ( GroupElementRepOfNearRingElement((f+NearRingElementByGroupRep(fam,r))*a)
					/
				   (GroupElementRepOfNearRingElement(f*a)) ) in R
			));
   od;

   return isIdeal;
end;

InstallMethod(
	IsSubgroupNearRingRightIdeal,
	"for NearRings",
	true,
	[IsNearRing, IsGroup],
	0,
  IsSubgroupNearRingRightIdealDefault );

InstallMethod(
	IsSubgroupNearRingRightIdeal,
	"for transformation nearrings",
	true,
	[IsTransformationNearRing, IsGroup],
	0,
  IsSubgroupTransformationNearRingRightIdeal );

InstallMethod(
	IsSubgroupNearRingRightIdeal,
	"ExpMulNR",
	true,
	[IsExplicitMultiplicationNearRing, IsGroup],
	0,
  function ( N, S )
  local addGroup, mul;
    addGroup := GroupReduct(N);

    if not IsNormal(addGroup,S) then
	return false;
    fi;

    mul := NRMultiplication(N);

    return ForAll( GeneratorsOfGroup(addGroup), m ->
	   ForAll( addGroup, n ->
	   ForAll( S, s ->
			( mul( n * s, m ) / mul( n, m ) ) in S ) ) );

  end );

#############################################################################
##
#M  IsSubgroupNearRingLeftIdeal

IsSubgroupNearRingLeftIdealDefault := function (
                          F,         # near-ring
                          L
                         )
#**************************************************
# true iff the subgroup L is a left ideal of F
#*************************************************

  local generatorsL, fam;

   if not IsSubgroup(GroupReduct(F),L) then
	Error("<L> has to be a subgroup of the additive group of <F>");
   fi;

   if not IsNormal(GroupReduct(F),L) then
	return false;
   fi;

   fam := F!.elementsInfo;
   generatorsL := GeneratorsOfGroup (L);

   return (ForAll (F,
              f -> ForAll (generatorsL,
                      l -> (GroupElementRepOfNearRingElement( f * NearRingElementByGroupRep(fam,l) ) in L)
                          )
                  )
          );
end;

InstallMethod(
	IsSubgroupNearRingLeftIdeal,
	"for transformation nearrings",
	true,
	[IsNearRing, IsGroup],
	0,
  IsSubgroupNearRingLeftIdealDefault );

InstallMethod(
	IsSubgroupNearRingLeftIdeal,
	"ExpMulNR",
	true,
	[IsExplicitMultiplicationNearRing, IsGroup],
	0,
  function ( N, S )
  local addGroup, mul;
    addGroup := GroupReduct(N);

    if not IsSubgroup(GroupReduct(N),S) then
	Error("<S> has to be a subgroup of the additive group of <N>");
    fi;

    if not IsNormal(addGroup,S) then
	return false;
    fi;

    mul := NRMultiplication(N);

    return ForAll( addGroup, n -> ForAll( S, s ->
             mul( n, s ) in S ) );
  end );

#############################################################################
##
#M  IsNearRingRightIdeal		for NR left ideals

InstallMethod(
	IsNearRingRightIdeal,
	"NR left ideals",
	true,
	[IsNRI and HasIsNearRingLeftIdeal],
	0,
  function ( l )
  local addGroup;
    addGroup := GroupReduct(l);

    return IsSubgroupNearRingRightIdeal( Parent(l), addGroup );
  end );

#############################################################################
##
#M  IsNearRingLeftIdeal		for NR right ideals

InstallMethod(
	IsNearRingLeftIdeal,
	"NR right ideals",
	true,
	[IsNRI and HasIsNearRingRightIdeal],
	0,
  function ( r )
  local addGroup;
    addGroup := GroupReduct(r);

    return IsSubgroupNearRingLeftIdeal( Parent(r), addGroup );
  end );

#############################################################################
##
#M  IsNearRingIdeal		for NR left and right ideals

InstallMethod(
	IsNearRingIdeal,
	"NR right ideals",
	true,
	[IsNRI and HasIsNearRingRightIdeal and
		IsNearRingRightIdeal],
	0,
  function ( r )
    return IsNearRingLeftIdeal( r );
  end );

InstallMethod(
	IsNearRingIdeal,
	"NR left ideals",
	true,
	[IsNRI and HasIsNearRingLeftIdeal and
		IsNearRingLeftIdeal],
	0,
  function ( l )
    return IsNearRingRightIdeal( l );
  end );

#############################################################################
##
#M  GroupReduct		For left near ring ideals

InstallMethod(
	GroupReduct,
	"for left near ring ideals",
	true,
	[IsNRI and HasGeneratorsOfNearRingLeftIdeal],
	2,
  function ( I )
  local subgroup, parent;
    parent := Parent(I);
    subgroup := SubgroupNC( GroupReduct(parent),
			List(GeneratorsOfNearRingLeftIdeal(I),GroupElementRepOfNearRingElement) );
    subgroup := NearRingLeftIdealClosureOfSubgroup( parent, subgroup );

    return subgroup;

  end );

#############################################################################
##
#M  GroupReduct		For right near ring ideals

InstallMethod(
	GroupReduct,
	"for right near ring ideals",
	true,
	[IsNRI and HasGeneratorsOfNearRingRightIdeal],
	1,
  function ( I )
  local subgroup, parent;
    parent := Parent(I);
    subgroup := SubgroupNC( GroupReduct(parent),
			List(GeneratorsOfNearRingRightIdeal(I),GroupElementRepOfNearRingElement) );
    subgroup := NearRingRightIdealClosureOfSubgroup( parent, subgroup );

    return subgroup;

  end );

#############################################################################
##
#M  GroupReduct		For near ring ideals

InstallMethod(
	GroupReduct,
	"for near ring ideals",
	true,
	[IsNRI and HasGeneratorsOfNearRingIdeal],
	0,
  function ( I )
  local subgroup, parent;
    parent := Parent(I);
    subgroup := SubgroupNC( GroupReduct(parent),
			List(GeneratorsOfNearRingIdeal(I),GroupElementRepOfNearRingElement) );
    subgroup := NearRingIdealClosureOfSubgroup( parent, subgroup );

    return subgroup;

  end );

###########
# subnearrings

#############################################################################
##
#M  SubNearRing			by generators

InstallMethod(
	SubNearRing,
	"TfmNRs",
	true,
	[IsTransformationNearRing, IsCollection],
	0,
  function ( nr , gens )
  local SubNR;
    if not ForAll(gens, g -> g in nr) then
	Error("generators have to be from the near ring");
    fi;
    SubNR := TransformationNearRingByGenerators(Gamma(nr),gens);
    SetParent( SubNR, nr );

    return SubNR;
  end );
    
InstallMethod(
	SubNearRing,
	"ExplicitMultiplicationNearRings",
	true,
	[IsExplicitMultiplicationNearRing, IsCollection],
	0,
  function ( nr , gens )
  local SubNR;
    Error("not yet implemented");
  end );

#############################################################################
##
#M  AsSubNearRing

InstallMethod(
	AsSubNearRing,
	"for ideals",
	true,
	[IsNearRing, IsNRI],
	0,
  function ( nr, i )
  local subNR, parent, addGroup, fam;
    addGroup := GroupReduct(i);
    fam := i!.elementsInfo;
    subNR := SubNearRing( nr,
		List(GeneratorsOfGroup(addGroup), g -> NearRingElementByGroupRep(fam, g) ) );
    SetGroupReduct( subNR, addGroup );

    return subNR;
  end );

#############################################################################
##
#F  NearRingIdealBySubgroupNC( <nr>, <group> )	constructs an ideal of the
##						nearring <nr> the additive
##						group of which is <group>
##
##		No Check!

NearRingIdealBySubgroupNC := function ( nr, group )
local fam, gens, ideal;
  fam := nr!.elementsInfo;
  gens := List( GeneratorsOfGroup(group), gen -> NearRingElementByGroupRep(fam, gen) );
  ideal := NRI( nr );
  SetGeneratorsOfNearRingIdeal( ideal, gens );
  SetParent( ideal, nr );
  SetIsNearRingIdeal( ideal, true );
  SetGroupReduct( ideal, group );

  return ideal;
end;

#############################################################################
##
#F  NearRingLeftIdealBySubgroupNC( <nr>, <group> )	constructs a left
##						ideal of the nearring <nr>
##						the additive group of which is
##						<group>
##		No Check!

NearRingLeftIdealBySubgroupNC := function ( nr, group )
local fam, gens, ideal;
  fam := nr!.elementsInfo;
  gens := List( GeneratorsOfGroup(group), gen -> NearRingElementByGroupRep(fam, gen) );
  ideal := NRI( nr );
  SetGeneratorsOfNearRingLeftIdeal( ideal, gens );
  SetParent( ideal, nr );
  SetIsNearRingLeftIdeal( ideal, true );
  SetGroupReduct( ideal, group );

  return ideal;
end;

#############################################################################
##
#F  NearRingRightIdealBySubgroupNC( <nr>, <group> )	constructs a right
##						ideal of the nearring <nr>
##						the additive group of which is
##						<group>
##		No Check!

NearRingRightIdealBySubgroupNC := function ( nr, group )
local fam, gens, ideal;
  fam := nr!.elementsInfo;
  gens := List( GeneratorsOfGroup(group), gen -> NearRingElementByGroupRep(fam, gen) );
  ideal := NRI( nr );
  SetGeneratorsOfNearRingRightIdeal( ideal, gens );
  SetParent( ideal, nr );
  SetIsNearRingRightIdeal( ideal, true );
  SetGroupReduct( ideal, group );

  return ideal;
end;

#############################################################################
##
#M  NearRingLeftIdeals( <nr> )	computes a list of all left ideals of <nr>
##

InstallMethod(
	NearRingLeftIdeals,
	"filter normal subgroups",
	true,
	[IsNearRing],
	5,
  function ( nr )
    return List(
	Filtered( NormalSubgroups( GroupReduct(nr) ),
			s -> IsSubgroupNearRingLeftIdeal( nr,s ) ),
	sg -> NearRingLeftIdealBySubgroupNC( nr, sg )
		);
  end );

#############################################################################
##
#M  NearRingRightIdeals( <nr> )	computes a list of all right ideals of <nr>
##

InstallMethod(
	NearRingRightIdeals,
	"filter normal subgroups",
	true,
	[IsNearRing],
	5,
  function ( nr )
    return List(
	Filtered( NormalSubgroups( GroupReduct(nr) ),
			s -> IsSubgroupNearRingRightIdeal( nr,s ) ),
	sg -> NearRingRightIdealBySubgroupNC( nr, sg )
		);
  end );

#############################################################################
##
#M  NearRingIdeals( <nr> )		computes a list of all ideals of <nr>
##

InstallImmediateMethod(
	NearRingIdeals,
	IsSimpleNearRing,
	0,
  function ( nr )
  local addGroup, ideals;
    addGroup := GroupReduct( nr );
    ideals := [ TrivialSubgroup( addGroup ), addGroup ];

    return List( ideals, id -> NearRingIdealBySubgroupNC( nr, id ) );
  end );

InstallMethod(
	NearRingIdeals,
	"known left ideals",
	true,
	[IsNearRing and HasNearRingLeftIdeals],
	100,
  function ( nr )
    return Filtered( NearRingLeftIdeals(nr),
			l -> IsNearRingIdeal(l) );
  end );

InstallMethod(
	NearRingIdeals,
	"known right ideals",
	true,
	[IsNearRing and HasNearRingRightIdeals],
	100,
  function ( nr )
    return Filtered( NearRingRightIdeals(nr),
			l -> IsNearRingIdeal(l) );
  end );

InstallMethod(
	NearRingIdeals,
	"filter normal subgroups",
	true,
	[IsNearRing],
	5,
  function ( nr )
    return List(
	Filtered( NormalSubgroups( GroupReduct(nr) ),
			s -> IsSubgroupNearRingLeftIdeal( nr,s ) and
				IsSubgroupNearRingRightIdeal( nr,s ) ),
	sg -> NearRingIdealBySubgroupNC( nr, sg )
		);
  end );

##### faster ######

#################################################################
##
#F  NearRingLeftIdeals ( <R> ) ... compute a list of all left ideals
##

InstallMethod(
	NearRingLeftIdeals,
	"from the subgroup lattice",
	true,
	[IsNearRing],
	2,		# slightly better than the original one
  function ( NR )
    local addGroup, lattice, upInclusions, downInclusions, CantorList,
        cl, sum, NumberOfSubgroups, idealLattice,
	vertex, idealInclusions, candidate,
	maxsubgrlist, vertexset, LCCS, ideals;

    addGroup := GroupReduct(NR);
    lattice := LatticeSubgroups( addGroup );
    LCCS := lattice!.conjugacyClassesSubgroups;

    # [i,j] = AsList( LCCS[i] ) [j]

    upInclusions := MinimalSupergroupsLattice(lattice);
    downInclusions := MaximalSubgroupsLattice(lattice);

    # the CantorList represents a bijection between [i,j] and
    # the place of [i,j] in the lists up/downInclusions

    CantorList := [0]; sum := 0;
    for cl in LCCS do
        sum := sum + Size(cl);
        Add(CantorList,sum);
    od;
    NumberOfSubgroups := CantorList[Length(CantorList)];

    idealLattice := [true];                    # (0) is an ideal
    idealLattice[NumberOfSubgroups] := true;   #  NR is an ideal
    ideals := [ TrivialSubgroup(addGroup), addGroup ];

    for vertexset in upInclusions do
     for vertex in vertexset do

      if Size( LCCS [vertex[1]] ) = 1 and	# normal subgroup
	 not IsBound(idealLattice[CantorList[vertex[1]]+vertex[2]]) then

        candidate := AsList( LCCS [vertex[1]] ) [vertex[2]];
	maxsubgrlist := downInclusions[vertex[1]];

	if Length(Filtered(maxsubgrlist,
		x -> idealLattice[CantorList[x[1]]+x[2]])) > 1 then

	# if at least 2 max subgroups are ideals, we have an ideal

	     idealLattice[CantorList[vertex[1]]+vertex[2]] := true;
	     Add( ideals, candidate );

	else

	# otherwise we have to check

	     idealLattice[CantorList[vertex[1]]+vertex[2]] :=
		IsSubgroupNearRingLeftIdeal( NR, candidate );
	     if idealLattice[CantorList[vertex[1]]+vertex[2]] then
		Add( ideals, candidate );
	     fi;

	fi;
      fi;
     od;
    od;

    # transform subgroups into ideals
    ideals := List( ideals, I -> NearRingLeftIdealBySubgroupNC( NR, I ) );
    return ideals;
  end );

#################################################################
##
#F  NearRingRightIdeals ( <R> ) ... compute a list of all right ideals
##

InstallMethod(
	NearRingRightIdeals,
	"from the subgroup lattice",
	true,
	[IsNearRing],
	2,		# slightly better than the original one
  function ( NR )
    local addGroup, lattice, upInclusions, downInclusions, CantorList,
        cl, sum, NumberOfSubgroups, idealLattice,
	vertex, idealInclusions, candidate,
	maxsubgrlist, vertexset, LCCS, ideals;

    addGroup := GroupReduct(NR);
    lattice := LatticeSubgroups( addGroup );
    LCCS := lattice!.conjugacyClassesSubgroups;

    # [i,j] = AsList( LCCS[i] ) [j]

    upInclusions := MinimalSupergroupsLattice(lattice);
    downInclusions := MaximalSubgroupsLattice(lattice);

    # the CantorList represents a bijection between [i,j] and
    # the place of [i,j] in the lists up/downInclusions

    CantorList := [0]; sum := 0;
    for cl in LCCS do
        sum := sum + Size(cl);
        Add(CantorList,sum);
    od;
    NumberOfSubgroups := CantorList[Length(CantorList)];

    idealLattice := [true];                    # (0) is an ideal
    idealLattice[NumberOfSubgroups] := true;   #  NR is an ideal
    ideals := [ TrivialSubgroup(addGroup), addGroup ];

    for vertexset in upInclusions do
     for vertex in vertexset do

      if Size( LCCS [vertex[1]] ) = 1 and	# normal subgroup
	 not IsBound(idealLattice[CantorList[vertex[1]]+vertex[2]]) then

        candidate := AsList( LCCS [vertex[1]] ) [vertex[2]];
	maxsubgrlist := downInclusions[vertex[1]];

	if Length(Filtered(maxsubgrlist,
		x -> idealLattice[CantorList[x[1]]+x[2]])) > 1 then

	# if at least 2 max subgroups are ideals, we have an ideal

	     idealLattice[CantorList[vertex[1]]+vertex[2]] := true;
	     Add( ideals, candidate );

	else

	# otherwise we have to check

	     idealLattice[CantorList[vertex[1]]+vertex[2]] :=
		IsSubgroupNearRingRightIdeal( NR, candidate );
	     if idealLattice[CantorList[vertex[1]]+vertex[2]] then
		Add( ideals, candidate );
	     fi;

	fi;
      fi;
     od;
    od;

    # transform subgroups into right ideals
    ideals := List( ideals, I -> NearRingRightIdealBySubgroupNC( NR, I ) );

    # lattice info in idealLattice
    return ideals;
  end );

#################################################################
##
#F  NearRingIdeals ( <R> ) ... compute a list of all ideals
##

InstallMethod(
	NearRingIdeals,
	"from the subgroup lattice",
	true,
	[IsNearRing],
	2,		# slightly better than the original one
  function ( NR )
    local addGroup, lattice, upInclusions, downInclusions, CantorList,
        cl, sum, NumberOfSubgroups, idealLattice,
	vertex, idealInclusions, candidate,
	maxsubgrlist, vertexset, LCCS, ideals;

    addGroup := GroupReduct(NR);
    lattice := LatticeSubgroups( addGroup );
    LCCS := lattice!.conjugacyClassesSubgroups;

    # [i,j] = AsList( LCCS[i] ) [j]

    upInclusions := MinimalSupergroupsLattice(lattice);
    downInclusions := MaximalSubgroupsLattice(lattice);

    # the CantorList represents a bijection between [i,j] and
    # the place of [i,j] in the lists up/downInclusions

    CantorList := [0]; sum := 0;
    for cl in LCCS do
        sum := sum + Size(cl);
        Add(CantorList,sum);
    od;
    NumberOfSubgroups := CantorList[Length(CantorList)];

    idealLattice := [true];                    # (0) is an ideal
    idealLattice[NumberOfSubgroups] := true;   #  NR is an ideal
    ideals := [ TrivialSubgroup(addGroup), addGroup ];

    for vertexset in upInclusions do
     for vertex in vertexset do

      if Size( LCCS [vertex[1]] ) = 1 and	# normal subgroup
	 not IsBound(idealLattice[CantorList[vertex[1]]+vertex[2]]) then

        candidate := AsList( LCCS [vertex[1]] ) [vertex[2]];
	maxsubgrlist := downInclusions[vertex[1]];

	if Length(Filtered(maxsubgrlist,
		x -> idealLattice[CantorList[x[1]]+x[2]])) > 1 then

	# if at least 2 max subgroups are ideals, we have an ideal

	     idealLattice[CantorList[vertex[1]]+vertex[2]] := true;
	     Add( ideals, candidate );

	else

	# otherwise we have to check

	     idealLattice[CantorList[vertex[1]]+vertex[2]] :=
		IsSubgroupNearRingLeftIdeal( NR, candidate ) and
		IsSubgroupNearRingRightIdeal( NR, candidate );
	     if idealLattice[CantorList[vertex[1]]+vertex[2]] then
		Add( ideals, candidate );
	     fi;

	fi;
      fi;
     od;
    od;

    # transform subgroups into ideals
    ideals := List( ideals, I -> NearRingIdealBySubgroupNC( NR, I ) );

    # lattice info in idealLattice
    return ideals;
  end );

#### end: faster #####


#############################################################################
##
#M  Enumerator						For FNRs

InstallMethod(
	Enumerator,
	true,
	[ IsNRI ],
	0,
  function( nri )
    return Objectify( NewType( FamilyObj( nri ),
			IsList and IsNearRingIdealEnumerator ),
           rec( additiveGroup := GroupReduct(nri),
		elementsInfo := nri!.elementsInfo ) );
  end );

InstallMethod(
	Length,
	true,
	[ IsList and IsNearRingIdealEnumerator ],
	1,
    e -> Size( e!.additiveGroup ) );

InstallMethod(
	\[\],
	true,
	[ IsList and IsNearRingIdealEnumerator, IsPosInt ],
	1,
  function( e, pos )
  local groupElement, group, mult, fam;
    group := e!.additiveGroup;
    fam := e!.elementsInfo;

    groupElement := Enumerator(group)[pos];

    return NearRingElementByGroupRep( fam, groupElement );
  end );

InstallMethod(
	Position,
	true,
	[ IsList and IsNearRingIdealEnumerator,
		IsNearRingElement, IsZeroCyc ],
	1,
  function( e, emnrelm, zero )
    return Position(Enumerator(e!.additiveGroup),emnrelm![1],zero);
  end );

InstallMethod(
	ViewObj,
	true,
	[ IsNearRingIdealEnumerator ],
	1,
  function( G )
    Print( "<enumerator of near-ring ideal>" );
  end );

InstallMethod(
	PrintObj,
	true,
	[ IsNearRingIdealEnumerator ],
	1,
  function( G )
    Print( "<enumerator of near-ring ideal>" );
  end );

#############################################################################
##
#M  IsPrimeNearRingIdeal			For NearRing ideals

InstallMethod(
	IsPrimeNearRingIdeal,
	"brute force method",
	true,
	[IsNRI and IsNearRingIdeal ],
	0,
  function ( ideal )
    
  local ideal_list, size, N;

  size := Size( ideal );
  N := Parent( ideal );
  ideal_list := Filtered( AsSSortedList( NearRingIdeals( N ) ), 
    id -> Size( id ) >= size and id <> ideal );

  return
  ForAll( ideal_list, I1 -> 
    ForAll( ideal_list, I2 ->
      ForAny( I1, e1 -> 
        ForAny( I2, e2 -> 
          not ( e1 * e2 in ideal )  ) ) ) );

  end );

#############################################################################
##
#M  IsMaximalNearRingIdeal			For nearring ideals

InstallMethod(
	IsMaximalNearRingIdeal,
	"simple factor?",
	true,
	[IsNRI and IsNearRingIdeal ],
	20,
  function ( ideal )
    if Size(ideal)=0 or Size(ideal)=Size(Parent(ideal)) then
	return false;
    else
	TryNextMethod();
    fi;
  end );

InstallMethod(
	IsMaximalNearRingIdeal,
	"simple factor?",
	true,
	[IsNRI and IsNearRingIdeal ],
	10,
  function ( ideal )
    return IsSimpleNearRing ( FactorNearRing( Parent( ideal ), ideal ) );
  end );

InstallMethod(
	IsMaximalNearRingIdeal,
	"brute force method",
	true,
	[IsNRI and IsNearRingIdeal ],
	0,
  function ( ideal )

  local ideal_list, size, N;

  size := Size( ideal );
  N := Parent( ideal );
  ideal_list := AsSSortedList( NearRingIdeals( N ) );

  return
  not ForAny( ideal_list, id -> Size( id ) > size and Size( id ) < Size( N ) and
        ForAll( ideal, e -> e in id ) );

  end );

#############################################################################
##
#M  NoetherianQuotient2( <NR>, <L>, <SubNR> )
##
#
#InstallMethod(
#	NoetherianQuotient2,
#	"left ideals",
#	true,
#	[IsNearRing, IsNRI and IsNearRingLeftIdeal, IsNearRing],
#	0,
#  function ( NR, L, SubNR )
#  local nq;
#    nq := Filtered( NR, n -> ForAll( SubNR, s -> n*s in L ) );  
#    nq := Subgroup( GroupReduct(NR), List( nq, GroupElementRepOfNearRingElement ) );
#
#    return NearRingIdealBySubgroupNC( NR, nq );
#  end );

#############################################################################
##
#M  IsSubset
##

InstallMethod(
	IsSubset,
	"two NRIs second has right generators",
	IsIdenticalObj,
	[IsNRI and IsNearRingIdeal,
		IsNRI and HasGeneratorsOfNearRingRightIdeal],
	0,
  function ( I, S )
    return ForAll( GeneratorsOfNearRingLeftIdeal( S ), gen -> gen in I );
  end );

InstallMethod(
	IsSubset,
	"two NRIs second has right generators",
	IsIdenticalObj,
	[IsNRI and IsNearRingRightIdeal,
		IsNRI and HasGeneratorsOfNearRingRightIdeal],
	0,
  function ( I, S )
    return ForAll( GeneratorsOfNearRingRightIdeal( S ), gen -> gen in I );
  end );

InstallMethod(
	IsSubset,
	"two NRIs second has left generators",
	IsIdenticalObj,
	[IsNRI and IsNearRingIdeal,
		IsNRI and HasGeneratorsOfNearRingLeftIdeal],
	0,
  function ( I, S )
    return ForAll( GeneratorsOfNearRingLeftIdeal( S ), gen -> gen in I );
  end );

InstallMethod(
	IsSubset,
	"two NRIs second has left generators",
	IsIdenticalObj,
	[IsNRI and IsNearRingLeftIdeal,
		IsNRI and HasGeneratorsOfNearRingLeftIdeal],
	0,
  function ( I, S )
    return ForAll( GeneratorsOfNearRingLeftIdeal( S ), gen -> gen in I );
  end );

InstallMethod(
	IsSubset,
	"two NRIs second has ideal generators",
	IsIdenticalObj,
	[IsNRI and IsNearRingIdeal,
		IsNRI and HasGeneratorsOfNearRingIdeal],
	0,
  function ( I, S )
    return ForAll( GeneratorsOfNearRingIdeal( S ), gen -> gen in I );
  end );

#############################################################################
##
#M  GeneratorsOfNearRingIdeal

InstallMethod(
	GeneratorsOfNearRingIdeal,
	"default",
	true,
	[IsNRI and IsNearRingIdeal],
	0,
  function( I )
    return List( GeneratorsOfGroup( GroupReduct( I ) ),
			g -> AsNearRingElement( Parent(I), g ) );
end );

#############################################################################
##
#M  GeneratorsOfNearRingLeftIdeal

InstallMethod(
	GeneratorsOfNearRingLeftIdeal,
	"default",
	true,
	[IsNRI and IsNearRingLeftIdeal],
	0,
  function( I )
    return List( GeneratorsOfGroup( GroupReduct( I ) ),
			g -> AsNearRingElement( Parent(I), g ) );
end );

#############################################################################
##
#M  GeneratorsOfNearRingRightIdeal

InstallMethod(
	GeneratorsOfNearRingRightIdeal,
	"default",
	true,
	[IsNRI and IsNearRingRightIdeal],
	0,
  function( I )
    return List( GeneratorsOfGroup( GroupReduct( I ) ),
			g -> AsNearRingElement( Parent(I), g ) );
end );

#############################################################################
##
#M  AdditiveGenerators			for NRI's

InstallMethod(
	AdditiveGenerators,
	"NRI",
	true,
	[IsNRI],
	0,
  function( I )
  local fam;
    fam := Parent( I )!.elementsInfo;
    return List( GeneratorsOfGroup( GroupReduct( I ) ),
			g -> NearRingElementByGroupRep( fam, g ) );
  end );




