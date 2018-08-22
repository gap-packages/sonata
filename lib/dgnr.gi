#############################################################################
##  File: DGNR.G                                                           ##
#############################################################################
##
#############################################################################
##  Provide some GAP4 functions for function nearrings.
##  A nearring of functions that operates on G is represented as 
##  subgroup of the additive group of (M(G),+). 
##  To the outside world, the elements of the nearring appear
##  as transformations.
#############################################################################




#############################################################################
##
#F IntRepOfFunctionOnG ( <G>, <f> )
##
##   returns the internal representation of f as subelement of G^G.
##

BindGlobal( "IntRepOfFunctionOnG",
  function (G, f)
#
   local Glist, gtog;
#
   gtog := SizeFoldDirectProduct( G );

   Glist := AsSSortedList (G);
   return Product( [1..Length (Glist)],
                   i -> Image ( Embedding ( gtog, i ),
                                f ( Glist [i] )));
end );


#############################################################################
##
#F UAtThePlacesS ( <G>, <U>, <S> )
##
##    U has to be a subgroup of some group G, GtoG has to be G^|G| and
##    S has to be a subset of G.
##
##    returns a subgroup T of GtoG
##    with the property
##    that the set
##       { GroupGeneralMappingByGroupElement (G, t) | t in T }
##    is equal to the set 
##       { f : G -> G  |  f(S) \subseteq U }.
##    Intuitively, this means that the set G x G x U x G x U ...
##    is returned, where the U's stand at the places of S.
##

BindGlobal( "UAtThePlacesS",
  function ( G, U, S )

  local UAtS,             # the subgroup of G^|G| to be constructed
        g,                # runs through the elements of G
        i,
        actualComponent,  # the component belonging to the i-th element
        gtog;      
  
  gtog := SizeFoldDirectProduct( G );

#
  UAtS := TrivialSubgroup (gtog);
#
  i := 0;
  for g in AsSSortedList (G) do
     i := i + 1;
     
     # compute the i-th component
     if g in S then
       actualComponent := U;
     else
       actualComponent := G;
     fi;
     
     # add this i-th component.
     UAtS := ClosureGroup (UAtS, Images (
                                      Embedding (gtog, i),
                                      actualComponent
                                        )
                     );

  od;
#
  return UAtS;
#
end );

#############################################################################
##
#F ZerosAtThePlacesS ( <G>, <S> )
##
##    GtoG has to be G^|G| for some group G;
##    S has to be a subset of G.
##
##    returns a subgroup T of SizeFoldDirectProduct ( <G> )
##    (= GtoG) with the property
##    that the set
##       { GroupGeneralMappingByGroupElement (G, t) | t in T }
##    is equal to the set 
##       { f : G -> G  |  f(S) \subseteq {0} }.
##    Intuitively, this means that the set G x G x 0 x G x 0 ...
##    is returned, where the 0's stand at the places of S.
##

BindGlobal( "ZerosAtThePlacesS",
  function ( G, S )
   
  return UAtThePlacesS (G, TrivialSubgroup (G), S);

end );

##############################
# CONSTRUCTORS FOR NEARRINGS
##############################


#############################################################################
##
#M  PolynomialNearRing( <G> ). . . . . . . . . .  compute the nearring 
##  of all polynomials on the group G
##
##  Constructor function for nearrings. 
##  Returns the nearring P(G).
##
InstallMethod (
	PolynomialNearRing,
	"default function for computing polynomial nearrings",
	true,
	[IsGroup],
        0,
function (G)
#
  local additiveGenerators,
        c,
        positionOfC,
        NR;

#						compute constant mappings.
  additiveGenerators := [];
  for c in GeneratorsOfGroup (G) do
     positionOfC := Position (AsSSortedList (G), c);
     Add (additiveGenerators,
          List ([1..Size (G)],
                i -> positionOfC));
  od;

#						compute identity mapping.
  Add (additiveGenerators, [1..Size(G)]);
 
  additiveGenerators := 
	List (additiveGenerators,
              v -> EndoMappingByTransformation( G, EndoMappingFamily(G), Transformation(v) ) );

  NR := TransformationNearRingByAdditiveGenerators ( G, additiveGenerators );
  
  SetPolynomialNearRingFlag( NR, true );
  SetOne( NR, IdentityMapping(G) );
    
  return NR;
end);

#############################################################################
##
#M  ViewObj			polynomial nearrings

InstallMethod(
	ViewObj,
	"PolynomialNR",
	true,
	[IsTransformationNearRing and IsPolynomialNearRing],
	10,
 function ( nr )
   Print( "PolynomialNearRing( " );
   View( Gamma(nr) );
   Print( " )" );
 end );

#############################################################################
##
#F  Size		for polynomial nearrings on groups of nilp class <3
##

SizeOfPGnpc2 := function ( G )

# compute the size of G if G is nilpotent of class 2

    local pees, # possible values for k
	  n,	# |G|
	  k,	# the exact k
	  z;	# |Z(G)|

    n := Size(G); z := Size(Centre(G));

    k := FindKInGroup(G);

    return n^2*k/z;

end;

InstallMethod(
	Size,
	"polyNR over group of npc <=2",
	true,
	[IsNearRing and IsPolynomialNearRing],
	10,
  function ( P )
  local gamma, npc, addGroup, size;
    gamma := Gamma(P);
    if Size( gamma ) = 1 then
	return 1;
    fi;
    npc := NilpotencyClassOfGroup( gamma );
    if npc = fail or npc > 2 then
	TryNextMethod();
    elif npc = 1 then
	size := Size(gamma)*Exponent(gamma);
    elif npc = 2 then
	size := SizeOfPGnpc2(gamma);
    fi;
 
    addGroup := GroupReduct(P);

    # when the size is computed, there is a faster (random) method for
    # determining the stabilizer chain of the group, so we set:

    SetStabChainOptions( addGroup, rec( random := 10, size := size ) );
    
    return size;
  end );



#############################################################################
##
#F MegaBytesAvailable ()
##
##    should return the value that stands after the -o option.
##    The value is used for avoiding some calculations when
##    computing the size of nr
##    that would exceed the permitted memory. Less memory consuming
##    functions for computing the size of a nr. are used instead.
##
BindGlobal ("MegaBytesAvailable", 
  function ()
    return 75;
  end);



#############################################################################
##
#F  Size		for I(G), E(G), A(G), P(G), restricted
##                      endomorphism near-ring if big.
##
##  this function uses that the elements of these nrs are determined
##  by their values on the centre and on a transversal through
##  the cosets of the centre. For E(G) and a restricted endomorphism
##  near-ring, this is only valid
##  if the centre is fullinvariant.
##
##
InstallMethod (Size,       
               "computation of size by computing images on the centre and on representatives modulo the centre",
               true,
               [IsNearRing and IsTransformationNearRing],
                8,
  function (N)
#
     local Z,  # centre of G 
           G,
           gen,
           setOfGraphs,
           additiveGenerators,
           argumentList;
#
     G := Gamma (N);
#
     
     if (not (IsEndomorphismNearRing (N) or 
              IsAutomorphismNearRing (N)  or
              IsInnerAutomorphismNearRing (N) or
              IsRestrictedEndomorphismNearRing (N) or
              IsPolynomialNearRing (N) ))
         or Size (Centre (G)) < 2 
         or     # exclude cases when the method does not pay off
            (IsPermGroup (G) and Size (G) * NrMovedPoints (G)
                                 <= MegaBytesAvailable () * 100)
         or Size (G) <= MegaBytesAvailable () * 2 
        then
         TryNextMethod ();
     fi;
#
     Z := Centre (G);
#
#    for computing the size of E(G),  the centre Z
#    must be a fullinvariant subgroup
#
     if (IsEndomorphismNearRing (N) or IsRestrictedEndomorphismNearRing (N)) and 
        ForAny (GeneratorsOfNearRing (N), 
                 gen -> (not (IsSubset (Z, Image (gen, Z))))) then
           TryNextMethod ();
     fi;
#
     argumentList := List ([1..Length (GeneratorsOfGroup (Z))],
                         i -> GeneratorsOfGroup (Z) [i]);
     Append (argumentList, NontrivialRepresentativesModNormalSubgroup (G, Z));
     Append (argumentList, [Identity (G)]);
#
#				 Compute the list of ranges
#
     setOfGraphs := DirectProduct (List ([1..Length (argumentList)],
                                         i ->  G));
#
     return Size ( Subgroup ( setOfGraphs, 
               List ( AdditiveGenerators (N),
                        a ->
                        Product( [1..Length(argumentList)],
                                 i -> Image (Embedding (setOfGraphs, i),
                                             Image (a, argumentList [i]))))));
  end );

                                            



#############################################################################
##
#M  EndomorphismNearRing( <G> ). . . . . . . . . .  compute the nearring 
##  generated by all endomorphisms in G.
##
##  Constructor function for nearrings. 
##  Returns the nearring E(G).
##

InstallMethod (
	EndomorphismNearRing,
	"computing endomorphism nearrings on abelian groups",
	true,
	[IsGroup and IsAbelian],
        0,
function (G)
#
  local additiveGenerators,
	NR;
 
  additiveGenerators := AdditiveGeneratorsForEndomorphisms( G );
  NR := TransformationNearRingByAdditiveGenerators ( G, additiveGenerators );
  
## NR.additiveGroup.abstractGenerators := abstrGens;  

  SetEndomorphismNearRingFlag( NR, true );
  SetOne( NR, IdentityMapping(G) );
   
  return NR;
end);


InstallMethod (
	EndomorphismNearRing,
	"default function for computing endomorphism nearrings",
	true,
	[IsGroup],
        0,
function (G)
#
  local additiveGenerators,
        c,
        positionOfC,
        NR,
        Glist;

  Glist := AsSSortedList (G);
#						compute generators
  additiveGenerators := List (Endomorphisms (G),
                              e -> List (Glist,
                                         g -> Position (Glist, Image (e, g))));  
  additiveGenerators := 
	List (additiveGenerators,
              v -> EndoMappingByTransformation( G, EndoMappingFamily(G), Transformation(v) ) );
  NR := TransformationNearRingByAdditiveGenerators ( G, additiveGenerators );
  
## NR.additiveGroup.abstractGenerators := abstrGens;  

  SetEndomorphismNearRingFlag( NR, true );
  SetOne( NR, IdentityMapping(G) );
   
  return NR;
end);

#############################################################################
##
#M  ViewObj			Endomorphism nearrings

InstallMethod(
	ViewObj,
	"EndomorphismNR",
	true,
	[IsTransformationNearRing and IsEndomorphismNearRing],
	10,
 function ( nr )
   Print( "EndomorphismNearRing( " );
   View( Gamma(nr) );
   Print( " )" );
 end );

# Automorphism nearrings

#############################################################################
##
#M  AutomorphismNearRing( <G> ). . . . . . . . . .  compute the nearring 
##  generated by all Automorphisms in G.
##
##  Constructor function for nearrings. 
##  Returns the nearring A(G).
##

InstallMethod (
	AutomorphismNearRing,
	"default function for computing automorphism nearrings",
	true,
	[IsGroup],
        0,
function (G)
#
  local additiveGenerators,
        c,
        positionOfC,
        NR,
        Glist;

  Glist := AsSSortedList (G);
#						compute generators
  additiveGenerators := List (Automorphisms (G),
                              e -> List (Glist,
                                         g -> Position (Glist, Image (e, g))));             
  additiveGenerators := 
	List (additiveGenerators,
              v -> EndoMappingByTransformation( G, EndoMappingFamily(G), Transformation(v) ) );
          
  NR := TransformationNearRingByAdditiveGenerators ( G, additiveGenerators );
  
## NR.additiveGroup.abstractGenerators := abstrGens;  

  SetAutomorphismNearRingFlag( NR, true );
  SetOne( NR, IdentityMapping(G) );
    
  return NR;
end);

#############################################################################
##
#M  ViewObj			Automorphism nearrings

InstallMethod(
	ViewObj,
	"AutomorphismNR",
	true,
	[IsTransformationNearRing and IsAutomorphismNearRing],
	10,
 function ( nr )
   Print( "AutomorphismNearRing( " );
   View( Gamma(nr) );
   Print( " )" );
 end );

# Inner Automorphism nearrings

#############################################################################
##
#M  InnerAutomorphismNearRing( <G> ). . . . . . . . . .  compute the nearring 
##  generated by all inner automorphisms in G.
##
##  Constructor function for nearrings. 
##  Returns the nearring A(G).
##

InstallMethod (
	InnerAutomorphismNearRing,
	"default function for computing inner automorphism nearrings",
	true,
	[IsGroup],
        0,
function (G)
#
  local additiveGenerators,
        c,
        positionOfC,
        NR,
        Glist;

  Glist := AsSSortedList (G);
#						compute generators
  additiveGenerators := List (InnerAutomorphisms (G),
                              e -> List (Glist,
                                         g -> Position (Glist, Image (e, g))));             
  
  additiveGenerators := 
	List (additiveGenerators,
              v -> EndoMappingByTransformation( G, EndoMappingFamily(G), Transformation(v) ) );
          
  NR := TransformationNearRingByAdditiveGenerators ( G, additiveGenerators );
  
## NR.additiveGroup.abstractGenerators := abstrGens;  

  SetInnerAutomorphismNearRingFlag( NR, true );
  SetOne( NR, IdentityMapping(G) );
   
  return NR;
end);
  
#############################################################################
##
#M  ViewObj			InnerAutomorphism nearrings

InstallMethod(
	ViewObj,
	"InnerAutomorphismNR",
	true,
	[IsTransformationNearRing and IsInnerAutomorphismNearRing],
	10,
 function ( nr )
   Print( "InnerAutomorphismNearRing( " );
   View( Gamma(nr) );
   Print( " )" );
 end );

#############################################################################
##
#M  InnerAutomorphismNearRingGeneratedByCommutators( <G> ) 
##    computes the nearring generated by all inner automorphisms 
##    on G.
##
##  Constructor function for nearrings. 
##  Returns the nearring I(G).

InstallMethod(
	InnerAutomorphismNearRingGeneratedByCommutators,
	"computes the inner automorphism nearring",
	true,
	[IsGroup],
        0,
function (G)
#
  local CommutatorWithG, 
        GeneratorList,
        repsModCentre, 
        abstractGensNames, 
        NR, 
        Glist;

  # compute the generators.

  Glist := AsSSortedList (G);
#
#                 I(G) is additively generated by all mappings of the form
#                 x -> [x,g] and the identity.
#
#                 compute the mappings x -> [x,g]
  CommutatorWithG := function (g)
            return EndoMappingByTransformation(
			G, EndoMappingFamily(G), Transformation(
                                   List (Glist,
                                         x -> Position (Glist, Comm(x, g)) ) )
			);
                     end;

   repsModCentre :=
	NontrivialRepresentativesModNormalSubgroup( G, Centre(G));
  
  GeneratorList := List (repsModCentre, g -> CommutatorWithG (g));
  Add (GeneratorList, IdentityMapping (G));

  NR := TransformationNearRingByAdditiveGenerators (G, GeneratorList);

  SetInnerAutomorphismNearRingByCommutatorsFlag( NR, true );
  SetOne( NR, IdentityMapping(G) );
   
  return NR;


end);

#############################################################################
##
#M  ViewObj			inner automorphism nearrings by commutators

InstallMethod(
	ViewObj,
	"PolynomialNR",
	true,
	[IsTransformationNearRing and
		IsInnerAutomorphismNearRingByCommutators],
	10,
 function ( nr )
   Print( "InnerAutomorphismNearRingByCommutators( " );
   View( Gamma(nr) );
   Print( " )" );
 end );

#############################################################################
##
#M  CentralizerNearRing ( <G>, <endos> )
##    computes the nearring of all mappings m on G with the property that
##    m(e(x)) = e(m(x)) for all e in <endos> and x in <G>.
## 
##  Constructor function for nearrings. 
##  Returns the nearring M_{<endos>}(<G>). Note that the resulting nearring
##  need not be zerosymmetric if the zero-endomorphism is not included
##  in <endos>.
##

InstallMethod(
	CentralizerNearRing,
	"default method for centralizer nearrings",
	true,
	[IsGroup, IsListOrCollection],
        0,
function (G, endos)
#
#
  local NR,           # nearring to be computed
        addGroup,     # the additive group
        e,            # runs through the endomorphisms
        x,            # runs through the elements of G
        eOfx,         # e (x)
        y,
        eOfy,                               
        gen,          # runs through the generators of G
        generatorsOfMapsWithTheRightImagesAtX,   
                      # the generators of the sets depend on e and x 
        mapsWithTheRightImagesAtXandEofX,
        mapsWithTheRightImagesEverywhere,
        additiveGroup,
        gtog;           
  
#
   gtog := SizeFoldDirectProduct(G);

   # additive group

#               all mappings are computed in their internal representation,
#               i.e., as elements of $G^|G|$.

    mapsWithTheRightImagesEverywhere := gtog; 
    for e in endos do
    for x in AsSSortedList (G) do

    #           compute the vector representation of all functions such that
    #           f (e (x)) = e (f (x)) and f is zero everywhere else

       generatorsOfMapsWithTheRightImagesAtX := [];
       eOfx := Image (e, x); 
    
    #           two cases, according to x = e (x)
    #
                   
       if Order (x^(-1) *  eOfx) = 1 then             # if x = eOfx

    #           in this case, we have to compute all mappings such that
    #           f(x) = y with e (y) = y                    

          for y in AsSSortedList (G) do
             eOfy := Image (e, y);
   
    #           y is only a possible image of x iff y = e (y) 
    #                     
             if Order (y^(-1) * eOfy) = 1  then     # if y = eOfy
    
    #           a function is added that maps x to y, 
    #           and all other elements to 0.
    #                                            
                Add( generatorsOfMapsWithTheRightImagesAtX,
                     IntRepOfFunctionOnG (
                               G,
                               function (x1)
                                 if Order (x1^(-1) * x) = 1  then   # x = x1
                                    return y;
                                 else 
                                    return Identity (G);
                                 fi;
                              end));
                  
             fi;
          od;  # for y in Elements (G)
         
      else   # x <> eOfx
                
         for y in GeneratorsOfGroup(G) do
            
            eOfy := Image (e, y);
               
            Add (generatorsOfMapsWithTheRightImagesAtX,
                  IntRepOfFunctionOnG (
                         G,
                         function (x1)
                            if Order (x1^(-1) * x) = 1 then        # x1 = x
                                    return y;
                            elif Order (x1^(-1) * eOfx) = 1 then  # x1 = eOfx 
                                    return eOfy;
                            else
                                    return Identity (G);
                            fi;
                         end));
             
         od; # for y in GeneratorsOfGroup(G)
      
      fi;  # if x = eOfx
      
      mapsWithTheRightImagesAtXandEofX := 
         Subgroup (gtog, generatorsOfMapsWithTheRightImagesAtX);

#                at all the places apart from x and e(x)  a mapping
#                may take any values.

      mapsWithTheRightImagesAtXandEofX := 
         ClosureGroup (
                       ZerosAtThePlacesS ( G, [x, eOfx] ), 
                       Subgroup (gtog, generatorsOfMapsWithTheRightImagesAtX)
                      );
     
      
      mapsWithTheRightImagesEverywhere :=
         Intersection (mapsWithTheRightImagesEverywhere,
                       mapsWithTheRightImagesAtXandEofX);
 


   od;       # end for x;
   od;       # end for e;
#
   additiveGroup := mapsWithTheRightImagesEverywhere;

   NR := TransformationNearRingByGenerators (
            G,
            List (GeneratorsOfGroup(additiveGroup),
                  g -> GroupGeneralMappingByGroupElement (G, g)));

   SetGroupReduct(NR, additiveGroup);

   SetCentralizerNearRingFlag( NR, true );
   SetOne( NR, IdentityMapping(G) );

   return NR;
end);

#############################################################################
##
#M  ViewObj			Centralizer nearrings

InstallMethod(
	ViewObj,
	"CentralizerNR",
	true,
	[IsTransformationNearRing and IsCentralizerNearRing],
	10,
 function ( nr )
   Print( "CentralizerNearRing( " );
   View( Gamma(nr) );
   Print( ", ... )" );
 end );

##############################################################################
##
#M  RestrictedEndomorphismNearRing

InstallMethod(
	RestrictedEndomorphismNearRing,
	"default",
	IsIdenticalObj,
	[IsGroup, IsGroup and IsAbelian],
	0,
  function (G, U)
#
   local H,       # the nearring to be returned
   restrictedEndos;

   restrictedEndos := AdditiveGeneratorsForHomomorphisms( G, U );
    
   # make GroupTransformations out of the endomorphisms

   H := TransformationNearRingByAdditiveGenerators( G, restrictedEndos );

   # restricted to what?
   H!.range := U;

   SetRestrictedEndomorphismNearRingFlag( H, true );

   return H;
#
  end );


InstallMethod(
	RestrictedEndomorphismNearRing,
	"default",
	IsIdenticalObj,
	[IsGroup, IsGroup],
	0,
  function (G, U)
#
   local H,       # the nearring to be returned
   restrictedEndos;

#   restrictedEndos := Filtered (Endomorphisms (G),
#                                e -> IsSubset (U,
#                                               Images (e, G)));
   restrictedEndos := Homomorphisms( G, U );
    
   # make GroupTransformations out of the endomorphisms

   H := TransformationNearRingByAdditiveGenerators( G, restrictedEndos );

   # restricted to what?
   H!.range := U;

   SetRestrictedEndomorphismNearRingFlag( H, true );

   return H;
#
  end );

#############################################################################
##
#M  ViewObj			RestrictedEndomorphism nearrings

InstallMethod(
	ViewObj,
	"RestrictedEndomorphismNR",
	true,
	[IsTransformationNearRing and IsRestrictedEndomorphismNearRing],
	10,
 function ( nr )
   Print( "RestrictedEndomorphismNearRing( " );
   View( Gamma(nr) );
   Print( ", " );
   View( nr!.range );
   Print( " )" );
 end );

#############################################################################
##
#M  LocalInterpolationNearRing
##

InstallMethod(
	LocalInterpolationNearRing,
	"TfmNRs",
	true,
	[IsTransformationNearRing, IsInt and IsPosRat],
	0,
  function ( NR, m )
  local  LmNR,           # the near-ring to be computed
         G,              # group on which NR operates
         S,              # subset of the group on which NR operates
         ZerosAtS,       # a subgroup of G^|G|
         ZerosAtSPlusNR, # a subgroup of G^|G|
	 addGroup,	 # the additive group of the near ring
	 fam;		 # the family of the elements of the near ring

     # additive group
     G := Gamma(NR);
     fam := NR!.elementsInfo;
    
     if m >= Size (G) then
        addGroup := GroupReduct(NR);
     else
        addGroup := SizeFoldDirectProduct( Gamma(NR) );
       
        for S in Combinations ( AsSSortedList(G), m ) do
        #                 compute G x G x {0} x G x {0} x ... , where
        #                 the zeros stand at the elements of S
           ZerosAtS := ZerosAtThePlacesS ( fam, S );
           ZerosAtSPlusNR := ClosureGroup ( ZerosAtS, GroupReduct(NR) );
 
           addGroup := Intersection (addGroup, ZerosAtSPlusNR);
       od;
     fi;

     LmNR := TransformationNearRingByAdditiveGenerators( G,
		List( GeneratorsOfGroup(addGroup), gen -> GroupGeneralMappingByGroupElement( G, gen ) ) );

     # how many points?
     LmNR!.m := m;
     # from which near ring?
     LmNR!.nearring := NR;

     SetLocalInterpolationNearRingFlag( LmNR, true );

     return LmNR;
  end );

#############################################################################
##
#M  ViewObj			Local nearrings

InstallMethod(
	ViewObj,
	"LocalNR",
	true,
	[IsTransformationNearRing and IsLocalInterpolationNearRing],
	10,
 function ( nr )
   Print("LocalInterpolationNearRing( "); View( nr!.nearring ); Print( ", ",nr!.m, " )");
 end );

##############################################################################
##
#M  NoetherianQuotient2
##
##    Input conditions: (1) <NR> is a near-ring of functions <Gamma>,
##                      (2) <Target> is a subgroup of G,
##                      (3) <Source> is a subset of G,
##                      (4) <Target> is a subset of <Source>.
##    Then this function returns all mappings in <NR> that
##    map <Source> into <Target>.
##    Condition (4) guarantees that the resulting functions are closed
##    under composition. Returns a right ideal. 
##

InstallMethod(
	NoetherianQuotient2,
	"default",
	true,
	[IsTransformationNearRing, IsGroup, IsGroup,
		IsMultiplicativeElementCollection],
	10,
  function ( NR, gamma, target, source )
#  
   local NQuot,      # the nearring to be returned.
         additiveGroup
        ;
   
   if not IsNormal( gamma, target ) then
     TryNextMethod();
   fi;

   additiveGroup := Intersection (
                         GroupReduct (NR), 
                         UAtThePlacesS (NR!.elementsInfo, target, source)
                                 );                     
                                

   NQuot := NearRingRightIdealBySubgroupNC(NR, additiveGroup);


   if IsSubset (source, target) then       # if Target is a 
                                           # subset of Source
      SetIsClosedUnderComposition (NQuot, true);
   fi;

   SetNoetherianQuotientFlag (NQuot, true);

   NQuot!.target := target;
   NQuot!.source := source;
    
   return NQuot;

end );

#############################################################################
##
#M  ViewObj			Noetherian Quotients

InstallMethod(
	ViewObj,
	"NoetherianQuotient",
	true,
	[IsNearRingElementCollection and IsNoetherianQuotient],
	10,
 function ( N )
   Print( "NoetherianQuotient( " );
   View( N!.target ); 
   Print( " ," );
   View( N!.source );
   Print( " )" );
 end );





##############################################################################
##
#M  ZeroSymmetricPart
##

InstallMethod(
	ZeroSymmetricPart,
	"TfmNRs",
	true,
	[IsTransformationNearRing],
	0,
  function ( NR )
  local Z;         # the nearring to be returned

    Z :=  NoetherianQuotient (NR, TrivialSubgroup( Gamma(NR) ), 
                                  TrivialSubgroup( Gamma(NR) ) );
    return AsSubNearRing (NR, Z);

  end );



#############################################################################
##
#M  CongruenceNoetherianQuotient ( <P>, <A>, <B>, <C> )
##
##    for nearrings of all polynomial functions.
##    Input conditions: (1) <P> is the near-ring of polynomial 
##                              functions on a group G,
##                      (2) <A> is a normal subgroup of G,
##                      (3) <B> is a normal subgroup of G,
##                      (4) <C> is a normal subgroup of G,
##                      (5) [C,B] <= A.
##    Then this function returns all mappings in <P> that
##    map every element of P into C, and maps two elements that
##    are congruent modulo B into elements that are congruent modulo
##    A.
##    The result is an ideal of the near-ring.
##

InstallMethod(
	CongruenceNoetherianQuotient,
	"default",
	true,
	[IsPolynomialNearRing, IsGroup, IsGroup,
		IsGroup],
	10,
  function ( P, A, B, C )
#  
   local NQuot,      # the nearring to be returned.
         additiveGroup,
         G, 
         X,
         c,
         positionOfC,
         additiveGeneratorsOfConstantFunctionsWithImageInC;
   
   G := Gamma (P);

   if not
      (IsNormal (G, B) and
       IsNormal (G, A) and
       IsNormal (G, C) and
       IsSubgroup (B, A) and 
       IsSubgroup (A, CommutatorSubgroup (C, B))) then
      TryNextMethod();
   fi;
 
   additiveGroup := Intersection (GroupReduct (P),
                                  UAtThePlacesS (P!.elementsInfo, C, G));


#						compute constant mappings.

   additiveGeneratorsOfConstantFunctionsWithImageInC := [];
   for c in GeneratorsOfGroup (C) do
      Add (additiveGeneratorsOfConstantFunctionsWithImageInC,
           IntRepOfFunctionOnG (P!.elementsInfo, x -> c));
   od;

   if Size (Group (additiveGeneratorsOfConstantFunctionsWithImageInC)) <>
      Size (C) then
      Print ("\nCongruenceNoetherianQuotient: dgnr.gi : this should never happen.\n");
   fi;

   for X in RightCosets (G,B) do
#
#      Print ("\nSize", Size (additiveGroup), "\n");
#
      additiveGroup := Intersection (
                         additiveGroup, 
                         ClosureGroup (UAtThePlacesS (P!.elementsInfo, A,  X),
                                       additiveGeneratorsOfConstantFunctionsWithImageInC));
   od;
                                

   NQuot := NearRingIdealBySubgroupNC(P, additiveGroup);


   SetIsClosedUnderComposition (NQuot, true);
 
   return NQuot;
end );



#############################################################################
##
#M  CongruenceNoetherianQuotientForInnerAutomorphismNearRings ( <I>, <A>, <B>, <C> )
##
##    for nearrings of all polynomial functions.
##    Input conditions: (1) <I> is the inner automorphism nr. on a group G,
##                      (2) <A> is a normal subgroup of G,
##                      (3) <B> is a normal subgroup of G,
##                      (4) <C> is a normal subgroup of G,
##                      (5) [C,B] <= A.
##    Then this function returns all mappings in <I> that
##    map every element of P into C, and maps two elements that
##    are congruent modulo B into elements that are congruent modulo
##    A.

InstallMethod(
	CongruenceNoetherianQuotientForInnerAutomorphismNearRings,
	"default",
	true,
	[IsInnerAutomorphismNearRing, IsGroup, IsGroup,
		IsGroup],
	10,
  function ( I, A, B, C )
#  
   local NQuot,      # the nearring to be returned.
         additiveGroup,
         G, 
         X,
         additiveGeneratorsOfConstantFunctionsWithImageInC;
   
   G := Gamma (I);

   return NearRingIdealBySubgroupNC (I,
            Intersection (UAtThePlacesS (I!.elementsInfo, TrivialSubgroup (G), TrivialSubgroup (G)),
                          GroupReduct (CongruenceNoetherianQuotient
                                         (PolynomialNearRing (G),
                                          A,B,C))));
end );


















