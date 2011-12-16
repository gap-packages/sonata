
## N!.multiplication ersetzt PM


#############################################################################
##
#M  DirectProductNearRing
##

InstallMethod(
	DirectProductNearRing,
	"ExpMulNR*ExpMulNR",
	true,
	[IsExplicitMultiplicationNearRing, IsExplicitMultiplicationNearRing],
	0,
  function ( N, M )
  local NM, pr1, pr2, e1, e2, mul, D;
    NM := DirectProduct(GroupReduct(N),GroupReduct(M));
    e1 := Embedding( NM, 1 );
    e2 := Embedding( NM, 2 );
    pr1 := Projection( NM, 1 );
    pr2 := Projection( NM, 2 );
    mul := function( x, y ) 
      return Image(e1,NRMultiplication(N)(Image(pr1,x),Image(pr1,y)))
            *Image(e2,NRMultiplication(M)(Image(pr2,x),Image(pr2,y)));
    end;

    D := ExplicitMultiplicationNearRingNC( NM, mul );
    SetDirectProductNearRingFlag( D, true );
    # remember the factors
    D!.factors := [ N, M ];

    return D;
end );

#############################################################################
##
#M  DirectProductNearRing	for 2 transformation nearrings
##

InstallMethod(
	DirectProductNearRing,
	"TfmNRs",
	true,
	[IsTransformationNearRing, IsTransformationNearRing],
	0,
  function ( nr1, nr2 )
  local gamma1, gamma2, newGamma,
	elms, fam,
	pr1, pr2, emb1, emb2,
	embedTransformation,
	zero1, zero2,
	addGens1, addGens2, addGens,
	D;

    gamma1 := Gamma( nr1 ); gamma2 := Gamma( nr2 );

    newGamma := DirectProduct( gamma1, gamma2 );
    elms := AsSSortedList( newGamma );
    fam := EndoMappingFamily( newGamma );

    pr1 := Projection( newGamma, 1 );
    pr2 := Projection( newGamma, 2 );

    emb1 := Embedding( newGamma, 1 );
    emb2 := Embedding( newGamma, 2 );

    embedTransformation := function ( f, g )
    local vec, elm;
      vec := [];
      for elm in elms do
	Add( vec, Position( elms, 
		Image( emb1, Image( f, Image( pr1, elm ) ) ) *
		Image( emb2, Image( g, Image( pr2, elm ) ) )
	   )              );
      od;
      return EndoMappingByTransformation( newGamma, fam, Transformation(vec) );
    end;

    zero1 := ConstantEndoMapping( gamma1, Identity( gamma1 ) );
    zero2 := ConstantEndoMapping( gamma2, Identity( gamma2 ) );

    addGens1 := List( GeneratorsOfGroup( GroupReduct( nr1 ) ),
			gen -> GroupGeneralMappingByGroupElement( GroupReduct(nr1), gen ) );
    addGens2 := List( GeneratorsOfGroup( GroupReduct( nr2 ) ),
			gen -> GroupGeneralMappingByGroupElement( GroupReduct(nr2), gen ) );

    addGens := Concatenation(
		List( addGens1, gen -> embedTransformation( gen, zero2 ) ),
		List( addGens2, gen -> embedTransformation( zero1, gen ) ) );

    D := TransformationNearRingByAdditiveGenerators( newGamma, addGens );

    SetDirectProductNearRingFlag( D, true );
    # remember the factors
    D!.factors := [ nr1, nr2 ];

    return D;
  end );

#############################################################################
##
#M  ViewObj			for direct products of nearrings
##

InstallMethod(
	ViewObj,
	"direct products of nearrings",
	true,
	[IsNearRing and IsDirectProductNearRing],
	10,			# overwrite all methods for ExpMulNRs
  function ( nr )
    Print("DirectProductNearRing( ",nr!.factors[1],", ",nr!.factors[2]," )");
  end );

#############################################################################
##
#M  NearRingIdeals			for direct products of nearrings
##

InstallMethod(
	NearRingIdeals,
	"direct products",
	true,
	[IsNearRingWithOne and IsDirectProductNearRing],
	10,
  function ( nr )
  local	addGroup, 
	idealsOf1stFactor, idealsOf2ndFactor, 
	embedding1, embedding2, 
	ideals, I, J;

    addGroup := GroupReduct(nr);

    idealsOf1stFactor := NearRingIdeals(nr!.factors[1]);
    idealsOf2ndFactor := NearRingIdeals(nr!.factors[2]);

    embedding1 := Embedding( addGroup, 1 );
    embedding2 := Embedding( addGroup, 2 );

    idealsOf1stFactor := List( idealsOf1stFactor,
	id -> Image( embedding1, GroupReduct( id ) ) );
    idealsOf2ndFactor := List( idealsOf2ndFactor,
	id -> Image( embedding2, GroupReduct( id ) ) );

    ideals := [];
    for I in idealsOf1stFactor do
      for J in idealsOf2ndFactor do
        Add( ideals, ClosureGroup( I, J ) );
      od;
    od;

    return List( ideals, I -> NearRingIdealBySubgroupNC( nr, I ) );
  end );

#############################################################################
##
#M  NearRingLeftIdeals			for direct products of nearrings
##

InstallMethod(
	NearRingLeftIdeals,
	"direct products",
	true,
	[IsNearRingWithOne and IsDirectProductNearRing],
	10,
  function ( nr )
  local	addGroup, 
	idealsOf1stFactor, idealsOf2ndFactor, 
	embedding1, embedding2, 
	ideals, I, J;

    addGroup := GroupReduct(nr);

    idealsOf1stFactor := NearRingLeftIdeals(nr!.factors[1]);
    idealsOf2ndFactor := NearRingLeftIdeals(nr!.factors[2]);

    embedding1 := Embedding( addGroup, 1 );
    embedding2 := Embedding( addGroup, 2 );

    idealsOf1stFactor := List( idealsOf1stFactor,
	id -> Image( embedding1, GroupReduct( id ) ) );
    idealsOf2ndFactor := List( idealsOf2ndFactor,
	id -> Image( embedding2, GroupReduct( id ) ) );

    ideals := [];
    for I in idealsOf1stFactor do
      for J in idealsOf2ndFactor do
        Add( ideals, ClosureGroup( I, J ) );
      od;
    od;

    return List( ideals, I -> NearRingLeftIdealBySubgroupNC( nr, I ) );
  end );

#############################################################################
##
#M  NearRingRightIdeals			for direct products of nearrings
##

InstallMethod(
	NearRingRightIdeals,
	"direct products",
	true,
	[IsNearRingWithOne and IsDirectProductNearRing],
	10,
  function ( nr )
  local	addGroup, 
	idealsOf1stFactor, idealsOf2ndFactor, 
	embedding1, embedding2, 
	ideals, I, J;

    addGroup := GroupReduct(nr);

    idealsOf1stFactor := NearRingRightIdeals(nr!.factors[1]);
    idealsOf2ndFactor := NearRingRightIdeals(nr!.factors[2]);

    embedding1 := Embedding( addGroup, 1 );
    embedding2 := Embedding( addGroup, 2 );

    idealsOf1stFactor := List( idealsOf1stFactor,
	id -> Image( embedding1, GroupReduct( id ) ) );
    idealsOf2ndFactor := List( idealsOf2ndFactor,
	id -> Image( embedding2, GroupReduct( id ) ) );

    ideals := [];
    for I in idealsOf1stFactor do
      for J in idealsOf2ndFactor do
        Add( ideals, ClosureGroup( I, J ) );
      od;
    od;

    return List( ideals, I -> NearRingRightIdealBySubgroupNC( nr, I ) );
  end );

#############################################################################
##
#M  FactorNearRing
##

InstallMethod(
	FactorNearRing,
	"really an ideal?",
	IsIdenticalObj,
	[IsExplicitMultiplicationNearRing, IsNRI],
	100,
  function ( N, I )
  if IsNearRingRightIdeal(I) and
     IsNearRingLeftIdeal(I) then
     TryNextMethod();
  else
     Error( "FactorNearRing( N, I ): I has to be a two-sided ideal in N" );
  fi;
end );

InstallMethod(
	FactorNearRing,
	"ExpMulNR",
	IsIdenticalObj,
	[IsExplicitMultiplicationNearRing, IsNRI],
	0,
  function ( N, I )
  local NfI, f, mul, addN, addI, F;
    addN := GroupReduct(N); addI := GroupReduct(I);
    f := NaturalHomomorphismByNormalSubgroupNC( addN, addI );
    NfI := Image( f, addN );

    mul := function ( x, y )
      return Image( f,
	NRMultiplication(N)(
		PreImagesRepresentative( f, x ),
		PreImagesRepresentative( f, y )
			 )
		  );
    end;
    F := ExplicitMultiplicationNearRingNC( NfI, mul );
    SetFactorNearRingFlag( F, true );
    F!.mother := N;			# the generators of
    F!.father := I;			# a factor nearring

    return F;
end );
  
InstallMethod(
	FactorNearRing,
	"generic",
	IsIdenticalObj,
	[IsNearRing, IsNRI],
	0,
  function ( N, I )
  local NfI, f, mul, addN, addI, fam, F;
    fam := N!.elementsInfo;
    addN := GroupReduct(N); addI := GroupReduct(I);
    f := NaturalHomomorphismByNormalSubgroupNC( addN, addI );
    NfI := Image( f, addN );

    mul := function ( x, y )
      return Image( f,
	GroupElementRepOfNearRingElement(
		NearRingElementByGroupRep( fam, PreImagesRepresentative( f, x ) ) *
		NearRingElementByGroupRep( fam, PreImagesRepresentative( f, y ) )
		   )
		  );
    end;
    F := ExplicitMultiplicationNearRingNC( NfI, mul );
    SetFactorNearRingFlag( F, true );
    F!.mother := N;			# the generators of
    F!.father := I;			# a factor nearring

    return F;
end );

#############################################################################
##
#M  \/				for a nearring and an ideal
##

InstallOtherMethod(
	\/,
	"nearrings",
	true,
	[IsNearRing, IsNRI],
	0,
  function ( N, I )
    return FactorNearRing( N, I );
  end );
 
#############################################################################
##
#M  ViewObj			for factor NearRings
##

InstallMethod(
	ViewObj,
	"factor NearRings",
	true,
	[IsNearRing and IsFactorNearRing],
	10,			# overwrite all methods for ExpMulNRs
  function ( nr )
    Print( "FactorNearRing( ",nr!.mother,", ",nr!.father," )" );
  end );




