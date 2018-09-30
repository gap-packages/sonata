#############################################################################
##
#M  ClosureNearRingIdeal

InstallMethod(
	ClosureNearRingIdeal,
	"nrid + nrid",
	IsIdenticalObj,
	[IsNRI and IsNearRingIdeal,IsNRI and IsNearRingIdeal],
	2,
  function ( i, j )
  local sum;
    if Parent(i)<>Parent(j) then
	Error("the two ideals must have equal parents");
    fi;

    sum := ClosureGroup( GroupReduct(i), GroupReduct(j) );
    sum := NearRingIdealBySubgroupNC( Parent(i), sum );

    if HasGeneratorsOfNearRingIdeal(i) and HasGeneratorsOfNearRingIdeal(j) then
	SetGeneratorsOfNearRingIdeal( sum,
	    Concatenation( GeneratorsOfNearRingIdeal(i),
			   GeneratorsOfNearRingIdeal(j) ) );
    fi;		

    return sum;
  end );

#############################################################################
##
#M  ClosureNearRingLeftIdeal 

InstallMethod(
	ClosureNearRingLeftIdeal,
	"nrLeftId + nrLeftId",
	IsIdenticalObj,
	[IsNRI and IsNearRingLeftIdeal,
		IsNRI and IsNearRingLeftIdeal],
	1,
  function ( i, j )
  local sum;
    if Parent(i)<>Parent(j) then
	Error("the two ideals must have equal parents");
    fi;

    sum := ClosureGroup( GroupReduct(i), GroupReduct(j) );
    sum := NearRingLeftIdealBySubgroupNC( Parent(i), sum );

    if HasGeneratorsOfNearRingLeftIdeal(i) and
       HasGeneratorsOfNearRingLeftIdeal(j) then
	 SetGeneratorsOfNearRingLeftIdeal( sum,
	    Concatenation( GeneratorsOfNearRingLeftIdeal(i),
			   GeneratorsOfNearRingLeftIdeal(j) ) );
    fi;		

    return sum;
  end );

#############################################################################
##
#M  ClosureNearRingRightIdeal

InstallMethod(
	ClosureNearRingRightIdeal,
	"nrRightId + nrRightId",
	IsIdenticalObj,
	[IsNRI and IsNearRingRightIdeal,
		IsNRI and IsNearRingRightIdeal],
	0,
  function ( i, j )
  local sum;
    if Parent(i)<>Parent(j) then
	Error("the two ideals must have equal parents");
    fi;

    sum := ClosureGroup( GroupReduct(i), GroupReduct(j) );
    sum := NearRingRightIdealBySubgroupNC( Parent(i), sum );

    if HasGeneratorsOfNearRingRightIdeal(i) and
       HasGeneratorsOfNearRingRightIdeal(j) then
	 SetGeneratorsOfNearRingRightIdeal( sum,
	    Concatenation( GeneratorsOfNearRingRightIdeal(i),
			   GeneratorsOfNearRingRightIdeal(j) ) );
    fi;		

    return sum;
  end );

#############################################################################
##
#M  Intersection2

InstallMethod(
	Intersection2,
	"two nearring ideals",
	IsIdenticalObj,
	[IsNRI and IsNearRingIdeal,
		IsNRI and IsNearRingIdeal],
	2,
  function ( i, j )
  local intersect;
    if Parent(i)<>Parent(j) then
	Error("the two ideals must have equal parents");
    fi;

    intersect := Intersection( GroupReduct(i), GroupReduct(j) );

    return NearRingIdealBySubgroupNC( Parent(i), intersect );
  end );

InstallMethod(
	Intersection2,
	"two nearring LeftIdeals",
	IsIdenticalObj,
	[IsNRI and IsNearRingLeftIdeal,
		IsNRI and IsNearRingLeftIdeal],
	1,
  function ( i, j )
  local intersect;
    if Parent(i)<>Parent(j) then
	Error("the two ideals must have equal parents");
    fi;

    intersect := Intersection( GroupReduct(i), GroupReduct(j) );

    return NearRingLeftIdealBySubgroupNC( Parent(i), intersect );
  end );
    
InstallMethod(
	Intersection2,
	"two nearring RightIdeals",
	IsIdenticalObj,
	[IsNRI and IsNearRingRightIdeal,
		IsNRI and IsNearRingRightIdeal],
	0,
  function ( i, j )
  local intersect;
    if Parent(i)<>Parent(j) then
	Error("the two ideals must have equal parents");
    fi;

    intersect := Intersection( GroupReduct(i), GroupReduct(j) );

    return NearRingRightIdealBySubgroupNC( Parent(i), intersect );
  end );

#############################################################################
##
#M  IsSimpleNearRing
##

InstallTrueMethod( IsSimpleNearRing, IsFullTransformationNearRing );

InstallTrueMethod( IsSimpleNearRing, IsNearField );

InstallMethod(
	IsSimpleNearRing,
	"small nearrings",		# test principal ideals
	true,
	[IsNearRing],
	1,
  function ( N )
    if Size( N ) > 1000 then
	TryNextMethod();
    else
	return ForAll( List( RationalClasses( GroupReduct(N) ), Representative ), x ->
		Size( NearRingIdealClosureOfSubgroup( N, NormalClosure( GroupReduct(N), Subgroup( GroupReduct(N), [x] ) ) ) ) in [1,Size(N)] );
    fi;
  end );

InstallMethod(
	IsSimpleNearRing,
	"known ideals",			# count ideals
	true,
	[IsNearRing and HasNearRingIdeals],
	1,
  function ( N )
    return Length(NearRingIdeals(N)) = 2;
  end );
	
InstallMethod(
	IsSimpleNearRing,
	"default",			# count ideals
	true,
	[IsNearRing],
	0,
  function ( N )
    return Length(NearRingIdeals(N)) = 2;
  end );

#############################################################################
##
#M  NearRingCommutator

InstallMethod(
	NearRingCommutator,
	"first",
	IsIdenticalObj,
	[IsNRI and IsNearRingIdeal, IsNRI and IsNearRingIdeal],
	0,
  function ( X, Y )
  local N,d,d2,comm, m;
    N := Parent(X);
    if N<>Parent(Y) or not IsNearRing(N) then return fail; fi;
    if AdditiveGenerators(X)=[] or AdditiveGenerators(Y)=[] then
      return NearRingIdealByGenerators(N,[]);
    elif IsNearRingWithOne(N) then
      if N=X then
        return Y;
      elif N=Y then
        return X;
      fi;
    fi;
    d:= Union(
        Union(List(N,
               a->Union(List(AdditiveGenerators(X),
                   x->List(AdditiveGenerators(Y),
                       y->(a+y)*x-a*x))))),
        Union(List(N,
               a->Union(List(AdditiveGenerators(X),
                   x->List(AdditiveGenerators(Y),
                       y->(a+x)*y-a*y))))),
        Union(List(X,
                   x->List(Y,
                       y-> x+y-x-y)))
      );
    d2:=Union(List(AdditiveGenerators(N),
               a->Union(List(X,
                   x->Union(List(Y,
                       y->List(N,
                          b->(b+x+y)*a - (b+x)*a + b*a - (b+y)*a)))))));

    return NearRingIdealByGenerators(N, Union(d,d2));
end );

#############################################################################
##
#M  NearRingCommutatorsTable

InstallMethod(
	NearRingCommutatorsTable,
	"default",
	true,
	[IsNearRing],
	0,
  function(N)
    local ids,i;
    ids:=NearRingIdeals(N);
      return List(ids,
   ix->List(ids,
   iy->Position(ids,NearRingCommutator(ix,iy))));
 end );

#############################################################################
##
#M  PrintNearRingCommutatorsTable
##  As above, but for better printing (to be improved)

InstallMethod(
	PrintNearRingCommutatorsTable,
	"default",
	true,
	[IsNearRing],
	0,
  function(N)
  local line;
    for line in NearRingCommutatorsTable(N) do
      Print(line,"\n");
    od;
  end );


