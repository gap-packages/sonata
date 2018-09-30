################################################################################
##
#W  expmulnrnr.gi             Near-ring Library                   J"urgen Ecker
##

##  13.01.00:  N!.multiplication ersetzt, PM

#############################################################################
##
#M  ExplicitMultiplicationNearRingElementFamilies       Initializing method
##

InstallMethod(
	ExplicitMultiplicationNearRingElementFamilies,
	true,
	[IsGroup],
	0,
  f -> [] );

#############################################################################
##
#F  StoreExpMultFam(<fun>,<group>,<fam>)  store fam as Exp.Mul.Fam. for fun
##                                       over group
##

StoreExpMultFam := function(fun,group,fam)
local emf;
  emf:=ExplicitMultiplicationNearRingElementFamilies(group);
  if not ForAny(emf,i->i[1]=fun) then
    Add(emf,[fun,fam]);
  fi;
end;

#############################################################################
##
#M  ExplicitMultiplicationNearRingElementFamily      generic method
##

InstallMethod(
	ExplicitMultiplicationNearRingElementFamily,
	"generic",
	true,
	[IsGroup,IsFunction],
	0,
  function(group,fun)
  local fam,i;
    fam := ExplicitMultiplicationNearRingElementFamilies(Parent(group));
    i := PositionProperty(fam,i->i[1]=fun);
    if i<>fail then
	return fam[i][2];
    fi;

    fam := NewFamily( "ExplicitMultiplicationNearRingElementFamily(...)",
			IsExplicitMultiplicationNearRingElement,
         		IsObject,
			IsExplicitMultiplicationNearRingElementFamily );

  # The default kind
  fam!.defaultKind := NewType( fam,
			IsExplicitMultiplicationNearRingElementDefaultRep);

  # Important trivia
  fam!.multiplication := fun;
  fam!.group := group;

#  set one and zero
#  SetZero(fam,ObjByExtRep(fam,Zero(f)));;

  StoreExpMultFam(fun,group,fam);

  return fam;
end);

#############################################################################  
##
##  IsNearRingMultiplication( <G>, <mul> [, <what>] ). .test if <mul> is a 
##                                       nearring multiplication on <G>
##
##  This function tests if a specified multiplication function <mul> is a 
##  nearring multiplication on the group <G>.
##
##  <what> is an optional list of properties that the user knows to be true.
##  Properties in this are then not checked to speed up the construction
##  process. Possible properties are "closed", "ass" and "ldistr"
##  If no third argument is given all properties are checked.

IsNearRingMultiplication := function( arg )
  local G, mul, elms,
	test_closed, test_assoc, test_ldistr;

  if Length(arg) = 2 then
	test_closed := true;
	test_assoc  := true;
	test_ldistr := true;
	G := arg[1];
	mul := arg[2];
  elif Length(arg) = 3 then
	test_closed := not ("closed" in arg[3]);
	test_assoc  := not ("ass"    in arg[3]);
	test_ldistr := not ("ldistr" in arg[3]);
	G := arg[1];
	mul := arg[2];
  else
	Error("Usage: IsNearRingMultiplication( <G>, <mul> [,{''closed''|''ass''|''ldistr''}] )"  );
  fi;

  if not ( IsGroup( G ) and IsFunction( mul ) ) then
	Error("Usage: IsNearRingMultiplication( <G>, <mul> [,{''closed''|''ass''|''ldistr''}] )"  );
  fi;

  elms := Enumerator( G );
  
  if test_closed then
  # check if mul is G x G -> G
     Info( InfoNearRing, 1, "checking closedness");
     if not ForAll( elms, n1 -> ForAll( elms, n2 -> mul( n1, n2 ) in G ) )
	then
		Info( InfoWarning, 1,
		      "group is not closed underspecified multiplication" );
		return false;
     fi;
  fi;

  if test_assoc then  
  # check if mul is ASSOCIATIVE
     Info( InfoNearRing, 1, "checking associativity"); 
     if not ForAll( elms, n1 -> ForAll( elms, n2 -> ForAll( elms, n3 ->
		mul( n1, mul( n2, n3 ) ) = mul( mul( n1, n2 ), n3 ) ) ) ) 
	then
		Info( InfoWarning, 1,
		      "specified multiplication is not associative" );
		return false;
     fi;
  fi;

  if test_ldistr then
  # check if mul is LEFT DISTRIBUTIVE 
  # (note that the addition is denoted by '*' )
     Info( InfoNearRing, 1, "checking left distributivity");
     if not ForAll( elms, n1 -> ForAll( elms, n2 -> ForAll( elms, n3 ->
		mul( n1, n2 * n3 ) =  mul( n1, n2 ) * mul( n1, n3 )  ) ) )
	then
		Info( InfoWarning, 1,
		      "specified multiplication is not left distributive");
    		return false;
     fi;
  fi;

  # passed all tests
  return true;
end;

#############################################################################
##
#M  ExplicitMultiplicationNearRing		Constructor
##

InstallMethod(
	ExplicitMultiplicationNearRing,
	"generic",
	true,
	[IsGroup,IsFunction],
	0,
  function ( group , mult )
    if not IsNearRingMultiplication(group,mult) then
	Error("<mult> is not a valid multiplication");
    fi;
    return ExplicitMultiplicationNearRingNC( group, mult );
  end );

#############################################################################
##
#F  ExplicitMultiplicationNearRingNC		For EMNRs
##						multiplication is not tested

InstallGlobalFunction(
	ExplicitMultiplicationNearRingNC,
  function ( group , mult )
  local fam,N;
    fam := ExplicitMultiplicationNearRingElementFamily(group,mult);

    N := Objectify(
		NewType(CollectionsFamily(fam),
		IsExplicitMultiplicationNearRing and
			IsExplicitMultiplicationNearRingDefaultRep),
		rec() );

    SetNRMultiplication( N, mult );

    N!.elementsInfo := fam;

    SetGroupReduct( N, group );

    if HasSymbols( group ) then
	SetSymbols( N, Symbols( group ) );
    fi;
   
    return N;
end );

#############################################################################
##
#M  NearRingMultiplicationByOperationTable
##
##

InstallMethod(
	NearRingMultiplicationByOperationTable,
        "default",
	true,
	[IsGroup, IsMatrix, IsList],
	0,
  function( G, OT, elms )

  local	p, p_inv, mul,
	x, y;

    p := PermList( List( elms, x -> Position( AsSSortedList( G ), x ) ) );
    p_inv := p^-1;    

    mul := function( x, y ) 
  
      return AsSSortedList( G )[  
		( OT [Position( AsSSortedList( G ), x )^p]
				[Position( AsSSortedList( G ), y )^p] )
								   ^p_inv ]; 
    end;

  return mul;
end );

 
#############################################################################
##
#M  ViewObj				For EMNRs

InstallMethod(
	ViewObj,
	"EMNearRings",
	true,
	[IsExplicitMultiplicationNearRingDefaultRep],
	0,
  function ( N )
    Print("ExplicitMultiplicationNearRing ( ");
    View( GroupReduct(N) );
    Print( " , multiplication )");
  end );

InstallMethod(
	PrintObj,
	"EMNearRings",
	true,
	[IsExplicitMultiplicationNearRingDefaultRep],
	0,
  function ( N )
    Print("ExplicitMultiplicationNearRing ( ");
    View( GroupReduct(N) );
    Print( " , multiplication )");
  end );

#############################################################################
##
#M  ExtRepOfObj				EMNR element -> group element

InstallMethod(
	ExtRepOfObj,
	"EMNRDefaultRep->GroupElm",
	true,
	[IsExplicitMultiplicationNearRingElement and
		IsExplicitMultiplicationNearRingElementDefaultRep],
	0,
  function ( emnrelm )
    return emnrelm![1];
  end );

#############################################################################
##
#M  NearRingElementByGroupRep			Group element -> EMNRDefaultRep

InstallMethod(
	NearRingElementByGroupRep,
	"EMNRDefaultRep",
	true,
	[IsExplicitMultiplicationNearRingElementFamily,
		IsMultiplicativeElementWithInverse],
	0,
  function ( fam , grpelm )
  local obj;
    obj := [grpelm];
    Objectify(fam!.defaultKind,obj);
    return obj;
  end );

#############################################################################
##
#M  GroupElementRepOfNearRingElement	  EMNRDefaultRep -> Group Element

InstallMethod(
	GroupElementRepOfNearRingElement,
	"EMNRDefaultRep",
	true,
	[IsExplicitMultiplicationNearRingElement],
	0,
  function ( elm )
    return elm![1];
  end );

#############################################################################
##
#M  ViewObj				For EMNR elements

InstallMethod(
	ViewObj,
	"EMNRDefaultRep",
	true,
	[IsExplicitMultiplicationNearRingElementDefaultRep],
	0,
  function ( elm )
    Print("(",elm![1],")");
  end );

InstallMethod(
	PrintObj,
	"EMNRDefaultRep",
	true,
	[IsExplicitMultiplicationNearRingElementDefaultRep],
	0,
  function ( elm )
    Print("(",elm![1],")");
  end );

#############################################################################
##
#M  \=					For EMNR elements

InstallMethod(
	\=,
	"EMNRElementDefaultRep",
	IsIdenticalObj,
	[IsExplicitMultiplicationNearRingElementDefaultRep,
		IsExplicitMultiplicationNearRingElementDefaultRep],
	0,
  function ( a , b )
    return a![1] = b![1];
  end );

#############################################################################
##
#M  \<					For EMNR elements

InstallMethod(
	\<,
	"EMNRElementDefaultRep",
	IsIdenticalObj,
	[IsExplicitMultiplicationNearRingElementDefaultRep,
		IsExplicitMultiplicationNearRingElementDefaultRep],
	0,
  function ( a , b )
    return a![1] < b![1];
  end );

#############################################################################
##
#M  \+				For EMNR elements

InstallOtherMethod(
	\+,
	"EMNRElementsDefaultRep",
	IsIdenticalObj,
	[IsExplicitMultiplicationNearRingElementDefaultRep,
		IsExplicitMultiplicationNearRingElementDefaultRep],
	0,
  function ( a , b )
  local fam;
    fam := FamilyObj(a);
    return NearRingElementByGroupRep( fam, a![1]*b![1] );
  end );

#############################################################################
##
#M  AdditiveInverse				For EMNR elements

InstallOtherMethod(
	AdditiveInverse,
	"EMNRElementDefaultRep",
	true,
	[IsExplicitMultiplicationNearRingElementDefaultRep],
	0,
  function ( a )
    return NearRingElementByGroupRep( FamilyObj(a), a![1]^(-1) );
  end );

#############################################################################
##
#M  Zero				For EMNR elements

InstallOtherMethod(
	ZeroMutable,
	"EMNRElementDefaultRep",
	true,
	[IsExplicitMultiplicationNearRingElementDefaultRep],
	0,
  function ( a )
    return NearRingElementByGroupRep( FamilyObj(a), One(a![1]) );
  end );


#############################################################################
##
#M  \*					For EMNR elements

InstallMethod(
	\*,
	"EMNRElementDefaultRep",
	IsIdenticalObj,
	[IsExplicitMultiplicationNearRingElementDefaultRep,
		IsExplicitMultiplicationNearRingElementDefaultRep],
	0,
  function ( a , b )
  local fam;
    fam := FamilyObj(a);
    return NearRingElementByGroupRep( 
		fam, fam!.multiplication(a![1],b![1]) );
  end );

##############################################################################
##
#M  AsTransformationNearRing( < N > )		for ExplicitMultiplicationNRs
##

InstallMethod(
	AsTransformationNearRing,
	"for ExpMulNRs",
	true,
	[IsExplicitMultiplicationNearRing],
	0,
  function( N )
  local nc2,			# (N,+) + C_2, the group the tfms act on
	C2,			# C_2
	f,			# how it acts
	listOfGenerators,	# additive generators of the TrfmNR 
	NR,			# the TransformationNearRing
        cat,			# category of group reduct
	mul;

  if IsPermGroup( GroupReduct(N) ) then
    cat := IsPermGroup;
  else
    cat := IsPcGroup;
  fi;
  C2    := CyclicGroup( cat, 2 );
  nc2 := DirectProduct( GroupReduct(N), C2   );
  if HasName( GroupReduct(N) ) then
    SetName( nc2, Concatenation( Name( GroupReduct(N) )," x C_2" ) );
  fi;

  f := function( n, gamma )
    local projNgamma, prod;
      projNgamma := Image( Projection(nc2,1), gamma );    
      if Image( Projection(nc2,2), gamma ) = Identity(C2) then
        prod := NRMultiplication(N)( projNgamma, n );
      else
        prod := n;
      fi;
      return Image( Embedding(nc2,1), prod );
  end;

  listOfGenerators := List( 
	GeneratorsOfGroup( GroupReduct(N) ),
		gen -> MappingByFunction( nc2, nc2, x -> f( gen, x ) ) );

  NR := TransformationNearRingByAdditiveGenerators( nc2,
						    listOfGenerators );

  return NR;

  end ); 

#############################################################################
##
#M  Size			For nearrings with known additive group

InstallMethod(
	Size,
	"nearring with known add group",
	true,
	[IsTransformationNearRing],
	0,
  function ( nr )
    return Size(GroupReduct(nr));
  end );

InstallMethod(
	Size,
	"nearring with known add group",
	true,
	[IsExplicitMultiplicationNearRing],
	0,
  function ( nr )
    return Size(GroupReduct(nr));
  end );

#############################################################################
##
#M  AsList				For EMNRs

InstallMethod(
	AsList,
	"EMNearRings",
	true,
	[IsNearRing and IsExplicitMultiplicationNearRingDefaultRep],
	0,
  function ( N )
  local addGroup,fam;
    addGroup := GroupReduct(N);
    fam := N!.elementsInfo;
    return List(AsList(addGroup),e->NearRingElementByGroupRep(fam,e));
  end );

#############################################################################
##
#M  AsSSortedList				For EMNRs

InstallMethod(
	AsSSortedList,
	"EMNearrings",
	true,
	[IsExplicitMultiplicationNearRingDefaultRep],
	0,
  function ( N )
  local addGroup,fam;
    addGroup := GroupReduct(N);
    fam := N!.elementsInfo;
    return List(AsSSortedList(addGroup),e->NearRingElementByGroupRep(fam,e));
  end );
 
#############################################################################
##
#M  Enumerator			For ExpMulNRs

InstallMethod(
	Enumerator,
	true,
	[ IsExplicitMultiplicationNearRingDefaultRep ],
	0,
  function( nr )
    return Objectify( NewType( FamilyObj( nr ),
		IsList and IsExplicitMultiplicationNearRingEnumerator ),
           rec( groupReduct := GroupReduct(nr),
		elementsInfo := nr!.elementsInfo ) );
  end );

#####################################################################
##
#M  Length			for nearring enumerators

InstallMethod( Length,
	"for enumerator of explicit multiplication nearring",
	true,
	[ IsList and IsExplicitMultiplicationNearRingEnumerator ],
	0,
  e -> Size( e!.groupReduct ) );

#####################################################################
##
#M  \[\]			for nearring enumerators

InstallMethod(
	\[\],
	true,
	[ IsList and IsExplicitMultiplicationNearRingEnumerator, IsPosInt ],
	0,
  function( e, pos )
  local groupElement, group, fam;
    group := e!.groupReduct;
    fam := e!.elementsInfo;

    groupElement := Enumerator(group)[pos];

    return NearRingElementByGroupRep( fam, groupElement );
  end );

#####################################################################
##
#M  Position			for nearring enumerators

InstallMethod(
	Position,
	true,
	[ IsList and IsExplicitMultiplicationNearRingEnumerator,
		IsExplicitMultiplicationNearRingElementDefaultRep, IsZeroCyc ],
	0,
  function( e, emnrelm, zero )
    return Position(Enumerator(e!.groupReduct),emnrelm![1],zero);
  end );

#####################################################################
##
#M  ViewObj			for nearring enumerators
#
InstallMethod(
	ViewObj,
	true,
	[ IsExplicitMultiplicationNearRingEnumerator ],
	0,
  function( G )
    Print( "<enumerator of near ring>" );
  end );

#############################################################################
##
#M  SubNearRingBySubgroupNC
##
##  No Check!

InstallMethod(
	SubNearRingBySubgroupNC,
	"ExpMultNRs",
	true,
	[IsExplicitMultiplicationNearRing, IsGroup],
	0,
  function ( nr, sg )
  local fam, subnr;
    fam := nr!.elementsInfo;
    subnr := Objectify(
		NewType(CollectionsFamily(fam),
		IsExplicitMultiplicationNearRing and
			IsExplicitMultiplicationNearRingDefaultRep),
		rec() );
    SetNRMultiplication( subnr, NRMultiplication( nr ) );
    subnr!.elementsInfo := fam;

    SetGroupReduct( subnr, sg );
    SetParent( subnr, nr );

    return subnr;
  end );

#############################################################################
##
#M  InvariantSubNearRings
##

InstallMethod(
	InvariantSubNearRings,
	"ExpMultNRs",
	true,
	[IsNearRing and IsExplicitMultiplicationNearRing],
	0,
  function( N )
  local mul, sgps, g, m, invars, isinv, i;
    mul := NRMultiplication(N);
    sgps := Subgroups( GroupReduct(N) );
  
    invars := [];
    for g in sgps do
      isinv := true;
      i := 0;
      while i < Size(g) and isinv do
        i := i+1;
        m := Enumerator(g)[i];
        if ForAny( N, n -> not ( mul( m, GroupElementRepOfNearRingElement(n) ) in g ) or 
			   not ( mul( GroupElementRepOfNearRingElement(n), m ) in g ) ) then
	  isinv := false;
        fi;
      od;
      if isinv then Add( invars, g ); fi;
    od;

    return List( invars, inv -> SubNearRingBySubgroupNC( N, inv ) ); 
  end );

#############################################################################
##
#M  SubNearRings		for ExpMulNRs
##

InstallMethod(
	SubNearRings,
	"ExpMultNRs",
	true,
	[IsNearRing and IsExplicitMultiplicationNearRing],
	0,
  function( N )
  local mul, sgps, subnrs;
    mul := NRMultiplication(N);
    sgps := Subgroups( GroupReduct(N) );

    subnrs := Filtered( sgps,
	g -> ForAll( g, m -> ForAll( g, n -> mul(m,n) in g ) ) );
  
    return List( subnrs, sg -> SubNearRingBySubgroupNC( N, sg ) ); 
  end );

#############################################################################
##
#M  ZeroSymmetricPart		for ExpMulNRs
##

InstallMethod(
	ZeroSymmetricPart,
	"ExpMulNRs",
	true,
	[IsNearRing and IsExplicitMultiplicationNearRing],
	0,
  function ( N )
  local addGroup, zero, sg, n;
    addGroup := GroupReduct( N );
    zero := Identity( addGroup );
    sg := TrivialSubgroup( addGroup );
    for n in addGroup do
      if not ( n in sg ) and NRMultiplication(N)( zero, n ) = zero then
		sg := ClosureSubgroup( sg, n );
      fi;
    od;

    return SubNearRingBySubgroupNC( N, sg );
  end );

















