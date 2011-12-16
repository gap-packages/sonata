#############################################################################
##
#F  RandomClosure( <G>, <fam>, <e> )
##
##	<G>: group the nearring is acting on as a transformation nearring
##	<fam>: the family of the elements of the nearring
##	<e>: a record with the components
##		p : the result has probability 1/p to be incorrect
##		size : if the size of the nearring is known it can
##			be given to the function which produces a
##			correct result in this case
##
##	Note: only one of p and size has to be set via the function
##		SetErrorProbability

RandomClosure := function ( group, fam, e )
local i, m, badp, p, size, addGens;

  if IsBound( e.size ) then
	size := e.size;
	p := 10^100;
  else
	size := Size(fam!.group)^Size(fam!.group);
	p := e.p;
  fi;

  Info( InfoNearRing, 2, "actual size: ",Size(group) );

  addGens := GeneratorsOfGroup( group );
  i := 0; 
  badp := Length(addGens)/(Length(addGens)-1);
  while badp^i < p  and Size(group) < size do
#    Print("->",QuotientRemainder(NumeratorRat(badp^i),DenominatorRat(badp^i))[1],"\n");
    i := i + 1;
    Info( InfoNearRing, 2, "pass: ", i );
    m := GroupElementRepOfNearRingElement(
	NearRingElementByGroupRep( fam, Random(group) ) *
	NearRingElementByGroupRep( fam, Random( GeneratorsOfGroup(group) ) )
    );
    if not m in group then
	return RandomClosure( ClosureGroup( group, m ), fam, e );
    fi;
  od;

  return group;
end;

#############################################################################
##
#M  GroupReduct( <TfmNR> )		random method
##

InstallMethod(
	GroupReduct,
	"random",
	true,
	[IsNearRing and IsTransformationNearRing and HasErrorProbability],
	0,
  function ( nr )
  local addGroup, id;

  if ErrorProbability( nr ) = 0 then
	TryNextMethod();
  fi;

  id := GroupElementRepOfNearRingElement( GeneratorsOfNearRing(nr)[1] )^0;
  addGroup := Group( List( GeneratorsOfNearRing(nr), GroupElementRepOfNearRingElement ), id );

  return RandomClosure( addGroup, nr!.elementsInfo, ErrorProbability( nr ) );
  end );











