InstallMethod(
	GroupReduct,
	"rings",
	true,
	[IsRing],
	0,
  function ( R )
  local sortedElms;
    sortedElms := AsSSortedList( R );
    return GroupByMultiplicationTable( 
	List( sortedElms, x -> List( sortedElms, y ->
		Position( sortedElms, x + y ) ) ) );
  end );

