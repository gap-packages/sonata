BindGlobal( "ClearGreen",
  function( latt )
  local lev, class, vert, sel;
    sel := Selected( latt );
    for lev in Levels( latt ) do
      for class in Classes( latt, lev ) do
        for vert in Vertices( latt, lev, class ) do
	  if not( vert in sel ) then
		Recolor( latt, vert, COLORS.black );
	  fi;
	od;
      od;
    od;
end );
 
InstallMethod(
	ChooseShape,
	"NRIs",
	true,
	[IsGraphicPosetRep and IsGraphicIdealLattice,IsRecord],
	0,
  function( latt, data )
  local ideal;
    ideal := data.ideal;
    if HasIsNearRingIdeal( ideal ) and 
	IsNearRingIdeal( ideal ) then
		return "circle"; 		# two-sided ideal
    elif HasIsNearRingLeftIdeal( ideal ) and
	IsNearRingLeftIdeal( ideal ) then
		return "diamond";		# left ideal
    else
		return "rectangle";
    fi;
end );

InstallMethod(
	ChooseLevel,
	"NRIs",
	true,
	[IsGraphicPosetRep and IsGraphicIdealLattice, IsRecord],
	0,
  function( latt, data )
    return Size( data.ideal );
end );

InstallMethod( 
	GraphicIdealLattice,
	true,
	[IsNearRing,IsString],
	0,
  function ( nr, which )
  local latt, v, levels, vertices, i, j, union, 
	ups, newups, n, newvertex, ideals, I, funcclose;

    latt := GraphicPoset( "", 800, 600 );

    SetFilterObj( latt, IsGraphicIdealLattice );
    # which ideals should be shown ?
    ideals := [];
    if 'r' in which then
	SetTitle( latt, "Computing right ideals ..." );
	ideals := NearRingRightIdeals( nr );
    fi;
    if 'l' in which then
	SetTitle( latt, "Computing left ideals ..." );
	ideals := Union( ideals, NearRingLeftIdeals( nr ) );
    fi;
    if 'i' in which and not( 'l' in which or 'r' in which ) then	
	SetTitle( latt, "Computing two-sided ideals ..." );
	ideals := NearRingIdeals( nr );
    fi;

    if ideals=[] then
	Error( "no ideal type selected, select 'l', 'r' or 'i'" );
    fi;

    SetTitle( latt, "Computing covers ..." );

    n := Length(ideals);

    ups := List( [1..n], i -> Filtered( [1..n], j -> 
	RemInt( Size(ideals[j]), Size(ideals[i]) ) = 0 and 
	j<>i and 
	IsSubset( ideals[j], ideals[i] ) ) );
    for i in [1..n] do
      union := [];
      for j in ups[i] do
	union := Union( union, ups[j] );
        ups[i] := Difference( ups[i], union );
      od;
    od;

    if HasName( nr ) then
	    SetTitle( latt, 
		Concatenation( "Ideal lattice of ", Name(nr) ) );
    else
	    SetTitle( latt,
		"Ideal lattice of nearring" );
    fi;

    i := 0; vertices := [];
    for I in ideals do
	i := i+1;
	CreateLevel( latt, Size(I) );
	Add( vertices, Vertex( latt, rec( ideal := I ), 
			rec( label := String( i )
			) ) ); 
    od;
    for i in [1..n] do
      for j in ups[i] do
	Edge( latt, vertices[i], vertices[j] );
      od;
    od;

    Menu( latt, "Ideals", 
	[
	"Intersection",
	"Closure",
	"Commutator",
	"Covers",
	"Subcovers",
	"Ideal type",
	], 
	[
	"forsubset",
	"forsubset",
	"forsubset",
	"forone",
	"forone",
	"forsubset"
	], 
	[
	# Intersection
	function(arg)
	  local sel, C, latt;
	    latt := arg[1];
	    sel := Selected( latt );
	    C := Intersection( List( sel, I -> I!.data.ideal ) );
	    C := WhichVertex( latt, C, 
		function( N, R ) return R.ideal = N; end ); 
	    ClearGreen( latt );
	    Recolor( latt, C, COLORS.green ); 
	end,

	# Closure
	function(arg)
	  local sel, I, C, latt;
	    latt := arg[1];
	    sel := Selected( latt );
	    sel := List( sel, I -> I!.data.ideal );
	    C := sel[1];
	    for I in sel{[2..Length(sel)]} do
	      C := ClosureNearRingIdeal( C, I );
	    od;
	    C := WhichVertex( latt, C, 
		function( N, R ) return R.ideal = N; end ); 
	    ClearGreen( latt );
	    Recolor( latt, C, COLORS.green ); 
	end,

	# Commutator
	function(arg) 
	  local sel, I, J, C, latt;
	    latt := arg[1];
	    sel := Selected( latt );
	    if Length( sel )=1 then
		I := sel[1]; J := sel[1];
	    elif Length( sel )=2 then
	        I := sel[1]; J := sel[2];
	    else
		return;
	    fi;
	    C := NearRingCommutator(I!.data.ideal,J!.data.ideal);
	    C := WhichVertex( latt, C, 
		function( N, R ) return R.ideal = N; end ); 
	    ClearGreen( latt );
	    Recolor( latt, C, COLORS.green ); 
	end,

	# Covers
	function(arg)
	  local V, I, Cs, latt;
	    latt := arg[1];
	    I := Selected(latt)[1];
	    Cs := MaximalIn( latt, I );
	    ClearGreen( latt );
	    for V in Cs do
	      Recolor( latt, V, COLORS.green ); 
	    od;
	end,

	# Subcovers
	function(arg)
	  local V, I, Cs, latt;
	    latt := arg[1];
	    I := Selected(latt)[1];
	    Cs := Maximals( latt, I );
	    ClearGreen( latt );
	    for V in Cs do
	      Recolor( latt, V, COLORS.green ); 
	    od;
	end,

	# Ideal type
	function(arg)
	local latt, sel, vert;
	  latt := arg[1];
	  sel := Selected( latt ); 
	  for vert in sel do
	    if IsNearRingIdeal( vert!.data.ideal ) then
		Reshape( latt, vert, "circle" );
            fi;
	  od;
	end
	] );

  # close text selector
  funcclose := function( sel, bt )
    Close(sel);
    return true;  
  end;

    InstallPopup( latt,
	function( sheet, vert, x, y )
	local id, text;
#          text := Concatenation( "LibraryNearRing( ",Name(id[1]),", ",String(id[2])," )");
	  sheet!.infobox := TextSelector( 
	Concatenation( "Information on near ring ideal ",vert!.label ), 
	[ 
	  "Size : ",
	function(x,y) 
	  Relabel( x, 1, Concatenation( "Size : ", String(Size(vert!.data.ideal)) ) );
	  return Size(vert!.data.ideal); 
	end,

	  "twosided ideal : ",
	function(x,y)
	  Relabel( x, 2, Concatenation( "twosided ideal : ",
		String( IsNearRingIdeal( vert!.data.ideal ) ) ) );
	  if IsNearRingIdeal( vert!.data.ideal ) then
		Reshape( latt, vert, "circle" );
          fi;
	  return IsNearRingIdeal( vert!.data.ideal );
	end,

	  "Factor isomorphic to",
	function(x,y)
	local factor, id; 
	  factor := FactorNearRing( Parent(vert!.data.ideal),
					vert!.data.ideal );
	  if Size(factor) > 15 then
		return factor;
	  else
		id := IdLibraryNearRing( factor );
		Relabel( x, 3, Concatenation( "Factor isomorphic to ", 
		"LibraryNearRing( ",Name(id[1]),", ",String(id[2])," )") );
		return factor;
	  fi;
	end,

	  "Export ideal to GAP", 
	function(x,y) return vert!.data.ideal; end
	], 

	[ "close", funcclose ] );
	end
	);
    return latt;
end);

InstallMethod(
	CompareLevels,
	true,
	[ IsGraphicPosetRep and IsGraphicIdealLattice, IsInt, IsInt ],
	0,
  function( poset, a, b )
    if a < b then
	return 1;
    elif a > b then
	return -1;
    else
	return 0;
    fi;
  end);

