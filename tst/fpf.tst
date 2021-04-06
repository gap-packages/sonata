gap> START_TEST("fpf.tst");
gap> C9 := CyclicGroup( 9 );
<pc group of size 9 with 2 generators>
gap> a := GroupHomomorphismByFunction( C9, C9, x -> x^-1 );;
gap> phi := Group( a );;
gap> Size( phi );
2
gap> IsFpfAutomorphismGroup( phi, C9 );
true
gap> G := ElementaryAbelianGroup( 49 );;
gap> FpfAutomorphismGroupsMaxSize( G );
[ 48, 2 ]
gap> C15 := CyclicGroup( 15 );;
gap> FpfAutomorphismGroupsMaxSize( C15 );
[ 2, 1 ]
gap> aux := FpfAutomorphismGroupsMetacyclic( [3,3,5,5], 4, -1 ); 
[ [ [ [ f1, f2, f3, f4 ] -> [ f1^2, f2^3, f3*f4, f3*f4^2 ], 
          [ f1, f2, f3, f4 ] -> [ f2^4, f1, f4^2, f3 ] ] ], 
  <pc group of size 225 with 4 generators> ]
gap> phi := Group( aux[1][1] );
<group with 2 generators>
gap> G := aux[2];
<pc group of size 225 with 4 generators>
gap> FrobeniusGroup( phi, G );
<pc group of size 1800 with 7 generators>
gap> F := GF(5);;
gap> A := [[2,0],[0,3]]*One(F);
[ [ Z(5), 0*Z(5) ], [ 0*Z(5), Z(5)^3 ] ]
gap> IsFpfRepresentation( [A], F );
true
gap> DegreeOfIrredFpfRepCyclic( 5, 9 );
6
gap> DegreeOfIrredFpfRepMetacyclic( 5, 4, -1 );
2
gap> DegreeOfIrredFpfRep2( 7, 30, 11, -1 );    
8
gap> DegreeOfIrredFpfRep3( 5, 3, 1 );                                  
2
gap> DegreeOfIrredFpfRep4( 5, 3, 1, -1 );   
4
gap> aux := FpfRepresentationsCyclic( 5, 8 );
[ [ [ [ Z(5)^3, Z(5)^2 ], [ Z(5), Z(5) ] ], 
      [ [ Z(5)^2, Z(5) ], [ Z(5)^0, Z(5)^0 ] ] ], [ 1, 7 ] ]
gap> mats := aux[1];
[ [ [ Z(5)^3, Z(5)^2 ], [ Z(5), Z(5) ] ], 
  [ [ Z(5)^2, Z(5) ], [ Z(5)^0, Z(5)^0 ] ] ]
gap> indexlist := aux[2];
[ 1, 7 ]
gap> aux := FpfRepresentationsMetacyclic( 7, 4, -1 );
[ [ [ [ [ Z(7)^2, Z(7) ], [ Z(7), Z(7)^5 ] ], 
          [ [ 0*Z(7), Z(7)^3 ], [ Z(7)^0, 0*Z(7) ] ] ] ], [ 1 ] ]
gap> mats := aux[1];
[ [ [ [ Z(7)^2, Z(7) ], [ Z(7), Z(7)^5 ] ], 
      [ [ 0*Z(7), Z(7)^3 ], [ Z(7)^0, 0*Z(7) ] ] ] ]
gap> DegreeOfIrredFpfRep2( 11, 30, 11, -1 );
4
gap> aux := FpfRepresentations2( 11, 30, 11, -1 );
[ [ [ <block matrix of dimensions (2*2)x(2*2)>, 
          <block matrix of dimensions (2*2)x(2*2)>, 
          <block matrix of dimensions (2*2)x(2*2)> ], 
      [ <block matrix of dimensions (2*2)x(2*2)>, 
          <block matrix of dimensions (2*2)x(2*2)>, 
          <block matrix of dimensions (2*2)x(2*2)> ] ], [ 1, 13 ] ]
gap> aux := FpfRepresentations3( 5, 3, 1 );
[ [ [ [ [ Z(5), 0*Z(5) ], [ 0*Z(5), Z(5)^3 ] ], 
          [ [ 0*Z(5), Z(5)^2 ], [ Z(5)^0, 0*Z(5) ] ], 
          [ [ Z(5)^3, Z(5)^0 ], [ Z(5), Z(5)^0 ] ] ] ], [ 1 ] ]
gap> aux := FpfRepresentations4( 7, 3, 1, -1 );
[ [ [ [ [ Z(7)^2, Z(7) ], [ Z(7), Z(7)^5 ] ], 
          [ [ 0*Z(7), Z(7)^3 ], [ Z(7)^0, 0*Z(7) ] ], 
          [ [ Z(7)^2, 0*Z(7) ], [ Z(7)^0, Z(7)^4 ] ], 
          [ [ Z(7)^5, Z(7) ], [ Z(7), Z(7)^2 ] ] ], 
      [ [ [ Z(7)^2, Z(7) ], [ Z(7), Z(7)^5 ] ], 
          [ [ 0*Z(7), Z(7)^3 ], [ Z(7)^0, 0*Z(7) ] ], 
          [ [ Z(7)^2, 0*Z(7) ], [ Z(7)^0, Z(7)^4 ] ], 
          [ [ Z(7)^2, Z(7)^4 ], [ Z(7)^4, Z(7)^5 ] ] ] ], 
  [ [ 1, 1 ], [ -1, 1 ] ] ]
gap> aux := FpfAutomorphismGroupsCyclic( [25,5], 4 ); 
[ [ [ f1, f3 ] -> [ f1^2*f2, f3^2 ], [ f1, f3 ] -> [ f1^2*f2, f3^3 ] ], 
  <pc group of size 125 with 2 generators> ]
gap> as := aux[1];
[ [ f1, f3 ] -> [ f1^2*f2, f3^2 ], [ f1, f3 ] -> [ f1^2*f2, f3^3 ] ]
gap> G := aux[2];
<pc group of size 125 with 2 generators>
gap> aux := FpfAutomorphismGroupsMetacyclic( [3,3,5,5], 4, -1 );
[ [ [ [ f1, f2, f3, f4 ] -> [ f1^2, f2^3, f3*f4, f3*f4^2 ], 
          [ f1, f2, f3, f4 ] -> [ f2^4, f1, f4^2, f3 ] ] ], 
  <pc group of size 225 with 4 generators> ]
gap> fs := aux[1];
[ [ [ f1, f2, f3, f4 ] -> [ f1^2, f2^3, f3*f4, f3*f4^2 ], 
      [ f1, f2, f3, f4 ] -> [ f2^4, f1, f4^2, f3 ] ] ]
gap> phi := Group( fs[1] );
<group with 2 generators>
gap> G := aux[2];
<pc group of size 225 with 4 generators>
gap> aux := FpfAutomorphismGroupsMetacyclic( [7,7,17,17], 8, -1 );;
gap> fs := aux[1];
[ [ [ f1, f2, f3, f4 ] -> [ f1^9, f2^2, f3^4*f4^2, f3*f4^6 ], 
      [ f1, f2, f3, f4 ] -> [ f2^16, f1, f3^4*f4^5, f3^5*f4^3 ] ], 
  [ [ f1, f2, f3, f4 ] -> [ f1^9, f2^2, f3^3*f4^5, f3^6*f4 ], 
      [ f1, f2, f3, f4 ] -> [ f2^16, f1, f3^3*f4^4, f3*f4^4 ] ] ]
gap> phis := List( fs, Group );
[ <group with 2 generators>, <group with 2 generators> ]
gap> G := aux[2];
<pc group of size 14161 with 4 generators>
gap> aux := FpfAutomorphismGroups2( [11,11,11,11], 30, 11, -1 );
[ [ [ [ f1, f2, f3, f4 ] -> [ f1^5*f2^4, f1^3*f2^10, f3^2*f4^8, f3^6*f4 ], 
          [ f1, f2, f3, f4 ] -> [ f1^3*f2^10, f1^10*f2^8, f3^8*f4, f3*f4^3 ], 
          [ f1, f2, f3, f4 ] -> [ f3^10, f4^10, f1, f2 ] ] ], 
  <pc group of size 14641 with 4 generators> ]
gap> phi := Group( aux[1][1] );
<group with 3 generators>
gap> G := aux[2];
<pc group of size 14641 with 4 generators>
gap> aux := FpfAutomorphismGroups3( [5,5], 3, 1 ); 
[ [ [ [ f1, f2 ] -> [ f1^2, f2^3 ], [ f1, f2 ] -> [ f2^4, f1 ], 
          [ f1, f2 ] -> [ f1^3*f2, f1^2*f2 ] ] ], 
  <pc group of size 25 with 2 generators> ]
gap> phi := Group( aux[1][1] );
<group with 3 generators>
gap> G := aux[2];
<pc group of size 25 with 2 generators>
gap> aux := FpfAutomorphismGroups4( [7,7], 3, 1, -1 );
[ [ [ [ f1, f2 ] -> [ f1^2*f2^3, f1^3*f2^5 ], [ f1, f2 ] -> [ f2^6, f1 ], 
          [ f1, f2 ] -> [ f1^2, f1*f2^4 ], 
          [ f1, f2 ] -> [ f1^5*f2^3, f1^3*f2^2 ] ] ], 
  <pc group of size 49 with 2 generators> ]
gap> phi := Group( aux[1][1] );
<group with 4 generators>
gap> G := aux[2];
<pc group of size 49 with 2 generators>
gap> IsPairOfDicksonNumbers( 5, 4 );
true
gap> DicksonNearFields( 5, 4 );
[ ExplicitMultiplicationNearRing ( <pc group of size 625 with 
    4 generators> , multiplication ), 
  ExplicitMultiplicationNearRing ( <pc group of size 625 with 
    4 generators> , multiplication ) ]
gap> NumberOfDicksonNearFields( 5, 4 );
2
gap> ExceptionalNearFields( 25 );
[ ExplicitMultiplicationNearRing ( <pc group of size 25 with 
    2 generators> , multiplication ) ]
gap> AllExceptionalNearFields();
[ ExplicitMultiplicationNearRing ( <pc group of size 25 with 
    2 generators> , multiplication ), 
  ExplicitMultiplicationNearRing ( <pc group of size 49 with 
    2 generators> , multiplication ), 
  ExplicitMultiplicationNearRing ( <pc group of size 121 with 
    2 generators> , multiplication ), 
  ExplicitMultiplicationNearRing ( <pc group of size 121 with 
    2 generators> , multiplication ), 
  ExplicitMultiplicationNearRing ( <pc group of size 529 with 
    2 generators> , multiplication ), 
  ExplicitMultiplicationNearRing ( <pc group of size 841 with 
    2 generators> , multiplication ), 
  ExplicitMultiplicationNearRing ( <pc group of size 3481 with 
    2 generators> , multiplication ) ]
gap> C7 := CyclicGroup( 7 );;
gap> i := GroupHomomorphismByFunction( C7, C7, x -> x^-1 );;
gap> phi := Group( i );;
gap> orbs := Orbits( phi, C7 );
[ [ <identity> of ... ], [ f1, f1^6 ], [ f1^2, f1^5 ], [ f1^3, f1^4 ] ]
gap> reps := [orbs[2][1], orbs[3][2]];
[ f1, f1^5 ]
gap> n := PlanarNearRing( C7, phi, reps );;
gap> C7 := CyclicGroup( 7 );;
gap> i := GroupHomomorphismByFunction( C7, C7, x -> x^-1 );;
gap> phi := Group( i );;
gap> reps := OrbitRepresentativesForPlanarNearRing( C7, phi, 2 );
[ [ f1, f1^2 ], [ f1, f1^5 ] ]
gap> n1 := PlanarNearRing( C7, phi, reps[1] );;
gap> n2 := PlanarNearRing( C7, phi, reps[2] );;
gap> IsIsomorphicNearRing( n1, n2 );
false
gap> points := [1..7];;
gap> blocks := [[1,2,3],[1,4,5],[1,6,7],[2,4,7],[2,5,6],[3,5,7],       
>                                                          [3,4,6]];;  
gap> D := DesignFromPointsAndBlocks( points, blocks );                 
<an incidence structure with 7 points and 7 blocks>
gap> M := [[1,0,1,1],
>          [1,1,0,0],
>          [1,1,1,0]];;
gap> DesignFromIncidenceMat( M ); 
<an incidence structure with 3 points and 4 blocks>
gap> n := LibraryNearRing( GTW9_2, 90 );
LibraryNearRing(9/2, 90)
gap> IsPlanarNearRing( n );
true
gap> D1 := DesignFromPlanarNearRing( n, "*" );
<a 2 - ( 9, 4, 3 ) nearring generated design>
gap> D2 := DesignFromPlanarNearRing( n, " " );
<a 2 - ( 9, 5, 5 ) nearring generated design>
gap> aux := FpfAutomorphismGroupsCyclic( [3,3], 4 );
[ [ [ f1, f2 ] -> [ f1*f2, f1*f2^2 ] ], 
  <pc group of size 9 with 2 generators> ]
gap> f := aux[1][1];
[ f1, f2 ] -> [ f1*f2, f1*f2^2 ]
gap> phi := Group( f );;
gap> IsCyclic(phi);
true
gap> G := aux[2]; 
<pc group of size 9 with 2 generators>
gap> D3 := DesignFromFerreroPair( G, phi, "*" );
<a 2 - ( 9, 4, 3 ) nearring generated design>
gap> # D3 is actually isomorphic to D1
gap> n := LibraryNearRing( GTW9_1, 202 );
LibraryNearRing(9/1, 202)
gap> IsWdNearRing( n );
true
gap> DesignFromWdNearRing( n );
<a 1 - ( 9, 5, 10 ) nearring generated design>
gap> D1;
<a 2 - ( 9, 4, 3 ) nearring generated design>
gap> PointsOfDesign( D1 );;
gap> Length( BlocksOfDesign( D1 ) );
18
gap> BlocksOfDesign( D1 ){[3]};;
gap> DesignParameter( D1 );
[ 2, 9, 18, 8, 4, 3 ]
gap> # t = 2, v = 9, b = 18, r = 8, k = 4, lambda = 3
gap> M1 := IncidenceMat( D1 );
[ [ 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1 ], 
  [ 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1 ], 
  [ 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0 ], 
  [ 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1 ], 
  [ 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1 ], 
  [ 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0 ], 
  [ 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0 ], 
  [ 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0 ], 
  [ 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0 ] ]
gap> PrintIncidenceMat( D1 );
..1.1.1..1.11..1.1
1...1..11..1.11..1
1.1....1.11..1.11.
1..1.1..1.1.1..1.1
.11..11...1..11..1
.1.11.1.1....1.11.
1..1.11..1.1..1.1.
.11..1.11..11...1.
.1.11..1.11.1.1...
gap> BlockIntersectionNumbers( D1, 2 );
[ 0, 4, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 2, 2, 1, 2, 1 ]
gap> # the second has empty intersection with the first block
gap> # and intersects all others in at most 2 points
gap> IsCircularDesign( D1 );
true
gap> IsPointIncidentBlock( D1, 3, 1 );
true
gap> # point 3 is incident with block 1
gap> IsPointIncidentBlock( D1, 3, 2 );       
false
gap> PointsIncidentBlocks( D1, [1, 4] );                 
[ 4, 7 ]
gap> # block 1 and block 4 are together incident with 
gap> # points 4 and 7
gap> BlocksIncidentPoints( D1, [2, 7] );   
[ 1, 12, 15 ]
gap> # point 2 and point 7 are together incident with     
gap> # blocks 1, 12, 15
gap> BlocksOfDesign( D1 ){last};
[ [ ((4,5,6)), ((4,6,5)), ((1,2,3)), ((1,3,2)) ], 
  [ ((1,3,2)), ((1,3,2)(4,5,6)), (()), ((4,5,6)) ], 
  [ ((1,3,2)(4,6,5)), ((1,3,2)), ((4,5,6)), ((1,2,3)(4,5,6)) ] ]
gap> # the actual point sets of blocks 1, 12, and 15 
gap> BlocksIncidentPoints( D1, [2, 3, 7] );
[ 1 ]
gap> # points 2, 3, 7 are together incident with block 1
gap> PointsIncidentBlocks( D1, [1] );
[ 2, 3, 4, 7 ]
gap> # block 1 is incident with points 2, 3, 4, 7 
gap> STOP_TEST( "fpf.tst", 10000);
