gap> START_TEST("tfms.tst");
gap> G := GTW4_2;
4/2
gap> t1 := EndoMappingByPositionList ( G, [1, 2, 4, 4] );
<mapping: 4/2 -> 4/2 >
gap> t2 := EndoMappingByFunction ( GTW8_2, g -> g^-1 );
<mapping: 8/2 -> 8/2 >
gap> IsGroupHomomorphism ( t2 );
true
gap> t3 := EndoMappingByFunction ( GTW6_2, g -> g^-1 );
<mapping: 6/2 -> 6/2 >
gap> IsGroupHomomorphism ( t3 );
false
gap> G1 := Group ((1,2,3), (1, 2));
Group([ (1,2,3), (1,2) ])
gap> G2 := Group ((2,3,4), (2, 3));
Group([ (2,3,4), (2,3) ])
gap> f1 := IsomorphismGroups ( G1, G2 );
[ (1,2,3), (1,2) ] -> [ (2,3,4), (2,3) ]
gap> f2 := IsomorphismGroups ( G2, G1 );
[ (2,3,4), (2,3) ] -> [ (1,2,3), (1,2) ]
gap> AsEndoMapping ( CompositionMapping ( f1, f2 ) );
<mapping: Group( [ (2,3,4), (2,3) ] ) -> Group( [ (2,3,4), (2,3) ] ) >
gap> m := IdentityEndoMapping ( GTW6_2 );
<mapping: 6/2 -> 6/2 >
gap> AsGroupGeneralMappingByImages ( m );
[ (1,2), (1,2,3) ] -> [ (1,2), (1,2,3) ]
gap> IsEndoMapping ( InnerAutomorphisms ( GTW6_2 ) [3] );
true
gap> AsList ( UnderlyingRelation ( IdentityEndoMapping ( Group ((1,2,3,4)) ) ) );
[ DirectProductElement( [ (), () ] ), 
  DirectProductElement( [ (1,2,3,4), (1,2,3,4) ] ), 
  DirectProductElement( [ (1,3)(2,4), (1,3)(2,4) ] ), 
  DirectProductElement( [ (1,4,3,2), (1,4,3,2) ] ) ]
gap> C3 := CyclicGroup (3);;
gap> m := ConstantEndoMapping (C3, AsSortedList (C3) [2]);;
gap> List (AsList (C3), x -> Image (m, x));
[ f1, f1, f1 ]
gap> IsIdentityEndoMapping (EndoMappingByFunction ( 
> AlternatingGroup ( [1..5] ), x -> x^31));
true
gap> C3 := CyclicGroup ( 3 );;
gap> IsConstantEndoMapping ( EndoMappingByFunction ( C3,  x -> x^3 ));
true
gap> G := Group ( (1,2,3), (1,2) );
Group([ (1,2,3), (1,2) ])
gap> IsDistributiveEndoMapping ( EndoMappingByFunction ( G, x -> x^3));
false
gap> IsDistributiveEndoMapping ( EndoMappingByFunction ( G, x -> x^7));
true
gap> t1 := ConstantEndoMapping ( GTW2_1, ());
MappingByFunction( 2/1, 2/1, function( x ) ... end )
gap> t2 := ConstantEndoMapping (GTW2_1, (1, 2));
MappingByFunction( 2/1, 2/1, function( x ) ... end )
gap> List ( AsList ( GTW2_1 ), x -> Image ( t1 * t2, x ));
[ (1,2), (1,2) ]
gap> G := SymmetricGroup ( 3 );
Sym( [ 1 .. 3 ] )
gap> invertingOnG := EndoMappingByFunction ( G, x -> x^-1 );
<mapping: SymmetricGroup( [ 1 .. 3 ] ) -> SymmetricGroup( [ 1 .. 3 ] ) >
gap> identityOnG := IdentityEndoMapping (G);
<mapping: SymmetricGroup( [ 1 .. 3 ] ) -> SymmetricGroup( [ 1 .. 3 ] ) >
gap> AsSortedList ( G );
[ (), (2,3), (1,2), (1,2,3), (1,3,2), (1,3) ]
gap> List ( AsSortedList (G), 
>              x -> Image ( identityOnG * invertingOnG, x ));
[ (), (2,3), (1,2), (1,3,2), (1,2,3), (1,3) ]
gap> List ( AsSortedList (G),
>              x -> Image ( identityOnG + invertingOnG, x ));
[ (), (), (), (), (), () ]
gap> IsIdentityEndoMapping ( - invertingOnG );
true
gap> - invertingOnG = identityOnG;
true
gap> G := SymmetricGroup ( 3 );
Sym( [ 1 .. 3 ] )
gap> m := ConstantEndoMapping (G, (1,2,3)) + IdentityEndoMapping( G );
MappingByFunction( Sym( [ 1 .. 3 ] ), Sym( [ 1 .. 3 ] ), function( g ) ... end\
 )
gap> PrintArray( AsSortedList( GraphOfMapping( m ) ) );
[ [       (),  (1,2,3) ],
  [    (2,3),    (1,3) ],
  [    (1,2),    (2,3) ],
  [  (1,2,3),  (1,3,2) ],
  [  (1,3,2),       () ],
  [    (1,3),    (1,2) ] ]
gap> g := AlternatingGroup ( 4 );
Alt( [ 1 .. 4 ] )
gap> AsSortedList ( g );
[ (), (2,3,4), (2,4,3), (1,2)(3,4), (1,2,3), (1,2,4), (1,3,2), (1,3,4), 
  (1,3)(2,4), (1,4,2), (1,4,3), (1,4)(2,3) ]
gap> t := EndoMappingByPositionList ( g, [1,3,4,5,2,1,1,1,1,1,1,1] );
<mapping: AlternatingGroup( [ 1 .. 4 ] ) -> AlternatingGroup( [ 1 .. 4 ] ) >
gap> m := TransformationNearRingByGenerators ( g, [t] );
TransformationNearRingByGenerators(
[ 
  <mapping: AlternatingGroup( [ 1 .. 4 ] ) -> AlternatingGroup( [ 1 .. 4 ] ) > 
 ])
gap> Size (m); # may take a few moments
20736
gap> IsCommutative ( m );
false
gap> G := SymmetricGroup(3);;
gap> endos := Endomorphisms ( G );;
gap> Endo := TransformationNearRingByAdditiveGenerators ( G, endos );
< transformation nearring with 10 generators >
gap> Size ( Endo );
54
gap> m := MapNearRing ( GTW32_12 );
TransformationNearRing(32/12)
gap> Size ( m );
1461501637330902918203684832716283019655932542976
gap> NearRingIdeals ( m );
[ < nearring ideal >, < nearring ideal > ]
gap> g := CyclicGroup ( 4 );
<pc group of size 4 with 2 generators>
gap> m := MapNearRing ( g );
TransformationNearRing(<pc group of size 4 with 2 generators>)
gap> gens := Filtered ( AsList ( m ), 
>       f -> IsFullTransformationNearRing ( 
>               TransformationNearRingByGenerators ( g, [ f ] )));;
gap> Length(gens);
12
gap> P := PolynomialNearRing ( GTW16_6 );
PolynomialNearRing( 16/6 )
gap> Size ( P );
256
gap> ES4 := EndomorphismNearRing ( SymmetricGroup ( 4 ) );
EndomorphismNearRing( Sym( [ 1 .. 4 ] ) )
gap> Size ( ES4 );
927712935936
gap> A := AutomorphismNearRing ( DihedralGroup ( 8 ) );
AutomorphismNearRing( <pc group of size 8 with 3 generators> )
gap> Length(NearRingRightIdeals ( A ));
28
gap> Size (A);
32
gap> I := InnerAutomorphismNearRing ( AlternatingGroup ( 4 ) );
InnerAutomorphismNearRing( Alt( [ 1 .. 4 ] ) )
gap> Size ( I );
3072
gap> m := Enumerator( I )[1000];
<mapping: AlternatingGroup( [ 1 .. 4 ] ) -> AlternatingGroup( [ 1 .. 4 ] ) >
gap> graph := List ( AsSet ( AlternatingGroup ( 4 ) ),
> x -> [x, Image (m, x)] );
[ [ (), () ], [ (2,3,4), (1,2,3) ], [ (2,4,3), (2,3,4) ], 
  [ (1,2)(3,4), (1,2)(3,4) ], [ (1,2,3), (1,2,4) ], [ (1,2,4), (1,4,2) ], 
  [ (1,3,2), (1,3,4) ], [ (1,3,4), (1,3,2) ], [ (1,3)(2,4), (1,3)(2,4) ], 
  [ (1,4,2), (1,4,3) ], [ (1,4,3), (2,4,3) ], [ (1,4)(2,3), (1,4)(2,3) ] ]
gap> autos := Automorphisms ( GTW8_4 );;
gap> C := CentralizerNearRing ( GTW8_4, autos );
CentralizerNearRing( 8/4, ... )
gap> C0 := ZeroSymmetricPart ( C );
< transformation nearring with 4 generators >
gap> Size ( C0 );
32
gap> Is := NearRingIdeals ( C0 );
[ < nearring ideal >, < nearring ideal >, < nearring ideal >, 
  < nearring ideal >, < nearring ideal >, < nearring ideal >, 
  < nearring ideal >, < nearring ideal >, < nearring ideal >, 
  < nearring ideal >, < nearring ideal >, < nearring ideal >, 
  < nearring ideal > ]
gap> G := GTW16_8;
16/8
gap> U := First ( NormalSubgroups ( G ),
>              x -> Size (x) = 2 );
Group([ (1,5)(2,10)(3,11)(4,12)(6,15)(7,16)(8,9)(13,14) ])
gap> HGU := RestrictedEndomorphismNearRing (G, U);
RestrictedEndomorphismNearRing( 16/8, Group(
[ (1,5)(2,10)(3,11)(4,12)(6,15)(7,16)(8,9)(13,14) ]) )
gap> Size (HGU);
8
gap> IsDistributiveNearRing ( HGU );
true
gap> Filtered ( AsList ( HGU),
>       x -> x = x * x );
[ <mapping: 16/8 -> 16/8 > ]
gap> P := PolynomialNearRing ( GTW8_5 );
PolynomialNearRing( 8/5 )
gap> L := LocalInterpolationNearRing ( P, 2 );
LocalInterpolationNearRing( PolynomialNearRing( 8/5 ), 2 )
gap> Size ( L ) / Size ( P );
16
gap> Gamma ( PolynomialNearRing ( CyclicGroup ( 25 ) ) );
<pc group of size 25 with 2 generators>
gap> IsCyclic (last);
true
gap> L := LibraryNearRing (GTW8_3, 12);
LibraryNearRing(8/3, 12)
gap> Lt := AsTransformationNearRing ( L );
< transformation nearring with 3 generators >
gap> Gamma ( Lt );
8/3 x C_2
gap> P := PolynomialNearRing ( GTW4_2 );
PolynomialNearRing( 4/2 )
gap> n := AsExplicitMultiplicationNearRing ( P );
ExplicitMultiplicationNearRing ( Group([ (1,2)(5,6)(9,10)(13,14), (3,4)(7,8)
  (11,12)(15,16), (7,8)(9,10)(13,14)(15,16) ]) , multiplication )
gap> G := SymmetricGroup ( 4 );
Sym( [ 1 .. 4 ] )
gap> V := Group([ (1,4)(2,3), (1,3)(2,4) ]);
Group([ (1,4)(2,3), (1,3)(2,4) ])
gap> P := InnerAutomorphismNearRing ( G );
InnerAutomorphismNearRing( Sym( [ 1 .. 4 ] ) )
gap> N := NoetherianQuotient ( P, V, G );
NoetherianQuotient( Group([ (1,4)(2,3), (1,3)(2,4) ]) ,Sym( [ 1 .. 4 ] ) )
gap> Size ( P ) / Size ( N );
54
gap> G := GTW8_4;
8/4
gap> P := PolynomialNearRing (G);
PolynomialNearRing( 8/4 )
gap> A := TrivialSubgroup (G);;
gap> B := DerivedSubgroup (G);
Group([ (1,3)(2,4) ])
gap> C := G;
8/4
gap> I := CongruenceNoetherianQuotient (P, A, B, C);
< nearring ideal >
gap> Size (P/I);
2
gap> I := InnerAutomorphismNearRing (G);
InnerAutomorphismNearRing( 8/4 )
gap> j := CongruenceNoetherianQuotientForInnerAutomorphismNearRings (I,A,B,C);
< nearring ideal >
gap> Size (I/j);
2
gap> I = ZeroSymmetricPart ( P );
true
gap> STOP_TEST( "tfms.tst", 10000);
