gap> START_TEST("nr.tst");    
gap> G := TWGroup( 24, 6 );
24/6
gap> mul_l := function ( x, y ) return y; end;
function( x, y ) ... end
gap> IsNearRingMultiplication( G, mul_l );
true
gap> mul_r := function ( x, y ) return x; end;
function( x, y ) ... end
gap> IsNearRingMultiplication( G, mul_r );
#I  specified multiplication is not left distributive
false
gap> IsNearRingMultiplication( G, mul_r, ["closed","ldistr"] );
true
gap> G := CyclicGroup( 4 );       
<pc group of size 4 with 2 generators>
gap> GeneratorsOfGroup( G );
[ f1, f2 ]
gap> a := last[1];
f1
gap> Order( a );
4
gap> # a generates G indeed
gap> elmlist := List( [0..3], x -> a^x );
[ <identity> of ..., f1, f2, f1*f2 ]
gap> # Let:  1 := identity of ..., 2 := f1, 3 := f2, 4 := f1*f2
gap> # Consider the following multiplication table on G:
gap> OT := [[1, 1, 1, 1],
> [1, 4, 3, 2],                                                   
> [1, 1, 1, 1],
> [1, 2, 3, 4]];;
gap> mul := NearRingMultiplicationByOperationTable( G, OT, elmlist );
function( x, y ) ... end
gap> IsNearRingMultiplication( G, mul );
true
gap> n := ExplicitMultiplicationNearRing( GTW18_3, mul_l );
ExplicitMultiplicationNearRing ( 18/3 , multiplication )
gap> n = ExplicitMultiplicationNearRingNC( GTW18_3, mul_l );
true
gap> IsNearRing( n );                                       
true
gap> IsNearRing( GroupReduct( n ) );
false
gap> IsExplicitMultiplicationNearRing( n );
true
gap> n := ExplicitMultiplicationNearRingNC( GTW18_3, mul_l );
ExplicitMultiplicationNearRing ( 18/3 , multiplication )
gap> zero_mul := function ( x, y ) return (); end;
function( x, y ) ... end
gap> z := ExplicitMultiplicationNearRingNC( GTW12_3, zero_mul );
ExplicitMultiplicationNearRing ( 12/3 , multiplication )
gap> d := DirectProductNearRing( n, z );
DirectProductNearRing( ExplicitMultiplicationNearRing ( 18/3 , multiplication \
), ExplicitMultiplicationNearRing ( 12/3 , multiplication ) )
gap> IsExplicitMultiplicationNearRing( d );
true
gap> n := ExplicitMultiplicationNearRingNC( CyclicGroup( 3 ), mul_l );;
gap> SetSymbols( n, ["0","1","2"] );
gap> PrintTable( n );               
Let:
0 := (<identity> of ...)
1 := (f1)
2 := (f1^2)

  +  | 0  1  2  
  ------------
  0  | 0  1  2  
  1  | 1  2  0  
  2  | 2  0  1  

  *  | 0  1  2  
  ------------
  0  | 0  1  2  
  1  | 0  1  2  
  2  | 0  1  2  
gap> mul_l := function ( x, y ) return y; end;              
function( x, y ) ... end
gap> n := ExplicitMultiplicationNearRingNC( GTW6_2, mul_l );
ExplicitMultiplicationNearRing ( 6/2 , multiplication )
gap> AsList( n );
[ (()), ((2,3)), ((1,2)), ((1,2,3)), ((1,3,2)), ((1,3)) ]
gap> e := AsNearRingElement( n, (2,3) );
((2,3))
gap> e in n;
true
gap> f := AsNearRingElement( n, (1,3) );
((1,3))
gap> e + f;
((1,3,2))
gap> e * f;
((1,3))
gap> n := LibraryNearRing( GTW6_2, 39 );                    
LibraryNearRing(6/2, 39)
gap> e := Enumerator( n );                                  
<enumerator of near ring>
gap> e[1];
(())
gap> Length(e);
6
gap> AsSortedList( n );                                     
[ (()), ((2,3)), ((1,2)), ((1,2,3)), ((1,3,2)), ((1,3)) ]
gap> n := ExplicitMultiplicationNearRingNC( GTW8_4, mul_l );
ExplicitMultiplicationNearRing ( 8/4 , multiplication )
gap> GeneratorsOfNearRing( n );
[ ((1,2,3,4)), ((2,4)) ]
gap> n := LibraryNearRingWithOne( GTW24_3, 8 );
LibraryNearRingWithOne(24/3, 8)
gap> Size(n);
24
gap> GroupReduct( LibraryNearRingWithOne( GTW24_3, 8 ) );
24/3
gap> Endomorphisms ( LibraryNearRing( GTW12_4, 4 ) ) ;
[ [ (1,2,4), (2,3,4) ] -> [ (), () ], 
  [ (1,2,4), (2,3,4) ] -> [ (1,2,4), (2,3,4) ] ]
gap> Length( Endomorphisms( GTW12_4 ) );
33
gap> Automorphisms( LibraryNearRing( GTW12_4, 4 ) );
[ ^() ]
gap> IsIsomorphicNearRing( MapNearRing( GTW2_1 ),                       
> LibraryNearRingWithOne( GTW4_2, 5 ) );
true
gap> n := LibraryNearRing( GTW12_4, 8 );
LibraryNearRing(12/4, 8)
gap> SubNearRings( n );
[ ExplicitMultiplicationNearRing ( Group(()) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (1,4)(2,3) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (1,2)(3,4) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (2,3,4) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (1,2,4) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (1,3,2) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (1,4,3) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (1,4)(2,3), (1,3)(2,4) 
     ]) , multiplication ), ExplicitMultiplicationNearRing ( Group(
    [ (1,4)(2,3), (1,3)(2,4), (2,3,4) ]) , multiplication ) ]
gap> n := LibraryNearRing( GTW12_4, 8 );
LibraryNearRing(12/4, 8)
gap> i := InvariantSubNearRings( n );
[ ExplicitMultiplicationNearRing ( Group(()) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (1,2)(3,4) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (2,3,4) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (1,4,3) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (1,4)(2,3), (1,3)(2,4), (2,3,4) 
     ]) , multiplication ) ]
gap> n := LibraryNearRing( GTW12_4, 8 );            
LibraryNearRing(12/4, 8)
gap> S := Subgroup( GTW12_4, [ (1,2)(3,4) ] );
Group([ (1,2)(3,4) ])
gap> sn := SubNearRingBySubgroupNC( n, S );
ExplicitMultiplicationNearRing ( Group([ (1,2)(3,4) ]) , multiplication )
gap> n := LibraryNearRingWithOne( GTW27_4, 5 );
LibraryNearRingWithOne(27/4, 5)
gap> si := Filtered( SubNearRings( n ), s -> Identity( n ) in s );
[ ExplicitMultiplicationNearRing ( Group([ (1,23,14)(2,13,6)(3,27,22)(4,18,9)
      (5,20,12)(7,16,26)(8,25,17)(10,21,19)(11,24,15) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group(
    [ (1,22,16)(2,12,21)(3,26,23)(4,17,11)(5,19,13)(6,20,10)(7,14,27)(8,24,18)
      (9,25,15), (1,26,27)(2,19,20)(3,14,16)(4,24,25)(5,6,21)(7,22,23)(8,9,11)
      (10,12,13)(15,17,18) ]) , multiplication ), 
  ExplicitMultiplicationNearRing ( Group([ (1,6,15)(2,9,7)(3,13,25)(4,14,10)
      (5,18,27)(8,23,20)(11,22,19)(12,24,16)(17,26,21), (1,5,17)(2,8,22)
      (3,12,4)(6,18,26)(7,20,11)(9,23,19)(10,25,16)(13,24,14)(15,27,21), 
      (1,2,4)(3,6,11)(5,9,16)(7,13,17)(8,14,21)(10,18,22)(12,15,23)(19,24,26)
      (20,25,27) ]) , multiplication ) ]
gap> Intersection( si );
ExplicitMultiplicationNearRing ( Group([ (1,23,14)(2,13,6)(3,27,22)(4,18,9)
  (5,20,12)(7,16,26)(8,25,17)(10,21,19)(11,24,15) ]) , multiplication )
gap> Size( last );
3
gap> n := LibraryNearRing( GTW12_4, 8 );            
LibraryNearRing(12/4, 8)
gap> Identity( n );
fail
gap> One( n );
fail
gap> n := LibraryNearRingWithOne( GTW24_4, 8 ); 
LibraryNearRingWithOne(24/4, 8)
gap> Identity( n );
((1,2,3,4,5,6)(7,8))
gap> One( n );
((1,2,3,4,5,6)(7,8))
gap> n := LibraryNearRing( GTW12_4, 8 );             
LibraryNearRing(12/4, 8)
gap> IsNearRingWithOne( n );
false
gap> n := LibraryNearRingWithOne( GTW24_4, 8 ); 
LibraryNearRingWithOne(24/4, 8)
gap> Identity( n );
((1,2,3,4,5,6)(7,8))
gap> IsNearRingWithOne( n );
false
gap> n := LibraryNearRingWithOne( GTW24_4, 8 );
LibraryNearRingWithOne(24/4, 8)
gap> NearRingUnits( n );    
[ ((1,2,3,4,5,6)(7,8)), ((1,6,5,4,3,2)(7,8)) ]
gap> n := LibraryNearRing( GTW12_4, 8 );             
LibraryNearRing(12/4, 8)
gap> IsNearRingWithOne( n );
false
gap> Distributors( n );
[ (()), ((2,3,4)), ((2,4,3)), ((1,2)(3,4)), ((1,2,3)), ((1,2,4)), ((1,3,2)), 
  ((1,3,4)), ((1,3)(2,4)), ((1,4,2)), ((1,4,3)), ((1,4)(2,3)) ]
gap> DistributiveElements( n );
[ (()) ]
gap> IsDistributiveNearRing( n );
false
gap> ZeroSymmetricElements( n );
[ (()), ((2,3,4)), ((2,4,3)), ((1,2)(3,4)), ((1,2,3)), ((1,2,4)), ((1,3,2)), 
  ((1,3,4)), ((1,3)(2,4)), ((1,4,2)), ((1,4,3)), ((1,4)(2,3)) ]
gap> IdempotentElements( n );
[ (()), ((1,4)(2,3)) ]
gap> NilpotentElements( n );
[ [ (()), 1 ], [ ((2,3,4)), 2 ], [ ((2,4,3)), 2 ], [ ((1,2)(3,4)), 2 ], 
  [ ((1,2,3)), 2 ], [ ((1,2,4)), 2 ], [ ((1,3,2)), 2 ], [ ((1,3,4)), 2 ], 
  [ ((1,4,2)), 2 ], [ ((1,4,3)), 2 ] ]
gap> QuasiregularElements( n );
[ (()), ((2,3,4)), ((2,4,3)), ((1,2)(3,4)), ((1,2,3)), ((1,2,4)), ((1,3,2)), 
  ((1,3,4)), ((1,3)(2,4)), ((1,4,2)), ((1,4,3)) ]
gap> RegularElements( n );
[ (()), ((1,3)(2,4)), ((1,4)(2,3)) ]
gap> IsAbelianNearRing( n );
false
gap> IsAbstractAffineNearRing( n );
false
gap> IsBooleanNearRing( n );
false
gap> IsNilNearRing( n );
false
gap> IsNilpotentNearRing( n );
false
gap> IsNilpotentFreeNearRing( n );
false
gap> IsCommutative( n );
false
gap> IsDgNearRing( n );
false
gap> IsIntegralNearRing( n );
false
gap> IsPrimeNearRing( n );
true
gap> IsQuasiregularNearRing( n );
false
gap> IsRegularNearRing( n );
false
gap> IsNearField( n );                        
false
gap> n := LibraryNearRing( GTW9_2, 90 );
LibraryNearRing(9/2, 90)
gap> IsPlanarNearRing( n );     
true
gap> nr := LibraryNearRing( GTW9_1, 185 );
LibraryNearRing(9/1, 185)
gap> IsWdNearRing( nr );
true
gap> l := AllLibraryNearRings( GTW3_1 );
[ LibraryNearRing(3/1, 1), LibraryNearRing(3/1, 2), LibraryNearRing(3/1, 3), 
  LibraryNearRing(3/1, 4), LibraryNearRing(3/1, 5) ]
gap> Filtered( l, IsNearField );
[ LibraryNearRing(3/1, 3) ]
gap> NumberLibraryNearRings( GTW14_2 );
1821
gap> LN14_2_1234 := LibraryNearRing( GTW14_2, 1234 );
LibraryNearRing(14/2, 1234)
gap> NumberLibraryNearRingsWithOne( GTW24_6 );
0
gap> NumberLibraryNearRingsWithOne( GTW24_4 );
10
gap> LNwI24_4_8 := LibraryNearRingWithOne( GTW24_4, 8 );
LibraryNearRingWithOne(24/4, 8)
gap> AllLibraryNearRingsWithOne( GTW24_6 );
[  ]
gap> p := PolynomialNearRing( GTW4_2 );
PolynomialNearRing( 4/2 )
gap> IdLibraryNearRing( p );
[ 8/3, 833 ]
gap> n := LibraryNearRing( GTW3_1, 4 );
LibraryNearRing(3/1, 4)
gap> d := DirectProductNearRing( n, n );
DirectProductNearRing( LibraryNearRing(3/1, 4), LibraryNearRing(3/1, 4) )
gap> IdLibraryNearRing( d );
[ 9/2, 220 ]
gap> l := LibraryNearRingWithOne( GTW12_3, 1 );
LibraryNearRingWithOne(12/3, 1)
gap> IdLibraryNearRingWithOne( l );           
[ 12/3, 1 ]
gap> IsLibraryNearRing( LNwI24_4_8 );
true
gap> STOP_TEST( "nr.tst", 10000);
