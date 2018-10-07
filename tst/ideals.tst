gap> START_TEST("ideals.tst");
gap> n := LibraryNearRing( GTW8_4, 12 );
LibraryNearRing(8/4, 12)
gap> e := AsNearRingElement( n, (1,3)(2,4) );         
((1,3)(2,4))
gap> r := NearRingRightIdealByGenerators( n, [e] );
< nearring right ideal >
gap> l := NearRingLeftIdealByGenerators( n, [e] );
< nearring left ideal >
gap> i := NearRingIdealByGenerators( n, [e] );
< nearring ideal >
gap> r = i;
true
gap> l = i;
false
gap> l = r;
false
gap> a := GroupReduct( n );                                          
8/4
gap> nsgps := AsSortedList( NormalSubgroups( a ) );;
gap> List(nsgps, StructureDescription);;
gap> nsgps;
[ 1, 8/4, C2 x C2, C2 x C2, C4, C2 ]
gap> l := Filtered( nsgps,                                             
> s -> IsSubgroupNearRingRightIdeal( n, s ) );
[ 1, 8/4, C2 x C2 ]
gap> l := List( l,      
> s -> NearRingRightIdealBySubgroupNC( n, s ) );
[ < nearring right ideal >, < nearring right ideal >, 
  < nearring right ideal > ]
gap> NearRingIdeals( n );
[ < nearring ideal >, < nearring ideal >, < nearring ideal > ]
gap> NearRingRightIdeals( n );
[ < nearring right ideal >, < nearring right ideal >, 
  < nearring right ideal > ]
gap> NearRingLeftIdeals( n );
[ < nearring left ideal >, < nearring left ideal >, < nearring left ideal >, 
  < nearring left ideal > ]
gap> n := LibraryNearRing( GTW6_2, 39 );                    
LibraryNearRing(6/2, 39)
gap> s := Subgroups( GroupReduct( n ) );;
gap> List( s, sg -> IsSubgroupNearRingLeftIdeal( n, sg ) );
[ true, false, false, false, true, true ]
gap> List( s, sg -> IsSubgroupNearRingRightIdeal( n, sg ) );
[ true, false, false, false, true, true ]
gap> n := LibraryNearRingWithOne( GTW27_2, 5 );
LibraryNearRingWithOne(27/2, 5)
gap> AsSortedList( Filtered( NearRingIdeals( n ), IsPrimeNearRingIdeal ) );
[ < nearring ideal of size 27 >, < nearring ideal of size 9 > ]
gap> n := LibraryNearRingWithOne( GTW27_2, 5 );
LibraryNearRingWithOne(27/2, 5)
gap> Filtered( NearRingIdeals( n ), IsMaximalNearRingIdeal );
[ < nearring ideal of size 9 > ]
gap> n := LibraryNearRing( GTW8_2, 2 );
LibraryNearRing(8/2, 2)
gap> li := NearRingLeftIdeals( n );
[ < nearring left ideal >, < nearring left ideal >, < nearring left ideal >, 
  < nearring left ideal >, < nearring left ideal >, < nearring left ideal > ]
gap> l := LibraryNearRing( GTW6_2, 3 );
LibraryNearRing(6/2, 3)
gap> i := AsSortedList( NearRingIdeals( l ) );
[ < nearring ideal >, < nearring ideal > ]
gap> List( i, Size );
[ 1, 6 ]
gap> NearRingCommutator( i[2], i[2] );
< nearring ideal of size 6 >
gap> l := LibraryNearRing( GTW8_4, 13 );
LibraryNearRing(8/4, 13)
gap> NearRingIdeals( l );
[ < nearring ideal >, < nearring ideal >, < nearring ideal > ]
gap> NumberLibraryNearRings( GTW4_2 );                         
23
gap> Filtered( AllLibraryNearRings( GTW4_2 ), IsSimpleNearRing );
[ LibraryNearRing(4/2, 3), LibraryNearRing(4/2, 16), LibraryNearRing(4/2, 17) 
 ]
gap> n := LibraryNearRing( GTW8_2, 2 );
LibraryNearRing(8/2, 2)
gap> e := AsNearRingElement( n, (1,2) );
((1,2))
gap> e in n;
true
gap> i := NearRingRightIdealByGenerators( n, [e] );
< nearring right ideal >
gap> Size(i);
4
gap> IsNearRingLeftIdeal( i );
true
gap> i;
< nearring ideal of size 4 >
gap> f := n/i;          
FactorNearRing( LibraryNearRing(8/2, 2), < nearring ideal of size 4 > )
gap> IdLibraryNearRing(f);
[ 2/1, 1 ]
gap> G := GTW4_2;                
4/2
gap> n := MapNearRing( G );
TransformationNearRing(4/2)
gap> action := function ( g, f )
> return Image( f, g );
> end;  
function( g, f ) ... end
gap> gamma := NGroup( G, n, action );
< N-group of TransformationNearRing(4/2) >
gap> IsNGroup( gamma );
true
gap> NearRingActingOnNGroup( gamma );
TransformationNearRing(4/2)
gap> ActionOfNearRingOnNGroup( gamma );
function( g, f ) ... end
gap> n := LibraryNearRing( GTW8_2, 3 );
LibraryNearRing(8/2, 3)
gap> NGroupByNearRingMultiplication( n ) = GTW8_2;
true
gap> N := LibraryNearRing( GTW4_2, 11 );
LibraryNearRing(4/2, 11)
gap> R := NearRingRightIdeals( N )[ 3 ];
< nearring right ideal >
gap> ng := NGroupByRightIdealFactor( N, R );
< N-group of LibraryNearRing(4/2, 11) >
gap> PrintTable( ng );
Let:
n0 := (())
n1 := ((3,4))
n2 := ((1,2))
n3 := ((1,2)(3,4))
--------------------------------------------------------------------
g0 := <identity> of ...
g1 := f1

N = LibraryNearRing(4/2, 11) acts on 
G = Group( [ f1 ] )
from the right by the following action: 

      | g0  g1  
  ------------
  n0  | g0  g0  
  n1  | g0  g0  
  n2  | g0  g1  
  n3  | g0  g1  

gap> n := LibraryNearRing( TWGroup( 8, 2 ), 3 );
LibraryNearRing(8/2, 3)
gap> gamma := NGroupByNearRingMultiplication( n );
< N-group of LibraryNearRing(8/2, 3) >
gap> PrintTable( gamma );
Let:
n0 := (())
n1 := ((3,4,5,6))
n2 := ((3,5)(4,6))
n3 := ((3,6,5,4))
n4 := ((1,2))
n5 := ((1,2)(3,4,5,6))
n6 := ((1,2)(3,5)(4,6))
n7 := ((1,2)(3,6,5,4))
--------------------------------------------------------------------
g0 := ()
g1 := (3,4,5,6)
g2 := (3,5)(4,6)
g3 := (3,6,5,4)
g4 := (1,2)
g5 := (1,2)(3,4,5,6)
g6 := (1,2)(3,5)(4,6)
g7 := (1,2)(3,6,5,4)

N = LibraryNearRing(8/2, 3) acts on 
G = Group( [ (1,2), (3,4,5,6) ] )
from the right by the following action: 

      | g0  g1  g2  g3  g4  g5  g6  g7  
  ------------------------------------
  n0  | g0  g0  g0  g0  g0  g0  g0  g0  
  n1  | g0  g0  g0  g0  g0  g0  g0  g2  
  n2  | g0  g0  g0  g0  g0  g0  g0  g0  
  n3  | g0  g0  g0  g0  g0  g0  g0  g2  
  n4  | g0  g0  g0  g0  g0  g0  g0  g0  
  n5  | g0  g0  g0  g0  g0  g0  g0  g2  
  n6  | g0  g0  g0  g0  g0  g0  g0  g0  
  n7  | g0  g0  g0  g0  g0  g0  g0  g2  

gap> n := LibraryNearRing( TWGroup( 8, 2 ), 3 );
LibraryNearRing(8/2, 3)
gap> gamma := NGroupByNearRingMultiplication( n );
< N-group of LibraryNearRing(8/2, 3) >
gap> NearRingActingOnNGroup( gamma );                   
LibraryNearRing(8/2, 3)
gap> ActionOfNearRingOnNGroup( gamma );
function( g, n ) ... end
gap> n := LibraryNearRing(GTW12_3,20465);
LibraryNearRing(12/3, 20465)
gap> ng := NGroupByNearRingMultiplication( n );
< N-group of LibraryNearRing(12/3, 20465) >
gap> Length( N0Subgroups( ng ) );
9
gap> NIdeals( ng );
[ < N-group of LibraryNearRing(12/3, 20465) >, 
  < N-group of LibraryNearRing(12/3, 20465) >, 
  < N-group of LibraryNearRing(12/3, 20465) > ]
gap> TypeOfNGroup( ng );
fail
gap> N := LibraryNearRing( GTW12_3, 100 );
LibraryNearRing(12/3, 100)
gap> I := AsSortedList( NearRingIdeals( N ) );
[ < nearring ideal >, < nearring ideal >, < nearring ideal > ]
gap> List(I,Size);
[ 1, 12, 6 ]
gap> NN := NGroupByNearRingMultiplication( N );
< N-group of LibraryNearRing(12/3, 100) >
gap> NoetherianQuotient( N, NN, GroupReduct(I[2]), GroupReduct(I[2]) );
< nearring ideal >
gap> Size(last);
12
gap> f := LibraryNearRing( GTW8_4, 3 );
LibraryNearRing(8/4, 3)
gap> NuRadicals( f );
rec( J0 := < nearring ideal >, J1 := < nearring ideal >, 
  J1_2 := < nearring right ideal >, J2 := < nearring ideal > )
gap> NuRadical( f, 1/2 );
< nearring right ideal >
gap> Size( NuRadical( f, 0 ) );  
8
gap> AsSortedList( NuRadical( f, 1 ) );
[ (()), ((2,4)), ((1,2)(3,4)), ((1,2,3,4)), ((1,3)), ((1,3)(2,4)), 
  ((1,4,3,2)), ((1,4)(2,3)) ]
gap> NuRadical( f, 1/2 ) = NuRadical( f, 2 );
true
gap> STOP_TEST( "tfms.tst", 10000);
