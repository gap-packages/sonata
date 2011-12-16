
##
##  AUXILIARY FUNCTIONS FOR DETERMINING FPF REPRESENTATIONS
##

  Maxfactor := function( m, prime )

## returns the maximal power of prime dividing m

  local q;

    q := prime;  
    while m mod q = 0 do
      q := q*prime;  
    od;
    return q/prime;
  end; 



  Cofactor := function( m, n )

## returns the maximal integer mprime dividing m and coprime to n
## if each prime divisor of n divides m, otherwise 0 is returned
 
  local mprime, prime, q;

    if n = 1 then
      return m;
    fi; 
	
    mprime := m;
    for prime in Set( FactorsInt( n ) ) do    
      q := Maxfactor( m, prime );
      if q = 1 then
## undefined, m and r are not feasible for our purpose
	return 0;
      fi;
      mprime := mprime/q;
    od;

    return mprime; 
  end;

  MatrixInt := function( matrix ) 
    return List( matrix, x -> List( x, Int ) );
  end;


#############################################################################
##
  BlockDiagonalMatrix := function( blocks )
##
## note: as in the case of BlockMatrix( arg ) all the blocks must have
## the same dimensions rpb, cpb

    return BlockMatrix( 
	List( [1..Length( blocks )], x -> [x, x, blocks[x]] ),
	Length( blocks ), Length( blocks ) );
  end;



#############################################################################
##
  SolutionDiophant := function( a, b, c )
##
## gives one solution (x, y) of the diophant equation a*x+b*y = c 
## if existing
##

  local coeff, d, l;

    coeff := GcdRepresentation( a, b );
    d := a*coeff[1]+b*coeff[2];

    if c mod d <> 0 then
      return fail;
    fi;

    return QuoInt( c, d )*coeff;
  end;



#############################################################################
##
  SolutionMatMod := function ( mat, vec, p )
##
##  returns one solution <x> of <mat>*<x> = <vec> or `fail' over the
##  ring ZmodnZ(p) with <p> a prime power
##
##  this function is an adaption of the GAP-function
##  SolutionMat (SolutionMatNoCo) avoiding inversion 
##

    local h, v, i, l, r, s, c, zero, root,
	  A, b, d, m, n, found, done, indices, minindex, q, qnow, res,
	  x, t, temp;

    # solve <mat>*x = <vec>.
    vec := ShallowCopy( vec );
    A := MatrixInt( mat );
    b := List( vec, Int );
    m := Length( mat );
    n := Length( mat[1] );
    d := 0; r := 0; c := 0;
    zero := Zero( mat[1][1] );
    root := SmallestRootInt( p );
    q := root;	
    indices := [1..m];
    Info( InfoNearRing, 1, "SolutionMatMod called" );

    done := false;
    while d < n and not done do
## run through all columns of the matrix to search for minimal pivot-element
      c := d; 
      qnow := p; found := false;
      while not found and c < n  do
        c := c+1; r := d;
        while not found and r < m do
          r := r+1;
          if A[r][c] mod q <> 0 then
	    found := true;
	    minindex := [r,c]; qnow := QuoInt( q, root );
	  elif A[r][c] mod qnow <> 0 then
	    minindex := [r,c];
	    qnow := Gcd( A[r][c], qnow );
	  fi;
        od;	      
      od;

      if qnow = p then
## all the subsequent entries of mat are zero
	done := true;
      else
## put this column in the appropriate position
        d := d+1;	
        r := minindex[1]; c := minindex[2]; 
        temp := indices[c]; 
        indices[c] := indices[d];
        indices[d] := temp;
        for i in [1..m] do 
	  temp := A[i][c]; 	
	  A[i][c] := A[i][d];
 	  A[i][d] := temp;
        od;	

## normalize this row and clear this column 

        q := qnow;

#if Gcd( A[r][d], p ) <> q then
#  return false;
#fi;
        t := SolutionDiophant( A[r][d], p, q )[1]; 
        l := List( A[r], x -> t*x mod p );
	Info( InfoNearRing, 3, "l = ", l );
        temp := t*b[r] mod p;
        A[r] := A[d]; b[r] := b[d];
        A[d] := l; b[d] := temp;
	
        for i in [d+1..m] do
#if A[i][d] mod q <> 0 then
#  return false;
#fi;
	  t := QuoInt( A[i][d], q );
          A[i] := (A[i]-t*l) mod p; b[i] := (b[i]-t*b[d]) mod p;
      	od;
      fi;
    od;  ##  while d < n and not done do

    Info( InfoNearRing, 2, "rowreduced matrix = ", A );
    Info( InfoNearRing, 2, "vector = ", b );

## now find a solution

    temp := [];
    for i in [1..d] do
      if b[i] mod A[i][i] <> 0 then return fail; fi;	
    od;
    for i in [d+1..m] do
      if b[i] mod p <> 0 then return fail; fi;
      temp[i] := 0;		
    od;

    i := d-1;
    temp[d] := QuoInt( b[d], A[d][d] );		
    while i > 0 do
      temp[i] := QuoInt( b[i]-A[i]{[i+1..d]}*temp{[i+1..d]}, A[i][i] ); 
      i := i-1;
    od; 
      
    res := [];
    for i in [1..n] do
      res[indices[i]] := temp[i];	
    od;	 

    Info( InfoNearRing, 2, "mat * x = vec solved by x^t = ", res );
    Info( InfoNearRing, 1, "SolutionMatMod returns" );
    return res*One( mat[1][1] );
end;


#############################################################################
##
  GroupHomomorphismByMatrix := function( G, gens, mats )
##
##  for an abelian group G with generators gens this function returns
##     GroupHomomorphismByImages( G, G, gens, imgs )  
##  where imgs is described by mats (the coefficients of images with respect
##  to gens)   
##  e.g., for G elementary abelian with "basis" gens and mats = [A] 
##  this would simply return the homomorphism x -> x*A  
##  
    local d, n,
	  imgs,
	  i, j, x;
    
      imgs := []; d := 0;            
      for i in [1..Length( mats )] do
	n := Length( mats[i] );
	for j in [1..n] do
## add the image of the (d+j)-th generator:
          Add( imgs,
		Product( List( [1..n], x -> gens[d+x]^Int(mats[i][j][x]) ) ) );
	od;
	d := d+n;
      od;

      return GroupHomomorphismByImages( G, G, gens, imgs );
   end;






##
##  FPF BASICS
##



############################################################################
##
#M  IsFpfAutomorphismGroup( <phi>, <G>  )
##
##  - tests whether <phi> acts fpf on <G> by converting <phi> to a
##  permutation group
##


InstallMethod( 
	IsFpfAutomorphismGroup,
	"default",
	true,
	[IsGroup, IsGroup],
	0,
  function( phi, G  )

  local k, n, O;
 
    k := Size( phi );
    n := Size( G );
    if n mod k <> 1 then
      return false;
    fi;

    O := SortedList( G );
    RemoveSet( O, Identity( G ) );
    
    return IsSemiRegular( phi, O );   
  end );



############################################################################
##
#M  FpfAutomorphismGroupsMaxSize( <G> )
##
##  - returns an upper bound for the maximal size of a fixed point free 
##  automorphism group on <G>
##  - tests whether a fixed point free automorphism group on <G> has to be
##  necessarily cyclic
##  - uses only the size of <G>
##


InstallMethod(
	FpfAutomorphismGroupsMaxSize,
	"default",
	true,
	[IsGroup],
	0,
  function( G )

  local primesandexp, 	# list of primes and exponents [[p1,d1],...[pn,dn]]
 			# such that Size(G) = p1^d1*...pn^dn
	kmax, 		# maximum possible size of an fpf aut grp on <G>
	dmax,		# maximum possible size of a factor in a noncyclic
			# fpf aut grp on <G>, see presentations
			# dmax = 1 means that all fpf aut grps are cyclic
	factors, f, x;


      if not IsNilpotentGroup( G ) then
        return [1,1];
      fi;   	
    	
      primesandexp := Collected( Factors( Size( G ) ) );

      if IsCyclic( G ) then
	return [Gcd( List( primesandexp, x -> x[1]-1 ) ), 1];
      fi;	

      kmax := Gcd( List( primesandexp, x -> x[1]^x[2]-1 ) );

      if not IsAbelian( G ) then	
## <kmax> is certainly odd 
        while IsEvenInt( kmax ) do
	  kmax := kmax/2;
        od;
      fi;	 		

      dmax := Gcd( List( primesandexp, x -> x[2] ) );	
      dmax := Gcd( [dmax, kmax, Phi(kmax)] );
      factors := Collected( Factors( dmax ) );
      for f in factors do
## pq-condition	
        if kmax mod f[1]^(f[2]+1) <> 0 then
	  dmax := dmax/f[1];
	fi;
      od;		 		
      return [kmax,dmax];     
   end );



##
## functions returning the degree of irreducible fpf representations over
## GF(p) for solvable groups as described in 
##
## PM: Fpf representations over fields of prime characteristic. Technical
## Report
##
## NOTE: no checking whether the input-parameters are feasible
##


############################################################################
##
  DegreeOfIrredFpfRepCyclic := function( p, m )
## error if p and m are not coprime
    return OrderMod( p, m );
  end;

############################################################################
##
  DegreeOfIrredFpfRepMetacyclic := function( p, m, r )
##   
  local n, q, mprime;      

## check the necessary conditions whether the input-parameters are feasible:
    n := OrderMod( r, m );
    if n = 0 then 
      Error( "parameters are not feasible: <m> and <r> coprime" );	
    fi;
	
    q := Maxfactor( m, 2 );
    if q > 2 and (r+1) mod q = 0 and n mod 4 = 2 then
## type II, quaternion 2-Sylow subgroup
      mprime := Cofactor( m/2, n/2 );
      if mprime = 0 or r mod (m/mprime) <> 1 then
	Error( "parameters are not feasible" );
      fi;
      Info( InfoNearRing, 1,
	 "parameters are feasible; metacyclic group of type II" );
    else	
## type I, all Sylow subgroups are cyclic
      mprime := Cofactor( m, n );
      if mprime = 0 or r mod (m/mprime) <> 1 then
	Error( "parameters are not feasible" );
      fi;
      Info( InfoNearRing, 1,
	 "parameters are feasible; metacyclic group of type I" );
    fi;

    return Size( GroupByPrimeResidues( [p,r], m ) );

  end;

############################################################################
##
  DegreeOfIrredFpfRep2 := function( p, m, r, k )
##
## necessary conditions:
## all prime divisors of n := OrderMod( r, m ) divide m
## n/2 is an odd number
## not k in GroupByPrimeResidues( [r], m )
## k = -1 mod Maxfactor( m, 2 )
## OrderMod( k, m ) = 2

   
  local n, q, mprime;      

## check the necessary conditions whether the input-parameters are feasible:
    n := OrderMod( r, m );
    q := Maxfactor( m, 2 );
    mprime := Cofactor( m, n );
    if n mod 4 <> 2 then
      Error( "parameters are not feasible: <m> and <r> coprime" );	
    elif q = 1 then
      Error( "parameters are not feasible: <m> is even" );
#    elif q > 1 and (r+1) mod q = 0 then
#      Error( "parameters are not feasible: 4 divides <r>-1" ); 	
    elif mprime = 0 or r mod (m/mprime) <> 1 then
      Error( "parameters are not feasible: pq-condition" );
    elif OrderMod( k, m ) <> 2 or (k+1) mod q <> 0 or 
			( m <> mprime*q and k mod (m/(mprime*q)) <> 1 ) then
      Error( "parameters are not feasible: pq-condition" ); 
    fi;
  
    return Size( GroupByPrimeResidues( [p,r,k], m ) );
  end;


############################################################################
##
  DegreeOfIrredFpfRep3 := function( p, m, r )
##
## necessary conditions:
## 3 divides m, 
## m is odd
## all prime divisors of n := OrderMod( r, m ) divide m

  local n, mprime;

    n := OrderMod( r, m );
    if n = 0 then 
      Error( "parameters are not feasible: <m> and <r> coprime" );	
    fi;
	
    mprime := Cofactor( m, n );
    if m mod 2 = 0 or m mod 3 <> 0 or mprime = 0 or
			 (n > 1 and r mod (m/mprime) <> 1) then
      Error( "parameters are not feasible" );
    elif m mod 9 <> 0 and n mod 3 <> 0 then
##      return 2*DegreeOfIrredFpfRepMetacyclic( p, m/3, r );
      return 2*Size( GroupByPrimeResidues( [p, r], m/3 ) );
    else
##      return 2*DegreeOfIrredFpfRepMetacyclic( p, m, r );
      return 2*Size( GroupByPrimeResidues( [p, r], m ) );
    fi;	
  end;


############################################################################
##
  DegreeOfIrredFpfRep4 := function( p, m, r, k )
##
## necessary conditions:
## m is odd
## all prime divisors of n := OrderMod( r, m ) divide m
##    in particular n is odd
## 3 divides m, but 3 does not divide n
## OrderMod( k, m ) = 2
## k = -1 mod 3
## k = 1 mod prime divisors of n

  local n, mprime;

    n := OrderMod( r, m );
    if n = 0 then 
      Error( "parameters are not feasible: <m> and <r> coprime" );	
    fi;
	
    mprime := Cofactor( m, n );
    if m mod 2 = 0 or mprime mod 3 <> 0 or mprime = 0 or
			 (n > 1 and r mod (m/mprime) <> 1) then
      Error( "parameters are not feasible: 3" );
    elif OrderMod( k, m ) <> 2 or (k+1) mod 3 <> 0 or
			 (n > 1 and k mod (m/mprime) <> 1) then
      Error( "parameters are not feasible: 4" );
    elif m mod 9 <> 0 and p^2 mod 16 = 1 then
      return 2*Size( GroupByPrimeResidues( [p,r,k], m/3 ) );
    elif m mod 9 <> 0 then
      return Lcm( 4, 2*Size( GroupByPrimeResidues( [p,r,k], m/3 ) ) );
    else
      return 2*Size( GroupByPrimeResidues( [p,r,k], m ) );  
    fi;
  end;



##
##  DETERMINING THE REPRESENTATIONS OVER GF(P)
##


############################################################################
##
#M  IsFpfRepresentation( <matrices>, <F> )
##
##  - tests whether the group of d times d matrices in the list <matrices>
##  over F acts fpf by multiplication on the d-dimensional vector space
##  (module) over F 
##  - converts <phi> to a permutation group acting on the row vectors F^d from
##  the right (GAP-convention, x -> x*A)     
##

InstallMethod( 
	IsFpfRepresentation,
	"default",
	true,
	[IsList, IsDomain],
	0,
  function( matrices, F )

  local d, 		#  degree of the matrix representation
	phi,		#  matrix group generated by <matrices> over F
        n,
        o,
        I,
        l;
 

    d := Length( matrices[1] );
    phi := Group( matrices*One(F) );
    n := Size( F )^d;
 
    if n mod Size( phi ) <> 1 then
      return false;
    fi;
    
    o := Zero(F);
    I := IdentityMat( d, F );
    l := SortedList( phi );
    RemoveSet( l, I );

    return ForAll( l, x -> Determinant( x-I ) <> o ); 
  end );



############################################################################
##
  FGRepC := function( p, f, m )
##
## returns a matrix <AR> representing an fpf automorphism of order m over
## the ring R = ZmodnZ( p^f ) with p a prime
## <AR> acts from the right, x -> x*AR
##

  local e, F, gens, a, A, AR, R, 
	i, x, y;

    e := OrderMod( p, m );
    if e = 0 then
      Error( "size of the group, m, and characteristic, p, have to be coprime" );
    fi;
    F := GF( p^e ); 
    gens := Basis( F );
    a := Z(p^e)^((p^e-1)/m);
    A := List( gens, x -> Coefficients( gens, a*x ) );

    if f = 1 then 
      return A;
    fi;

## the representation over GF(p) determines a representation over ZmodnZ( p^f )
    R := ZmodnZ( p^f );
    AR := MatrixInt( A )*One( R );
    AR := AR^(p^(f-1));
## now AR has order m over R, but AR is not necessarily congruent to A mod p 
    return AR; 
  end;  



############################################################################
##
#M  FpfRepresentationsCyclic( <p>, <size> )
##
##  determines the irreducible fixed point free representations of a cyclic
##  group of order <size> over ZmodnZ( p )
##   
##  returns a list of matrices { A^i | i in indexlist }
##  such that each matrix A^i operates from the right on a ZmodnZ(p)-module
##  thus determining a representation.
##  The A^i describe all irred. representation up to equivalence.
##  additionally <indexlist> is returned.
##

InstallMethod(
	FpfRepresentationsCyclic,
	"default",
	true,
	[IsInt, IsInt],
	0,
  function( p, size )

  local A,		# matrix (i.e., representation of the generator
			# of the cyclic group of order <size> )
			# linear map: x -> x*A
	CmodsizeCstar,	# multiplicative group of residues prime to <size>
	U, 	
	indexlist, 	# A^i for i in <indexlist> form all the irreducible
			# nonequivalent representations  
	prime, f,
	x;

    prime := SmallestRootInt( p );
    f := LogInt( p, prime );
   		 	

    if size mod prime = 0 then 	
      Error( "size of group and characteristic of field have to be coprime" );
    elif size = 1 then
      return [[[[One( ZmodnZ( p ) )]]], [1]];
    elif size = 2 then 
      return [[[[-One( ZmodnZ( p ) )]]], [1]];	  
    fi; 	
	
    A := FGRepC( prime, f, size );		 
    CmodsizeCstar := Units( Integers mod size );
    U := GroupByPrimeResidues( [prime], size );
    indexlist := List( RightCosets( CmodsizeCstar, U ),
				 x -> Int( Representative(x) ) );
    
    return [List( indexlist, x -> A^x ), indexlist];      	
  end );


############################################################################
##
  FGRepQ := function( prime, f )
##
## returns matrices P, Q  representing an fpf quaternion group over Z(prime^f)
## this irreducible fpf representation is unique (upto equivalence)
## note that p has to be an odd prime 
##

  local P, Q,
	u, v, p,
	x;

      p := prime^f;
      u := 0; v := 0;	 
## find the solution by trial and error:
      while (u^2+v^2+1) mod p <> 0 and u < p do		
	u := u+1; v := u;
        while (u^2+v^2+1) mod p <> 0 and v < p do	
 	  v := v+1;
	od;
      od;	 

      P := [[u,v],[v,-u]];
      Q := [[0,-1],[1,0]];

      return [P,Q]*One( ZmodnZ( p ) );
    end;



############################################################################
##
  FGRepMC := function( p, f, m, mprime, d )
##
## returns matrices AR, WR^u, BR representing a metacyclic fpf automorphism
## group over the ring R = ZmodnZ( p^f ) with p a prime such that
##   	m ... order of AR
##	d ... (BR*WR^u)^-1*AR*(BR*WR^u) = AR^(p^d)
##	WR is induced by x -> x*w and w is primitive in GF(p^e)
## additionally, with n = e/gcd(e,d) and q = p^d:   
##      (BR*WR^u)^n = (WR^u)^[1+p^d+p^2d+...+p^(n-1)d] 
##		    = (WR^u)^[1+q+q^2+...+q^(n-1)] 
##		    = (WR^u)^[(q^n-1)/(q-1)] 
##		    = AR^mprime 
##

  local e,	# degree of irred. representation of C_m over GF(p) 
	n,    	# n = e/gcd(e,d)
	q,	# q = p^d
	u,
	F, gens, 
	w, W, WR, 
	a, A, AR, B, BR, R, ARq, XR, M, y, I,
	i, j, x;

    e := OrderMod( p, m );
    if e = 0 then 	
      Error( "size of group and characteristic of field have to be coprime" );
    fi;
    n := e/Gcd(e,d);

## now: beta^n = alpha^mprime
## m/mprime divides q-1

    q := p^d;
    if q mod (m/mprime) <> 1 then
      Error( "parameters not feasible: m/mprime has to divide q-1" );
    fi;	

    F := GF( p^e ); 
    w := Z(p^e);
    gens := Basis( F );	
    W := List( gens, x -> Coefficients( gens, x*w ) );
    B := List( gens, x -> Coefficients( gens, x^q ) );

## u is determined by (WR^u)^[(q^n-1)/(q-1)] = AR^mprime 
    u := ((p^(e/n)-1)*mprime)/m;
## for an integer i coprime to m: (W^i)^((p^e-1)/m) and
## B*(W^i)^u together form the irred representations

    if f = 1 then       
      return [W^((p^e-1)/m), W^u, B];
    fi;

## the representation over GF(p) determines a representation over ZmodnZ( p^f )
    R := ZmodnZ( p^f );
    WR := (MatrixInt( W )*One( R ))^(p^(f-1));
## now WR has order relatively prime to p over R
## but WR is not necessarily congruent to W mod p 

    AR := WR^((p^e-1)/m);
    BR := MatrixInt( B )*One( R );
## BR does not necessarily normalize the group generated by AR  
    ARq := AR^q;
    y := Flat( AR*BR-BR*ARq );
    I := IdentityMat( e )*One( R );
    M := KroneckerProduct( I, TransposedMat( ARq ))-KroneckerProduct( AR, I );
    x := SolutionMatMod( M, y, p^f );

    if x = fail then
      return fail;
    fi;
	
    XR := List( [1..e], i -> x{[(i-1)*e+1..i*e]} );	
## Now (BR+XR)^-1*AR*(BR+XR) = AR^q but (BR+XR) has not yet order n.
## Let j be minimal such that j*e > f. Then (BR+XR)^(p^(j*e)) has order n
## and maps AR to AR^q by conjugation. 
    BR := (BR+XR)^( p^((QuoInt(f-1, e)+1)*e) );

    return [AR, WR^u, BR];
  end;  



############################################################################
##
#M  FpfRepresentationsMetacyclic( <p>, <m>, <r> )
##
##  determines the fixed point free representations of a metacyclic group
##  with generators a, b and relations a^m = 1, b^n = a^mprime, 
##  b^(-1)*a*b = a^r where n is the multiplicative order of r in the group 
##  of prime residues modulo m.
##  Additional conditions are: mprime divides m
##  each prime divisor of n divides m/mprime and
##  r = 1 mod (m/mprime)
##   
##  returns a list of pairs of matrices corresponding to a and b 
##  which operate from the right on a ZmodnZ(p)-module
##  additionally a list <indexlist> of indices is returned
##

InstallMethod(
	FpfRepresentationsMetacyclic,
	"quaternion group",
	true,
	[IsInt, IsInt, IsInt],
	10,
  function( p, m, r )

  local	prime, f;

     if m <> 4 then
       TryNextMethod();
     fi;
     prime := SmallestRootInt( p );
     f := LogInt( p, prime );
     return [[FGRepQ( prime, f )], [1]];     
  end );


InstallMethod(
	FpfRepresentationsMetacyclic,
	"default",
	true,
	[IsInt, IsInt, IsInt],
	0,
  function( p, m, r )

  local A, B, C, I,	# auxiliary matrices (blocks) for the representations
	AA, BB,         # matrices (representations of the generators of the
			# metacyclic group of determined by the 
			# parameters <m> and <r> )
			# linear maps, 	a: x -> x*AA
			#		b: x -> x*BB  
	fpfreps, 	# list of matrices [AA, BB] determining the
			# nonequivalent representations	
	indexlist, 	# AA^i, BB(i) for i in <indexlist> form all the
			# nonequivalent representations  
	CmodmCstar,	# multiplicative group of residues prime to <m>
	e, t, q, d,
	Ubyrandprime,   # subgroup of CmodmCstar generated by r and prime
	n,		# size of Ubyr
        mprime,	
	factors, aux, subdiagonal, prime, f,
	i, x;


    prime := SmallestRootInt( p );
    f := LogInt( p, prime );
    e := OrderMod( prime, m );
## e is the degree of an irreducible fpf representation of <a> over GF(prime)
    if e = 0 then 	
      Error( "size of group and characteristic of field have to be coprime" );
    fi; 

## check the necessary conditions whether the input-parameters are feasible:
    r := r mod m;	
    n := OrderMod( r, m );
    if n = 0 then 
      Error( "parameters are not feasible: <m> and <r> coprime" );
    elif n = 1 then
      Error( "parameters of a cyclic group of size <m>" );	
    fi;
	
    q := Maxfactor( m, 2 );
    if q > 2 and (r+1) mod q = 0 and n mod 4 = 2 then
## type II, quaternion 2-Sylow subgroup
      mprime := Cofactor( m/2, n/2 );
      if mprime = 0 or r mod (m/mprime) <> 1 then
	Error( "parameters are not feasible" );
      fi;      
      Info( InfoNearRing, 1, "parameters are feasible; metacyclic group of type II" );
    else	
## type I, all Sylow subgroups are cyclic
      mprime := Cofactor( m, n );
      if mprime = 0 or r mod (m/mprime) <> 1 then
	Error( "parameters are not feasible" );
      fi;
      Info( InfoNearRing, 1, "parameters are feasible; metacyclic group of type I" );
    fi;

    Ubyrandprime := GroupByPrimeResidues( [r, prime], m );
    t := Size( Ubyrandprime )/e; 
## r^t is congruent to a power of prime modulo m

    if n = t then 
## prime^x = r^t mod m implies that x = e
      A := FGRepC( prime, f, m );  	
      C := A^mprime;
      B := IdentityMat( Length( A ), ZmodnZ(p) ); 		
    else	
      q := r^t mod m;
      d := LogMod( q, prime, m ); 
      aux := FGRepMC( prime, f, m, mprime, d );      
      A := aux[1]; C := aux[2]; B := aux[3]; 
    fi;

    CmodmCstar := Units( Integers mod m );
    indexlist := List( RightCosets( CmodmCstar, Ubyrandprime ), x ->
					 Int( Representative(x) ) );
 
    I := IdentityMat( Length( A ), ZmodnZ( p ) );
    subdiagonal := List( [1..t-1], x -> [x+1, x, I] );

    fpfreps := [];
    for i in indexlist do
      AA := BlockDiagonalMatrix( List( [0..t-1], x -> A^(i*r^x) ) );
      BB := BlockMatrix( Concatenation( subdiagonal, [[1, t, B*C^i]] ), t, t );
      Add( fpfreps, [AA,BB] );
    od;	
	
    return [fpfreps, indexlist];
  end );




############################################################################
##
  FGRep2 := function( p, f, m, mprime, d, l )
##
## returns matrices AR, WR^u, BR = DR^2, WR^v, QR = DR^n representing an fpf
## automorphism group of type 2 over the ring R = ZmodnZ(p^f) such that
##   	m ... order of AR with AR a power of WR
##	d ... (BR*WR^u)^-1*AR*(BR*WR^u) = AR^(p^d)  
##	WR is induced by x -> x*w and w is primitive in GF(p^e)
## Note that d has to be even and n = e/gcd(e,d) odd; d <> 0 but possibly
## d = e.
##      DR is induced by x -> x^(p^(d/2))  
## Then B = D^2 and Q = D^n commute and with q = p^d:
##      (BR*WR^u)^n = (WR^u)^[1+p^d+p^2d+...+p^(n-1)d] 
##		    = (WR^u)^[1+q+q^2+...+q^(n-1)] 
##		    = (WR^u)^[(q^n-1)/(q-1)] 
##		    = AR^mprime 
##      e ... (QR*WR^v)^-1*AR*(QR*WR^v) = AR^(p^(e/2))
##	l ... (QR*WR^v)^-1*(BR*WR^u)*(QR*WR^v) = (BR*WR^u)^l
##

  local e,	# degree of irred. representation of C_m over GF(p) 
	n,    	# n = e/gcd(e,d)
	q,	# q = p^d
	u, v, k,
	F, gens, 
	w, W, WR, D, DR,
	a, A, AR, R, ARq, ARk, XR, M, y, I,
	i, j, x;

    e := OrderMod( p, m );

    if e = 0 or e mod 2 <> 0 then
      Error( "parameters not feasible: e has to be even" );
    fi;

    n := e/Gcd( e, d );

## now: beta^n = alpha^mprime
## m/mprime divides q-1

    k := p^(e/2);
    q := p^d;
    Info( InfoNearRing, 3, " p^d = ", q );
    if q mod (m/mprime) <> 1 then
      Error( "parameters not feasible: m/mprime divides q-1" );
    fi;	

## u is determined by (WR^u)^[(q^n-1)/(q-1)] = AR^mprime
## same as in FGRepMC( p, f, m, mprime, d) with possibly q = p^e
    u := ((p^(e/n)-1)*mprime)/m;
    if n = 1 then
## v is determined by ( Q*W^[v*(p^(e/2)-1)/2] )^2 = -I, that is, for example
      v := 1;
    else
## v has to be determined from the equality  q^-1*beta*q = beta^l
      v := SolutionDiophant( (q-1)*(k-1)/2, p^e-1, u*(k-(q^l-1)/(q-1)) );
      if v = fail then
        Error( "parameters not feasible" );
      fi;
      v := v[1]; 
    fi;     

    F := GF( p^e ); 
    w := Z(p^e);
    gens := Basis( F );	
    W := List( gens, x -> Coefficients( gens, w*x ) );
    D := List( gens, x -> Coefficients( gens, x^(p^(d/2)) ) );

## for an integer i coprime to m the irred representations are formed by: 
## (W^i)^QuoInt( p^e-1, m )
## D^2*W^(iu)
## D^n*(W^iv)^[(p^(e/2)-1)/2]
    if f = 1 then
      return [W^QuoInt( p^e-1, m ), W^u, D^2, W^(v*((k-1)/2)), D^n];
    fi;

## the representation over GF(p) determines a representation over ZmodnZ( p^f )
    R := ZmodnZ( p^f );
    WR := (MatrixInt( W )*One( R ))^(p^(f-1));
## now WR has order relatively prime to p over R
## but WR is not necessarily congruent to W mod p 
    AR := WR^QuoInt( p^e-1, m );
    DR := MatrixInt( D )*One( R );
    ARq := AR^(p^(d/2));
    y := Flat( AR*DR-DR*ARq );
    I := IdentityMat( e )*One( R );
    M := KroneckerProduct( I, TransposedMat(ARq) )-KroneckerProduct( AR, I );
    x := SolutionMatMod( M, y, p^f );
    XR := List( [1..e], i -> x{[(i-1)*e+1..i*e]} );	
## Now (DR+XR)^-1*AR*(DR+XR) = AR^(p^(d/2)) but (DR+XR) has not yet order 2n.
## Let j be minimal such that j*e > f. Then (DR+XR)^(p^(j*e)) has order 2n
## (important: 2n divides p^e-1)  and maps AR to AR^(p^(d/2)) by conjugation. 
    DR := (DR+XR)^( p^((QuoInt(f-1, e)+1)*e) );

    return [AR, WR^u, DR^2, WR^(v*(k-1)/2), DR^n];
  end;  







############################################################################
##
#M  FpfRepresentations2( <p>, <m>, <r>, <k> )
##
##  determines the fixed point free representations of a group of type 2
##  that is, with generators a, b, q
##  and relations a^m = 1, b^n = a^mprime, q^2 = a^(m/2)  
##  b^(-1)*a*b = a^r where n is the multiplicative order of r in the group of
##  prime residues modulo m
##  q^(-1)*a*q = a^k, q^(-1)*b*q = b^l and k has order 2 in the group of prime
##  residues modulo m 
##  l is determined by:  l = -1 mod 2s and l = 1 mod (nm)/(2s*mprime) where
##       s is the maximal power of 2 dividing m
##  Additional conditions are: mprime divides m
##  each prime divisor of n divides m/mprime
##  r = 1 mod (m/mprime)
##  m is even, n is 2 times an odd number
##  k = -1 mod 4
##  k = l mod (m/mprime)
##  k <> r^(n/2) mod m
##  thus: <a,b,q>/<a> is isomorphic to C_2 x C_n not cyclic
##   
##  if k = p^(e/2)*r^(n/2) mod m, then a different presentation for the
##  same group with k = p^(e/2) mod m is chosen to compute the representations
##  more efficiently
##
##  returns a list of triples of matrices corresponding to a, b and q
##  which operate from the right on a ZmodnZ(p)-module
##  additionally a list <indexlist> of indices is returned
##


InstallMethod(
	FpfRepresentations2,
	"default",
	true,
	[IsInt, IsInt, IsInt, IsInt],
	0,
  function( p, m, r, k )

  local CmodmCstar,		# group of prime residues mod <m>
	e,       		# multiplicative order of <p> mod <m>
	n,       		# multiplicative order of <r> mod <m>
	Ubyrandprime, t,	# subgroup of the group of prime residues mod
				# <m> generated by <r,prime> 
	indexlist_ab, fpfreps_ab,	# representations of <a,b> 
	Ubykpr,			# subgroup of the group of prime residues mod
				# <m> generated by <k,prime,r>
	conjugates,		# elements of Ubykpr
				# sublist of indexlist_ab desbcribing the
				# representations of <a,b> that are conjugate
				# to another list of reps
	indexlist, fpfreps,	# result, representations of <a,b,q>
	A, B, Q, I,		# auxiliary matrices (blocks) for the
	C, S,			# 			representation
	AA, BB, QQ,		# matrices (representations of the generators
				# of the group of type II determined by the
			 	# parameters m, r and k
				# linear maps   a: x -> x*AA 
				#	        b: x -> x*BB
				#		q: x -> x*QQ 
	kchanged,		# <true> if k = r^(n/2)*p^(e/2) mod m;
				# then for computational reasons k = p^(e/2)
	mprime, l, s,
	q, d, prime, f,
	factors, auxiliary, inv, subdiagonal, M, 
	i, x;

    prime := SmallestRootInt( p );
    f := LogInt( p, prime );	 	
    r := r mod m;  
    k := k mod m; 
    n := OrderMod( r, m );
    s := Maxfactor( m, 2 );
    mprime := Cofactor( m, n );
## determine <l>:
    l := ChineseRem( [(n*m)/(2*s*mprime), 2*s], [1,-1] );

    if n mod 4 <> 2 then
      Error( "parameters are not feasible: <m> and <r> coprime" );	
    elif s = 1 then
      Error( "parameters are not feasible: <m> is even" );
#   elif s > 2 and (r+1) mod s = 0 then
#     Error( "parameters are not feasible: 4 divides <r>-1" ); 	
    elif mprime = 0 or r mod (m/mprime) <> 1 then
      Error( "parameters are not feasible: pq-condition" );
    elif k = r^(n/2) mod m then
      Error( "parameters are not feasible: k = r^(n/2) mod m, metacyclic" );
    elif OrderMod( k, m ) <> 2 or (k-l) mod (m/mprime) <> 0 then
      Error( "parameters are not feasible: k = l mod (m/mprime)" ); 
    fi;
  

    indexlist := []; fpfreps := [];
    Ubyrandprime := GroupByPrimeResidues( [r, prime], m );
    conjugates := List( Ubyrandprime, Int );

    if not k in conjugates then
## The irreducible fpf representations of <a,b,q> are induced by the
## irreducible fpf representations of the metacyclic group <a,b>. 
## Note: one rep of <a,b,q> is induced by 2 nonequivalent reps of <a,b>
      Info( InfoNearRing, 1, "GF(", prime,
		 ")-representations of the type-II-group are induced" );
      auxiliary := FpfRepresentationsMetacyclic( p, m, r );  
      fpfreps_ab := auxiliary[1]; 
      indexlist_ab := auxiliary[2];
## the degrees of the reps of <a,b> are Length( fpfreps_ab[1][1] )
      I := IdentityMat( Length( fpfreps_ab[1][1] ), ZmodnZ(p) );
      QQ := BlockMatrix( [[2, 1, I], [1, 2, -I]], 2, 2 );
      for i in [1..Length( indexlist_ab )] do	
	inv := SolutionDiophant( indexlist_ab[i], m, 1 )[1];
	if not ForAny( indexlist, x -> (x*k*inv) mod m in conjugates ) then
## the induction of the conjugate representation gives again the same
## and is therefore omitted
	  A := fpfreps_ab[i][1];
	  B := fpfreps_ab[i][2];
	  AA := BlockDiagonalMatrix( [A, A^k] );
	  BB := BlockDiagonalMatrix( [B, B^l] ); 
	  
	  Add( indexlist, indexlist_ab[i] );
	  Add( fpfreps, [AA,BB,QQ] );
	fi;
      od;	

      return [fpfreps, indexlist];	
    fi;

    Info( InfoNearRing, 1, "GF(", prime,
		 ")-representations of the type-II-group are not induced" );

    e := OrderMod( prime, m );
## either k = p^(e/2) mod m or k = r^(n/2)*p^(e/2) mod m
## in any case e is even

    kchanged := false;
    if k = (r^(n/2)*prime^(e/2)) mod m then
## intermediate change of the presentation for computational convenience  
      k := prime^QuoInt( e, 2 ) mod m;
      kchanged := true;	 
      Info( InfoNearRing, 3, "intermediate change of presentation, k = ", k ); 
    fi;   	

## for the rest of this function we may assume that k = prime^(e/2) mod m
## but the returned representations are for the initial value of k

    t := QuoInt( Size( Ubyrandprime ), e ); 
## r^t is congruent to a power of prime modulo m
    q := r^t mod m;
    if q = 1 then
      d := e;
    else		
      d := LogMod( q, prime, m );
    fi;

    auxiliary := FGRep2( prime, f, m, mprime, d, l );
    A := auxiliary[1]; C := auxiliary[2]; B := auxiliary[3];
    S := auxiliary[4]; Q := auxiliary[5];
    I := IdentityMat( Length( A ), ZmodnZ(p) );

    CmodmCstar := Units( Integers mod m );
    indexlist := List( RightCosets( CmodmCstar, Ubyrandprime ), x ->
					 	Int( Representative(x) ) );   
    subdiagonal := List( [1..t-1], x -> [x+1, x, I] ); 
    M := ((l-1)/n)*mprime*prime^(e/2);
    fpfreps := [];
    for i in indexlist do 	
      AA := BlockDiagonalMatrix( List( [0..t-1], x -> A^(i*r^x) ) );
      BB := BlockMatrix( Concatenation( subdiagonal, [[1, t, C^i*B]] ), t, t );
      QQ := BlockDiagonalMatrix( List( [0..t-1], x -> S^i*Q*A^(M*(t-1-x) ) ) );

      if kchanged then
## the conjugation of AA by BB^(m*n/(mprime*2*s))*QQ yields AA^(r^(n/2)*p^(e/2)
        Add( fpfreps, [AA,BB,BB^(m*n/(mprime*2*s))*QQ] );
      else
        Add( fpfreps, [AA,BB,QQ] );
      fi;
    od;	

    return [fpfreps, indexlist];
  end); 



############################################################################
##
  FGRepT := function( prime, f )
##
## returns matrices P, Q, R  representing an fpf tetrahedral group, SL(2,3), 
## over Z(prime^f)
## this irreducible fpf representation is unique (upto equivalence)
## note that prime has to be a prime distinct from 2 and 3
##

  local P, Q, R,
	u, v, p,
	x;

      p := prime^f;
      u := 0; v := 0;	 
      while (u^2+v^2+1) mod p <> 0 and u < p do		
	u := u+1; v := u;
        while (u^2+v^2+1) mod p <> 0 and v < p do	
 	  v := v+1;
	od;
      od;	 

      P := [[u,v],[v,-u]];
      Q := [[0,-1],[1,0]];
## (p+1)/2 represents 1/2 in ZmodnZ( p )	
      R := (p+1)/2*[[-1+u+v,-1-u+v],[1-u+v,-1-u-v]];	

      return [P, Q, R]*One( ZmodnZ( p ) );
    end;




############################################################################
##
#M  FpfRepresentations3( <p>, <m>, <r> )
##
##  determines the fixed point free representations of a group of type 3
##  that is, with generators p, q, a, b
##  and relations a^m = 1, b^n = a^mprime  
##  b^(-1)*a*b = a^r where n is the multiplicative order of r in the group of
##  prime residues modulo m
##  p and q generate the quaternion group of size 8
##  if 3 divides n, then a commutes with p,q and
##					b^(-1)*p*b = q, b^(-1)*q*b = pq
##  if 3 does not divide n, then b commutes with p,q and
##					a^(-1)*p*a = q, a^(-1)*q*a = pq
##
##  m and r have to fulfill the following conditions:
##  3 divides m; m is odd; m and r are coprime.
##  Let n be the mult. order of r mod m. Then each prime divisor of n
##  divides m.
##  Let m' be maximal such that m' divides m and m' is coprime to n. Then
##  r = 1 \mod (m/m').
##
##  returns a list of lists of matrices corresponding to p, q, a and b
##  (or if r = 1 a list of triples of matrices corresponding to p, q and a)
##  which operate from the right on a ZmodnZ(p)-module
##  additionally a list <indexlist> of indices is returned as for metacyclic
##  or cyclic groups
##


InstallMethod(
	FpfRepresentations3,
	"SL(2,3)",
	true,
	[IsInt, IsInt, IsInt],
	10,
  function( p, m, r )

  local	prime, f;

     if m <> 3 or r mod m <> 1 then
       TryNextMethod();
     fi;
     prime := SmallestRootInt( p );
     f := LogInt( p, prime );
     return [[FGRepT( prime, f )], [1]];     
  end );

 

InstallMethod(
	FpfRepresentations3,
	"default",
	true,
	[IsInt, IsInt, IsInt],
	0,
  function( p, m, r )

  local CmodmCstar,	# group of prime residues mod <m>
	n,		# multiplicative order of <r> modulo <m>
	P, Q, R,	# matrix representation of the binary tetrahedral
			# group of size 24 
	fpfreps_ab,	# representations of <a,b> 
	A, AandB,	# auxiliary matrices
	PP, QQ, AA, BB,	# matrices (representations of the generators
			# of the type III group determined by the 
			# parameters <m> and <r> )
			# linear maps, 	a: x -> x*AA
			#		b: x -> x*BB  
	fpfreps, 	# list of matrices [PP, QQ, AA, BB]
			# determining the nonequivalent representations
	indexlist, 	# AA^i, BB(i) for i in <indexlist> form all the
			# nonequivalent representations of <p,q,a,b> 
	prime, f, 
	t, mprime,
	auxiliary, I, I2,
	x;

      prime := SmallestRootInt( p );
      f := LogInt( p, prime );	 	
      r := r mod m;
      n := OrderMod( r, m );	
      mprime := Cofactor( m, n );
 
      if n = 0 then 
        Error( "parameters are not feasible: <m> and <r> coprime" );	
      elif prime = 2 or prime = 3 then
	Error( "field characteristic has to be coprime to 6" );
      elif m mod 2 = 0 or m mod 3 <> 0 or mprime = 0 or
				(n > 1 and r mod (m/mprime) <> 1) then 
	Error( "parameters are not feasible" );
      fi;

      auxiliary := FGRepT( prime, f );
      if m = 3 then 
      	return [auxiliary,[1]];
      fi; 
      P := auxiliary[1]; Q := auxiliary[2]; R := auxiliary[3];

      fpfreps := [];
      if n = 1 then
## that is, if <a,b> is cyclic
	if m mod 9 = 0 then
	  auxiliary := FpfRepresentationsCyclic( p, m ); 	
	else
	  auxiliary := FpfRepresentationsCyclic( p, m/3 );
  	fi;
	fpfreps_ab := auxiliary[1];
	indexlist := auxiliary[2];
	I := IdentityMat( Length( fpfreps_ab[1] ), ZmodnZ(p) );
	PP := KroneckerProduct( I, P );
	QQ := KroneckerProduct( I, Q );
	for A in fpfreps_ab do
	  AA := KroneckerProduct( A, R );
 	  Add( fpfreps, [PP,QQ,AA] );
	od;	
	return [fpfreps, indexlist];
      fi;

## <a,b> is not cyclic

      if n mod 3 = 0 or m mod 9 = 0 then 
	auxiliary := FpfRepresentationsMetacyclic( p, m, r );
      else 
	auxiliary := FpfRepresentationsMetacyclic( p, m/3, r );
      fi;
      fpfreps_ab := auxiliary[1];
      indexlist := auxiliary[2];
      I := IdentityMat( Length( fpfreps_ab[1][1] ), ZmodnZ(p) );
      PP := KroneckerProduct( I, P );
      QQ := KroneckerProduct( I, Q );	
      I2 := IdentityMat( 2, ZmodnZ(p) );	

      if n mod 3 = 0 then
	for AandB in fpfreps_ab do
	  AA := KroneckerProduct( AandB[1], I2 );
	  BB := KroneckerProduct( AandB[2], R );
 	  Add( fpfreps, [PP,QQ,AA,BB] );
	od;	
      elif m mod 9 = 0 then	        
	for AandB in fpfreps_ab do
	  AA := KroneckerProduct( AandB[1], R );
	  BB := KroneckerProduct( AandB[2], I2 );
 	  Add( fpfreps, [PP,QQ,AA,BB] );
	od;
      else
# Note: if 9 does not divide m, then we still want AA^mprime = BB^n,
# and not AA^mprime = BB^(3*n). Thus:
        t := SolutionDiophant( 3, m/3, 1)[1];
        for AandB in fpfreps_ab do
	  AA := KroneckerProduct( AandB[1]^t, R );
	  BB := KroneckerProduct( AandB[2], I2 );
 	  Add( fpfreps, [PP,QQ,AA,BB] );
	od;
      fi;

      return [fpfreps, indexlist]; 			
  end ); 





############################################################################
##
  FGRepO := function( prime, f )
##
## returns matrices P, Q, R, Z  representing an fpf binary octahedral group
## of order 48 over ZmodnZ(prime^f)
## note that prime has to be a prime distinct from 2 and 3
##

  local P, Q, R, Z, I,
	u, v, one, c, p,
	x;

      p := prime^f;
      u := 0; v := 0;	 
      while (u^2+v^2+1) mod p <> 0 and u < p do		
	u := u+1; v := u;
        while (u^2+v^2+1) mod p <> 0 and v < p do	
 	  v := v+1;
	od;
      od;      

      P := [[u,v],[v,-u]];
      Q := [[0,-1],[1,0]];
      R := [[-1+u+v,-1-u+v],[1-u+v,-1-u-v]]*((p+1)/2); 
      Z := [[u-v,u+v],[u+v,-u+v]]; 	

      if prime^2 mod 16 = 1 then
##  x^2-2 splits over GF(p), c^2 = 1/2 
## In this case we obtain 2 non-equivalent irreducible fpf representations of 
## degree 2.
       c := RootMod( (p+1)/2, p );
       return [[P, Q, R, c*Z], [P, Q, R, -c*Z]]*One( ZmodnZ (p) );
      else	
##  x^2-2 does not split over GF(p)
## In this case we obtain one unique (upto equivalence) irreducible fpf
## representation of degree 4, which is induced from the unique irreducible fpf
## representation of the binary tetrahedral group, <p,q,r>.
	I := [[1,0],[0,1]];
	return [[BlockDiagonalMatrix( [P, Q*P] ),
		BlockDiagonalMatrix( [Q, -Q] ), 
		BlockDiagonalMatrix( [R, R^2] ),
	        BlockMatrix( [[2,1,I], [1,2,-I]], 2, 2 )]*One( ZmodnZ (p) )];
      fi;	
    end;




############################################################################
##
  FGRep4C := function( p, f, m )
##
## returns matrices AR, CR such that
## AR^m = 1,
## CR^(-1)*AR*CR = AR^(p^(e/2)),
## CR^2 = 1/2
## for the representation of an fpf type-IV-group over R = ZmodnZ(p^f)
## cases c,d) with cyclic complement of the quaternion group in H
##
## note that p has to be a prime distinct from 2 and 3
##

  local e,	# degree of irred. representation of C_m over GF(p) 
	k,	# k = p^(e/2)
	l,	# logarithm of 1/2 for the base w 
	gens, w,
	x;

    if f > 1 then
      Error( "Sorry, these ZmodnZ(p^f)-reps are not yet implemented" ); 
    fi;

    e := OrderMod( p, m );
    k := p^(e/2);
    w := Z(p^e);
    gens := Basis( GF( p^e ) );	
    l := -LogFFE( 2*Z(p)^0, w );
## thus w^l = 1/2
    return [List( gens, x -> Coefficients( gens, x*w^((p^e-1)/m) ) ), 
	    List( gens, x -> Coefficients( gens, x^k*w^(l/(k+1)) ) )];
  end;



############################################################################
##
  FGRep4 := function( p, f, m, mprime, d  )
##
## returns matrices A, Wu, B, Ws, C such that
##   A^m = 1,
##   (B*Wu)^(-1)*A*(B*Wu) = A^(p^d), (B*Wu)^n = A^mprime,
##   (C*Ws)^(-1)*A*(C*Ws) = A^(p^(e/2)), (B*Wu)*(C*Ws) = (C*Ws)*(B*Wu)
##   (C*Ws)^2 = 1/2*I
## for the representation of an fpf type-IV-group over R = ZmodnZ(p^f)
## cases c,d) with non-cyclic complement of the quaternion group in H
##
## note that p has to be a prime distinct from 2 and 3
##

  local e,	# degree of irred. representation of C_m over GF(p) 
	n,    	# n = e/gcd(e,d)
	q,	# q = p^d
	k,	# k = p^(e/2)
	l,	# logarithm of 1/2 for the base w 
	u, v, r, s,
	F, gens, w,
	W, B, C, I,
	x;

    if f > 1 then 
      Error( "Sorry, these ZmodnZ(p^f)-reps are not yet implemented" ); 
    fi;

    e := OrderMod( p, m );
    n := e/Gcd( e, d );

## now: beta^n = alpha^mprime
## m/mprime divides q-1

    k := p^(e/2);
    q := p^d;
    F := GF( p^e ); 
    w := Z(p^e);
    gens := Basis( F );	
 
    W := List( gens, x -> Coefficients( gens, w*x ) );
    C := List( gens, x -> Coefficients( gens, x^k ) );

    l := -LogFFE( 2*Z(p)^0, w );
## thus w^l = 1/2
    v := l/(k+1);

    if n = 1 then
      u := ((p^e-1)*mprime)/m;
      I := IdentityMat(e)*One(F);
      return [W^((p^e-1)/m), W^u, I, I, C*W^v];
    fi;

    B := List( gens, x -> Coefficients( gens, x^q ) );    
## u is determined by (WR^u)^[(q^n-1)/(q-1)] = AR^mprime
## same as in FGRepMC( p, f, m, mprime, d)
    u := ((p^(e/n)-1)*mprime)/m;
## v is determined by (C*W^v)^2 = 1/2*I and (B*W^u)*(C*W^v) = (C*W^v)*(B*W^u)
    r := SolutionDiophant( q-1, k+1, -(v*(q-1))/(k-1) )[1];
    s := SolutionDiophant( q-1, k+1, u )[1];

    B := List( gens, x -> Coefficients( gens, x^q ) );

    return [W^((p^e-1)/m), W^u, B, W^((k-1)*s), C*W^(v+(k-1)*r)];
  end;  


############################################################################
##
  FGRepSQHalf := function( p, f, e )
##
##  returns an e times e matrix C over ZmodnZ(p^f) such that C^2 = 1/2*I
##  e is even and 16 does not divide p^2-1
##

  local w, gens,
        n, c, half, C,  
	x, i;

    if f > 1 then
## This case poses a problem:
## C has to commute with the representation-matrices, A and B, of the
## metacyclic complement for the quaternion group in H.
## To guarantee this, we would have to compute C together with A and B. 
      Error( "Sorry, these ZmodnZ(p^f)-reps are not yet implemented" ); 
    fi;

    w := Z(p^e);
    gens := Basis( GF(p^e) );	

    c := w^(-LogFFE( 2*Z(p)^0, w )/2);
    C := List( gens, x -> Coefficients( gens, x*c ) );
    return C;
  end;



############################################################################
##
#M  FpfRepresentations4( <p>, <m>, <r>, <k> )
##
##  determines the fixed point free representations of a group of type 4
##  that is, with generators p, q, a, b, z
##  and relations a^m = 1, b^n = a^mprime,    
##  b^(-1)*a*b = a^r where n is the multiplicative order of r in the group of
##  prime residues modulo m
##  p and q generate the quaternion group of size 8
##  b commutes with p,q and a^(-1)*p*a = q, a^(-1)*q*a = p*q
##
##  Hence, p, q, a, b generate a group of type 3, determined by m and r.
##  
##  z^2 = p^2, z^(-1)*p*z = q*p, z^(-1)*q*z = q^(-1), z^(-1)*a*z = a^k and
##  z commutes with b
##
##  m, r and k have to fulfill the following conditions:
##  3 divides m but 3 does not divide n; m is odd; m and r are coprime.
##  Let n be the mult. order of r mod m. Then each prime divisor of n
##  divides m.
##  Let m' be maximal such that m' divides m and m' is coprime to n. Then
##  r = 1 \mod (m/m').
##  k = -1 mod 3^s where s is maximal such that 3^s divides m
##  k = 1 mod (m/mprime) and k^2 = 1 mod m
##
##  returns a list of lists of matrices corresponding to p, q, a, b, z
##  (or if r = 1 a list of lists of matrices corresponding to p, q, a and z)
##  which operate from the right on a ZmodnZ(p)-module
##  additionally a list <indexlist> of indices is returned
##


InstallMethod(
	FpfRepresentations4,
	"m = 3",
	true,
	[IsInt, IsInt, IsInt, IsInt],
	50,
  function( p, m, r, k )

  local prime, f,
	fpfreps;

    if m <> 3 then 
      TryNextMethod();	
    fi;	  

    prime := SmallestRootInt( p );
    f := LogInt( p, prime );

    if prime = 2 or prime = 3 then
      Error( "field characteristic has to be coprime to 6" );
    elif r mod 3 <> 1 or k mod 3 <> 2 then  
      Error( "parameters are not feasible" );
    fi;

    fpfreps := FGRepO( prime, f );
    if Length( fpfreps ) = 1 then
## case b)      
      Info( InfoNearRing, 1, "induced representation from SL(2,3)" ); 
      return [fpfreps, [1]];
    else
## case a)
      Info( InfoNearRing, 1,
		 "exception (a): representations as direct products" ); 
      return [fpfreps, [[1,1],[-1,1]]];
    fi;
end ); 



InstallMethod(
	FpfRepresentations4,
	"case a",
	true,
	[IsInt, IsInt, IsInt, IsInt],
	40,
  function( p, m, r, k )

  local P, Q,		# fpf representation of the quaternion group over 
	quaternion, 	# ZmodnZ(p)
	fpfreps_H,	# fpf representations of H over ZmodnZ(p)
	indexlist_H,
	c, Z2, ZZ,
	fpfreps,	# list of matrices [PP,QQ,AA,BB,ZZ] (resp. for r = 1
			# [PP,QQ,AA,ZZ]) determining
			# the nonequivalent representations
	indexlist,
	n,		# mult. order of r modulo m
	auxiliary,			
	prime, f, 
	mprime, 
 	x;

## checking the necessary conditions on the input parameters m, r, k:
    prime := SmallestRootInt( p );
    n := OrderMod( r, m );
    mprime := Cofactor( m, n );

    if prime = 2 or prime = 3 then
      Error( "field characteristic has to be coprime to 6" );
    elif m mod 2 = 0 or n mod 3 = 0 or mprime = 0 or
			 (n > 1 and r mod (m/mprime) <> 1) then
      Error( "parameters are not feasible: 3 divides m; pq-condition" );
    elif OrderMod( k, m ) <> 2 or (k+1) mod 3 <> 0 or
			 (n > 1 and k mod (m/mprime) <> 1) then
      Error( "parameters are not feasible: k has order 2" );
    fi; 

 
    if not( m mod 9 <> 0 and (k mod (m/3) = 1 or m = 3) and
						 prime^2 mod 16 = 1 ) then
       TryNextMethod();	
    fi;
##
## exception (a): 2 is a square in GF(prime)
##
    Info( InfoNearRing, 1,
		 "exception (a): representations as direct products" ); 

    auxiliary := FpfRepresentations3( p, m, r );
    fpfreps_H := auxiliary[1];  
    indexlist_H := auxiliary[2];
    P := fpfreps_H[1][1]; Q := fpfreps_H[1][2];
    Z2 := P-P*Q;
    c := RootMod( (p+1)/2, p );
    ZZ := c*Z2;
## Each irred representation of H can be extended to two non-equivalent
## irred representations of <p,q,a,b,z> of the same degree by adding
## ZZ resp. -ZZ	
    fpfreps := Concatenation( List( fpfreps_H, x -> 
		[Concatenation( x, [ZZ] ), Concatenation( x, [-ZZ] )] ) );
    indexlist := Concatenation( List( indexlist_H, x -> [[1,x],[-1,x]] ) );
  
    return [fpfreps, indexlist];
end );



InstallMethod(
	FpfRepresentations4,
	"case b",
	true,
	[IsInt, IsInt, IsInt, IsInt],
	30,
  function( p, m, r, k )

  local P, Q,		# fpf representation of the quaternion group over 
	quaternion, 	# ZmodnZ(p)
	fpfreps_H,	# fpf representations of H over ZmodnZ(p)
	indexlist_H,
	C, Z2, ZZ,
	e, auxiliary, t,			
	prime, f, 
 	x;

    prime := SmallestRootInt( p );
    e := OrderMod( p, m/3 );

    if not (m mod 9 <> 0 and (k mod (m/3) = 1 or m = 3) and e mod 2 = 0) then
       TryNextMethod();	
    fi;
##
## exception (b): determine a 2 times 2 matrix C induced by scalar
## multiplication in GF(prime^2) such that C^2 = 1/2 I2 
##
    Info( InfoNearRing, 1, "exception (b): extending type-III representations" ); 
    auxiliary := FpfRepresentations3( p, m, r );
    fpfreps_H := auxiliary[1];  
    indexlist_H := auxiliary[2];

    f := LogInt( p, prime );
    quaternion := FGRepQ( p, f );  	
    P := quaternion[1]; Q := quaternion[2];
    Z2 := P-P*Q;
## Note: For f > 1 there is a problem:
## C has to commute with the representation-matrices, A and B, of the
## metacyclic complement for the quaternion group in H.
## To guarantee this, we would have to compute C together with A and B. 
    t := Size( GroupByPrimeResidues( [p,r], m/3 ) )/e; 	
    C := FGRepSQHalf( prime, f, e );
    ZZ := BlockDiagonalMatrix( List( [1..t], x ->
					 KroneckerProduct( C, Z2 ) ) );

## each irred representation of H can be extended to an irred rep of
## <p,q,a,b,z> of the same degree by adding ZZ

    return [List( fpfreps_H, x -> Concatenation( x, [ZZ] ) ), indexlist_H];
end );



InstallMethod(
	FpfRepresentations4,
	"cases c,d",
	true,
	[IsInt, IsInt, IsInt, IsInt],
	20,
  function( p, m, r, k )

  local P, Q, R,	# representation of the binary tetrahedral group of
	tetrahedral,	# size 24 
	fpfreps,	# fpf representations of H over ZmodnZ(p)
	indexlist,
	CmodmCstar,	# group of prime residues modulo m (resp. m/3 if
			# m mod 9 <> 0 )
	Ubykpr,
	A, B, C, Wu, Ws, I,
	Z2, I2, subdiagonal,
	PP, QQ, AA, BB, ZZ,
	n, mprime,
	e, t, auxiliary,			
	prime, f,
	q, d, 
 	x, i;

    prime := SmallestRootInt( p );
    if m mod 9 = 0 then
      e := OrderMod( prime, m );
      if not( e mod 2 = 0 and (k-prime^(e/2)) mod m = 0 ) then  
       TryNextMethod();	 	
      fi;
##  exceptional case d)
      Info( InfoNearRing, 1, "exception (d): extending type-III representations" );
      n := OrderMod( r, m );	
      mprime := Cofactor( m, n );		
    else   ## m only divisible by 3	
      e := OrderMod( prime, m/3 );
      if not( e mod 2 = 0 and (k-prime^(e/2)) mod (m/3) = 0 ) then  
       TryNextMethod();      
      fi;
##  exceptional case c)
      Info( InfoNearRing, 1, "exception (c): extending type-III representations" );
      n := OrderMod( r, m );  ## OrderMod( r, m ) = OrderMod( r, m/3 )	
      mprime := Cofactor( m, n );		
      m := m/3;	
    fi;

##
## Note: finding matrix representations for a,b,z fulfilling
## z^(-1)*a*z = a^k and z^(-1)*b*z = b is best done simultanously.
## Therefore we do not use the representations of H and extend them with a
## matrix ZZ, but compute the matrices AA, BB, ZZ from scratch.
##

    CmodmCstar := Units( Integers mod m );
    Ubykpr := GroupByPrimeResidues( [prime, r], m );
    indexlist := List( RightCosets( CmodmCstar, Ubykpr ),
					 x -> Int( Representative(x) ) );   
    f := LogInt (p, prime);
    tetrahedral := FGRepT( prime, f );
    P := tetrahedral[1]; Q := tetrahedral[2]; R := tetrahedral[3]; 
    Z2 := P-P*Q;
    I := IdentityMat(Size(Ubykpr))*One(ZmodnZ(p));	
    PP := KroneckerProduct( I, P );
    QQ := KroneckerProduct( I, Q );

    if r mod m = 1 then
## that is, the complement of the quaternion group in H is cyclic
      auxiliary := FGRep4C( prime, f, m );
      A := auxiliary[1];
      C := auxiliary[2];
      ZZ := KroneckerProduct( C, Z2 );
      fpfreps := List( indexlist, x ->
			 [PP,QQ, KroneckerProduct( A^x, R ), ZZ] );
    else
## that is, the complement of the quaternion group in H is not cyclic,
## but metacyclic
      t := Size( Ubykpr )/e;
      r := r mod m;
      q := r^t mod m;
      d := LogMod( q, prime, m ); 
# If q = 1, then d = 0. 
      auxiliary := FGRep4( prime, f, m, mprime, d  );        
      A := auxiliary[1];
      Wu := auxiliary[2]; B := auxiliary[3];
      Ws := auxiliary[4]; C := auxiliary[5];
      I := IdentityMat(e)*One(ZmodnZ(p));
      subdiagonal := List( [1..t-1], x -> [x+1, x, I] );
      I2 := IdentityMat(2)*One(ZmodnZ(p));

      fpfreps := [];
      for i in indexlist do
      	AA := KroneckerProduct(
		 BlockDiagonalMatrix( List( [0..t-1], x -> A^(i*r^x) ) ), R );
        BB := KroneckerProduct(
     BlockMatrix( Concatenation( subdiagonal, [[1, t, B*Wu^i]] ), t, t ), I2);
        ZZ := KroneckerProduct(
		BlockDiagonalMatrix( List( [0..t-1], x -> C*Ws^i ) ), Z2 );
        Add( fpfreps, [PP,QQ,AA,BB,ZZ] );
      od;	
    fi;
 	
    return [fpfreps, indexlist]; 
end );



InstallMethod(
	FpfRepresentations4,
	"induced representations",
	true,
	[IsInt, IsInt, IsInt, IsInt],
	0,
  function( p, m, r, k )

  local P, Q, A, B, I,	# auxiliary matrices (representation of the
	auxiliary,	# generators of H)
	indexlist_H, fpfreps_H,
	PP, QQ,		# matrices  (representing the generators of the
	AA, BB, ZZ,	# type-IV-group determined by m,r and k)
	fpfreps,	# list of matrices [PP,QQ,AA,BB,ZZ] (resp. for r = 1
			# [PP,QQ,AA,ZZ]) determining
			# the nonequivalent representations
	indexlist,
	CmodmCstar,	# group of prime residues modulo m (resp. m/3 if
			# m mod 9 <> 0 ) 
	n,		# size of the subgroup of CmodmCstar generated by r
	Ubykpr,		# subgroup of CmodmCstar generated by k, prime and r
	rcs, 		# RightCosets( CmodmCstar, Ubykpr )
	constituents,	# for induction: list of indices i such that the
			# fpfreps_H[i] induce nonequivalent representations 
	prime,
 	i, x, y;

##
## If none of the exceptional cases holds, then the irreducible representations
## are simply those induced by the irred reps of H of type III and index 2.
##

    prime := SmallestRootInt( p );
    n := OrderMod( r, m );

    Info( InfoNearRing, 1, "GF(", prime,
		 ")-representations of the type-IV-group are induced" );

    auxiliary := FpfRepresentations3( p, m, r );
    fpfreps_H := auxiliary[1];
    indexlist_H := auxiliary[2];
    I := IdentityMat( Length(fpfreps_H[1][1]), ZmodnZ(p) );
    P := fpfreps_H[1][1]; Q := fpfreps_H[1][2];
    PP := BlockDiagonalMatrix( [P, Q*P] );
    QQ := BlockDiagonalMatrix( [Q, -Q] ); 
    ZZ := BlockMatrix( [[2, 1, I], [1, 2, -I]], 2, 2 );
 
    if m mod 9 <> 0 and (k mod (m/3) = 1 or m = 3) then
## Note: if 9 does not divide m and k = 1 modulo m/3 (and e is odd),
## then the nonequivalent irred reps of H all induce nonequivalent irred reps
## of <p,q,a,b,z>
     constituents := [1..Length(indexlist_H)];
    else
## 2 respective nonequivalent irred reps of H (index i and i*k)
## induce the same irred rep of <p,q,a,b,z>
      if m mod 9 = 0 then	
        CmodmCstar := Units( Integers mod m );
        Ubykpr := GroupByPrimeResidues( [k, prime, r], m );
      else
       CmodmCstar := Units( Integers mod (m/3) );
        Ubykpr := GroupByPrimeResidues( [k, prime, r], m/3 );
      fi; 
      rcs := RightCosets( CmodmCstar, Ubykpr );
      constituents := List( rcs, x ->
		PositionProperty( indexlist_H, y -> y*One(CmodmCstar) in x ) );
    fi;

    k := k mod m;
    fpfreps := []; indexlist := [];
    if n = 1 then
## that is, if <a,b> is cyclic
      for i in constituents do
        A := fpfreps_H[i][3];
	AA := BlockDiagonalMatrix( [A, A^k] );
	Add( indexlist, indexlist_H[i] );
	Add( fpfreps, [PP, QQ, AA, ZZ] );
      od;
    else
      for i in constituents do	
      	A := fpfreps_H[i][3]; B := fpfreps_H[i][4];
      	AA := BlockDiagonalMatrix( [A, A^k] );
      	BB := BlockDiagonalMatrix( [B, B] );   
      	Add( indexlist, indexlist_H[i] );
      	Add( fpfreps, [PP, QQ, AA, BB, ZZ] );
      od;
    fi;
    
    return [fpfreps, indexlist]; 

end );







##
##  FPF AUTOMORPHISM GROUPS OF ABELIAN GROUPS 
##


##
##  In order to build the generators of the non-conjugate fixed point free
##  automorphism groups from the irreducible fpf representations, some
##  combinatorical functions are needed. 
##


  OnRightSets := function( list, g )   
    return SortedList( OnRight( list, g ) );
  end;

  OnRightTuplesSets := function( e, g )
    return List( e, function(i) return OnRightSets( i, g ); end );
  end;  


############################################################################
##
  IndexCombs := function( nr, indexlist, invariancegrp )
##
##  nr[i]   		number of indices to be combined	
##  indexlist[i] 	indices to be combined, coset representatives
##			of the respective invariancegrp[i]
##
##  returns combinations of coset-representatives not necessarily
##  corresponding to the indices in indexlist;
##  used to find the non-conjugate CYCLIC fpf aut groups acting on 
##  an abelian group from fpf representations.
##  Don't use this function for non-cyclic fpf aut groups. Take IndexCombs2 
##  instead.
##
  local G,		## group acting on the index combinations; index combs
			## from the same G-orbit yield conjugate aut groups
	one, U, 
	cosets, reps,
	tuples, l,
	i, tuple, x, y;
 

    G := Parent( invariancegrp[1] );
    one := One( invariancegrp[1] );
    l := Length(nr);
    cosets := []; tuples := [];
    for i in [1..l] do
      U := invariancegrp[i];	
      Add( cosets, List( indexlist[i], x -> RightCoset( U, x*one ) ) );
      Add( tuples, UnorderedTuples( cosets[i], nr[i] ) );
    od;

    reps := List( Orbits( G, Cartesian( tuples ), OnRightTuplesSets ),
							 Representative ); 
##
## Note: the indices as determined in the next step are not necessarily
## the same numbers as the initial representatives as given in indexlist. 
## However, for the use of the combinations in FpfAutGrpsC 
## this does not matter 
##
   return List( reps, tuple -> List( tuple, set ->
			 List( set, x -> Int(Representative(x)) ) ) );
 end;


############################################################################
##
  IndexCombs2 := function( nr, indexlist, invariancegrp, G )
##
##  nr[i]   		number of indices to be combined	
##  indexlist[i] 	indices to be combined, coset representatives
##			of the respective invariancegrp[i]
##  G			group acting on the index combinations; index combs
##			from the same G-orbit yield conjugate aut groups
##
##  returns combinations of the positions of cosets with respect to indexlist 
##

  local one,
	cosets, reps, U,
	tuples, l, positions,
	i, tuple, x, y;
 
    one := One(G);
    l := Length(nr);
    cosets := []; tuples := [];
    for i in [1..l] do
      U := invariancegrp[i];	
      Add( cosets, List( indexlist[i], x -> RightCoset( U, x*one ) ) );
      Add( tuples, UnorderedTuples( cosets[i], nr[i] ) );
    od;
    Info( InfoNearRing, 3, "Cartesian( tuples ) = ", Cartesian( tuples ) );
    reps := List( Orbits( G, Cartesian( tuples ), OnRightTuplesSets ),
							 Representative ); 
    positions := [];
    for tuple in reps do
      Add( positions, List( [1..l], i -> List( tuple[i],
					x -> Position( cosets[i], x ) ) ) );
    od;
    return positions;
 end;




############################################################################
##
#M  FpfAutGrpsC( <primepowers>, <dimensions>, <m> )
##
##  returns the "matrix"-generators for non conjugate cyclic fpf groups of
##  order m on the abelian group
##  (Z_p1)^d1 x...x (Z_pl)^dl for <primepowers> = [p1,...,pl]
##  and the integers <dimensions> = [d1,...,dl]
##
##  <primepowers> has to be sorted, [8,4,2,27,3,...], according to the order
##  as given in FpfAutomorphismGroupsCyclic
##

InstallMethod(
	FpfAutGrpsC,
	"default",
	true,
	[IsList, IsList, IsInt],
	0,
	function( ps, ds, m )  


  local CmodmCstar,	# multiplicative group of residues prime to m
	prime, f,
	l,
	nr, indexlist, invariancegrp,
	A,
	fpfreps,
        U, e,		# subgroup of CmodmCstar generated by prime and
			# its size (degree of the irr. fpf rep over GF(prime))
	matrices,
  	pnew,
	i, tuple, x, y;

    CmodmCstar := Units( Integers mod m );

    prime := 0;
    l := Length( ps ); 	
    nr := []; indexlist := []; invariancegrp := []; fpfreps := [];
    for i in [1..l] do
      pnew := SmallestRootInt( ps[i] );
      f := LogInt( ps[i], pnew );
      if pnew <> prime then
	prime := pnew;  
	A := MatrixInt( FGRepC( prime, f, m ) );
        U := GroupByPrimeResidues( [prime], m );
	e := Size( U );	
      fi;

      if ds[i] mod e <> 0 then
	Error( "no fpf aut group of size m on this group: degree" );
      fi; 	
 
      Add( nr, ds[i]/e );
      Add( indexlist, List( RightCosets(CmodmCstar, U), Representative ) );
      Add( invariancegrp, U );

      Add( fpfreps, A*One(ZmodnZ( ps[i] )) );
    od;

    matrices := [];
    for tuple in IndexCombs( nr, indexlist, invariancegrp ) do
      Add( matrices, List( [1..l],
	 i -> BlockDiagonalMatrix( List( tuple[i], x -> fpfreps[i]^x ) ) ) );
    od;

    return matrices;
  end );


############################################################################
##
#M  FpfAutomorphismGroupsCyclic ( <ints>, <m> )
##
## determines one representative for each conjugacy class of cyclic
## fpf automorphism groups of size m acting on the abelian group
## AbelianGroup( ints )
## returns this list of nonconjugate fpf aut groups and AbelianGroup( ints )
##
## ints has to be a list of prime power integers and is ordered in
## the following function
##

InstallMethod(
	FpfAutomorphismGroupsCyclic,
	"default",
	true,
	[IsList, IsInt],
	0,
	function( ints, m )  

  local coll,
	ps, ds,
	matrices,
	G, gens,
	x, y;

    coll := Collected( ints );
    Sort( coll, function( x, y ) 
		local p, q;
		  p := SmallestRootInt(x[1]);
	          q := SmallestRootInt(y[1]);
	        return p > q or ( p = q and x[1] > y[1] ); end );

    ps := List( coll, x -> x[1] ); 
    ds := List( coll, x -> x[2] );
    
    matrices := FpfAutGrpsC( ps, ds, m );

    G := AbelianGroup( Concatenation( List( [1..Length( ps )], i ->
		 			List( [1..ds[i]], x -> ps[i] ) ) ) );  
    gens := GeneratorsOfGroup( G );  

    return [List( matrices, A -> GroupHomomorphismByMatrix( G, gens, A ) ),
									 G];
end );




############################################################################
##
#M  FpfAutGrpsMC( <primepowers>, <dimensions>, <m>, <r> )
##
##  returns the matrix generators for non conjugate metacyclic fpf groups of
##  order m*OrderMod(r,m) on the abelian group
##  (Z_p1)^d1 x...x (Z_pl)^dl for <primepowers> = [p1,...,pl]
##  and the integers <dimensions> = [d1,...,dl]
##
##  <primepowers> has to be sorted, [8,4,2,27,3,...], according to the order
##  as given in FpfAutomorphismGroupsMetacyclic, etc.
##

InstallMethod(
	FpfAutGrpsMC,
	"default",
	true,
	[IsList, IsList, IsInt, IsInt],
	0,
	function( ps, ds, m, r )  


  local CmodmCstar,	# multiplicative group of residues prime to m
	prime, f,
	l,
	nr, indexlist, invariancegrp,
	aux,
	fpfreps,
        Ubyrandprime,	# subgroup of CmodmCstar generated by r and prime and
	deg,		# its size (degree of the irr. fpf rep over GF(prime))
	n,		# OrderMod( r, m )
	mprime, q,
	G,		## subgroup of CmodmCstar, acting on the index
			## combinations; index combs from the same G-orbit
			## yield conjugate aut groups
 	nrmatgens,
	matrices,
  	pnew,
	i, g, aandb, tuple, x, y;

    CmodmCstar := Units( Integers mod m );

    prime := 0;
    l := Length( ps ); 	
    nr := []; indexlist := []; invariancegrp := []; fpfreps := [];
    for i in [1..l] do
      pnew := SmallestRootInt( ps[i] );
      f := LogInt( ps[i], pnew );
      if pnew <> prime then
	prime := pnew;  
	aux := FpfRepresentationsMetacyclic( ps[i], m, r );
        Ubyrandprime := GroupByPrimeResidues( [prime, r], m );
	deg := Size( Ubyrandprime );	
      fi;

      if ds[i] mod deg <> 0 then
	Error( "no such fpf aut group on this group: degree" );
      fi; 	
 
      Add( nr, ds[i]/deg );
      Add( indexlist, aux[2] );
      Add( invariancegrp, Ubyrandprime );
      Add( fpfreps, List( aux[1], aandb -> List( aandb, MatrixInt ) ) );
    od;
##
## Determine the group G acting on the combinations of representation indices
## such that index combs from the same G-orbit yield conjugate aut groups.
## Note: For reps of cyclic groups G = CmodmCstar;
##	 for metacyclic groups G can be smaller than CmodmCstar.
##
    n := OrderMod( r, m ); 	
    q := Maxfactor( m, 2 );
    if q > 2 and (r+1) mod q = 0 and n mod 4 = 2 then
## type II, quaternion 2-Sylow subgroup
      mprime := Cofactor( m/2, n/2 );
      if mprime = 0 or r mod (m/mprime) <> 1 then
	Error( "parameters are not feasible" );
      fi;
      Info( InfoNearRing, 1,
	 	"parameters are feasible; metacyclic group of type II" );
    else	
## type I, all Sylow subgroups are cyclic
      mprime := Cofactor( m, n );
      if mprime = 0 or r mod (m/mprime) <> 1 then
	Error( "parameters are not feasible" );
      fi;
      Info( InfoNearRing, 1,
 		"parameters are feasible; metacyclic group of type I" );
    fi;

    g := Gcd( n, m/mprime ); 
    G := GroupByPrimeResidues( 
	Filtered( List( [0..m/g-1], x -> 1+x*g ), y -> Gcd(m,y) = 1 ), m );

    Info( InfoNearRing, 3, " Size( G ) = ", Size( G ) );

    matrices := [];
    nrmatgens := 2;    ##  2 generators for the metacyclic group
    for tuple in IndexCombs2( nr, indexlist, invariancegrp, G ) do
      Add( matrices, List( [1..nrmatgens], z -> List( [1..l], i ->
	BlockDiagonalMatrix( List( tuple[i], x -> fpfreps[i][x][z] ) ) ) ) );  
    od;

    return matrices;
  end );





############################################################################
##
#M  FpfAutomorphismGroupsMetacyclic( ints, m, r )
##
## returns a list of metacyclic fpf automorphism groups 
## (type I or type II) of size m*OrderMod(r,m)
## acting on the abelian group AbelianGroup( ints )
##
## ints has to be a list of prime power integers and is ordered in the
## following function 
##

InstallMethod(
	FpfAutomorphismGroupsMetacyclic,
	"default",
	true,
	[IsList, IsInt, IsInt],
	0,
	function( ints, m, r )  

  local coll,
	ps, ds, l,
	matrices,
	G, gens,
	x, y, mats;

    coll := Collected( ints );
    Sort( coll, function( x, y ) 
		local p, q;
		  p := SmallestRootInt(x[1]);
	          q := SmallestRootInt(y[1]);
	        return p > q or ( p = q and x[1] > y[1] ); end );

    ps := List( coll, x -> x[1] ); 
    ds := List( coll, x -> x[2] );
    l := Length( ps );
    
    matrices := FpfAutGrpsMC( ps, ds, m, r );

    G := AbelianGroup( Concatenation( List( [1..l], i ->
		 			List( [1..ds[i]], x -> ps[i] ) ) ) );  
    gens := GeneratorsOfGroup( G );  

    return [List( matrices, mats -> List( mats, x ->
			 GroupHomomorphismByMatrix( G, gens, x ) ) ), G];
end );







############################################################################
##
#M  FpfAutGrps2( <primepowers>, <dimensions>, <m>, <r>, <k> )
##
##  returns the matrix generators for non conjugate type II fpf groups of
##  order 2m*OrderMod(r,m) on the abelian group
##  (Z_p1)^d1 x...x (Z_pl)^dl for <primepowers> = [p1,...,pl]
##  and the integers <dimensions> = [d1,...,dl]
##
##  <primepowers> has to be sorted, [8,4,2,27,3,...], according to the order
##  as given in FpfAutomorphismGroupsMetacyclic, etc.
##

InstallMethod(
	FpfAutGrps2,
	"default",
	true,
	[IsList, IsList, IsInt, IsInt, IsInt],
	0,
	function( ps, ds, m, r, k )  


  local CmodmCstar,	# multiplicative group of residues prime to m
	prime, f,
	l,
	nr, indexlist, invariancegrp,
	aux,
	fpfreps,
        Ubykpr,		# subgroup of CmodmCstar generated by k, r, prime and
	deg,		# its size (degree of the irr. fpf rep over GF(prime))
	n,		# OrderMod( r, m )
	mprime, factors,
	G,		## subgroup of CmodmCstar, acting on the index
			## combinations; index combs from the same G-orbit
			## yield conjugate aut groups
	matrices, nrmatgens,
  	pnew,
	i, g, abq, tuple, x, y;


    CmodmCstar := Units( Integers mod m );

    prime := 0;
    l := Length( ps ); 	
    nr := []; indexlist := []; invariancegrp := []; fpfreps := [];
    for i in [1..l] do
      pnew := SmallestRootInt( ps[i] );
      f := LogInt( ps[i], pnew );
      if pnew <> prime then
	prime := pnew;  
	aux := FpfRepresentations2( ps[i], m, r, k );
	Ubykpr := GroupByPrimeResidues( [k, prime, r], m );
	deg := Size( Ubykpr );	
      fi;

      if ds[i] mod deg <> 0 then
	Error( "no such fpf aut group on this group: degree" );
      fi; 	
 
      Add( nr, ds[i]/deg );
      Add( indexlist, aux[2] );
      Add( invariancegrp, Ubykpr );
      Add( fpfreps, List( aux[1], abq -> List( abq, MatrixInt ) ) );
    od;

## Determine the group G acting on the combinations of representation indices
## such that index combs from the same G-orbit yield conjugate aut groups.
## Note: G is a subgroup of CmodmCstar.
## For the type-II-group (determined by m,r,k) G is the same as for the
## characteristic metacyclic group of index 2 (determined by m,r). 

    n := OrderMod( r, m );
    mprime := Cofactor( m, n );	
    g := Gcd( n, m/mprime ); 

    G := GroupByPrimeResidues( 
	Filtered( List( [0..m/g-1], x -> 1+x*g ), y -> Gcd(m,y) = 1 ), m );

    Info( InfoNearRing, 3, " Size( G ) = ", Size( G ) );

    matrices := [];
    nrmatgens := 3;    ##  3 generators for the type-II-group
    for tuple in IndexCombs2( nr, indexlist, invariancegrp, G ) do
      Add( matrices, List( [1..nrmatgens], z -> List( [1..l], i ->
	BlockDiagonalMatrix( List( tuple[i], x -> fpfreps[i][x][z] ) ) ) ) );  
    od;

    return matrices;
  end );





############################################################################
##
#M  FpfAutomorphismGroups2( ints, m, r, k )
##
## returns a list of fpf automorphism groups of type II of size
## 2m*OrderMod(r,m)
## acting on the abelian group AbelianGroup( ints )
##
## ints has to be a list of prime power integers and is ordered in the
## following function 
##

InstallMethod(
	FpfAutomorphismGroups2,
	"default",
	true,
	[IsList, IsInt, IsInt, IsInt],
	0,
	function( ints, m, r, k )  

  local coll,
	ps, ds, l,
	matrices,
	G, gens,
	x, y, mats;

    coll := Collected( ints );
    Sort( coll, function( x, y ) 
		local p, q;
		  p := SmallestRootInt(x[1]);
	          q := SmallestRootInt(y[1]);
	        return p > q or ( p = q and x[1] > y[1] ); end );

    ps := List( coll, x -> x[1] ); 
    ds := List( coll, x -> x[2] );
    l := Length( ps );
    
    matrices := FpfAutGrps2( ps, ds, m, r, k );

    G := AbelianGroup( Concatenation( List( [1..l], i ->
		 			List( [1..ds[i]], x -> ps[i] ) ) ) );  
    gens := GeneratorsOfGroup( G );  

    return [List( matrices, mats -> List( mats, x ->
			 GroupHomomorphismByMatrix( G, gens, x ) ) ), G];
end );





############################################################################
##
#M  FpfAutGrps3( <primepowers>, <dimensions>, <m>, <r> )
##
##  returns the matrix generators for non conjugate fpf groups of
##  type III and of order 8*m*OrderMod(r,m) on the abelian group
##  (Z_p1)^d1 x...x (Z_pl)^dl for <primepowers> = [p1,...,pl]
##  and the integers <dimensions> = [d1,...,dl]
##
##  <primepowers> has to be sorted, [8,4,2,27,3,...], according to the order
##  as given in FpfAutomorphismGroupsMetacyclic, etc.
##


InstallMethod(
	FpfAutGrps3,
	"default",
	true,
	[IsList, IsList, IsInt, IsInt],
	0,
	function( ps, ds, m, r )  


  local CmodmCstar,	# multiplicative group of residues prime to m
			# respectively m/3 if the order of the group is
			# not divisible by 9
	one,
	prime, f,
	l,
	nr, indexlist, invariancegrp,
	aux,
	fpfreps,
	nrmatgens,	# number of matrix generators of the type III group
        Ubyrandprime,	# subgroup of CmodmCstar generated by r and prime and
	deg,		# its size (degree of the irr. fpf rep over GF(prime))
	n,		# OrderMod( r, m )
	mprime, factors,
	G,		## subgroup of CmodmCstar, acting on the index
			## combinations; index combs from the same G-orbit
			## yield conjugate aut groups
	matrices,
  	pnew,
	i, g, pqab, tuple, x, y;

## Determine the group G acting on the combinations of representation indices
## such that index combs from the same G-orbit yield conjugate aut groups.
## Note: G is a subgroup of CmodmCstar.
## For the type-III-group (determined by m,r) G is the same as for the
## metacyclic group (determined by m,r resp. m/3,r). 

    n := OrderMod( r, m ); 	
    if (m*n) mod 9 = 0 then
      Info( InfoNearRing, 2, "Prop. 8 b,c" );
      CmodmCstar := Units( Integers mod m );     
    else
      Info( InfoNearRing, 2, "Prop. 8 a" );
      CmodmCstar := Units( Integers mod (m/3) );
    fi;

    one := One( CmodmCstar );
    mprime := Cofactor( m, n );
    g := Gcd( n, m/mprime );
    if n = 1 then
      G := CmodmCstar;
    else
      G := Subgroup( CmodmCstar,
	 List( Filtered( List( [0..m/g-1], x -> 1+x*g ), y -> Gcd(m,y) = 1 ),
	    z -> z*one) );
    fi;

    Info( InfoNearRing, 3, " Size( G ) = ", Size( G ) );

    prime := 0;
    l := Length( ps ); 	
    nr := []; indexlist := []; invariancegrp := []; fpfreps := [];
    for i in [1..l] do
      pnew := SmallestRootInt( ps[i] );
      f := LogInt( ps[i], pnew );
      if pnew <> prime then
	prime := pnew;  
	aux := FpfRepresentations3( ps[i], m, r );
        Ubyrandprime := Subgroup( CmodmCstar, [prime, r]*one );
	deg := 2*Size( Ubyrandprime );	
      fi;

      if ds[i] mod deg <> 0 then
	Error( "no such fpf aut group on this group: degree" );
      fi; 	
 
      Add( nr, ds[i]/deg );
      Add( indexlist, aux[2] );
      Add( invariancegrp, Ubyrandprime );
      Add( fpfreps, List( aux[1], pqab -> List( pqab, MatrixInt ) ) );
    od;

## The number of matrix generators equals either 3 or 4
    nrmatgens := Length( fpfreps[1][1] );

    matrices := [];
    for tuple in IndexCombs2( nr, indexlist, invariancegrp, G ) do
      Add( matrices, List( [1..nrmatgens], z -> List( [1..l], i ->
	BlockDiagonalMatrix( List( tuple[i], x -> fpfreps[i][x][z] ) ) ) ) );  
    od;

    return matrices;
  end );



############################################################################
##
#M  FpfAutomorphismGroups3( ints, m, r )
##
## returns a list of fpf automorphism groups of type III
## of size 8m*OrderMod(r,m)
## acting on the abelian group AbelianGroup( ints )
##
## ints has to be a list of prime power integers and is ordered in the
## following function 
##

InstallMethod(
	FpfAutomorphismGroups3,
	"default",
	true,
	[IsList, IsInt, IsInt],
	0,
	function( ints, m, r )  

  local coll,
	ps, ds, l,
	matrices,
	G, gens,
	x, y, mats;

    coll := Collected( ints );
    Sort( coll, function( x, y ) 
		local p, q;
		  p := SmallestRootInt(x[1]);
	          q := SmallestRootInt(y[1]);
	        return p > q or ( p = q and x[1] > y[1] ); end );

    ps := List( coll, x -> x[1] ); 
    ds := List( coll, x -> x[2] );
    l := Length( ps );
    
    matrices := FpfAutGrps3( ps, ds, m, r );
    G := AbelianGroup( Concatenation( List( [1..l], i ->
		 			List( [1..ds[i]], x -> ps[i] ) ) ) );  
    gens := GeneratorsOfGroup( G );  

    return [List( matrices, mats -> List( mats, x ->
			 GroupHomomorphismByMatrix( G, gens, x ) ) ), G];
end );






############################################################################
##
#M  FpfAutGrps4( <primepowers>, <dimensions>, <m>, <r>, <k> )
##
##  returns the matrix generators for non conjugate fpf groups of
##  type IV and of order 16*m*OrderMod(r,m) on the abelian group
##  (Z_p1)^d1 x...x (Z_pl)^dl for <primepowers> = [p1,...,pl]
##  and the integers <dimensions> = [d1,...,dl]

InstallMethod(
	FpfAutGrps4,
	"k = 1 mod (m/3)",
	true,
	[IsList, IsList, IsInt, IsInt, IsInt],
	10,
	function( ps, ds, m, r, k )  


  local CmodmCstar,	# multiplicative group of residues prime to 4m/3
	one,
	prime, f,
	primep, primem,
	l,
	nr, indexlist, invariancegrp,
	aux,
	fpfreps,
	nrmatgens,	# number of matrix generators of the type IV group
	Ubyrandprime,
        Ubykpr,		# subgroup of CmodmCstar generated by r, prime, k
	deg,		# degree of the irr. fpf rep over GF(prime))
	n,		# OrderMod( r, m )
	mprime, factors,
	G,		## subgroup of CmodmCstar, acting on the index
			## combinations; index combs from the same G-orbit
			## yield conjugate aut groups
	matrices,
  	pnew,
	i, g, pqabz, tuple, x, y;


## Determine the group G acting on the combinations of representation indices
## such that index combs from the same G-orbit yield conjugate aut groups
## for the case that the conditions of Prop. 10a occurs: 
##       m mod 9 <> 0, k mod (m/3) = 1
##	 and there is a prime in <ps> such that 16 divides prime^2-1
## Note: 10a is the only case where the representations of the type IV group
## are NOT determined by the representations of the complement of the binary
## octahedral group of size 48. 

    if m mod 9 = 0 or (k-1) mod (m/3) <> 0 or
      ForAll( List( ps, SmallestRootInt ), prime -> prime^2 mod 16 <> 1 ) then
      Info( InfoNearRing, 2, "Prop. 10 (a) does not apply" );
      TryNextMethod();
    fi; 

    Info( InfoNearRing, 2, "Prop. 10 (a) applies" );

    m := m/3;
    CmodmCstar := Units( Integers mod (4*m) );
    one := One( CmodmCstar );
    n := OrderMod( r, m );
    r := ChineseRem( [4,m], [1,r] );
    mprime := Cofactor( m, n ); 	
    g := Gcd( n, m/mprime );
    if n = 1 then
      G := CmodmCstar;
    else
      G := Subgroup( CmodmCstar, Concatenation( 
	List( Filtered( List( [0..m/g-1], x -> 1+x*g ),
		y -> Gcd(m,y) = 1 ), z -> ChineseRem( [4,m], [1,z] )*one ),
 	[ChineseRem( [4,m], [-1,1] )*one] ) );
    fi;

    Info( InfoNearRing, 3, " Size( G ) = ", Size( G ) );

    prime := 0;
    l := Length( ps ); 	
    nr := []; indexlist := []; invariancegrp := []; fpfreps := [];
    for i in [1..l] do
      pnew := SmallestRootInt( ps[i] );
      f := LogInt( ps[i], pnew );
      if pnew <> prime then
	prime := pnew; 
	primep := ChineseRem( [4,m], [1,prime] );
 	primem := ChineseRem( [4,m], [-1,prime] );
	aux := FpfRepresentations4( ps[i], 3*m, r, k );
	if prime^2 mod 16 = 1 then
	  aux[2] := List( aux[2], x -> ChineseRem( [4,m], x ) );	  
          Ubyrandprime := Subgroup( CmodmCstar, [primep, r]*one );
	  deg := DegreeOfIrredFpfRep4( prime, 3*m, r, k );
	else
          Ubyrandprime := Subgroup( CmodmCstar, [primep, primem, r]*one );
	  deg := DegreeOfIrredFpfRep4( prime, 3*m, r, k );          	
	fi;
      fi;

      if ds[i] mod deg <> 0 then
	Error( "no such fpf aut group on this group: degree" );
      fi; 	
 
      Add( nr, ds[i]/deg );
      Add( indexlist, aux[2] );
      Add( invariancegrp, Ubyrandprime );
      Add( fpfreps, List( aux[1], pqabz -> List( pqabz, MatrixInt ) ) );
    od;

## The number of matrix generators equals either 4 or 5
    nrmatgens := Length( fpfreps[1][1] );

    matrices := [];
    for tuple in IndexCombs2( nr, indexlist, invariancegrp, G ) do
      Add( matrices, List( [1..nrmatgens], z -> List( [1..l], i ->
	BlockDiagonalMatrix( List( tuple[i], x -> fpfreps[i][x][z] ) ) ) ) );  
    od;

    return matrices;
  end );
   


InstallMethod(
	FpfAutGrps4,
	"default",
	true,
	[IsList, IsList, IsInt, IsInt, IsInt],
	0,
	function( ps, ds, m, r, k )  


  local CmodmCstar,	# multiplicative group of residues prime to m
			# respectively m/3 if the order of the group is
			# not divisible by 9
	one,
	prime, f,
	l,
	nr, indexlist, invariancegrp,
	aux,
	fpfreps,
	nrmatgens,	# number of matrix generators of the type IV group
#	Ubyrandprime,
        Ubykpr,		# subgroup of CmodmCstar generated by r, prime, k
	deg,		# degree of the irr. fpf rep over GF(prime))
	n,		# OrderMod( r, m )
	mprime, factors,
	G,
	matrices,
  	pnew,
	i, g, pqab, tuple, x, y;

## Determine the group G acting on the combinations of representation indices
## such that index combs from the same G-orbit yield conjugate aut groups.
## Note: G is a subgroup of CmodmCstar.
## If the conditions of the previous method do not apply  for the
## type-IV-group (determined by m,r,k), then G is the same as for the
## metacyclic group (determined by m,r resp. m/3,r). 

    n := OrderMod( r, m ); 	
    if m mod 9 = 0 then    
      CmodmCstar := Units( Integers mod m );
      mprime := m;
    else 
      CmodmCstar := Units( Integers mod (m/3) );
      mprime := m/3;	
    fi;
    one := One( CmodmCstar );
    mprime := Cofactor( m, n );
    g := Gcd( n, m/mprime );
    if n = 1 then
      G := CmodmCstar;
    else
      G := Subgroup( CmodmCstar,
	 List( Filtered( List( [0..m/g-1], x -> 1+x*g ), y -> Gcd(m,y) = 1 ),
	    z -> z*one) );
    fi;

    Info( InfoNearRing, 3, " Size( G ) = ", Size( G ) );

    prime := 0;
    l := Length( ps ); 	
    nr := []; indexlist := []; invariancegrp := []; fpfreps := [];
    for i in [1..l] do
      pnew := SmallestRootInt( ps[i] );
      f := LogInt( ps[i], pnew );
      if pnew <> prime then
	prime := pnew;  
	aux := FpfRepresentations4( ps[i], m, r, k );
	Ubykpr := Subgroup( CmodmCstar, [prime, r, k]*one );
	deg := DegreeOfIrredFpfRep4( prime, m, r, k );	
      fi;

      if ds[i] mod deg <> 0 then
	Error( "no such fpf aut group on this group: degree" );
      fi; 	
 
      Add( nr, ds[i]/deg );
      Add( indexlist, aux[2] );
      Add( invariancegrp, Ubykpr );
      Add( fpfreps, List( aux[1], pqab -> List( pqab, MatrixInt ) ) );
    od;

## The number of matrix generators equals either 4 or 5
    nrmatgens := Length( fpfreps[1][1] );

    matrices := [];
    for tuple in IndexCombs2( nr, indexlist, invariancegrp, G ) do 
      Add( matrices, List( [1..nrmatgens], z -> List( [1..l], i ->
	BlockDiagonalMatrix( List( tuple[i], x -> fpfreps[i][x][z] ) ) ) ) );  
    od;

    return matrices;
  end );



############################################################################
##
#M  FpfAutomorphismGroups4( ints, m, r, k )
##
## returns a list of fpf automorphism groups of type IV
## of size 16m*OrderMod(r,m)
## acting on the abelian group AbelianGroup( ints )
##
## ints has to be a list of prime power integers and is ordered in the
## following function 
##

InstallMethod(
	FpfAutomorphismGroups4,
	"default",
	true,
	[IsList, IsInt, IsInt, IsInt],
	0,
	function( ints, m, r, k )  

  local coll,
	ps, ds, l,
	matrices,
	G, gens,
	x, y, mats;

    coll := Collected( ints );
    Sort( coll, function( x, y ) 
		local p, q;
		  p := SmallestRootInt(x[1]);
	          q := SmallestRootInt(y[1]);
	        return p > q or ( p = q and x[1] > y[1] ); end );

    ps := List( coll, x -> x[1] ); 
    ds := List( coll, x -> x[2] );
    l := Length( ps );
    
    matrices := FpfAutGrps4( ps, ds, m, r, k );
    G := AbelianGroup( Concatenation( List( [1..l], i ->
		 			List( [1..ds[i]], x -> ps[i] ) ) ) );  
    gens := GeneratorsOfGroup( G );  

    return [List( matrices, mats -> List( mats, x ->
			 GroupHomomorphismByMatrix( G, gens, x ) ) ), G];
end );




############################################################################
##
#M  FrobeniusGroup( <phi>, <G> )
##
##  constructs the semidirect product of <G> with the fixed point free 
##  automorphism group <phi>
##


InstallMethod(
	FrobeniusGroup,
	"default",
	true,
	[IsGroup, IsGroup],
	0,
  function( phi, G )  

  local n, ipc, ipci, phipc;

    n := Size( phi );
    ipc := IsomorphismPcGroup( phi );
    phipc := Image( ipc );
    ipci := InverseGeneralMapping(ipc);

  return SemidirectProduct( phipc, ipci, G ); 
end );



