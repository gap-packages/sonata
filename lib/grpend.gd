##############################################################################
##
#A  Endomorphisms ( <D> )
##
## returns a list of all endomorphisms of the domain <D>.
##               

DeclareAttribute ( "Endomorphisms", IsDomain);

##############################################################################
##
#A  Automorphisms ( <D> )
##
## returns a list of all Automorphisms of the domain <D>.
##               

DeclareAttribute ( "Automorphisms", IsDomain );

##############################################################################
##
#A  InnerAutomorphisms ( <G> )
##
## returns a list of all InnerAutomorphisms of the group <G>.
##               

DeclareAttribute ( "InnerAutomorphisms", IsGroup);



##############################################################################
##
#M  AdditiveGeneratorsForHomomorphisms( <G>, <H> )
##
## returns a list of "elementary" homomorphisms from the group <G> to
## the abelian group <H>, such that all homomorphisms from <G> to <H> can
## be obtained as sums of these
##
DeclareOperation( "AdditiveGeneratorsForHomomorphisms", [IsGroup, IsGroup and IsAbelian] );


##############################################################################
##
#M  Homomorphisms( <G>, <H> )
##
## returns a list of all homomorphisms from the group <G> into the group <H>
##
DeclareOperation( "Homomorphisms", [IsGroup, IsGroup] );


##############################################################################
##
#M  AdditiveGeneratorsForEndomorphisms( <G> )
##
## returns a list of "elementary" endomorphisms of the abelian group <G> 
## such that all endomorphisms of <G> can be obtained as sums of these
##
DeclareOperation( "AdditiveGeneratorsForEndomorphisms", [IsGroup and IsAbelian] );


#############################################################################
##
#M  NearRingGeneratorsForHomomorphisms( <G>, <H> )
##
##  returns a list of homomorphisms h from G to H and a list
##  of the respective groups of automorphisms A of H which stabilize h
##  i.e., h=h*a for a in A.
##  Each homomorphism from G to H has a unique representation h*c for some
##  h and c some coset representative for A*c in Aut( H ).
## 
DeclareOperation( "NearRingGeneratorsForHomomorphisms", [IsGroup, IsGroup] );


















