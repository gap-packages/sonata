#ReadPackage( "sonata", "lib/banner.g" ); #print banner#

ReadPackage( "sonata", "lib/grpend.gd" );	# endomorphisms for groups
ReadPackage( "sonata", "lib/grpsupp.gd" );	# other supportive functions for groups
ReadPackage( "sonata", "lib/nr.gd" );	    # near rings in general
ReadPackage( "sonata", "lib/expmulnr.gd" );	# near rings with explicit multiplication
ReadPackage( "sonata", "lib/libnr.gd" );	# library near rings
ReadPackage( "sonata", "lib/grptfms.gd" );	# transformations and transformation near rings
ReadPackage( "sonata", "lib/nrid.gd" );	    # near ring ideals
ReadPackage( "sonata", "lib/dgnr.gd" );	    # distributively generated near rings
ReadPackage( "sonata", "lib/idlatt.gd" );	# lattice operations for near ring ideals
ReadPackage( "sonata", "lib/nrconstr.gd" );	# direct products and factors
ReadPackage( "sonata", "lib/ngroups.gd" );	# N-groups
#ReadPackage( "sonata", "lib/genlatt.gd" );	# general lattices
ReadPackage( "sonata", "lib/fpf.gd" );	    # fixed point free representations and automorphism groups
ReadPackage( "sonata", "lib/nfplwd.gd" );	# planar and weakly distributive nearrings
ReadPackage( "sonata", "lib/design.gd" );	# designs
ReadPackage( "sonata", "lib/random.gd" );	# random methods for large transformation nrs
ReadPackage( "sonata", "lib/xgap.gd" );	    # graphic ideal lattices for nrs
ReadPackage( "sonata", "lib/compatible.gd" ); # nearrings of compatible functions

ReadPackage( "sonata", "lib/readgrps.g" );	# groups ala TW


ReadPackage( "sonata", "lib/grpend.gi" );
ReadPackage( "sonata", "lib/grpsupp.gi" );
ReadPackage( "sonata", "lib/nr.gi" );
ReadPackage( "sonata", "lib/expmulnr.gi" );
ReadPackage( "sonata", "lib/libnr.gi" );	# NRs w.1!
ReadPackage( "sonata", "lib/grptfms.gi" );
ReadPackage( "sonata", "lib/nrid.gi" );
ReadPackage( "sonata", "lib/dgnr.gi" );
ReadPackage( "sonata", "lib/idlatt.gi" );
ReadPackage( "sonata", "lib/nrconstr.gi" );
ReadPackage( "sonata", "lib/ngroups.gi" );
#ReadPackage( "sonata", "lib/genlatt.gi" );
ReadPackage( "sonata", "lib/fpf.gi" );
ReadPackage( "sonata", "lib/nfplwd.gi" );
ReadPackage( "sonata", "lib/design.gi" );
ReadPackage( "sonata", "lib/random.gi" );
ReadPackage( "sonata", "lib/compatible.gi" );

ReadPackage( "sonata", "lib/ring.gi" );

# test if xgap is available and running
if TestPackageAvailability("xgap","0") = true then
   ReadPackage( "sonata", "lib/xgap.gi" );	# xgap is running
fi;
