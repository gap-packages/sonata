#############################################################################
##  
##  PackageInfo.g for the package `SONATA'                 Jürgen Ecker
##                                                         Erhard Aichinger
##                                                         Franz Binder
##                                                         Peter Mayr
##                                                         Christof Nöbauer
##
##  (created from Frank Lübeck's PackageInfo.g template file)
##  
##  This is a GAP readable file. Of course you can change and remove all
##  comments as you like.
##  
##  This file contains meta-information on the package. It is used by
##  the package loading mechanism and the upgrade mechanism for the
##  redistribution of the package via the GAP website.
##  
SetPackageInfo( rec(

PackageName := "SONATA",
Subtitle := "System of nearrings and their applications",
Version := "2.9.6",
Date := "06/12/2022", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [
  rec( 
    LastName      := "Aichinger",
    FirstNames    := "Erhard",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "erhard@algebra.uni-linz.ac.at",
    WWWHome       := "https://www.jku.at/institut-fuer-algebra/team/erhard-aichinger/",
    PostalAddress := Concatenation( [
                       "Institut für Algebra\n",
                       "Johannes Kepler Universität Linz\n",
                       "4040 Linz\n",
                       "Austria" ] ),
    Place         := "Linz",
    Institution   := "Johannes Kepler Universität Linz"
  ),
  rec( 
    LastName      := "Binder",
    FirstNames    := "Franz",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "franz.binder@algebra.uni-linz.ac.at",
    WWWHome       := "http://www.algebra.uni-linz.ac.at/~xbx/",
    PostalAddress := Concatenation( [
                       "Institut für Algebra\n",
                       "Johannes Kepler Universität Linz\n",
                       "4040 Linz\n",
                       "Austria" ] ),
    Place         := "Linz",
    Institution   := "Johannes Kepler Universität Linz"
  ),
  rec( 
    LastName      := "Ecker",
    FirstNames    := "Jürgen",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "juergen.fuss@fh-hagenberg.at",
    WWWHome       := "http://research.fh-ooe.at/en/staff/113",
    Place         := "Hagenberg",
    Institution   := "Upper Austria University of Applied Sciences"
  ),
  rec( 
    LastName      := "Mayr",
    FirstNames    := "Peter",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "baernstein@gmail.com",
    WWWHome       := "https://www.colorado.edu/math/peter-mayr",
    PostalAddress := Concatenation( [
                       "Department of Mathematics",
                       "CU Boulder\n",
                       "Boulder, Colorado",
                       "USA" ] ),
    Place         := "Boulder",
    Institution   := "University of Colorado"
  ),
  rec( 
    LastName      := "Nöbauer",
    FirstNames    := "Christof",
    IsAuthor      := true,
    IsMaintainer  := false
  ),
  
],

Status := "accepted",
CommunicatedBy := "Charles R.B. Wright (Univ. of Oregon)",
AcceptDate := "04/2003",

PackageWWWHome  := "https://gap-packages.github.io/sonata/",
README_URL      := Concatenation( ~.PackageWWWHome, "README" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
SourceRepository := rec(
    Type := "git",
    URL := "https://github.com/gap-packages/sonata",
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/sonata-", ~.Version ),
ArchiveFormats := ".tar.gz",

##  Here you  must provide a short abstract explaining the package content 
##  in HTML format (used on the package overview Web page) and an URL 
##  for a Webpage with more detailed information about the package
##  (not more than a few lines, less is ok):
##  Please, use '<span class="pkgname">GAP</span>' and
##  '<span class="pkgname">MyPKG</span>' for specifing package names.
AbstractHTML := 
   "The <span class=\"pkgname\">SONATA</span> package provides methods for \
    the construction and analysis of finite nearrings.",
     
PackageDoc := [
  rec(
  BookName  := "SONATA",
  ArchiveURLSubset := ["doc/ref","doc/htm/ref"],
  HTMLStart := "doc/htm/ref/chapters.htm",
  PDFFile   := "doc/ref/manual.pdf",
  SixFile   := "doc/ref/manual.six",
  LongTitle := "System of nearrings and their applications",
  Autoload  := true
  ),
  rec(
  BookName  := "SONATA Tutorial",
  ArchiveURLSubset := ["doc/tut","doc/htm/tut"],
  HTMLStart := "doc/htm/tut/chapters.htm",
  PDFFile   := "doc/tut/manual.pdf",
  SixFile   := "doc/tut/manual.six",
  LongTitle := "Eight easy pieces for SONATA: a SONATA tutorial",
  Autoload  := false
  ),
],


Dependencies := rec(
  GAP := ">=4.9",
  NeededOtherPackages := [],
  SuggestedOtherPackages := [["xgap",">=0"]],
  ExternalConditions := []
),

AvailabilityTest := ReturnTrue,

##  The LoadPackage mechanism can produce a default banner from the info
##  in this file. If you are not happy with it, you can provide a string
##  here that is used as a banner. GAP decides when the banner is shown and
##  when it is not shown. *optional* (note the ~-syntax in this example)
BannerString := Concatenation( 
"\n  ___________________________________________________________________________",
"\n /        ___",
"\n||       /   \\                 /\\    Version ", ~.Version,
"\n||      ||   ||  |\\    |      /  \\               /\\       Erhard Aichinger",
"\n \\___   ||   ||  |\\\\   |     /____\\_____________/__\\      Franz Binder",
"\n     \\  ||   ||  | \\\\  |    /      \\     ||    /    \\     Juergen Ecker",
"\n     ||  \\___/   |  \\\\ |   /        \\    ||   /      \\    Peter Mayr",
"\n     ||          |   \\\\|  /          \\   ||               Christof Noebauer",
"\n \\___/           |    \\|                 ||\n",
"\n System    Of   Nearrings     And      Their Applications\n",
" Info: ", ~.PackageWWWHome, "\n\n" ),


TestFile := "tst/testall.g",

Keywords := ["near ring", "endomorphism", "Frobenius group", "fixed point free automorphism"],

));

