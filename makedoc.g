#############################################################################
##
##  makedoc.g
##
##  Builds the package documentation with AutoDoc/GAPDoc.
##
##  SONATA ships two books.  AutoDoc builds the first one listed in
##  PackageInfo.g, so the tutorial goes through GAPDoc directly.
##
#############################################################################

LoadPackage("AutoDoc");

AutoDoc(rec(
    dir := "doc/ref",
    autodoc := rec(scan_dirs := []),
    gapdoc := rec(main := "main", files := []),
    extract_examples := true,
    scaffold := rec(
        includes := [
            "copyrigh.xml",
            "preface.xml",
            "grpsupp.xml",
            "nr.xml",
            "libnr.xml",
            "tfms.xml",
            "tfmnr.xml",
            "ideals.xml",
            "xsonata.xml",
            "ngroups.xml",
            "fpf.xml",
            "nfplwd.xml",
            "design.xml"
        ],
        bib := "sonata.bib",
    ),
));

# The tutorial is the second book, so AutoDoc will not build it (it always
# takes PackageDoc[1]).  Drive GAPDoc directly instead.
TUT := "doc/tut";;
MakeGAPDocDoc(TUT, "main",
    ["grpsupp.xml", "nr.xml", "libnr.xml", "tfms.xml", "tfmnr.xml",
     "ideals.xml", "plnr.xml", "design.xml"],
    "SONATA Tutorial", "MathJax");;
CopyHTMLStyleFiles(TUT);;
GAPDocManualLabFromSixFile("SONATA Tutorial",
    Filename(DirectoryCurrent(), Concatenation(TUT, "/manual.six")));;

QuitGap();
