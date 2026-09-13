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

# AutoDoc notes that it builds only the first of the two books; makedoc.g
# builds the second itself, and CI treats any warning as an error.
SetInfoLevel(InfoAutoDoc, 0);

AutoDoc(rec(
    dir := "doc/ref",
    autodoc := true,
    gapdoc := true,
    extract_examples := true,
    scaffold := rec(
        includes := [
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
        TitlePage := rec(
            Copyright := [
                "Copyright &copyright; 2006 by Aichinger, E., Binder, F., Ecker, J.,",
                "Mayr, P., and Nöbauer, C., 4040 Linz, Austria",
                "<P/>",
                "SONATA is distributed as a free package for &GAP;; you can redistribute",
                "it and/or modify it under the terms of the GNU General Public License as",
                "published by the Free Software Foundation; either version 2 of the",
                "License, or (at your option) any later version.  For details, see",
                "<URL>https://www.gnu.org/licenses/gpl.html</URL>.",
                "<P/>",
                "This program is distributed in the hope that it will be useful, but",
                "WITHOUT ANY WARRANTY; without even the implied warranty of",
                "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU",
                "General Public License for more details.",
            ],
            Colophon := [
                "If you obtain SONATA please send us a short notice to that effect, e.g.,",
                "an e-mail message to <Email>sonata@algebra.uni-linz.ac.at</Email>,",
                "containing your full name and address.  This allows us to keep track of",
                "the number of SONATA users.",
                "<P/>",
                "If you publish a mathematical result that was partly obtained using",
                "SONATA, please cite SONATA, just as you would cite another paper that",
                "you used.  We would appreciate it if you could inform us about such a",
                "paper.  Also please let us know if you modify any part of SONATA.",
                "<P/>",
                "Specifically, please refer to",
                "<P/>",
                "[SONATA] Aichinger, E., Binder, F., Ecker, J., Mayr, P., and",
                "Nöbauer, C.,<Br/>",
                "SONATA --- system of near-rings and their applications, GAP package,<Br/>",
                "Institut für Algebra, Johannes Kepler Universität Linz, Austria.<Br/>",
                "<URL>https://gap-packages.github.io/sonata/</URL>",
            ],
        ),
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
