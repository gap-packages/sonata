LoadPackage("sonata");
dir := DirectoriesPackageLibrary("sonata", "tst");
TestDirectory(dir, rec(exitGAP := true,
                       testOptions := rec(compareFunction := "uptowhitespace")));

FORCE_QUIT_GAP(1);

