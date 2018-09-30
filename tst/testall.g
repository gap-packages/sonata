LoadPackage("sonata");
dir := DirectoriesPackageLibrary("sonata", "tst");
TestDirectory(dir, rec(exitGAP := true));

FORCE_QUIT_GAP(1);

