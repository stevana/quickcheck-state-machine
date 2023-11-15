## Contributing

### Release checklist

  1. Update CHANGELOG.md;
  2. Bump version in .cabal file and fix bounds;
  3. Check cabal file with `cabal check`;
  4. Make tarball with `cabal sdist`;
  5. Upload tarball as a
     package
     [candidate](https://hackage.haskell.org/packages/candidates/upload), and if
     everything looks good then release it;
  6. Git tag the version: `git tag -a v$VERSION -m "Tag v$VERISON" && git push
     origin v$VERSION`.
