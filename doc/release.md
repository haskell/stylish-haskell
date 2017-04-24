# Release checklist

1.  First check if we can build against against all the newest dependencies.  If
    that's not the case, it's probably a good idea to first make a separate
    commit to bump the dependency upper bounds (and test it).

2.  Write up the `CHANGELOG`.  You can inspect the log of what changed by doing
    something like:

        git log A.B.C.D..

    Where `A.B.C.D` is the old version.

3.  Now figure out whether this is a minor or major version bump.  Follow the
    [PVP](https://pvp.haskell.org/) guidelines.  Assume the new version is
    `E.F.G.H`.

4.  Create a commit with the message `Bump version to E.F.G.H`.  This commit
    should only change two things:

    - The version number in the `.cabal` file
    - The top of the `CHANGELOG`

4.  Create a tarball using `cabal sdist` and upload this to Hackage.  If the
    upload succeeds, create an annotated git tag:

        git tag -am E.F.G.H{,}
        git push --tags
