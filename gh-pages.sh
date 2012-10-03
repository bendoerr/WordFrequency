# /bin/zsh

haddockOpts=--pretty-html
haddockOpts="$haddockOpts --source-base=\"https://github.com/bendoerr/WordFrequency/blob/master/src/\""
haddockOpts="$haddockOpts --source-module=\"https:/haddockOpts/github.com/bendoerr/WordFrequency/blob/master/%{FILE}\""
haddockOpts="$haddockOpts --source-entity=\"https://github.com/bendoerr/WordFrequency/blob/master/%{FILE}#L%{LINE}\""
haddockOpts="$haddockOpts --odir=\"gh-pages\""

cabal haddock \
    --executables \
    --internal \
    --html \
    --haddock-options="$haddockOpts"

cd gh-pages

git commit -a -m "Documentation update by script."
git push github gh-pages

cd ..
