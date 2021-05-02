ghc --make -auto-all -prof Main.hs
./Main.exe +RTS -p -RTS
profiteur Main.prof