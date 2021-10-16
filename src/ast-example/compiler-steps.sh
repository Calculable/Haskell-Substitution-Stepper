# For less output use:
# -dsuppress-all
# -dsuppress-idinfo

stack ghc -- Main.hs -dsuppress-timestamps -dsuppress-all -ddump-to-file -ddump-parsed-ast -ddump-rn-ast -ddump-tc-ast -ddump-ds-preopt -ddump-stg -ddump-cmm-raw -ddump-asm-native 

rm Main.hi Main.o
